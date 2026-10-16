# Exploratory analysis: CSV row order within facility-day is assumed arrival order.
# No resorting by record ID or timestamps before assigning this rank.
listed_arrival_diagnostics <- function(data, caps = c(1, 2, 3, 5)) {
  d <- data %>% mutate(listed_row = row_number(), y = as.numeric(rdt_result == 'Positive')) %>%
    group_by(data_source, site, date_arrival) %>%
    mutate(listed_rank = row_number(), day_cases = n()) %>% ungroup()
  bind_rows(lapply(caps, function(k) {
    bind_rows(lapply(unique(d$data_source), function(src) {
      all <- filter(d, data_source == src)
      dd <- filter(all, day_cases > k) %>% mutate(first = listed_rank <= k)
      days <- dd %>% group_by(site, date_arrival) %>%
        summarise(first_pos = sum(y[first]), first_neg = sum(1-y[first]),
          later_pos = sum(y[!first]), later_neg = sum(1-y[!first]), .groups = 'drop')
      informative <- with(days, first_pos + later_pos > 0 & first_neg + later_neg > 0)
      info <- days[informative, ]
      # Rows: first/later; columns: positive/negative. OR > 1 means first more positive.
      tab <- array(0, dim = c(2,2,nrow(info)))
      for (j in seq_len(nrow(info))) tab[,,j] <- matrix(unlist(info[j,c('first_pos','later_pos','first_neg','later_neg')]), nrow=2)
      fit <- mantelhaen.test(tab, correct = FALSE)
      selected <- all$listed_rank <= k
      flag <- all[[paste0('sample_first_', k, '_cases')]] == 1
      tibble(data_source = src, first_n = k, compared_days = nrow(days),
        informative_days = nrow(info), first_patients = sum(dd$first), later_patients = sum(!dd$first),
        positivity_first = 100 * mean(dd$y[dd$first]), positivity_later = 100 * mean(dd$y[!dd$first]),
        within_day_difference_pp = 100 * mean(with(days, first_pos / (first_pos+first_neg) - later_pos / (later_pos+later_neg))),
        odds_ratio = unname(fit$estimate), lower = fit$conf.int[1], upper = fit$conf.int[2], p = fit$p.value,
        tests = sum(selected), selection_disagreements = sum(selected != flag),
        selected_positives = sum(all$y[selected]), original_positives = sum(all$y[flag]))
    }))
  }))
}

listed_arrival_performance <- function(data, caps = c(1,2,3,5)) {
  # bm_prepare orders by date, then record_id (arrival order): replace the latter locally with row index.
  # This modifies only the in-memory copy, not source data or the main analysis.
  data$record_id <- seq_len(nrow(data))
  units <- split(data, interaction(data$data_source, data$site, data$year, drop=TRUE))
  bind_rows(lapply(units, function(d) {
    z <- bm_prepare(d)
    bind_rows(lapply(caps, function(k) {
      s <- bm_sample(z, k, 'daily_first')
      bm_metrics(z, s, initial_priors = 0.2) %>% filter(estimator == 'weekly_prior_0.2') %>%
        mutate(data_source = d$data_source[1], site = d$site[1], year = d$year[1], first_n = k)
    }))
  })) %>% group_by(data_source, first_n) %>%
    summarise(annual_bias_cases = mean(annual_error), annual_rmse_cases = sqrt(mean(annual_error^2)),
      weekly_rmse_cases = sqrt(sum(weekly_squared_error)/sum(weeks)),
      positivity_rmse_pp = 100*sqrt(sum(positivity_squared_error)/sum(weeks)), .groups='drop')
}
