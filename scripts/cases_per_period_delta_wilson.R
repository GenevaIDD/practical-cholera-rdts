# Calculates the number of cases at the weekly, monthly or annual scale when period is set to "week", "month" or "year", respectively.
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- x / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half <- (z / denom) * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  c(lo = max(0, center - half), hi = min(1, center + half))
}

cases_per_period <- function(data, period, truth_data, S = 20000) {
  message("Data should have column 'date' for week of arrival")
  # The section below ensures that all calendar weeks/months/years represented
  # in the exhaustive sampling dataset will be useful when determining consecutive
  # weeks later on ----


  # Reference weekly grid per site, crossed with all under5 levels
  weeks_ref <- truth_data %>%
    filter(time == "week") %>%
    group_by(data_source, site) %>%
    summarize(
      min_week = min(date, na.rm = TRUE),
      max_week = max(date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(date = list(seq(min_week, max_week, by = "1 week"))) %>%
    unnest(date) %>%
    select(data_source, site, date) %>%
    crossing(under5 = 0:2) %>%
    mutate(time = "week")

  # Months/years derived from the weekly grid
  months_ref <- weeks_ref %>%
    transmute(data_source, under5, site,
      time = "month",
      date = floor_date(date, "month")
    ) %>%
    distinct()

  years_ref <- weeks_ref %>%
    transmute(data_source, under5, site,
      time = "year",
      date = floor_date(date, "year")
    ) %>%
    distinct()

  # Fill in gaps in truth_data
  truth_data <- bind_rows(weeks_ref, months_ref, years_ref) %>%
    mutate(year = floor_date(date, "year")) %>%
    arrange(data_source, time, date, under5) %>%
    left_join(truth_data) %>%
    mutate(n.suspected = if_else(is.na(n.suspected), 0, n.suspected))

  # Aggregate and calculate RDT-positivity at the specified period for each
  # age group separately and overall----
  if (period %in% c("month", "year")) {
    data <- data %>% mutate(date = floor_date(date, period))
  }
  # Each separate age group
  period.data <- data %>%
    count(data_source, under5, rdt_result, date, year, site, name = "n.tested") %>%
    group_by(data_source, under5, date, year, site) %>%
    summarize(
      positivity = sum(n.tested[rdt_result == "Positive"], na.rm = T) / sum(n.tested),
      n.positive = sum(n.tested[rdt_result == "Positive"], na.rm = T),
      n.tested = sum(n.tested)
    ) %>%
    ungroup()
  # Overall
  period.data <- data %>%
    count(data_source, rdt_result, date, year, site, name = "n.tested") %>%
    group_by(data_source, date, year, site) %>%
    summarize(
      positivity = sum(n.tested[rdt_result == "Positive"], na.rm = T) / sum(n.tested),
      n.positive = sum(n.tested[rdt_result == "Positive"], na.rm = T),
      n.tested = sum(n.tested),
      under5 = 2
    ) %>%
    ungroup() %>%
    bind_rows(period.data) %>%
    mutate(time = period) %>%
    right_join(
      truth_data %>% filter(time == period) %>% mutate(truth.positivity = n.cases / n.suspected) %>% select(truth = n.cases, data_source, date, under5, time, n.suspected, year, site, truth.positivity)
    ) %>%
    arrange(date, under5) %>%
    mutate(missing = if_else(is.na(n.tested), TRUE, FALSE))

  # Estimate RDT positivity for a particular period when no suspected cases
  # were tested during that period: -----
  # In summary, missing data is estimated as either (i) the average in either
  # the preceding four weeks or (ii) the average in the preceding 365 days if
  # data is missing for at least than 4 consecutive weeks.
  # In the code immediately below, the number of consecutive weeks with missing
  # data are counted and then used to apply the appropriate approach (either i or ii)
  if (any(period.data$missing)) {
    placeholder <- tibble()

    for (i in unique(period.data$data_source)) {
      for (j in unique(period.data$site[period.data$data_source == i])) {
        temp <- period.data %>% filter(data_source == i & site == j)

        rle.0 <- rle(temp[temp$under5 == 0, ]$missing)
        rle.1 <- rle(temp[temp$under5 == 1, ]$missing)
        rle.2 <- rle(temp[temp$under5 == 2, ]$missing)

        placeholder <- bind_rows(
          temp %>% filter(under5 == 0) %>% mutate(
            consecutive = rep(rle.0$lengths, rle.0$lengths),
            consecutive_group = rep(1:length(rle.0$lengths), rle.0$lengths)
          ),
          temp %>% filter(under5 == 1) %>% mutate(
            consecutive = rep(rle.1$lengths, rle.1$lengths),
            consecutive_group = rep(1:length(rle.1$lengths), rle.1$lengths)
          ),
          temp %>% filter(under5 == 2) %>% mutate(
            consecutive = rep(rle.2$lengths, rle.2$lengths),
            consecutive_group = rep(1:length(rle.2$lengths), rle.2$lengths)
          )
        ) %>%
          bind_rows(placeholder)
      }
    }

    period.data <- placeholder %>%
      arrange(under5, site, date) %>%
      group_by(under5, site, data_source, consecutive_group) %>%
      mutate(
        consecutive_order = 1,
        consecutive_order = cumsum(consecutive_order),
        missing.year = missing & consecutive >= 4, # ie, missing data for at least 4 consecutive weeks; signal to impute missing positivity as the average over the past 365 days
        missing.month = missing & consecutive < 4
      ) %>% # ie, missing data for less 4 consecutive weeks; signal to impute missing positivity as the average over the preceding 4 weeks
      ungroup()


    # drops those dates without any observations even with exhaustive sampling
    # because we would not be able to estimate bias for those weeks
    period.data <- period.data %>%
      filter(!is.na(truth)) %>%
      tibble::rowid_to_column()

    missing.rows.year <- period.data$rowid[period.data$missing.year]
    missing.rows.month <- period.data$rowid[period.data$missing.month]

    # So we only build our averages on collected data
    impute.data <- period.data
    # Estimate weekly positivity as average over the past year if we do not have data in the past 4 weeks
    if (length(missing.rows.year) > 0) {
      for (j in 1:length(missing.rows.year)) {
        temp <- period.data[period.data$rowid < missing.rows.year[j] &
          period.data$missing == FALSE &
          period.data$date >= period.data[missing.rows.year[j], ]$date - 365 &
          period.data$under5 == period.data[missing.rows.year[j], ]$under5 &
          period.data$data_source == period.data[missing.rows.year[j], ]$data_source &
          period.data$site == period.data[missing.rows.year[j], ]$site, ]

        if (nrow(temp) > 0) {
          temp <- temp %>%
            summarize(
              n.positive = sum(n.positive, na.rm = T),
              n.tested = sum(n.tested, na.rm = T),
              positivity = n.positive / n.tested
            )

          impute.data[impute.data$rowid == missing.rows.year[j], ] <- impute.data[impute.data$rowid == missing.rows.year[j], ] %>%
            mutate(temp)
        }
      }
    }

    # Estimate weekly positivity as average over last month if we have data in the preceding 4 weeks
    if (length(missing.rows.month) > 0) {
      for (j in 1:length(missing.rows.month)) {
        temp <- period.data[period.data$rowid < missing.rows.month[j] &
          period.data$missing == FALSE &
          period.data$date >= period.data[missing.rows.month[j], ]$date - weeks(4) &
          period.data$under5 == period.data[missing.rows.month[j], ]$under5 &
          period.data$data_source == period.data[missing.rows.month[j], ]$data_source &
          period.data$site == period.data[missing.rows.month[j], ]$site, ]
        if (nrow(temp) > 0) {
          temp <- temp %>%
            summarize(
              n.positive = sum(n.positive, na.rm = T),
              n.tested = sum(n.tested, na.rm = T),
              positivity = n.positive / n.tested
            )

          impute.data[impute.data$rowid == missing.rows.month[j], ] <- impute.data[impute.data$rowid == missing.rows.month[j], ] %>%
            mutate(temp)
        }
      }
    }
  } else {
    impute.data <- period.data
  }
  # When RDT positivity was unavailable for one facility in a given time period,
  # we imputed it using the corresponding RDT positivity from the other facility.
  # Ultimately, this only affected the age-specific age group estimates (under5 == 0 or 1)
  impute.data <- impute.data %>%
    group_by(data_source, date, under5, time) %>%
    mutate(
      positivity = if_else(is.na(positivity) & !is.na(sum(positivity)), max(positivity), positivity),
      positivity = if_else(is.infinite(positivity), NA_real_, positivity)
    )

  impute.summed <- impute.data %>%
    group_by(data_source, date, year, under5, time) %>%
    group_modify(~ {
      df <- .x

      N_catchment <- sum(df$n.suspected, na.rm = TRUE)

      # Totals across sites in catchment area
      catchment <- tibble(
        n.tested = sum(df$n.tested, na.rm = TRUE),
        n.positive = sum(df$n.positive, na.rm = TRUE),
        n.suspected = N_catchment,
        truth = sum(df$truth, na.rm = TRUE)
      )

      # Return empty dataframe if no suspected cases or no rows
      if (is.na(N_catchment) || N_catchment <= 0) {
        return(catchment %>%
          mutate(
            n.cases = NA_real_, n.cases.lo = NA_real_, n.cases.hi = NA_real_,
            positivity = NA_real_, positivity.lo = NA_real_, positivity.hi = NA_real_,
            truth.positivity = NA_real_, n.catchment = N_catchment,
            y_draw = list(integer(0)), pbar_draw = list(numeric(0))
          ))
      }

      # Can only simulate if we have tested numbers
      ok_sim <- which(!is.na(df$n.tested) & df$n.tested > 0 &
        !is.na(df$n.suspected) & df$n.suspected > 0)

      if (length(ok_sim) == 0) {
        return(catchment %>%
          mutate(
            n.cases = NA_real_, n.cases.lo = NA_real_, n.cases.hi = NA_real_,
            positivity = NA_real_, positivity.lo = NA_real_, positivity.hi = NA_real_,
            truth.positivity = catchment$truth / N_catchment, n.catchment = N_catchment,
            y_draw = list(integer(0)), pbar_draw = list(numeric(0))
          ))
      }

      # point estimate: MLE
      N_used <- sum(df$n.suspected[ok_sim], na.rm = T)

      phat_site_mle <- df$n.positive[ok_sim] / df$n.tested[ok_sim]
      phat_mle <- sum(df$n.suspected[ok_sim] * phat_site_mle, na.rm = T) / N_used
      n_cases_mle <- phat_mle * N_used

      # Delta method var for cases
      alpha <- 0.05
      z <- qnorm(1 - alpha / 2)
      var_total_cases <- 0

      for (site_id in seq_along(ok_sim)) {
        j <- ok_sim[site_id]
        x_positive <- df$n.positive[j]
        N_tested <- df$n.tested[j]
        N_suspected <- df$n.suspected[j]

        # MLE site positivity
        phat_j <- x_positive / N_tested

        # wilson interval w/ approx SE
        ci <- wilson_ci(x_positive, N_tested, conf = 1 - alpha)
        se_p <- (ci["hi"] - ci["lo"]) / (2 * z)
        var_p <- se_p^2

        # law total var
        # var(y_j) = N_sus * p*(1-p) + N_sus^2 * Var(p)
        var_j <- N_suspected * phat_j * (1 - phat_j) + (N_suspected^2) * var_p
        var_total_cases <- var_total_cases + var_j
      }

      # cases var
      se_total_cases <- sqrt(var_total_cases)

      n_cases_lo <- n_cases_mle - z * se_total_cases
      n_cases_lo <- max(0, n_cases_lo)

      n_cases_hi <- n_cases_mle + z * se_total_cases
      n_cases_hi <- min(N_used, n_cases_hi)

      # pooled positivity
      positivity_lo <- n_cases_lo / N_used
      positivity_hi <- n_cases_hi / N_used

      catchment %>%
        mutate(
          n.cases = n_cases_mle,
          n.cases.lo = n_cases_lo,
          n.cases.hi = n_cases_hi,
          positivity = phat_mle,
          positivity.lo = positivity_lo,
          positivity.hi = positivity_hi,
          truth.positivity = catchment$truth / N_catchment,
          var.cases = var_total_cases,
          n.catchment = N_catchment
        )
    }) %>%
    ungroup()

  impute.summed <- impute.summed %>%
    select(
      data_source, date, truth, year, starts_with("n.cases"), under5,
      time, n.tested, n.suspected, n.positive, starts_with("positivity"),
      truth.positivity, var.cases,
      n.catchment
    )

  return(impute.summed)
}
