# Functions used by notebooks/sampling_schemes_note.qmd
# Everything operates on the RDT-tested suspected cases in data/sampled_data.csv,
# the universe the main simulation samples from, so the truth is known exactly.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
})

# Strategies shown in the note (the main-text Figure 2 set)
note_strategies <- tibble::tribble(
  ~col,                         ~label,        ~family,  ~size,
  "sample_every2",              "Every n=2",   "every",  2,
  "sample_every5",              "Every n=5",   "every",  5,
  "sample_every10",             "Every n=10",  "every",  10,
  "sample_first_1_cases",       "Daily n=1",   "daily",  1,
  "sample_first_2_cases",       "Daily n=2",   "daily",  2,
  "sample_first_3_cases",       "Daily n=3",   "daily",  3,
  "sample_first_5_cases",       "Daily n=5",   "daily",  5,
  "sample_first_1_cases_week",  "Weekly n=1",  "weekly", 1,
  "sample_first_2_cases_week",  "Weekly n=2",  "weekly", 2,
  "sample_first_3_cases_week",  "Weekly n=3",  "weekly", 3,
  "sample_first_10_cases_week", "Weekly n=10", "weekly", 10
)

# Same strategies under their names in data/case_data_delta.csv
main_text_labels <- c(
  "First n daily (n=1)", "First n daily (n=2)", "First n daily (n=3)", "First n daily (n=5)",
  "First n weekly (n=1)", "First n weekly (n=2)", "First n weekly (n=3)", "First n weekly (n=10)",
  "Every n (n=2)", "Every n (n=5)", "Every n (n=10)"
)

# RDT-tested cases with outcome and arrival index per facility (same ordering as everyn())
load_tested <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(y = as.numeric(rdt_result == "Positive")) %>%
    group_by(data_source, site) %>%
    arrange(date_arrival, record_id, .by_group = TRUE) %>%
    mutate(k = row_number()) %>%
    ungroup()
}

# Facility-week counts for one sampling indicator s
facility_weeks <- function(sd, s) {
  sd %>%
    mutate(s = s) %>%
    group_by(data_source, site, year, week) %>%
    summarise(N = n(), m = sum(s), x = sum(s * y), tx = sum(y), .groups = "drop")
}

# Per facility-year diagnostics: weekly vs pooled estimators, rho, kappa, VIF, MSE comparison.
# Weeks with no tested case are dropped (the weekly estimator is undefined there).
fy_diagnostics <- function(sd, strategies) {
  pmap_dfr(strategies, function(col, label, family, size, ...) {
    facility_weeks(sd, sd[[col]]) %>%
      filter(m > 0) %>%
      group_by(data_source, site, year) %>%
      summarise({
        n_tot <- sum(N); m_tot <- sum(m); w <- N / n_tot; f <- m / N
        p_obs <- x / m; p_true <- tx / N
        ef <- sum(w * f); ep <- sum(w * p_true); ep_obs <- sum(w * p_obs)
        sd_f <- sqrt(sum(w * (f - ef)^2)); sd_p <- sqrt(sum(w * (p_true - ep)^2))
        cov_true <- sum(w * (f - ef) * (p_true - ep))
        cov_obs <- sum(w * (f - ef) * (p_obs - ep_obs))
        rho <- if (sd_f > 0 && sd_p > 0) cov_true / (sd_f * sd_p) else 0
        kappa <- sd_p^2 / (ep * (1 - ep))
        v_case <- p_true * (1 - p_true) * (1 - f)           # per-case variance under within-week SRS
        var_weekly <- sum(N * v_case / f)
        var_pooled <- (n_tot^2 / m_tot^2) * sum(m * v_case)
        bias_pooled <- n_tot * cov_true / ef
        y_weekly <- sum(N * p_obs); y_pooled <- n_tot * sum(x) / m_tot
        tibble(
          N_year = n_tot, M = m_tot, rho = rho, kappa = kappa, VIF = ef * sum(w / f),
          rel_diff = (y_pooled - y_weekly) / y_weekly,
          identity_err = abs((y_pooled - y_weekly) - n_tot * cov_obs / ef),
          sd_weekly = sqrt(var_weekly), bias_pooled = bias_pooled,
          rmse_pooled = sqrt(bias_pooled^2 + var_pooled),
          M0 = 1 / (rho^2 * kappa)
        )
      }, .groups = "drop") %>%
      mutate(pooled_wins = rmse_pooled < sd_weekly, rule_pooled = M < M0,
             strategy = label, family = family, size = size)
  })
}

# Error of the weekly-summed estimate split into between-day weighting (B),
# within-day selection (C) and unsampled days (D), summed over facility-weeks
weekly_error_decomp <- function(sd, strategies) {
  pmap_dfr(strategies, function(col, label, family, size, ...) {
    sd %>%
      mutate(s = .data[[col]]) %>%
      group_by(data_source, site, year, week, date_arrival) %>%
      summarise(Nd = n(), md = sum(s), xd = sum(s * y), td = sum(y), .groups = "drop") %>%
      group_by(data_source, site, year, week) %>%
      filter(sum(md) > 0) %>%
      summarise(
        Nw = sum(Nd),
        p_hat = sum(xd) / sum(md),
        p_hat_day = sum(Nd[md > 0] * xd[md > 0] / md[md > 0]) / sum(Nd[md > 0]),
        p_sampled_days = sum(td[md > 0]) / sum(Nd[md > 0]),
        p_true = sum(td) / Nw,
        .groups = "drop"
      ) %>%
      group_by(data_source) %>%
      summarise(
        B = sum(Nw * (p_hat - p_hat_day)),
        C = sum(Nw * (p_hat_day - p_sampled_days)),
        D = sum(Nw * (p_sampled_days - p_true)),
        total = sum(Nw * (p_hat - p_true)),
        truth = sum(Nw * p_true),
        .groups = "drop"
      ) %>%
      mutate(pct = 100 * total / truth, strategy = label, family = family, size = size)
  })
}

# Tests allocated to the busiest vs quietest third of catchment-weeks
allocation <- function(sd, strategies) {
  pmap_dfr(strategies, function(col, label, ...) {
    sd %>%
      mutate(s = .data[[col]]) %>%
      group_by(data_source, week) %>%
      summarise(N = n(), m = sum(s), .groups = "drop") %>%
      group_by(data_source) %>%
      mutate(tertile = ntile(N, 3)) %>%
      summarise(
        caseload_ratio = median(N[tertile == 3]) / median(N[tertile == 1]),
        test_ratio = median(m[tertile == 3]) / max(median(m[tertile == 1]), 0.5),
        quiet_median_tests = median(m[tertile == 1]),
        quiet_pct_empty = 100 * mean(m[tertile == 1] == 0),
        .groups = "drop"
      ) %>%
      mutate(strategy = label)
  })
}

# Every-n over all possible start positions: does m_w vary, can weeks be empty,
# and what is the design bias of the weekly positivity (ratio) estimator?
everyn_start_bias <- function(sd, every) {
  map_dfr(seq_len(every), function(r) {
    sd %>%
      mutate(s = as.numeric((k - r) %% every == 0)) %>%
      group_by(data_source, site, week) %>%
      summarise(N = n(), m = sum(s), x = sum(s * y), tx = sum(y), .groups = "drop") %>%
      mutate(start = r)
  }) %>%
    group_by(data_source, site, week) %>%
    summarise(
      N = first(N), tx = first(tx),
      spread = max(m) - min(m), any_empty = any(m == 0),
      e_phat = if (all(m > 0)) mean(x / m) else NA_real_,
      ht_err = abs(mean(every * x) - first(tx)),
      .groups = "drop"
    ) %>%
    group_by(data_source) %>%
    summarise(
      pct_weeks_vary = 100 * mean(spread > 0),
      pct_weeks_can_be_empty = 100 * mean(any_empty),
      design_bias_pct = 100 * sum((N * e_phat - tx)[!any_empty]) / sum(tx[!any_empty]),
      ht_max_err = max(ht_err),
      .groups = "drop"
    ) %>%
    mutate(every = every)
}

# First k arrivals of a facility-day vs later arrivals: pooled comparison and
# Mantel-Haenszel test stratified by facility-day (days with more than k cases)
arrival_order <- function(sd, first_k) {
  ranked <- sd %>%
    arrange(date_arrival, record_id) %>%
    group_by(data_source, site, date_arrival) %>%
    mutate(rank = row_number(), K = n()) %>%
    ungroup()
  map_dfr(unique(ranked$data_source), function(src) {
    dd <- ranked %>%
      filter(data_source == src, K > first_k) %>%
      mutate(first = rank <= first_k, fd = paste(site, date_arrival))
    tab <- xtabs(~ first + y + fd, data = dd)
    keep <- apply(tab, 3, function(s) sum(s) > 1 & all(rowSums(s) > 0) & all(colSums(s) > 0))
    cmh <- mantelhaen.test(tab[, , keep], exact = FALSE)
    within <- dd %>% group_by(fd) %>% summarise(d = mean(y[first]) - mean(y[!first]), .groups = "drop")
    tibble(
      data_source = src, first_k = first_k,
      p_first = 100 * mean(dd$y[dd$first]), p_rest = 100 * mean(dd$y[!dd$first]),
      pooled_p = suppressWarnings(chisq.test(table(dd$first, dd$y))$p.value),
      within_diff_pp = 100 * mean(within$d),
      or = unname(cmh$estimate), lo = cmh$conf.int[1], hi = cmh$conf.int[2],
      cmh_p = cmh$p.value, facility_days = sum(keep)
    )
  })
}

weekday_positivity <- function(sd) {
  sd %>%
    mutate(day = wday(date_arrival, label = TRUE),
           day_full = as.character(wday(date_arrival, label = TRUE, abbr = FALSE))) %>%
    group_by(data_source, day, day_full) %>%
    summarise(positivity = 100 * mean(y), .groups = "drop")
}

# Between-week dispersion of true positivity per facility-year
kappa_by_facility_year <- function(sd) {
  sd %>%
    group_by(data_source, site, year, week) %>%
    summarise(N = n(), tx = sum(y), .groups = "drop") %>%
    group_by(data_source, site, year) %>%
    summarise({
      w <- N / sum(N); p <- tx / N; ep <- sum(w * p)
      tibble(pbar = ep, kappa = sum(w * (p - ep)^2) / (ep * (1 - ep)))
    }, .groups = "drop")
}

# Correlation between weekly caseload and weekly positivity (catchment-weeks)
caseload_positivity_cor <- function(sd) {
  sd %>%
    group_by(data_source, week) %>%
    summarise(N = n(), p = mean(y), .groups = "drop") %>%
    group_by(data_source) %>%
    summarise(weeks = n(), pearson = cor(N, p), spearman = cor(N, p, method = "spearman"),
              .groups = "drop")
}

# Latent-scale ICC of positivity within facility-days (random-intercept logistic model)
icc_facility_day <- function(sd) {
  sd %>%
    mutate(fd = paste(site, date_arrival)) %>%
    group_by(data_source) %>%
    group_modify(~ {
      fit <- lme4::glmer(y ~ 1 + (1 | fd), data = .x, family = binomial)
      v <- as.numeric(lme4::VarCorr(fit)$fd)
      tibble(icc = v / (v + pi^2 / 3))
    }) %>%
    ungroup()
}

# --- Quantities taken from the main analysis output (data/case_data_delta.csv) ---

severe_annual_bias <- function(cd) {
  cd %>%
    filter(strategy == "Severe cases only", aggregate, under5 == 2, !is.na(bias.ir)) %>%
    group_by(data_source) %>%
    summarise(lo = min(bias.ir), hi = max(bias.ir), .groups = "drop")
}

weekly_ci_coverage <- function(cd, strategy_name) {
  cd %>%
    filter(strategy == strategy_name, time == "week", !aggregate, under5 == 2) %>%
    group_by(data_source) %>%
    summarise(coverage = 100 * mean(within.ir, na.rm = TRUE), .groups = "drop")
}

# Median percent reduction in absolute annual bias from summing weekly estimates
# instead of applying one annual positivity (as in Supplemental Figure 3)
aggregation_reduction <- function(cd, ba, labels) {
  ba %>%
    left_join(
      cd %>%
        filter(time == "year", period == "annual", under5 == 2, !aggregate, !age_strat) %>%
        distinct(data_source, date, year, time, period, under5, strategy, bias.ir),
      by = c("data_source", "date", "year", "time", "period", "under5", "strategy")
    ) %>%
    filter(strategy %in% labels) %>%
    mutate(family = case_when(
      grepl("daily", strategy) ~ "daily",
      grepl("weekly", strategy) ~ "weekly",
      TRUE ~ "every"
    )) %>%
    group_by(data_source, family) %>%
    summarise(median_reduction = 100 * median(-abs.bias.aggregate.ir / abs(bias.ir)), .groups = "drop")
}
