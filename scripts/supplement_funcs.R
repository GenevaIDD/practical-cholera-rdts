# Functions used by notebooks/technical_supplement.qmd
# Builds on scripts/sampling_theory_funcs.R (load_tested, facility_weeks, fy_diagnostics, ...).
# Unless noted, the universe is RDT-tested suspected cases in data/sampled_data.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
})

# Strategies used in the supplement: main-text every n, daily n and weekly n
supp_strategies <- tibble::tribble(
  ~col,                         ~label,        ~family,  ~size,
  "sample_every2",              "Every n=2",   "every",  2,
  "sample_every5",              "Every n=5",   "every",  5,
  "sample_every10",             "Every n=10",  "every",  10,
  "sample_first_1_cases",       "Daily n=1",   "daily",  1,
  "sample_first_3_cases",       "Daily n=3",   "daily",  3,
  "sample_first_5_cases",       "Daily n=5",   "daily",  5,
  "sample_first_1_cases_week",  "Weekly n=1",  "weekly", 1,
  "sample_first_3_cases_week",  "Weekly n=3",  "weekly", 3,
  "sample_first_10_cases_week", "Weekly n=10", "weekly", 10
)

# Case-weighted correlation
wcor <- function(a, b, w) {
  k <- is.finite(a) & is.finite(b) & w > 0
  if (sum(k) < 3) return(NA_real_)
  w <- w[k] / sum(w[k]); a <- a[k]; b <- b[k]
  ca <- a - sum(w * a); cb <- b - sum(w * b)
  den <- sqrt(sum(w * ca^2) * sum(w * cb^2))
  if (den == 0) NA_real_ else sum(w * ca * cb) / den
}

# ---- Section 3: sampling fraction and positivity ---------------------------

# Per facility-year: correlation of weekly caseload with weekly positivity
caseload_positivity_fy <- function(sd) {
  sd %>%
    group_by(data_source, site, year, week) %>%
    summarise(N = n(), p = mean(y), .groups = "drop") %>%
    group_by(data_source, site, year) %>%
    summarise(weeks = n(), cases = sum(N), r_caseload = cor(N, p, method = "spearman"),
              .groups = "drop")
}

# Weekly tested fraction vs positivity in the routine Uvira data, where not every
# suspected case received an RDT (all Sitakunda cases in the clinical file were tested)
observed_testing_fy <- function(clinical) {
  clinical %>%
    filter(!is.na(site)) %>%
    mutate(year = floor_date(week, "year")) %>%
    group_by(data_source, site, year, week) %>%
    summarise(N = n(), m = sum(rdt_result %in% c("Positive", "Negative")),
              x = sum(rdt_result %in% "Positive"), .groups = "drop") %>%
    group_by(data_source, site, year) %>%
    summarise(
      suspected = sum(N), tested_pct = 100 * sum(m) / sum(N),
      weeks = n(), untested_weeks = sum(m == 0),
      r_fp = wcor(m / N, x / m, N),
      r_caseload_f = suppressWarnings(cor(N, m / N, method = "spearman")),
      .groups = "drop"
    )
}

# ---- Section 4: decomposition of error -------------------------------------

# Realized error of the pooled estimate, split into
#   A  pooled minus weekly-summed (cross-week weighting)
#   B  between-day weighting within a week
#   C  within-day selection
#   D  unsampled days within sampled weeks
#   E  positives in weeks with no test (omitted by both estimators here)
# Per facility-year; A + B + C + D + E = pooled estimate minus truth
error_decomposition <- function(sd, strategies) {
  pmap_dfr(strategies, function(col, label, family, size, ...) {
    sd %>%
      mutate(s = .data[[col]]) %>%
      group_by(data_source, site, year, week, date_arrival) %>%
      summarise(Nd = n(), md = sum(s), xd = sum(s * y), td = sum(y), .groups = "drop") %>%
      group_by(data_source, site, year, week) %>%
      summarise(
        Nw = sum(Nd), mw = sum(md), xw = sum(xd), tw = sum(td),
        p_hat = if (mw > 0) xw / mw else NA_real_,
        p_hat_day = if (mw > 0) sum((Nd * xd / pmax(md, 1))[md > 0]) / sum(Nd[md > 0]) else NA_real_,
        p_plus = if (mw > 0) sum(td[md > 0]) / sum(Nd[md > 0]) else NA_real_,
        # Variance of the week's contribution to term C if the tested cases on each
        # sampled day were a simple random sample of that day's cases (finite
        # population correction), holding fixed which days were sampled and m_d.
        var_C = if (mw > 0) (Nw / sum(Nd[md > 0]))^2 *
          sum((Nd^2 * ifelse(Nd > 1, (td / Nd) * (1 - td / Nd) / md * (Nd - md) / (Nd - 1), 0))[md > 0]) else 0,
        .groups = "drop"
      ) %>%
      group_by(data_source, site, year) %>%
      summarise({
        ok <- mw > 0
        y_weekly <- sum((Nw * p_hat)[ok])
        y_pooled <- sum(Nw[ok]) * sum(xw) / sum(mw)
        tibble(
          truth = sum(tw),
          A = y_pooled - y_weekly,
          B = sum((Nw * (p_hat - p_hat_day))[ok]),
          C = sum((Nw * (p_hat_day - p_plus))[ok]),
          D = sum((Nw * (p_plus - tw / Nw))[ok]),
          E = -sum(tw[!ok]),
          pooled_error = y_pooled - sum(tw),
          weekly_error = y_weekly - sum(tw),
          var_C = sum(var_C[ok])
        )
      }, .groups = "drop") %>%
      mutate(strategy = label, family = family, size = size)
  })
}

# ---- Section 5: first n arrivals -------------------------------------------

# Rank of each case within its facility-day and facility-week in recorded order
# (Sitakunda: record timestamp; Uvira: record order, as timestamps carry no time of day)
rank_arrivals <- function(sd) {
  sd %>%
    arrange(date_arrival, record_id) %>%
    group_by(data_source, site, date_arrival) %>%
    mutate(rank_day = row_number(), n_day = n()) %>%
    group_by(data_source, site, week) %>%
    mutate(rank_week = row_number(), n_week = n()) %>%
    ungroup()
}

# First n vs later cases within the same facility-day or facility-week.
# Crude positivity pools across periods; the Mantel-Haenszel OR and the mean
# within-period difference compare cases in the same period only.
first_n_comparison <- function(ranked, unit = c("day", "week"), ns = c(1, 2, 3, 5), var = "y") {
  unit <- match.arg(unit)
  rk <- paste0("rank_", unit); nk <- paste0("n_", unit)
  per <- if (unit == "day") "date_arrival" else "week"
  expand_grid(src = unique(ranked$data_source), first_n = ns) %>%
    pmap_dfr(function(src, first_n) {
      dd <- ranked %>%
        filter(data_source == src, .data[[nk]] > first_n) %>%
        mutate(first = .data[[rk]] <= first_n, stratum = paste(site, .data[[per]]),
               v = .data[[var]])
      tab <- table(dd$first, dd$v, dd$stratum)
      keep <- apply(tab, 3, function(s) all(rowSums(s) > 0) && all(colSums(s) > 0))
      mh <- mantelhaen.test(tab[, , keep, drop = FALSE], exact = FALSE, correct = FALSE)
      within <- dd %>%
        group_by(stratum) %>%
        summarise(d = mean(v[first]) - mean(v[!first]), .groups = "drop")
      # rows first = FALSE/TRUE, columns v = 0/1: the MH estimate is the odds ratio
      # of v = 1 for the first n relative to later cases
      tibble(
        data_source = src, unit = unit, first_n = first_n, var = var,
        periods = n_distinct(dd$stratum), informative = sum(keep),
        first_cases = sum(dd$first), later_cases = sum(!dd$first),
        pct_first = 100 * mean(dd$v[dd$first]), pct_later = 100 * mean(dd$v[!dd$first]),
        within_diff_pp = 100 * mean(within$d),
        or = unname(mh$estimate), lo = mh$conf.int[1], hi = mh$conf.int[2],
        p = mh$p.value
      )
    })
}

# Positivity by position within busy facility-days (at least min_cases cases)
positivity_by_rank <- function(ranked, min_cases = 6, top = 6) {
  ranked %>%
    filter(n_day >= min_cases) %>%
    mutate(position = pmin(rank_day, top)) %>%
    group_by(data_source, position) %>%
    summarise(cases = n(), positive = sum(y), .groups = "drop") %>%
    mutate(
      pos = 100 * positive / cases,
      lo = 100 * qbeta(0.025, positive, cases - positive + 1),
      hi = 100 * qbeta(0.975, positive + 1, cases - positive)
    )
}

# ---- Section 6: budget-matched comparison ----------------------------------

bm_labels <- c(
  annual_srs = "Annual random", annual_systematic = "Annual systematic",
  daily_first = "Daily first", daily_random = "Daily random",
  weekly_first = "Weekly first", weekly_random = "Weekly random"
)

# Weekly errors at matched budgets broken down by weekly caseload.
# Reuses bm_prepare()/bm_sample()/bm_metrics() from scripts/budget_matched_funcs.R;
# the weekly-summed estimate fills untested weeks with the same rule as bm_metrics().
bm_caseload_bands <- c(0, 2, 5, 10, 20, Inf)
bm_caseload_labels <- c("1-2", "3-5", "6-10", "11-20", ">20")

bm_weekly_filled <- function(z, s, prior = 0.2) {
  observed <- s$m > 0
  p <- s$x / s$m
  for (w in which(!observed)) {
    lag_days <- as.numeric(z$weeks[w] - z$weeks)
    history <- which(observed & lag_days > 0 & lag_days <= 28)
    if (!length(history)) history <- which(observed & lag_days > 0 & lag_days <= 365)
    p[w] <- if (length(history)) sum(s$x[history]) / sum(s$m[history]) else prior
  }
  p
}

bm_by_caseload <- function(data, cap = 3L, repetitions = 200L, seed = 20260914L) {
  set.seed(seed)
  schemes <- names(bm_labels)
  units <- split(data, interaction(data$data_source, data$site, data$year, drop = TRUE))
  out <- list()
  for (unit in units) {
    z <- bm_prepare(unit)
    for (scheme in schemes) {
      reps <- if (scheme == "daily_first") 1L else repetitions
      for (r in seq_len(reps)) {
        s <- bm_sample(z, cap, scheme)
        p <- bm_weekly_filled(z, s)
        e_count <- z$Nw * p - z$truth
        check <- bm_metrics(z, s, initial_priors = 0.2)
        stopifnot(abs(sum(e_count) - check$annual_error[check$estimator == "weekly_prior_0.2"]) < 1e-8)
        out[[length(out) + 1]] <- data.frame(
          data_source = unit$data_source[1], scheme = scheme, replicate = r,
          Nw = z$Nw, m = s$m, e_count = e_count, e_pos = e_count / z$Nw
        )
      }
    }
  }
  bind_rows(out) %>%
    mutate(band = cut(Nw, bm_caseload_bands, labels = bm_caseload_labels)) %>%
    group_by(data_source, scheme, band) %>%
    summarise(
      weeks = n(), mean_cases = mean(Nw), mean_tests = mean(m), untested = mean(m == 0),
      pos_rmse = sqrt(mean(e_pos^2)), count_rmse = sqrt(mean(e_count^2)),
      sse_pos = sum(e_pos^2), sse_count = sum(e_count^2), .groups = "drop"
    ) %>%
    group_by(data_source, scheme) %>%
    mutate(week_share = weeks / sum(weeks),
           pos_sse_share = sse_pos / sum(sse_pos),
           count_sse_share = sse_count / sum(sse_count)) %>%
    ungroup() %>%
    select(-sse_pos, -sse_count) %>%
    mutate(cap = cap, repetitions = repetitions)
}
