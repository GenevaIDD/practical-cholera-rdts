# Exact-budget retrospective sampling comparison. Base R apart from input loader.
# All randomization is independent of RDT outcomes. See notebook for estimands.
bm_allocate <- function(capacity, budget) {
  stopifnot(budget >= 0, budget <= sum(capacity), all(capacity > 0))
  # Largest equal integer allocation that fits, with capacity constraints.
  lo <- 0L; hi <- max(capacity)
  while (lo < hi) {
    mid <- ceiling((lo + hi) / 2)
    if (sum(pmin(capacity, mid)) <= budget) lo <- mid else hi <- mid - 1L
  }
  base <- pmin(capacity, lo)
  eligible <- which(capacity > lo)
  remainder <- budget - sum(base)
  expected <- base
  if (remainder > 0) {
    expected[eligible] <- expected[eligible] + remainder / length(eligible)
  }
  # Draw once: integer quotas total exactly budget; E(quota) = expected.
  quota <- pmin(capacity, lo)
  if (remainder > 0) {
    picked <- eligible[sample.int(length(eligible), remainder)]
    quota[picked] <- quota[picked] + 1L
  }
  list(quota = quota, expected = expected)
}

bm_prepare <- function(d) {
  d <- d[order(d$date_arrival, d$record_id), ]
  weeks <- sort(unique(as.Date(d$week)))
  wi <- match(as.Date(d$week), weeks)
  days <- split(seq_len(nrow(d)), as.character(d$date_arrival))
  week_rows <- split(seq_len(nrow(d)), factor(wi, levels = seq_along(weeks)))
  y <- as.numeric(d$rdt_result == 'Positive')
  list(y = y, N = length(y), days = days, week_rows = week_rows,
       wi = wi, weeks = weeks, Nw = lengths(week_rows),
       truth = as.numeric(rowsum(y, wi, reorder = FALSE)[as.character(seq_along(weeks)), 1]))
}

bm_sample <- function(z, cap, scheme) {
  daily_quota <- pmin(lengths(z$days), cap)
  B <- sum(daily_quota)
  pi <- rep(NA_real_, z$N)
  if (scheme %in% c('daily_first', 'daily_random')) {
    selected <- unlist(Map(function(idx, q) {
      if (scheme == 'daily_first') head(idx, q) else idx[sample.int(length(idx), q)]
    }, z$days, daily_quota), use.names = FALSE)
    if (scheme == 'daily_random') for (i in seq_along(z$days))
      pi[z$days[[i]]] <- daily_quota[i] / length(z$days[[i]])
  } else if (scheme == 'annual_srs') {
    selected <- sample.int(z$N, B); pi[] <- B / z$N
  } else if (scheme == 'annual_systematic') {
    # Fractional interval systematic sampling: exactly B distinct cases,
    # common inclusion probability B/N, random phase; not fixed integer every-n.
    selected <- floor((runif(1) + 0:(B - 1)) * z$N / B) + 1L
    pi[] <- B / z$N
  } else if (scheme %in% c('weekly_random', 'weekly_first')) {
    a <- bm_allocate(z$Nw, B)
    selected <- unlist(Map(function(idx, q) {
      if (scheme == 'weekly_first') head(idx, q) else idx[sample.int(length(idx), q)]
    }, z$week_rows, a$quota), use.names = FALSE)
    if (scheme == 'weekly_random') pi <- a$expected[z$wi] / z$Nw[z$wi]
  } else stop('Unknown scheme')
  stopifnot(length(selected) == B, !anyDuplicated(selected))
  m <- tabulate(z$wi[selected], length(z$weeks))
  x <- tabulate(z$wi[selected[z$y[selected] == 1]], length(z$weeks))
  ht <- NULL
  if (all(is.finite(pi)) && all(pi > 0)) {
    ht <- vapply(seq_along(z$weeks), function(w) {
      ii <- selected[z$wi[selected] == w]
      sum(z$y[ii] / pi[ii])
    }, numeric(1))
  }
  list(m = m, x = x, ht = ht, B = B)
}

bm_metrics <- function(z, s, initial_priors = c(0, 0.2, 0.5)) {
  observed <- s$m > 0
  p <- s$x / s$m
  estimates <- list(pooled = z$Nw * sum(s$x) / sum(s$m))
  # Past observations only; no use of imputed observations or future gap length.
  # Reset history at each facility-year so comparisons use identical boundaries.
  for (prior in initial_priors) {
    filled <- p
    for (w in which(!observed)) {
      lag_days <- as.numeric(z$weeks[w] - z$weeks)
      history <- which(observed & lag_days > 0 & lag_days <= 28)
      if (!length(history)) history <- which(observed & lag_days > 0 & lag_days <= 365)
      filled[w] <- if (length(history)) sum(s$x[history]) / sum(s$m[history]) else prior
    }
    estimates[[paste0('weekly_prior_', prior)]] <- z$Nw * filled
  }
  if (!is.null(s$ht)) estimates$horvitz_thompson <- s$ht
  do.call(rbind, lapply(names(estimates), function(estimator) {
    e <- estimates[[estimator]] - z$truth
    data.frame(estimator, annual_error = sum(e), weekly_squared_error = sum(e^2),
      weekly_absolute_error = sum(abs(e)), weeks = length(e),
      truth = sum(z$truth), suspected = z$N, tests = s$B,
      empty_weeks = sum(!observed), cases_in_empty_weeks = sum(z$Nw[!observed]),
      # Positivity error is defined on ALL case-bearing weeks after estimation.
      positivity_squared_error = sum((e / z$Nw)^2))
  }))
}

bm_run <- function(data, repetitions = 200L, caps = c(1L, 2L, 3L, 5L), seed = 20260913L) {
  stopifnot(repetitions >= 1, all(caps >= 1), !anyNA(data$rdt_result),
            all(data$rdt_result %in% c('Positive', 'Negative')))
  set.seed(seed)
  # Preserve the manuscript's week-derived year, including boundary weeks.
  units <- split(data, interaction(data$data_source, data$site, data$year, drop = TRUE))
  schemes <- c('daily_first', 'daily_random', 'annual_srs', 'annual_systematic',
               'weekly_random', 'weekly_first')
  results <- list(); k <- 0L
  for (unit in units) {
    z <- bm_prepare(unit)
    message(unit$data_source[1], ' / ', unit$site[1], ' / ', unit$year[1])
    for (cap in caps) for (rep in seq_len(repetitions)) for (scheme in schemes) {
      s <- bm_sample(z, cap, scheme)
      r <- bm_metrics(z, s)
      r$data_source <- unit$data_source[1]; r$site <- unit$site[1]
      r$year <- as.character(unit$year[1]); r$daily_cap <- cap
      r$scheme <- scheme; r$replicate <- rep
      k <- k + 1L; results[[k]] <- r
    }
  }
  do.call(rbind, results)
}
