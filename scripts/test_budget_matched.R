# Run from repository root: Rscript scripts/test_budget_matched.R
source('scripts/budget_matched_funcs.R')
set.seed(1)
for (capacity in list(c(1,2,10), c(1,1), c(3,3,3))) for (B in 0:sum(capacity)) {
  a <- bm_allocate(capacity, B)
  stopifnot(sum(a$quota) == B, all(a$quota <= capacity),
            abs(sum(a$expected) - B) < 1e-10)
}
d <- data.frame(record_id = 1:9, date_arrival = as.Date('2022-01-02') + c(0,0,1,7,7,7,14,14,14),
  week = as.Date('2022-01-02') + c(0,0,0,7,7,7,14,14,14),
  rdt_result = rep(c('Positive','Negative','Negative'), 3))
z <- bm_prepare(d)
for (scheme in c('daily_first','daily_random','annual_srs','annual_systematic','weekly_random','weekly_first')) {
  s <- bm_sample(z, 100, scheme)
  m <- bm_metrics(z, s)
  stopifnot(s$B == 9, all(m$annual_error == 0), all(m$weekly_squared_error == 0))
}
# No selected test is not equivalent to no cases: all three weeks remain scored.
s <- list(m = c(1,0,0), x = c(1,0,0), ht = NULL, B = 1)
m <- bm_metrics(z, s)
stopifnot(all(m$weeks == 3), all(m$empty_weeks == 2), all(m$truth == 3))
# Monte Carlo check of known-positive inclusion weighting, with unequal inclusion probabilities.
for (scheme in c('annual_srs','annual_systematic','daily_random','weekly_random')) {
  estimates <- replicate(3000, sum(bm_sample(z, 1, scheme)$ht))
  stopifnot(abs(mean(estimates) - sum(z$truth)) < 5 * sd(estimates)/sqrt(length(estimates)) + 1e-8)
}
# Initial fallback must not borrow from a future observed week.
s <- list(m = c(0,0,1), x = c(0,0,1), ht = NULL, B = 1)
m <- bm_metrics(z, s, initial_priors = 0)
stopifnot(m$annual_error[m$estimator == 'weekly_prior_0'] == 0)
cat('Budget, census, missing-week, causal-history and HT checks passed.\n')
# Quota expectations include assignments of zero in very small budgets.
quota_draws <- replicate(3000, bm_allocate(c(1, 2, 10), 1)$quota)
stopifnot(max(abs(rowMeans(quota_draws) - rep(1/3, 3))) < 0.04)
# Verify arrival ordering and daily budgets against manuscript sampling flags.
if (file.exists('data/sampled_data.csv')) {
  d <- read.csv('data/sampled_data.csv')
  units <- split(d, interaction(d$data_source, d$site, d$year, drop = TRUE))
  for (unit in units) for (cap in c(1,2,3,5)) {
    z <- bm_prepare(unit)
    s <- bm_sample(z, cap, 'daily_first')
    flag <- unit[[paste0('sample_first_', cap, '_cases')]]
    stopifnot(s$B == sum(flag), sum(s$x) == sum(flag * (unit$rdt_result == 'Positive')))
  }
  cat('Daily selections match saved manuscript flags in every facility-year.\n')
}
