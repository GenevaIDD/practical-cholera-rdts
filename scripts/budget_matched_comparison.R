# From repository root: Rscript scripts/budget_matched_comparison.R [repetitions] [output_dir]
# Does not modify the manuscript's data products. Default: 200 repetitions.
suppressPackageStartupMessages({library(dplyr); library(readr); library(ggplot2)})
args <- commandArgs(trailingOnly = TRUE)
repetitions <- if (length(args)) as.integer(args[1]) else 200L
out <- if (length(args) >= 2) args[2] else 'data/budget_matched'
source('scripts/budget_matched_funcs.R')
d <- read_csv('data/sampled_data.csv', show_col_types = FALSE)
r <- bm_run(d, repetitions = repetitions)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write_csv(r, file.path(out, 'replicates.csv.gz'))
# First quantify repeated-design behaviour within each fixed facility-year.
u <- r %>% group_by(data_source, site, year, daily_cap, scheme, estimator) %>%
  summarise(annual_bias = mean(annual_error), annual_rmse = sqrt(mean(annual_error^2)),
    annual_error_sd = sd(annual_error), annual_bias_mcse = sd(annual_error) / sqrt(n()),
    weekly_rmse = sqrt(sum(weekly_squared_error) / sum(weeks)),
    weekly_mae = sum(weekly_absolute_error) / sum(weeks),
    positivity_rmse = sqrt(sum(positivity_squared_error) / sum(weeks)),
    empty_week_fraction = sum(empty_weeks) / sum(weeks),
    untested_case_fraction = sum(cases_in_empty_weeks) / sum(suspected),
    tests = first(tests), suspected = first(suspected), truth = first(truth),
    repetitions = n(), .groups = 'drop')
write_csv(u, file.path(out, 'facility_year_summary.csv'))
# These are errors of facility-year totals, NOT catchment totals or population incidence.
a <- r %>% group_by(data_source, daily_cap, scheme, estimator) %>%
  summarise(annual_bias_cases = mean(annual_error),
    annual_rmse_cases = sqrt(mean(annual_error^2)),
    weekly_rmse_cases = sqrt(sum(weekly_squared_error) / sum(weeks)),
    weekly_mae_cases = sum(weekly_absolute_error) / sum(weeks),
    positivity_rmse = sqrt(sum(positivity_squared_error) / sum(weeks)),
    empty_week_fraction = sum(empty_weeks) / sum(weeks),
    untested_case_fraction = sum(cases_in_empty_weeks) / sum(suspected),
    sampling_fraction = sum(tests) / sum(suspected), .groups = 'drop')
write_csv(a, file.path(out, 'summary.csv'))
source('scripts/budget_matched_plots.R')
bm_save_panels(a, out)
p <- a %>% filter(estimator == 'weekly_prior_0.2') %>%
  ggplot(aes(sampling_fraction, weekly_rmse_cases, colour = scheme)) +
  geom_line() + geom_point() + facet_wrap(~data_source, scales = 'free_y') +
  labs(x = 'Fraction of available patients tested (exact matched budgets)',
       y = 'Weekly case-count RMSE', colour = 'Sampling scheme',
       title = 'Sampling performance at matched budgets',
       subtitle = 'Weekly expansion; missing weeks use past tests, initial positivity 0.2') +
  theme_minimal(base_size = 12) + theme(legend.position = 'bottom')
ggsave(file.path(out, 'weekly_rmse.png'), p, width = 11, height = 6, dpi = 160)
writeLines(c(paste('Repetitions:', repetitions), 'Seed: 20260913',
  'Target: RDT-positive counts among previously RDT-tested patients.',
  'Units: facility-years, using existing week-derived year.',
  'Budget: exact daily-first n=1,2,3,5 test counts within each unit.',
  'Histories reset at facility-year boundaries; inspect all initial-prior sensitivity results.',
  capture.output(sessionInfo())), file.path(out, 'run_info.txt'))
print(a %>% filter(daily_cap == 3, estimator == 'weekly_prior_0.2'), width = Inf)
