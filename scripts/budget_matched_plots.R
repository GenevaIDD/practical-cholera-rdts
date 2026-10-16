# Rebuild figures from saved summaries without repeating the simulation:
# Rscript scripts/budget_matched_plots.R [output_dir]
suppressPackageStartupMessages({library(dplyr); library(readr); library(ggplot2)})

bm_panel_plot <- function(summary, estimator = 'weekly_prior_0.2') {
  metrics <- c(annual_bias_cases = 'Annual bias\n(cases / facility-year)',
    annual_rmse_cases = 'Annual RMSE\n(cases / facility-year)',
    weekly_rmse_cases = 'Weekly RMSE\n(cases)',
    weekly_mae_cases = 'Weekly MAE\n(cases)',
    positivity_rmse = 'Weekly positivity RMSE\n(percentage points)',
    empty_week_fraction = 'Case-bearing weeks untested\n(%)',
    untested_case_fraction = 'Patients in untested weeks\n(%)')
  schemes <- c('annual_srs', 'annual_systematic', 'daily_first', 'daily_random',
               'weekly_first', 'weekly_random')
  labels <- c('Annual random', 'Annual systematic', 'Daily first arrivals',
              'Daily random', 'Weekly first arrivals', 'Weekly random')
  colours <- c('#0072B2', '#D55E00', '#009E73', '#CC79A7', '#E69F00', '#333333')
  names(colours) <- schemes
  titles <- c(pooled = 'Pooled annual positivity',
    weekly_prior_0 = 'Weekly expansion | initial positivity 0%',
    weekly_prior_0.2 = 'Weekly expansion | initial positivity 20%',
    weekly_prior_0.5 = 'Weekly expansion | initial positivity 50%',
    horvitz_thompson = 'Horvitz-Thompson expansion')
  a <- summary[summary$estimator == estimator, ]
  stopifnot(nrow(a) > 0, all(names(metrics) %in% names(a)))
  long <- do.call(rbind, lapply(names(metrics), function(metric) {
    value <- a[[metric]]
    if (metric %in% c('positivity_rmse', 'empty_week_fraction', 'untested_case_fraction')) value <- 100 * value
    data.frame(data_source = a$data_source, scheme = a$scheme,
      budget = 100 * a$sampling_fraction, metric = metric, value = value)
  }))
  long$metric <- factor(long$metric, levels = names(metrics), labels = metrics)
  long$scheme <- factor(long$scheme, levels = schemes)
  ggplot(long, aes(budget, value, colour = scheme, shape = scheme, group = scheme)) +
    geom_hline(yintercept = 0, colour = 'grey70', linewidth = 0.35, linetype = 'dashed') +
    geom_line(linewidth = 0.65) + geom_point(size = 2.3, stroke = 0.8) +
    facet_grid(metric ~ data_source, scales = 'free_y', switch = 'y') +
    scale_colour_manual(values = colours, breaks = schemes, labels = labels) +
    scale_shape_manual(values = c(16, 17, 15, 1, 2, 0), breaks = schemes, labels = labels) +
    scale_x_continuous(labels = function(x) paste0(x, '%')) +
    labs(x = 'Available patients tested (budgets equivalent to daily n = 1, 2, 3, 5)',
      y = NULL, colour = NULL, shape = NULL,
      title = 'RDT sampling performance at matched budgets', subtitle = titles[[estimator]],
      caption = paste('Each row shares its vertical scale across sites. Bias is best near zero; lower is better for the other metrics.',
        'All case-bearing weeks are scored. Annual errors refer to facility-year totals; points are four exact matched budgets.',
        'Coverage metrics depend on the sampling scheme, not the estimator. Coincident values overlap.',
        if (estimator == 'horvitz_thompson') 'Only the four probability sampling designs support this estimator.' else '', sep = '\n')) +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'bottom', legend.box = 'horizontal',
      strip.placement = 'outside', strip.text.y.left = element_text(angle = 0, hjust = 1, size = 11),
      strip.text.x = element_text(face = 'bold', size = 14),
      panel.grid.minor = element_blank(), panel.spacing.y = grid::unit(0.6, 'lines'),
      plot.title = element_text(face = 'bold', size = 19),
      plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
      plot.caption = element_text(hjust = 0, size = 10, lineheight = 1.2),
      plot.margin = margin(15, 20, 15, 15)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE))
}

bm_save_panels <- function(summary, out) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  p <- bm_panel_plot(summary)
  ggsave(file.path(out, 'all_metrics.png'), p, width = 14, height = 19, dpi = 200)
  ggsave(file.path(out, 'all_metrics.pdf'), p, width = 14, height = 19)
  # One page per estimator, including all initial-positivity sensitivity scenarios.
  grDevices::pdf(file.path(out, 'all_metrics_by_estimator.pdf'), width = 14, height = 19)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (estimator in unique(summary$estimator)) print(bm_panel_plot(summary, estimator))
  invisible(p)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args)) args[1] else 'data/budget_matched'
  bm_save_panels(read_csv(file.path(out, 'summary.csv'), show_col_types = FALSE), out)
}
