## ---------------------------------------------------------------------------
## Figure: schematic of the RDT sampling strategies
##
## Manuscript: "Cholera Surveillance with Rapid Diagnostic Tests: Sampling
##              Approaches and Threats to Inference"
##
## Illustrative (not data-derived) figure. A single health facility is followed
## over two weeks. Every suspected case that presents is drawn as one circle,
## ordered top to bottom by order of presentation within the day. Each panel
## shades the cases that a given sampling strategy would have selected for RDT
## testing. Strategy definitions follow Supplemental Table 1.
##
## Output: figures/fig_sampling_schematic.{png,pdf}
## ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

set.seed(20260313)

## --- Illustrative case stream ----------------------------------------------
## Number of suspected cases presenting at the facility on each of 14 days.
daily_counts <- c(4, 3, 6, 2, 5, 8, 4,   # week 1: Mon .. Sun
                  5, 9, 7, 6, 4, 3, 5)   # week 2: Mon .. Sun

cases <- tibble(day = rep(seq_along(daily_counts), times = daily_counts)) |>
  group_by(day) |>
  mutate(order_in_day = row_number()) |>
  ungroup() |>
  mutate(
    week          = ifelse(day <= 7, 1L, 2L),
    dow           = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[(day - 1) %% 7 + 1],
    order_overall = row_number(),
    ## severity is assigned at random, i.e. severe cases are scattered through
    ## the order of presentation rather than concentrated in it
    severe        = rbinom(n(), 1, 0.25) == 1
  )

## --- Strategy definitions ---------------------------------------------------
## Days drawn at random for the two strategies that key off a chosen day.
weekly_start_day <- c(`1` = 4L, `2` = 13L)   # random start day, weekly n strategy
random_day       <- c(`1` = 3L, `2` = 9L)    # random sampled day, once-a-day strategy

n_daily  <- 3   # daily n
n_every  <- 2   # systematic, every nth case
n_weekly <- 3   # weekly n

sel_weekly <- cases |>
  filter(day >= weekly_start_day[as.character(week)]) |>
  group_by(week) |>
  slice_head(n = n_weekly) |>
  ungroup() |>
  pull(order_overall)

strategies <- c(
  "Exhaustive (reference)",
  "Systematic (every 2nd case)",
  "Daily n = 3 (first 3 cases each day)",
  "Weekly n = 3 (first 3 cases from a random start day)",
  "Random day (all cases on one day each week)",
  "Severe cases only"
)

panel_data <- bind_rows(
  cases |> mutate(strategy = strategies[1], tested = TRUE),
  cases |> mutate(strategy = strategies[2], tested = order_overall %% n_every == 0),
  cases |> mutate(strategy = strategies[3], tested = order_in_day <= n_daily),
  cases |> mutate(strategy = strategies[4], tested = order_overall %in% sel_weekly),
  cases |> mutate(strategy = strategies[5], tested = day %in% random_day),
  cases |> mutate(strategy = strategies[6], tested = severe)
) |>
  mutate(
    strategy  = factor(strategy, levels = strategies),
    test_lab  = factor(ifelse(tested, "Tested with RDT", "Not tested"),
                       levels = c("Tested with RDT", "Not tested")),
    sev_lab   = factor(ifelse(severe, "Severe case", "Non-severe case"),
                       levels = c("Severe case", "Non-severe case"))
  )

## Realised sampling fraction in each panel, annotated in the corner.
frac_lab <- panel_data |>
  group_by(strategy) |>
  summarise(n_tested = sum(tested), n_total = n(), .groups = "drop") |>
  mutate(label = sprintf("%d / %d cases tested (%.0f%%)",
                         n_tested, n_total, 100 * n_tested / n_total))

## Light band behind the day(s) a strategy keys off.
key_days <- bind_rows(
  tibble(strategy = strategies[4], day = unname(weekly_start_day)),
  tibble(strategy = strategies[5], day = unname(random_day))
) |>
  mutate(strategy = factor(strategy, levels = strategies),
         xmin = day - 0.45, xmax = day + 0.45)

## --- Aesthetics -------------------------------------------------------------
col_tested   <- "#0072B2"   # blue,       cases selected for RDT testing
col_untested <- "#E4E7EA"   # light grey, cases not selected
col_severe   <- "#D55E00"   # vermillion, severe-case ring
col_band     <- "#F2F0EA"   # chosen-day band

y_max <- max(cases$order_in_day)

p <- ggplot(panel_data, aes(x = day, y = order_in_day)) +
  geom_rect(data = key_days,
            aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = y_max + 0.6),
            inherit.aes = FALSE, fill = col_band) +
  geom_vline(xintercept = 7.5, colour = "grey80", linewidth = 0.3, linetype = "22") +
  geom_point(aes(fill = test_lab, colour = sev_lab, stroke = sev_lab),
             shape = 21, size = 2.7) +
  scale_fill_manual(values = c("Tested with RDT" = col_tested,
                               "Not tested"      = col_untested),
                    name = "RDT testing:") +
  scale_colour_manual(values = c("Severe case"     = col_severe,
                                 "Non-severe case" = "grey78"),
                      name = "Case severity:") +
  scale_discrete_manual("stroke",
                        values = c("Severe case" = 1.0, "Non-severe case" = 0.35),
                        guide  = "none") +
  scale_x_continuous(breaks = 1:14,
                     labels = rep(c("M", "T", "W", "T", "F", "S", "S"), 2),
                     expand = expansion(add = 0.55)) +
  scale_y_reverse(breaks = c(1, 3, 5, 7, 9),
                  expand = expansion(add = 0.7)) +
  geom_text(data = frac_lab,
            aes(x = 14.5, y = y_max + 0.2, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1,
            size = 2.6, colour = "grey30") +
  facet_wrap(~ strategy, ncol = 2, axes = "all_x") +
  guides(fill   = guide_legend(order = 1, override.aes = list(colour = "grey78", stroke = 0.35)),
         colour = guide_legend(order = 2, override.aes = list(fill = "white",
                                                              stroke = c(1.0, 0.35)))) +
  labs(
    x = NULL,
    y = "Order of presentation within the day",
    title = "Suspected cholera cases selected for RDT testing under each sampling strategy",
    subtitle = paste("One health facility over two consecutive weeks (Monday to Sunday, dashed line separates the weeks).",
                     "\nEach circle is one suspected case, ordered top to bottom by order of presentation within the day.",
                     "\nShaded bands mark the day drawn at random under the weekly n and random-day strategies.")
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.25),
    panel.spacing      = unit(1.1, "lines"),
    strip.text         = element_text(hjust = 0, face = "bold", size = 8.5,
                                      margin = margin(b = 4)),
    axis.text.x        = element_text(colour = "grey45", size = 7),
    axis.ticks         = element_blank(),
    axis.title.y       = element_text(colour = "grey30", size = 8),
    plot.title         = element_text(face = "bold", size = 10.5),
    plot.subtitle      = element_text(colour = "grey35", size = 8.2,
                                      margin = margin(b = 8)),
    legend.position    = "bottom",
    legend.margin      = margin(t = 2),
    legend.text        = element_text(size = 8),
    legend.key.spacing.x = unit(4, "pt")
  )

dir.create("figures", showWarnings = FALSE)
ggsave("figures/fig_sampling_schematic.png", p, width = 9, height = 7.2, dpi = 300, bg = "white")
ggsave("figures/fig_sampling_schematic.pdf", p, width = 9, height = 7.2, bg = "white")

print(frac_lab)
cat("Weekly n start days:", unname(weekly_start_day), "\n")
cat("Random sampled days:", unname(random_day), "\n")
