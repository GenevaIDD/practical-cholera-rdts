# Save all figures, tables, and print Perc Difference estimates
# Extracted from analysis Rmd
library(tidyverse)
library(lubridate)
library(reactable)
library(zoo)
library(webshot2)
library(boot)
library(patchwork)
library(flextable)
library(ggh4x)
options(scipen = 100)

black_border <- fp_border_default(color = "black", width = 1)
print.ir <- T
plot.alt.labels <- T

### Helper functions ----
pretty_prop <- function(x, y, markdown = TRUE) {
  temp <- paste0(signif(x / y * 100, 2), "% (n=", x, "/", y, ")")
  if (markdown) {
    temp <- paste0("**", temp, "**")
  }
  return(temp)
}
pretty_num <- function(x) {
  paste0("**", x, "**")
}

### Labels ----
uvira_label <- "Uvira, DRC"
serochit_label <- "Sitakunda, BGD"
inc_modifier <- 10000
tab_style <- list(`font-size` = "13px", color = "black")

### Load population data ----
pop.serochit <- read_csv(here::here("data/census_data_sex_age_sitakunda.csv"), skip = 1)
pop.u5.serochit <- pop.serochit %>%
  filter(Age %in% c("0-04")) %>%
  pull(`Both Sexes Population`) %>%
  sum()
pop.o5.serochit <- pop.serochit %>%
  filter(!Age %in% c("0-04")) %>%
  pull(`Both Sexes Population`) %>%
  sum()
pop.serochit <- pop.serochit %>%
  pull(`Both Sexes Population`) %>%
  sum()

pop.uvira <- read.csv(here::here("data/uvira_pop_sum.csv"))
pop.u5.uvira <- pop.uvira %>%
  filter(age_group == "0-4") %>%
  pull(total) %>%
  sum() %>%
  as.numeric()
pop.o5.uvira <- pop.uvira %>%
  filter(age_group != "0-4") %>%
  pull(total) %>%
  sum() %>%
  as.numeric()
pop.uvira <- pop.uvira %>%
  pull(total) %>%
  sum() %>%
  as.numeric()

pop <- tibble(
  under5 = rep(c(0, 1, 2), 2),
  data_source = c(rep(uvira_label, 3), rep(serochit_label, 3)),
  pop_n = c(
    pop.u5.uvira,
    pop.o5.uvira,
    pop.uvira,
    pop.u5.serochit,
    pop.o5.serochit,
    pop.serochit
  )
)

### Load analysis data ----
case.data <- read_csv(here::here("data/case_data_delta.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))
sampled.data <- read_csv(here::here("data/sampled_data.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))
clinical.data <- read_csv(here::here("data/clinical_data.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))
volume.data <- read_csv(here::here("data/volume_data.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))
bias.age.data <- read_csv(here::here("data/bias_age.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))
bias.aggregate.data <- read_csv(here::here("data/bias_aggregate.csv")) %>% mutate(data_source = factor(
  data_source,
  levels = c("Sitakunda", "Uvira"),
  labels = c(serochit_label, uvira_label)
))

### Strategy labels ----
sampled_strategies <- colnames(sampled.data)[str_detect(colnames(sampled.data), "sample_")]
sampled_strategies_label <- case_when(
  str_detect(sampled_strategies, "cases_week") ~ paste0(
    "First n weekly (n=",
    str_extract(sampled_strategies, "\\d{1,3}"),
    ")"
  ),
  str_detect(sampled_strategies, "every") ~ paste0("Every n (n=", str_extract(sampled_strategies, "\\d{1,3}"), ")"),
  str_detect(sampled_strategies, "first") ~ paste0(
    "First n daily (n=",
    str_extract(sampled_strategies, "\\d{1,3}"),
    ")"
  ),
  sampled_strategies == "sample_severe_cases" ~ "Severe cases only",
  sampled_strategies == "sample_sun" ~ "All cases: Sunday",
  sampled_strategies == "sample_mon" ~ "All cases: Monday",
  sampled_strategies == "sample_tue" ~ "All cases: Tuesday",
  sampled_strategies == "sample_wed" ~ "All cases: Wednesday",
  sampled_strategies == "sample_thu" ~ "All cases: Thursday",
  sampled_strategies == "sample_fri" ~ "All cases: Friday",
  sampled_strategies == "sample_sat" ~ "All cases: Saturday",
  sampled_strategies == "sample_all_once_week" ~ "All cases once a week (random day)",
  sampled_strategies == "sample_all_cases" ~ "All cases"
)
sampled_strategies_label <- if_else(
  str_detect(sampled_strategies, "u5"),
  paste0(sampled_strategies_label, " (age)"),
  sampled_strategies_label
)

### Color palettes ----
col_vals_cases <- c(
  setNames("goldenrod2", serochit_label),
  setNames("darkgreen", uvira_label)
)
col_vals_ir <- c(
  setNames("orange3", serochit_label),
  setNames("seagreen4", uvira_label)
)
facet_colors <- setNames(list(c("white", "goldenrod2"), c("white", "darkgreen")), str_replace(c(serochit_label, uvira_label), "\\s", "\n"))

### Shading data ----
shading <- sampled.data %>%
  select(data_source, starts_with("sample_")) %>%
  pivot_longer(-data_source) %>%
  group_by(data_source, name) %>%
  summarize(sampled = sum(value)) %>%
  mutate(
    strategy_name = name,
    strategy = factor(name, levels = sampled_strategies, labels = sampled_strategies_label)
  )

sev.shading <- sampled.data %>%
  mutate(
    severity_measured = case_when(
      dehydration != "Don't know" &
        !is.na(dehydration) ~ TRUE, !is.na(los_days) &
        !is.na(date_arrival) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  count(data_source, severity_measured, name = "severity") %>%
  filter(severity_measured) %>%
  select(-severity_measured)

shading <- shading %>%
  left_join(sev.shading) %>%
  group_by(data_source) %>%
  mutate(
    prop = if_else(
      name == "sample_severe_cases",
      sampled / severity,
      sampled / max(sampled)
    ),
    age = str_detect(strategy, "age"),
    data_source = str_replace(data_source, ",\\s", ",\n")
  )

### Plotting functions ----
create_shading_plot <- function(facet, shading.data) {
  ggplot(
    shading.data %>% filter(data_source == facet),
    aes(
      x = 1,
      y = strategy,
      fill = prop,
      label = round(prop, 2)
    )
  ) +
    geom_tile(width = 1, height = 0.8) +
    geom_text(alpha = 0.5) +
    scale_fill_gradient(
      low = facet_colors[[as.character(facet)]][1],
      high = facet_colors[[as.character(facet)]][2],
      limits = c(0, 1)
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10),
      legend.position = "none"
    ) +
    labs(x = "% Eligible Suspected\nCases Sampled") +
    facet_wrap(~data_source)
}

plot_cases <- function(lab = "Cases",
                       comp_annual = FALSE,
                       col_vals = col_vals_cases,
                       plot.data = fig2_plot,
                       violin_scale = "width") {
  if (lab == "Cases") {
    x_lab <- "Bias in Cholera Cases"
    annual <- "Annualized Cases"
    weekly <- "Weekly Cases"
    breaks_annual <- c(-100, -50, 0, 50, 100)
    breaks_weekly <- c(-50, -25, 0, 25, 50)
    if (comp_annual) {
      weekly <- "Annual Cases"
      breaks_weekly <- c(-100, 0, 100, 200)
    }
  } else if (lab == "IR") {
    x_lab <- "Bias in Cholera IR per 10,000"
    annual <- "Annualized IR"
    weekly <- "Weekly IR"
    breaks_annual <- c(-2, -1, 0, 1, 2, 4)
    breaks_weekly <- c(-2, -1, 0, 1, 2, 4)
    if (comp_annual) {
      weekly <- "Annual IR"
    }
  }
  plot.data <- plot.data %>% mutate(plot_ci = str_detect(period_lab, "Annual"))
  p <- ggplot() +
    geom_linerange(
      data = subset(plot.data, plot_ci),
      aes(
        y = strategy,
        x = value,
        xmin = value.lo,
        xmax = value.hi,
        color = data_source,
        group = interaction(strategy, data_source)
      ),
      size = 0.5,
      alpha = 0.4,
      position = position_jitterdodge(
        jitter.width = 0.35,
        jitter.height = 0,
        dodge.width = 1,
        seed = 1
      )
    ) +
    geom_point(
      data = subset(plot.data, plot_ci),
      aes(
        y = strategy,
        x = value,
        color = data_source,
        group = interaction(strategy, data_source)
      ),
      size = 1.2,
      position = position_jitterdodge(
        jitter.width = 0.35,
        jitter.height = 0,
        dodge.width = 1,
        seed = 1
      )
    ) +
    labs(
      y = "Sampling Strategy",
      x = x_lab,
      color = "Catchment Area",
      fill = "Catchment Area"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 10,
        face = "bold"
      ),
      plot.margin = margin(2, 2, 2, 2),
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.4
      ),
      strip.placement = "outside",
      strip.text = element_text(size = 10),
      strip.text.x = element_text(margin = margin(b = 1)),
      strip.text.y = element_text(margin = margin(l = 1)),
      axis.text.y = element_text(size = 8, margin = margin(r = 1)),
      axis.text.x = element_text(size = 8, margin = margin(t = 1)),
      axis.title.x = element_text(margin = margin(t = 2)),
      axis.title.y = element_text(margin = margin(r = 2)),
      axis.ticks.length = unit(2, "pt"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0, "pt"),
      legend.key.height = unit(6, "pt"),
      legend.key.width = unit(10, "pt"),
      legend.key.size = unit(6, "pt"),
      legend.text = element_text(size = 10)
    ) +
    scale_color_manual(values = col_vals) +
    scale_fill_manual(values = col_vals) +
    facet_wrap(~period_lab, scales = "free_x") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    ggh4x::facetted_pos_scales(
      x = list(
        period_lab == weekly ~ scale_x_continuous(breaks = breaks_weekly),
        period_lab == annual ~ scale_x_continuous(breaks = breaks_annual)
      )
    )
  if (!comp_annual) {
    p <- p +
      geom_point(
        data = subset(plot.data, !plot_ci),
        aes(
          y = strategy,
          x = value,
          color = data_source
        ),
        size = 0.6,
        position = position_jitterdodge(
          jitter.width = 0.4,
          jitter.height = 0,
          dodge.width = 1
        )
      ) +
      geom_violin(
        data = subset(plot.data, period_lab == weekly),
        aes(
          y = strategy,
          x = value,
          fill = data_source,
          colour = after_scale(colorspace::darken(fill, 0.3))
        ),
        position = position_dodge(width = 1),
        width = 0.9,
        trim = TRUE,
        orientation = "y",
        scale = violin_scale,
        alpha = 0.3
      )
  }
  p + guides(
    color = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    ),
    size = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    ),
    fill = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    )
  )
}

### Clean case.data ----
case.data <- case.data %>%
  mutate(
    strategy_name = strategy,
    strategy = factor(strategy, levels = sampled_strategies_label, labels = sampled_strategies_label),
    period_label = case_when(
      aggregate & period == "annual" ~ "annual_weekly",
      aggregate &
        period == "month" ~ "monthly_weekly",
      TRUE ~ period
    ),
    period_ir_lab = factor(
      period_label,
      levels = c("weekly", "annual_weekly", "monthly", "annual"),
      labels = c("Weekly IR", "Annualized IR", "Monthly IR", "Annual IR")
    ),
    period_case_lab = factor(
      period_label,
      levels = c("weekly", "annual_weekly", "monthly", "annual"),
      labels = c(
        "Weekly Cases",
        "Annualized Cases",
        "Monthly Cases",
        "Annual Cases"
      )
    )
  ) %>%
  select(-period_label)

bias.age.data <- bias.age.data %>%
  mutate(
    strategy = factor(strategy, levels = sampled_strategies_label, labels = sampled_strategies_label)
  )
bias.aggregate.data <- bias.aggregate.data %>%
  mutate(
    strategy = factor(strategy, levels = sampled_strategies_label, labels = sampled_strategies_label)
  )

volume.data <- volume.data %>%
  group_by(year) %>%
  mutate(
    min_date = min(date_arrival),
    max_date = max(date_arrival)
  ) %>%
  pivot_longer(cols = starts_with("sample")) %>%
  mutate(strategy = factor(name, levels = sampled_strategies, labels = sampled_strategies_label))

sum.volume.data <- list()
for (i in c("week", "year", "date_arrival")) {
  time_period <- case_when(i == "week" ~ 7, i == "year" ~ 365.25, i == "date_arrival" ~ 1)
  temp <- bind_rows(
    volume.data %>%
      group_by(
        data_source,
        time = !!as.symbol(i),
        min_date,
        max_date,
        name,
        under5
      ) %>%
      summarize(n_sampled = sum(value), period = i),
    volume.data %>%
      group_by(data_source, time = !!as.symbol(i), min_date, max_date, name) %>%
      summarize(
        n_sampled = sum(value),
        under5 = 2,
        period = i
      )
  )
  temp <- temp %>%
    filter(name == "sample_all_cases") %>%
    ungroup() %>%
    select(-name) %>%
    rename(n_exhaustive = n_sampled) %>%
    left_join(temp)
  sum.volume.data[[i]] <- temp %>%
    left_join(pop) %>%
    group_by(name, under5, period, data_source) %>%
    summarize(
      pooled_rate = sum(n_sampled) / sum(pop_n * as.numeric(max_date - min_date) / time_period) * inc_modifier,
      mean_rate = mean(
        n_sampled / pop_n * as.numeric(max_date - min_date) / time_period
      ) * inc_modifier,
      mean_n_sampled = mean(n_sampled),
      mean_relative_sampled = mean(n_sampled / n_exhaustive),
      median_n_sampled = median(n_sampled),
      median_relative_sampled = median(n_sampled / n_exhaustive),
      min_rate = quantile(
        n_sampled / pop_n * as.numeric(max_date - min_date) / time_period,
        0.25
      ) * inc_modifier,
      min_n_sampled = min(n_sampled),
      min_relative_sampled = min(n_sampled / n_exhaustive),
      max_rate = quantile(
        n_sampled / pop_n * as.numeric(max_date - min_date) / time_period,
        0.75
      ) * inc_modifier,
      max_n_sampled = max(n_sampled),
      max_relative_sampled = max(n_sampled / n_exhaustive)
    )
}
sum.volume.data <- bind_rows(sum.volume.data) %>%
  mutate(strategy = factor(name, levels = sampled_strategies, labels = sampled_strategies_label))

select_strategies_plus <- c(
  "First n daily (n=1)",
  "First n daily (n=2)",
  "First n daily (n=3)",
  "First n daily (n=5)",
  "First n weekly (n=1)",
  "First n weekly (n=2)",
  "First n weekly (n=3)",
  "First n weekly (n=10)",
  "Every n (n=2)",
  "Every n (n=5)",
  "Every n (n=10)"
)

### ---- SAVE FIGURES ----

## Fig 1: Number of weekly care-seeking, suspected cholera cases by ----
## rapid diagnostic test (RDT) result and catchment area. The top panel (A) 
## represents data from Sitakunda, Bangladesh (BGD) (2021–2022) and the 
## bottom panel (B) represents data from Uvira, South Kivu, Democratic Republic 
## of the Congo (DRC) (2016–2022). Grey represents suspected cases who were 
## not tested by RDTs. 
fig.epicurve.sita <- clinical.data %>%
  filter(
    date_arrival >= as.Date("2016-04-03"),
    data_source == serochit_label
  ) %>%
  count(data_source, week = floor_date(date_arrival, "week"), rdt_result) %>%
  mutate(
    rdt_result = if_else(
      is.na(rdt_result) | rdt_result == "Invalid",
      "Missing",
      rdt_result
    ),
    rdt_result = factor(rdt_result, levels = c("Negative", "Positive"))
  ) %>%
  ggplot(aes(x = week, y = n, fill = rdt_result)) +
  geom_col() +
  facet_wrap(~data_source, scales = "free", nrow = 2) +
  labs(y = "Suspected cholera cases", x = "Suspected weekly cholera cases", fill = "RDT Result") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 8)
  ) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "6 months",
    expand = c(0.01, 0.01)
  ) +
  scale_fill_manual(
    values = c("goldenrod2", "goldenrod4"),
    drop = FALSE
  )

fig.epicurve.uvira <- clinical.data %>%
  filter(
    date_arrival >= as.Date("2016-04-03"),
    data_source == uvira_label
  ) %>%
  count(data_source, week = floor_date(date_arrival, "week"), rdt_result) %>%
  mutate(
    rdt_result = if_else(
      is.na(rdt_result) | rdt_result == "Invalid",
      "Missing",
      rdt_result
    ),
    rdt_result = factor(rdt_result, levels = c("Missing", "Negative", "Positive"))
  ) %>%
  ggplot(aes(x = week, y = n, fill = rdt_result)) +
  geom_col() +
  facet_wrap(~data_source, scales = "free", nrow = 2) +
  labs(y = "Suspected cholera cases", x = "Suspected weekly cholera cases", fill = "RDT Result") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 8)
  ) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "6 months",
    expand = c(0.01, 0.01)
  ) +
  scale_fill_manual(
    values = c("grey50", "chartreuse3", "darkgreen"),
    drop = TRUE
  )

fig1 <- cowplot::plot_grid(
  fig.epicurve.sita + theme(legend.position = "bottom"),
  fig.epicurve.uvira + theme(legend.position = "bottom"),
  ncol = 1,
  labels = c("A)", "B)")
) %>%
  cowplot::plot_grid(
    .,
    cowplot::get_plot_component(fig.epicurve.uvira, "guide-box", return_all = T)[[1]],
    ncol = 1,
    rel_heights = c(1, 0.1)
  )

ggsave(here::here("figures/fig_1.png"),
  fig1,
  width = 7,
  height = 8
)

message("Saved fig_1.png")

ggsave(here::here("figures/fig_1.tiff"), fig1, width = 7, height = 8, dpi = 300)

message("Saved fig_1.tiff")
## Figure 2. Estimated bias in cholera case estimation (sample strategy -----
# estimation versus true number of cholera cases) by sampling strategy and 
# timescale in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic 
# Republic of Congo (DRC) (2016–2022). The boxes in the middle indicate the 
# proportion of RDT-tested suspected cases that were sampled during the study 
# period for each catchment area and strategy. Sampling strategies presented 
# include sampling the first n cases each day (daily) or week (week) and sampling 
# every n cases (systematic). Violin areas are scaled to have the same maximum 
# width across strategies and time periods. Weekly and annual cases were 
# estimated by multiplying the cholera positivity among those sampled over 
# the corresponding period each week with the number of suspected cases for 
# the same period. Weekly cases were aggregated across a year to estimate 
# annualized cases/incidence rates. Cases and incidence rates are both shown 
# to help contextualize findings.

fig2_plot <- case.data %>%
  filter(strategy %in% select_strategies_plus & under5 == 2) %>%
  filter((time == "week" &
    aggregate == F) | (time == "year" & aggregate == T))

p2 <- plot_cases(
  plot.data = fig2_plot %>% rename(
    value = bias.cases,
    value.lo = bias.cases.lo,
    value.hi = bias.cases.hi,
    period_lab = period_case_lab
  ),
  col_vals = col_vals_cases,
  lab = "Cases",
  comp_annual = F
) +
  ggh4x::facetted_pos_scales(x = list(
    period_lab == "Annualized Cases" ~ scale_x_continuous(breaks = c(-150, -75, 0, 75, 150))
  ))

p2.ir <- plot_cases(
  plot.data = fig2_plot %>% rename(
    value = bias.ir,
    value.lo = bias.ir.lo,
    value.hi = bias.ir.hi,
    period_lab = period_ir_lab
  ),
  col_vals = col_vals_ir,
  lab = "IR",
  comp_annual = F
) +
  theme(
    axis.title.y.right = element_text(),
    axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2)),
    axis.ticks.y.right = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank()
  ) +
  scale_y_discrete(position = "right") +
  ggh4x::facetted_pos_scales(x = list(period_lab == "Annualized IR" ~ scale_x_continuous(breaks = c(
    -7, -5, -3, -1, 0, 1, 3, 5, 7
  ))))

fig2.shading.data <- shading %>%
  filter(strategy %in% unique(sort(
    p2@layers$geom_linerange$data$strategy_name
  ))) %>%
  mutate(strategy = factor(strategy, levels = levels(p2@layers$geom_linerange$data$strategy)))
fig2.combined_shading_plot <- patchwork::wrap_plots(lapply(unique(fig2.shading.data$data_source), function(x) {
  create_shading_plot(shading.data = fig2.shading.data, facet = x) + theme(strip.text = element_text(size = 8))
})) + patchwork::plot_layout(ncol = 2, axis_titles = "collect_x") &
  guides(fill = "none")

fig2 <- p2 + fig2.combined_shading_plot + p2.ir +
  patchwork::plot_layout(ncol = 3, widths = c(2.8, 1.4, 2.8)) &
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

ggsave(here::here("figures/fig_2.png"),
  fig2,
  width = 9,
  height = 8
)
message("Saved fig_2.png")

ggsave(here::here("figures/fig_2.tiff"), fig2, width = 9, height = 8, dpi = 300)

message("Saved fig_2.tiff")
## Figure 3. A) Estimated (color) and observed (black) weekly cases of cholera -----
# and B) distribution median bias in weekly cholera positivity for the 
# daily (n=3) and weekly (n=3) sampling strategies in Sitakunda, Bangladesh (BGD) 
# (green, 2021–2022) and Uvira, Democratic Republic of Congo (DRC) 
# (yellow, 2020–2022). Observed weekly cholera cases were based on tested 
# suspected cholera cases: all suspected cases in Sitakunda, and 84% of cases 
# in 2020, 83% of cases in 2021, and all (99.6%) but 3 cases in 2022 in Uvira. 
select_strategies_final <- c("First n daily (n=3)", "First n weekly (n=3)")

time.n <- case.data %>%
  filter(
    strategy %in% select_strategies_final,
    under5 == 2,
    time == "week",
    aggregate == F
  ) %>%
  select(
    data_source,
    under5,
    date,
    strategy,
    strategy_name,
    starts_with("n.cases"),
    truth,
    positivity,
    truth.positivity
  ) %>%
  mutate(pos.diff = truth.positivity - positivity) %>%
  filter(data_source == serochit_label |
    date >= as.Date("2020-01-01"))

time.n <- fig2.shading.data %>%
  filter(name %in% c("sample_first_3_cases", "sample_first_3_cases_week") &
    age == F) %>%
  select(data_source, strategy, prop) %>%
  mutate(data_source = if_else(
    str_detect(data_source, "Sitakunda"),
    serochit_label,
    uvira_label
  )) %>%
  right_join(time.n) %>%
  mutate(plot.strategy = str_remove(strategy, "\\)") %>% paste0(., "; ", signif(prop *
    100, 3), "% of cases sampled)"))

plot.case.time2 <- function(d, num_cols = 3) {
  set_scale_breaks <- seq(
    from = pmin(min(d$n.cases.lo, na.rm = T), 0),
    to = max(d$n.cases.hi, na.rm = T) + 1,
    length.out = 5
  ) %>% round()
  d %>%
    left_join(
      shading %>% mutate(data_source = str_replace(data_source, "\\n", " ")) %>%
        select(data_source, strategy, prop, age)
    ) %>%
    mutate(strategy = paste0(strategy, " (", signif(prop * 100, 2), "% of cases sampled)")) %>%
    ggplot() +
    geom_col(aes(x = date, y = truth), color = "darkgrey") +
    geom_line(aes(x = date, y = n.cases, color = data_source)) +
    geom_ribbon(
      aes(
        x = date,
        y = n.cases,
        ymin = n.cases.lo,
        ymax = n.cases.hi,
        fill = data_source
      ),
      alpha = 0.5
    ) +
    facet_wrap(~plot.strategy, ncol = num_cols, scales = "free") +
    theme_minimal() +
    scale_color_manual(values = col_vals_cases) +
    scale_fill_manual(values = col_vals_cases) +
    scale_x_date(date_labels = "%b '%y") +
    labs(y = "Number of cholera cases", x = "Week") +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 10)
    ) +
    scale_y_continuous(breaks = set_scale_breaks, limits = c(0, max(set_scale_breaks)))
}

plot.error.time <- function(d,
                            num_rows = 2,
                            col_guide = col_vals_cases) {
  names(col_guide) <- str_replace(names(col_guide), "\\s", "\n")
  d %>%
    mutate(data_source = str_replace(data_source, ",\\s", ",\n")) %>%
    ggplot(aes(y = pos.diff, x = strategy, color = data_source)) +
    geom_boxplot() +
    theme_minimal() +
    facet_wrap(~plot.strategy, nrow = num_rows, scales = "free") +
    scale_color_manual(values = col_guide) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      strip.text = element_blank()
    ) +
    labs(y = "Weekly difference in cholera positivity")
}

time.sero <- time.n %>%
  filter(data_source == serochit_label) %>%
  plot.case.time2(num_cols = 1)
dist.sero <- time.n %>%
  filter(data_source == serochit_label) %>%
  plot.error.time(num_rows = 2)
time.uvira <- time.n %>%
  filter(data_source == uvira_label) %>%
  plot.case.time2(num_cols = 1)
dist.uvira <- time.n %>%
  filter(data_source == uvira_label) %>%
  plot.error.time(num_rows = 2)

plot1 <- cowplot::plot_grid(time.sero,
  dist.sero,
  nrow = 1,
  rel_widths = c(0.7, 0.3),
  labels = c("A)", "B)"),
  label_size = 10,
  hjust = 0
)
plot2 <- cowplot::plot_grid(time.uvira,
  dist.uvira,
  nrow = 1,
  rel_widths = c(0.7, 0.3)
)

fig3 <- {
  plot1 + labs(title = serochit_label)
} / {
  plot2 + labs(title = uvira_label)
} &
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  )
ggsave(here::here("figures/fig_3.png"),
  fig3,
  width = 7,
  height = 8
)
message("Saved fig_3.png")

ggsave(here::here("figures/fig_3.tiff"), fig3, width = 7, height = 8, dpi = 300)

message("Saved fig_3.tiff")

## Supplemental Figure 1. Difference in the absolute bias of cholera estimates ------
# comparing sampling with age-stratification versus without in 
# Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic Republic of Congo 
# (DRC) (2016–2022). In age-stratified strategies, suspected cases were sampled 
# separately based on whether they are under 5 years old or not. The overall 
# number of estimated cholera cases was then aggregated from these age 
# group-specific estimates. Negative values indicate bias was smaller when 
# employing age-stratified sampling compared to sampling without age 
# stratification. The boxes in the middle indicate the proportion of RDT-tested 
# suspected cases that were sampled during the study period for each catchment
# area and non-age-stratified sampling strategy. Sampling strategies presented 
# include sampling the first n cases each day (daily) or week (week) and 
# sampling every n cases (systematic). Violin areas are scaled to have the 
# same maximum width across strategies and time periods. Weekly and annual 
# cases were estimated by multiplying the cholera positivity among those 
# sampled over the corresponding time period each week with the number of 
# suspected cases for the same time period. Weekly cases were aggregated 
# across a year to estimate annualized cases/incidence rates.

figsf1_plot <- bias.age.data %>%
  filter(strategy %in% select_strategies_plus) %>%
  mutate(
    period_label = case_when(
      aggregate & period == "annual" ~ "annual_weekly",
      aggregate &
        period == "month" ~ "monthly_weekly",
      TRUE ~ period
    ),
    period_ir_lab = factor(
      period_label,
      levels = c("weekly", "annual_weekly", "monthly", "annual"),
      labels = c("Weekly IR", "Annualized IR", "Monthly IR", "Annual IR")
    ),
    period_case_lab = factor(
      period_label,
      levels = c("weekly", "annual_weekly", "monthly", "annual"),
      labels = c(
        "Weekly Cases",
        "Annualized Cases",
        "Monthly Cases",
        "Annual Cases"
      )
    )
  )

psf1 <- figsf1_plot %>%
  rename(period_lab = period_case_lab, value = abs.bias.age.cases) %>%
  mutate(value.hi = value, value.lo = value) %>%
  plot_cases(plot.data = .) +
  labs(x = "Difference in Absolute Bias:\nSampling with age-stratification vs none") +
  ggh4x::facetted_pos_scales(x = list(
    period_lab == "Annualized Cases" ~
      scale_x_continuous(breaks = c(-50, -25, 0, 25, 50))
  ))

psf1.ir <- figsf1_plot %>%
  rename(value = abs.bias.age.ir, period_lab = period_ir_lab) %>%
  mutate(value.hi = value, value.lo = value) %>%
  plot_cases(
    plot.data = .,
    lab = "IR",
    comp_annual = F
  ) +
  theme(
    axis.title.y.right = element_text(),
    axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2)),
    axis.ticks.y.right = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank()
  ) +
  scale_y_discrete(position = "right") +
  ggh4x::facetted_pos_scales(x = list(
    period_lab == "Weekly IR" ~ scale_x_continuous(
      breaks = c(-1, -0.5, 0, 0.5, 1),
      labels = c("-1", "-0.5", "0", "0.5", "1")
    )
  )) +
  labs(x = "Difference in Absolute Bias:\nSampling with age-stratification vs none")

figs1.shading.data <- shading %>% filter(strategy %in% unique(sort(
  psf1@layers$geom_linerange$data$strategy
)))
figs1.shading_plots <- lapply(unique(figs1.shading.data$data_source), function(x) {
  create_shading_plot(shading.data = figs1.shading.data, facet = x) +
    theme(strip.text = element_text(size = 8))
})
figs1.combined_shading_plot <- patchwork::wrap_plots(figs1.shading_plots) +
  patchwork::plot_layout(ncol = 2, axis_titles = "collect_x") &
  guides(fill = "none")

figS1 <- psf1 + figs1.combined_shading_plot + psf1.ir +
  patchwork::plot_layout(ncol = 3, widths = c(2.8, 1.4, 2.8)) &
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

ggsave(here::here("figures/fig_S1.png"), figS1, 
       width = 9, height = 8, dpi = 200, type = "cairo-png")
message("Saved fig_S1.png")

## Supplemental Figure 2. Difference in the absolute bias of annual cholera ------
# estimates when positivity is calculated at the weekly versus annual scale in 
# Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic Republic of 
# Congo (DRC) (2016–2022). Annual cases were estimated either by summing weekly 
# cases based on weekly positivity (i.e., Weekly Annualized), or by applying a 
# single annual positivity to the year’s total suspected cases (i.e., Annual). 
# Negative values indicate the weekly annualized estimates have smaller bias 
# compared to annual estimates. The boxes in the middle indicate the proportion
# of RDT-tested suspected cases that were sampled with RDTs during the study period for 
# each catchment area and strategy.  

figsf5_plot <- bias.aggregate.data %>% filter(strategy %in% select_strategies_plus)

plot_diff <- function(x, col_vals = col_vals_cases) {
  x %>%
    ggplot() +
    geom_point(
      aes(
        y = strategy,
        x = value,
        color = data_source,
        group = interaction(strategy, data_source)
      ),
      alpha = 1,
      position = position_jitterdodge(
        jitter.width = 0.35,
        jitter.height = 0,
        dodge.width = 0.5,
        seed = 1
      )
    ) +
    labs(
      y = "Sampling Strategy",
      x = "Difference in Absolute Bias:\n Weekly Annualized vs. Annual Cholera Estimates",
      color = "Catchment Area",
      fill = "Catchment Area"
    ) +
    theme_minimal() +
    facet_wrap(~strip_title) +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      panel.spacing = unit(0.1, "lines"),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.4
      ),
      strip.text = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 1)),
      axis.text.x = element_text(size = 8, margin = margin(t = 1)),
      axis.title.x = element_text(margin = margin(t = 2)),
      axis.title.y = element_text(margin = margin(r = 2)),
      axis.ticks.length = unit(2, "pt"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0, "pt"),
      legend.key.height = unit(6, "pt"),
      legend.key.width = unit(10, "pt"),
      legend.key.size = unit(6, "pt"),
      legend.text = element_text(size = 9)
    ) +
    scale_color_manual(values = col_vals) +
    scale_fill_manual(values = col_vals) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    guides(
      color = guide_legend(
        title.position = "left",
        nrow = 2,
        byrow = TRUE
      ),
      fill = guide_legend(
        title.position = "left",
        nrow = 2,
        byrow = TRUE
      )
    )
}

psf5 <- figsf5_plot %>%
  rename(value = abs.bias.aggregate.cases) %>%
  mutate(strip_title = "Yearly Cholera Cases") %>%
  plot_diff() + scale_x_continuous(breaks = c(-150, -100, -50, 0, 50, 100))
psf5.ir <- figsf5_plot %>%
  rename(value = abs.bias.aggregate.ir) %>%
  mutate(strip_title = "Yearly Cholera IR") %>%
  plot_diff() +
  theme(
    axis.title.y.right = element_text(),
    axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2)),
    axis.ticks.y.right = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank()
  ) +
  scale_y_discrete(position = "right")

figs_agg.shading.data <- shading %>% filter(strategy %in% unique(sort(psf5@data$strategy)))
figs_agg.shading_plots <- lapply(unique(figs_agg.shading.data$data_source), function(x) {
  create_shading_plot(shading.data = figs_agg.shading.data, facet = x)
})
figs_agg.combined_shading_plot <- patchwork::wrap_plots(figs_agg.shading_plots) +
  patchwork::plot_layout(ncol = 2, axis_titles = "collect_x") &
  guides(fill = "none")

figS2 <- psf5 + figs_agg.combined_shading_plot + psf5.ir +
  patchwork::plot_layout(ncol = 3, widths = c(2.8, 1.4, 2.8)) &
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 8)
  )
ggsave(here::here("figures/fig_S2.pdf"),
  figS2,
  width = 9,
  height = 8
)
message("Saved fig_S2.pdf")

## Supplemental Figure 3. Percent change in the absolute bias when cholera ------
# positivity is calculated at the weekly versus annual scale in Sitakunda, 
# Bangladesh (BGD) (2021–2022) and Uvira, Democratic Republic of Congo (DRC) 
# (2016–2022). Annual cases were estimated either by summing weekly cases based
# on weekly positivity (i.e., Weekly Annualized), or by applying a single annual
# positivity to the year’s total suspected cases (i.e., Annual). We report the 
# difference in the absolute bias between the annual and weekly annualized 
# cholera estimates, divided by the absolute bias from the annual estimation 
# approach. The vertical lines show the median percent change in absolute bias 
# across all years for each sampling strategy type (e.g., daily n) and catchment 
# area. An outlier in the weekly n=10 strategy for Uvira was excluded to improve
# legibility, it had a negative value and affected only the tail of the 
# distribution. A negative value indicates the weekly annualized approach 
# increased bias compared to the annual estimates.

fig.bias.aggregate <- bias.aggregate.data %>%
  left_join(
    case.data %>%
      filter(
        time == "year",
        period == "annual",
        under5 == 2,
        aggregate == F,
        age_strat == F
      ) %>%
      distinct(
        data_source,
        date,
        year,
        time,
        period,
        under5,
        aggregate,
        strategy,
        bias.cases,
        bias.ir
      )
  ) %>%
  filter(strategy %in% select_strategies_plus) %>%
  mutate(strategy_group = case_when(
    str_detect(strategy, "daily") ~ "daily",
    str_detect(strategy, "weekly") ~ "weekly",
    str_detect(strategy, "Every") ~ "every"
  ))

summary.bias.aggregate <- fig.bias.aggregate %>%
  group_by(data_source, strategy_group) %>%
  summarize(median.agg.redux.ir = median(-abs.bias.aggregate.ir / abs(bias.ir)))

sfig8 <- fig.bias.aggregate %>%
  filter(!(strategy == "First n weekly (n=10)" &
    date == as.Date("2018-01-01"))) %>%
  group_by(data_source, strategy_group, date) %>%
  mutate(redux.ir = -abs.bias.aggregate.ir / abs(bias.ir) * 100) %>%
  left_join(
    summary.bias.aggregate %>% transmute(data_source, strategy_group, median.agg.redux.ir = median.agg.redux.ir * 100)
  ) %>%
  mutate(strategy_group = factor(
    strategy_group,
    levels = c("every", "weekly", "daily"),
    labels = c("Every n", "Weekly n", "Daily n")
  )) %>%
  ggplot(aes(
    x = redux.ir,
    y = strategy,
    color = data_source,
    fill = data_source
  )) +
  geom_point(position = position_jitterdodge(
    jitter.width = 0.3,
    jitter.height = 0,
    dodge.width = 1
  )) +
  geom_violin(
    position = position_dodge(1),
    scale = "width",
    alpha = 0.2
  ) +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = median.agg.redux.ir, color = data_source)) +
  scale_color_manual(values = col_vals_cases) +
  scale_fill_manual(values = col_vals_cases) +
  scale_x_continuous(
    breaks = c(-500, -100, -50, 0, 50, 100),
    labels = c("-500%", "-100%", "-50%", "0%", "50%", "100%")
  ) +
  labs(
    x = "Percent Change in Absolute Bias when using Weekly Annualized versus Annual Cholera Estimates.",
    y = "Sampling Strategy",
    color = "Catchment Area",
    fill = "Catchment Area"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.4
    ),
    panel.grid.minor.x = element_blank()
  ) +
  facet_wrap(~strategy_group, scales = "free_y", nrow = 3)

ggsave(here::here("figures/fig_S3.pdf"),
  sfig8,
  width = 12,
  height = 8
)
message("Saved fig_S3.pdf")

## Supplemental Figure 4. Proportion of weekly and monthly incidence rate -------
# estimates where the true cholera incidence falls within the estimated 95% 
# confidence intervals in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, 
# Democratic Republic of Congo (DRC) (2016–2022) by sampling strategy. Cholera 
# positivity was estimated over the corresponding time scale (i.e., week or 
# month) to derive cholera cases and incidence for each time period. Weeks with 
# suspected cases but no estimate of RDT-positivity were considered as time 
# periods where true cholera IR did not fall within the 95% CI. The boxes on 
# the right indicate the proportion of RDT-tested suspected cases that were 
# sampled during the study period for each catchment area and strategy.

fig3_plot <- case.data %>%
  filter(
    time %in% c("week", "month") &
      strategy %in% select_strategies_plus &
      age_strat == F &
      under5 == 2 & aggregate == FALSE
  ) %>%
  group_by(data_source, strategy, under5, time) %>%
  summarize(within = mean(within), within.ir = mean(within.ir)) %>%
  mutate(time = factor(
    time,
    levels = c("week", "month"),
    labels = c("Week", "Month")
  )) %>%
  ggplot(aes(
    y = strategy,
    x = within.ir,
    color = data_source,
    shape = time
  )) +
  geom_point(size = 2, position = position_dodge(0.5)) +
  labs(
    y = "Sampling Strategy",
    x = "% of time periods where true cholera IR fell within the 95% CI",
    color = "Catchment Area",
    shape = "Time scale"
  ) +
  theme_minimal() +
  facet_wrap(~time) +
  scale_x_continuous(breaks = c(0.8, 0.9, 0.95, 1.0)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.border = element_rect(
      color = "black",
      size = 0.5,
      fill = NA
    )
  ) +
  scale_color_manual(values = col_vals_cases) +
  guides(
    color = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    ),
    shape = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    )
  )

ggsave(
  here::here("figures/fig_S4.pdf"),
  width = 10,
  height = 10,
  fig3_plot + (fig2.combined_shading_plot & theme(strip.text = element_text(size = 10))) + patchwork::plot_layout(ncol = 2, widths = c(3, 1))
)
message("Saved fig_S4.pdf")

## Supplemental Figure 5. The proportion of weeks and months where the ------- 
# estimated cholera incidence rate is 50% higher or lower than the true incidence
# rate in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic Republic 
# of Congo (DRC) (2016–2022) by sampling strategy. Cholera positivity was 
# estimated over the corresponding time scale (i.e., week or month) to derive
# cholera cases and incidence for each time period. The boxes on the right 
# indicate the proportion of RDT-tested suspected cases that were sampled during
# the study period for each catchment area and strategy.

fig3s_empirical <- case.data %>%
  filter(
    strategy %in% select_strategies_plus &
      under5 == 2 & time %in% c("week", "month") &
      aggregate == FALSE
  ) %>%
  group_by(data_source, under5, date, strategy, strategy_name, time) %>%
  distinct(
    n.cases,
    truth,
    ir,
    true.ir,
    n.suspected,
    positivity,
    truth.positivity,
    time
  ) %>%
  arrange(data_source, under5, date) %>%
  rowwise() %>%
  mutate(perc_change = case_when(
    truth == 0 & n.cases == 0 ~ 0,
    TRUE ~ (ir <= 0.5 * true.ir ||
      ir >= 1.5 * true.ir)
  )) %>%
  group_by(data_source, strategy, strategy_name, time) %>%
  summarize(
    perc = sum(perc_change, na.rm = T) / n(),
    .groups = "drop"
  ) %>%
  mutate(time = factor(
    time,
    levels = c("week", "month"),
    labels = c("Week", "Month")
  ))

figs5 <- fig3s_empirical %>%
  ggplot(aes(
    y = strategy,
    x = perc,
    color = data_source,
    shape = time
  )) +
  geom_point(position = position_dodge(0.1)) +
  scale_color_manual(values = col_vals_cases) +
  theme_minimal() +
  facet_wrap(~time) +
  labs(
    x = "Proportion of time periods where the estimated cholera IR is\n50% higher or lower than the true cholera IR",
    y = "Sampling Strategy",
    color = "Catchment Area",
    shape = "Time scale"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.border = element_rect(
      color = "black",
      size = 0.5,
      fill = NA
    )
  ) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  coord_cartesian(xlim = c(0, 0.9)) +
  guides(
    color = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    ),
    shape = guide_legend(
      title.position = "left",
      nrow = 2,
      byrow = TRUE
    )
  )

ggsave(
  here::here("figures/fig_S5.pdf"),
  width = 10,
  height = 10,
  figs5 + (fig2.combined_shading_plot & theme(strip.text = element_text(size = 10))) + patchwork::plot_layout(ncol = 2, widths = c(3, 1))
)
message("Saved fig_S5.pdf")

## Supplemental Figure 6. Relative bias in weekly and monthly cholera case ------
# estimates by the tertiles of the normalized true incidence rate of cholera 
# (z-score) for select sampling strategies in Sitakunda, Bangladesh (BGD)
# (2021–2022) and Uvira, Democratic Republic of Congo (DRC) (2016–2022). All 
# values are scaled relative to the maximum absolute distance from zero, with 
# the dashed black line representing no bias (i.e. zero). 

select_strategies_p1 <- c(select_strategies_plus, "All cases once a week (random day)")
levels_strategies_p1 <- ifelse(
  str_detect(select_strategies_p1, "All cases once"),
  "All cases once\na week (random day)",
  select_strategies_p1
)

f4 <- case.data %>%
  filter(
    time %in% c("week", "month") & under5 == 2 &
      strategy %in% select_strategies_p1 &
      aggregate == FALSE
  ) %>%
  rename(bias = bias.cases) %>%
  group_by(data_source, time, period, under5, strategy) %>%
  filter(!is.na(ir)) %>%
  mutate(
    ir_norm = (true.ir - mean(true.ir, na.rm = T)) / sd(true.ir, na.rm = T),
    ntile_clean = {
      x <- ir_norm
      out <- rep(NA_integer_, length(x))
      not_na <- !is.na(x)
      out[not_na] <- ntile(x[not_na], 3)
      out
    },
    bir_norm = factor(ntile_clean, labels = c("T1", "T2", "T3")),
    strategy = if_else(
      str_detect(strategy, "All cases once"),
      "All cases once\na week (random day)",
      strategy
    ),
    strategy = factor(strategy, levels = levels_strategies_p1)
  )

box_theme <- theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(size = 12),
    strip.placement = "outside",
    strip.clip = "off",
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10, margin = margin(b = -3)),
    strip.text.y = element_text(size = 10, margin = margin(l = -3))
  )

f4.week <- f4 %>%
  filter(time == "week") %>%
  ggplot(aes(x = bir_norm, y = bias, color = data_source)) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~strategy) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(x = "Normalized IR Tertiles", y = "Bias in Cholera Cases", color = "Catchment Area") +
  scale_color_manual(values = col_vals_cases) +
  box_theme

f4.month <- f4 %>%
  filter(time == "month") %>%
  ggplot(aes(x = bir_norm, y = bias, color = data_source)) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~strategy) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(x = "Normalized IR Tertiles", y = "Bias in Cholera Cases", color = "Catchment Area") +
  scale_color_manual(values = col_vals_cases) +
  box_theme

f4.week.final <- f4.week +
  theme(
    legend.position = "none",
    plot.tag = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    plot.tag.position = "topleft"
  ) +
  facet_wrap(~strategy, scale = "free_y") +
  labs(tag = "A)", title = "Week")

f4.month.final <- f4.month +
  theme(
    legend.position = "none",
    plot.tag = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    plot.tag.position = "topleft"
  ) +
  facet_wrap(~strategy, scale = "free_y") +
  labs(tag = "B)", title = "Month")

ggsave(
  here::here("figures/fig_S6.pdf"),
  width = 9, height = 12,
  cowplot::plot_grid(
    f4.week.final + facet_wrap(~strategy, scale = "free_y", nrow = 2),
    f4.month.final + facet_wrap(~strategy, scale = "free_y", nrow = 2),
    cowplot::get_plot_component(f4.week, "guide-box", return_all = T),
    ncol = 1,
    rel_heights = c(0.45, 0.45, 0.1)
  )
)
message("Saved fig_S6.pdf")
## Supplemental Figure 7. The proportion of suspected cholera cases that were -----
# sampled each week with systematic (n=2), daily (n=3, n=5), weekly (n=3, n=10) 
# testing strategies in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, 
# Democratic Republic of Congo (DRC) (2016–2022). Top panels show the weekly
# number of RDT-tested suspected cases.
select_strategies_sf <- c(
    "Every n (n=2)",
    "First n daily (n=3)",
    "First n daily (n=5)",
    "First n weekly (n=3)",
    "First n weekly (n=10)"
)

figs6.shading <- sampled.data %>%
    select(data_source, week, starts_with("sample_")) %>%
    pivot_longer(-data_source:-week) %>%
    group_by(data_source, week, name) %>%
    summarize(sampled = sum(value)) %>%
    mutate(strategy = factor(name, levels = sampled_strategies, labels = sampled_strategies_label))

figs6.sev.shading <- sampled.data %>%
    mutate(severity_measured = case_when(
        dehydration != "Don't know" &
            !is.na(dehydration) ~ T, !is.na(los_days) &
            !is.na(date_arrival) ~ T,
        T ~ F
    )) %>%
    count(data_source, week, severity_measured, name = "severity") %>%
    filter(severity_measured) %>%
    select(-severity_measured)

figs6.shading <- figs6.shading %>%
    left_join(figs6.sev.shading) %>%
    group_by(data_source, week) %>%
    left_join(
        figs6.shading %>% filter(strategy == "All cases") %>%
            select(data_source, week, total.suspected = sampled)
    ) %>%
    mutate(
        prop = if_else(
            name == "sample_severe_cases",
            sampled / severity,
            sampled / total.suspected
        ),
        age = if_else(str_detect(strategy, "age"), "Yes", "No") %>% factor()
    ) %>%
    filter(age != "Yes" & strategy %in% select_strategies_sf) %>%
    filter(data_source == serochit_label |
               week >= as.Date("2020-01-01"))

fig6_plot <- figs6.shading %>%
    ggplot(aes(x = week, y = prop, color = data_source)) +
    geom_point(position = position_dodge(1)) +
    geom_line() +
    labs(y = "Proportion of suspected cases sampled", x = "Week", tag = "B)") +
    theme_minimal() +
    scale_color_manual(values = col_vals_cases) +
    facet_wrap(
        vars(strategy, data_source),
        scales = "free_x",
        ncol = 2,
        labeller = labeller(
            strategy = function(x) {
                paste0("**", x, "**")
            },
            data_source = label_value
        )
    ) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        strip.text = ggtext::element_markdown(size = 10, margin = margin(1, 1, 1, 1)),
        legend.position = "none",
        plot.tag = element_text(face = "bold")
    ) +
    scale_x_date(date_labels = "%b '%y")

# Epi curve of weekly tested suspected cases above each column
figs7_epi <- figs6.shading %>%
    ungroup() %>%
    distinct(data_source, week, total.suspected) %>%
    ggplot(aes(x = week, y = total.suspected, fill = data_source)) +
    geom_col() +
    facet_wrap(~data_source, ncol = 2, scales = "free_x") +
    labs(y = "Suspected cases", x = NULL, tag = "A)") +
    theme_minimal() +
    scale_fill_manual(values = col_vals_cases) +
    scale_x_date(date_labels = "%b '%y") +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.tag = element_text(face = "bold")
    )

figs7 <- figs7_epi / fig6_plot + plot_layout(heights = c(1, 5))

ggsave(here::here("figures/fig_S7.pdf"),
       figs7,
       width = 10,
       height = 11.5
)

ggsave(here::here("figures/fig_S7.png"),
       figs7,
       width = 10,
       height = 11.5,
       dpi = 300
)
message("Saved fig_S7.pdf")

## Supplemental Figure 8.  Estimated bias in cholera case estimation (sample ------
# strategy estimation versus true number of cholera case incidence) for 
# strategies that all cases on a specific day of the week or a random day 
# each week in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic 
# Republic of Congo (DRC) (2016–2022). The boxes in the middle indicate the
# proportion of RDT-tested suspected cases that were sampled during the study
# period for each catchment area and strategy. Violin areas are scaled to have
# the same maximum width across strategies and time periods. Weekly and annual 
# cases were estimated by multiplying the cholera positivity among those sampled 
# over the corresponding time period each week with the number of suspected 
# cases for the same time period. Weekly cases were aggregated across a year to
# estimate annualized cases/incidence rates.

select_strategies_dayofweek <- c(
  "All cases once a week (random day)",
  "All cases: Monday",
  "All cases: Tuesday",
  "All cases: Wednesday",
  "All cases: Thursday",
  "All cases: Friday",
  "All cases: Saturday",
  "All cases: Sunday"
)
day_levels <- levels(case.data$strategy)[str_detect(levels(case.data$strategy), "All cases")]
day_levels <- ifelse(str_detect(day_levels, "once"),
  "All cases once a\nweek (random day)",
  day_levels
)
day_levels <- c("Severe cases only", day_levels)

figs2_plot <- case.data %>%
  filter(under5 == 2 &
    time != "month" &
    strategy %in% select_strategies_dayofweek) %>%
  filter((time == "week" &
    aggregate == F) |
    (time == "year" & aggregate == T)) %>%
  mutate(
    strategy = if_else(
      str_detect(strategy, "once"),
      "All cases once a\nweek (random day)",
      strategy
    ),
    strategy = factor(strategy, levels = day_levels)
  )

ps2 <- plot_cases(
  plot.data = figs2_plot %>% rename(
    value = bias.cases,
    value.lo = bias.cases.lo,
    value.hi = bias.cases.hi,
    period_lab = period_case_lab
  ),
  col_vals = col_vals_cases,
  lab = "Cases",
  comp_annual = F
) +
  ggh4x::facetted_pos_scales(x = list(
    period_lab == "Annualized Cases" ~
      scale_x_continuous(breaks = c(-300, -200, -100, 0, 100, 200, 300))
  ))

ps2.ir <- plot_cases(
  plot.data = figs2_plot %>% rename(
    value = bias.ir,
    value.lo = bias.ir.lo,
    value.hi = bias.ir.hi,
    period_lab = period_ir_lab
  ),
  col_vals = col_vals_ir,
  lab = "IR",
  comp_annual = F
) +
    ggh4x::facetted_pos_scales(x = list(
        period_lab == "Annualized IR" ~
            scale_x_continuous(breaks = c(-5, -3, -1, 0, 1, 3, 5))
    )) +
  theme(
    axis.title.y.right = element_text(),
    axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2)),
    axis.ticks.y.right = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank()
  ) +
  scale_y_discrete(position = "right")

figs2.shading.data <- shading %>%
  filter(strategy %in% unique(sort(
    ps2@layers$geom_linerange$data$strategy_name
  ))) %>%
  mutate(strategy = factor(strategy, levels = levels(
    ps2@layers$geom_linerange$data$strategy
  )))
figs2.combined_shading_plot <- patchwork::wrap_plots(lapply(unique(figs2.shading.data$data_source), function(x) {
  create_shading_plot(shading.data = figs2.shading.data, facet = x) +
    theme(strip.text = element_text(size = 8))
})) + patchwork::plot_layout(ncol = 2, axis_titles = "collect_x") &
  guides(fill = "none")

figs8_plot = ps2 + figs2.combined_shading_plot + ps2.ir +
    patchwork::plot_layout(ncol = 3, widths = c(2.8, 1.4, 2.8)) &
    theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10)
    )

ggsave(
  here::here("figures/fig_S8.pdf"),
  width = 9,
  height = 8,
  figs8_plot
)

ggsave(here::here("figures/fig_S8.png"),
       figs8_plot,
       width = 9,
       height = 8, 
       dpi = 300
)

message("Saved fig_S8.pdf")


## Supplemental Figure 9. Estimated bias in cholera case estimation (sample strategy -----
# estimation versus true number of cholera cases) by sampling strategy and 
# timescale in Sitakunda, Bangladesh (BGD) (2021–2022) and Uvira, Democratic 
# Republic of Congo (DRC) (2016–2022). The boxes in the middle indicate the 
# proportion of RDT-tested suspected cases that were sampled during the study 
# period for each catchment area and strategy. Sampling strategies presented 
# include sampling the first n cases each day (daily) or week (week) and sampling 
# every n cases (systematic). Violin areas are scaled to have the same maximum 
# width across strategies and time periods. Weekly and annual cases were 
# estimated by multiplying the cholera positivity among those sampled over 
# the corresponding period each week with the number of suspected cases for 
# the same period. Weekly cases were aggregated across a year to estimate 
# annualized cases/incidence rates. Cases and incidence rates are both shown 
# to help contextualize findings.

figs9_plot <- case.data %>%
    filter(strategy %in% c(select_strategies_plus, "Severe cases only") & under5 == 2) %>%
    filter((time == "week" &
                aggregate == F) | (time == "year" & aggregate == T)) %>%
    # Restrict to periods with an actual estimate: years where a strategy (e.g.
    # "Severe cases only") sampled nobody all year have no estimate to compare
    # against truth and should be excluded, not plotted as a fabricated bias.
    filter(!is.na(bias.cases))

ps9 <- plot_cases(
    plot.data = figs9_plot %>% rename(
        value = bias.cases,
        value.lo = bias.cases.lo,
        value.hi = bias.cases.hi,
        period_lab = period_case_lab
    ),
    col_vals = col_vals_cases,
    lab = "Cases",
    comp_annual = F
) +
    ggh4x::facetted_pos_scales(x = list(
        period_lab == "Annualized Cases" ~ scale_x_continuous(
            breaks = c(-400, -200, 0, 200, 400))
    ))

ps9.ir <- plot_cases(
    plot.data = figs9_plot %>% rename(
        value = bias.ir,
        value.lo = bias.ir.lo,
        value.hi = bias.ir.hi,
        period_lab = period_ir_lab
    ),
    col_vals = col_vals_ir,
    lab = "IR",
    comp_annual = F
) +
    theme(
        axis.title.y.right = element_text(),
        axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2)),
        axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank()
    ) +
    scale_y_discrete(position = "right") +
    ggh4x::facetted_pos_scales(x = list(period_lab == "Annualized IR" ~ scale_x_continuous(breaks = c(
        -15, -10, -5, 0, 5, 10, 15
    ))))

figs9.shading.data <- shading %>%
    filter(strategy %in% unique(sort(
        ps9@layers$geom_linerange$data$strategy_name
    ))) %>%
    mutate(strategy = factor(strategy, levels = levels(ps9@layers$geom_linerange$data$strategy)))
figs9.combined_shading_plot <- patchwork::wrap_plots(lapply(unique(figs9.shading.data$data_source), function(x) {
    create_shading_plot(shading.data = figs9.shading.data, facet = x) + theme(strip.text = element_text(size = 8))
})) + patchwork::plot_layout(ncol = 2, axis_titles = "collect_x") &
    guides(fill = "none")

figs9 <- ps9 + figs9.combined_shading_plot + ps9.ir +
    patchwork::plot_layout(ncol = 3, widths = c(3.3, 1.4, 2.7)) &
    theme(
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10)
    )

ggsave(here::here("figures/fig_S9.png"),
       figs9,
       width = 12,
       height = 8, 
       dpi = 300
)
message("Saved fig_S9.png")

ggsave(here::here("figures/fig_S9.pdf"),
       fig6_plot,
       width = 12,
       height = 8
)
message("Saved fig_S9.pdf")

### ---- SAVE TABLE 1 ----
sf2 <- clinical.data %>%
  filter(data_source == serochit_label |
    date_arrival >= as.Date("2016-04-07")) %>%
  mutate(
    severe = if_else(
      dehydration == "Severe" |
        los_days > 2,
      1,
      0
    ),
    year = lubridate::year(date_arrival),
    n_hospitalized = !is.na(los_days),
    duration = los_days,
    rdt_tested = !is.na(rdt_result) &
      rdt_result != "Invalid"
  ) %>%
  select(
    data_source,
    date_arrival,
    age_group,
    under5,
    rdt_tested,
    severe,
    n_hospitalized,
    duration,
    year
  ) %>%
  gtsummary::tbl_summary(
    by = "data_source",
    label = list(
      date_arrival = "Visit Date (Range)",
      age_group = "Age group (years)",
      under5 = "Age <5 years",
      severe = "Severe Diarrhea",
      duration = "Duration of hospital stay"
    ),
    statistic = list(date_arrival ~ "{min} to {max}")
  )

tab_sf2 <- sf2 %>%
  gtsummary::as_flex_table() %>%
  theme_vanilla() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_outer(part = "all", border = black_border) %>%
  border_inner_h(part = "all", border = black_border) %>%
  border_inner_v(part = "all", border = black_border) %>%
  padding(padding = 2, part = "all")

save_as_docx(tab_sf2, path = here::here("tables/tab_1.docx"))
message("Saved tab_1.docx")

### ---- SAVE TABLE S2 ----
sf3 <- clinical.data %>%
  filter(data_source == uvira_label &
    date_arrival >= as.Date("2016-04-07"), 
    rdt_result != "Invalid" | is.na(rdt_result)) %>%
  mutate(
    severe = if_else(
      dehydration == "Severe" |
        los_days > 2,
      1,
      0
    ),
    year = lubridate::year(date_arrival),
    tested = if_else(is.na(rdt_result) | rdt_result == "Invalid", "Not Tested", "Tested")
  ) %>%
  select(tested, age_group, under5, severe, year) %>%
  gtsummary::tbl_summary(
    by = "tested",
    percent = "row",
    label = list(
      age_group = "Age group (years)",
      under5 = "Age <5 years",
      severe = "Severe Diarrhea",
      year = "Visit Year"
    )
  )

tab_sf3 <- sf3 %>%
  gtsummary::as_flex_table() %>%
  theme_vanilla() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_outer(part = "all", border = black_border) %>%
  border_inner_h(part = "all", border = black_border) %>%
  border_inner_v(part = "all", border = black_border) %>%
  padding(padding = 2, part = "all")

save_as_docx(tab_sf3, path = here::here("tables/tab_s2.docx"))
message("Saved tab_s2.docx")

### ---- TABLE S3: ICC / design effect by site and strategy ----
## Supplemental Table 3. Intraclass correlation coefficient (ICC) of RDT
# positivity within facility-days, mean cluster size, and the resulting design
# effect and confidence interval inflation, overall and under selected sampling
# strategies. Adapted from scripts/icc.R.
tab_s3_caption <- paste(
    "Supplemental Table 3. Intraclass correlation coefficient (ICC) of RDT positivity ",
    "within facility-days, mean cluster size, and the associated design effect and ",
    "inflation in confidence intervals (CI), overall and by sampling strategy. ",
    "The ICC was estimated on the logit scale from a logistic regression with a ",
    "fixed effect for facility and random intercepts for facility-week and facility-day ",
    "within week (lme4 package in R). It is the proportion of the variance within ",
    "facility-weeks due to differences between facility-days (facility-day variance ",
    "divided by the sum of the facility-day variance and the patient-level residual ",
    "variance on the logit scale). Mean cluster size is the size-weighted mean number of ",
    "RDT-tested suspected cases per facility-day: all tested cases for Overall, and the ",
    "cases sampled under each strategy. The design effect (DE) is 1+(m-1)  × ICC . ",
    "CI inflation (√(DE-1))) is the percent increase in CI width when accounting for the ",
    "design effect relative to those that assume cases are independent within facility-day."
)

icc_dat <- clinical.data %>%
  filter(!is.na(rdt_result)) %>%
  mutate(
    rdt_pos = as.integer(rdt_result == "Positive"),
    week = floor_date(date_arrival, "week")
  )

icc_m_sita <- lme4::glmer(rdt_pos ~ site + (1 | site:week) + (1 | site:week:date_arrival),
  data = icc_dat %>% filter(data_source == serochit_label), family = binomial
)
icc_m_uvira <- lme4::glmer(rdt_pos ~ site + (1 | site:week) + (1 | site:week:date_arrival),
  data = icc_dat %>% filter(data_source == uvira_label), family = binomial
)

# Facility-day ICC within facility-weeks (estimates are made per facility-week):
# day variance / (day variance + pi^2/3)
icc_vals <- bind_rows(
  as.data.frame(lme4::VarCorr(icc_m_sita)) %>% mutate(data_source = serochit_label),
  as.data.frame(lme4::VarCorr(icc_m_uvira)) %>% mutate(data_source = uvira_label)
) %>%
  filter(grp == "site:week:date_arrival") %>%
  transmute(data_source, icc = vcov / (vcov + pi^2 / 3))

# Size-weighted mean cluster size, sum(n^2) / sum(n), over facility-days with >0 sampled
icc_overall <- icc_dat %>%
  count(data_source, site, date_arrival) %>%
  group_by(data_source) %>%
  summarize(mean_cluster = sum(n^2) / sum(n), .groups = "drop") %>%
  mutate(strategy = "Overall")

icc_strategy <- sampled.data %>%
  group_by(data_source, date_arrival, site) %>%
  summarize(
    `First n daily (n=3)` = sum(sample_first_3_cases),
    `First n weekly (n=3)` = sum(sample_first_3_cases_week),
    `Every n (n=2)` = sum(sample_every2),
    .groups = "drop"
  ) %>%
  group_by(data_source) %>%
  summarize(across(
    c(`First n daily (n=3)`, `First n weekly (n=3)`, `Every n (n=2)`),
    ~ sum(.x^2) / sum(.x)
  ), .groups = "drop") %>%
  pivot_longer(-data_source, names_to = "strategy", values_to = "mean_cluster")

icc_table <- bind_rows(icc_overall, icc_strategy) %>%
  left_join(icc_vals, by = "data_source") %>%
  mutate(
    deff = 1 + (mean_cluster - 1) * icc,
    ci_inflation_pct = 100 * (sqrt(deff) - 1),
    strategy = factor(strategy, levels = c(
      "Overall", "First n daily (n=3)", "First n weekly (n=3)", "Every n (n=2)"
    ))
  ) %>%
  arrange(data_source, strategy) %>%
  transmute(
    Site = data_source,
    Strategy = strategy,
    ICC = round(icc, 3),
    `Mean cluster size` = round(mean_cluster, 3),
    `Design effect` = round(deff, 2),
    `CI inflation (%)` = round(ci_inflation_pct, 2)
  )

tab_icc <- icc_table %>%
  flextable() %>%
  set_caption(tab_s3_caption) %>%
  merge_v(j = "Site") %>%
  valign(j = "Site", valign = "top") %>%
  theme_vanilla() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  border_outer(part = "all", border = black_border) %>%
  border_inner_h(part = "all", border = black_border) %>%
  border_inner_v(part = "all", border = black_border) %>%
  padding(padding = 2, part = "all")

tab_icc

### ---- PRINT: Descriptions -----
values <- list()

values$suspected_sita <- clinical.data %>%
  count(data_source) %>%
  filter(data_source ==
    serochit_label) %>%
  pull(n)
values$suspected_uvira <- clinical.data %>%
  count(data_source) %>%
  filter(data_source ==
    uvira_label) %>%
  pull(n)

temp <- clinical.data %>%
  filter(
    (data_source == serochit_label & week <= as.Date("2022-08-28")) |
      (
        data_source == uvira_label & week <= as.Date("2022-12-25") &
          date_arrival >= as.Date("2016-04-07")
      )
  ) %>%
  group_by(data_source) %>%
  summarize(
    start_date = min(date_arrival),
    end_date = max(date_arrival),
    n.suspected = n(),
    n.cases = length(rdt_result[rdt_result == "Positive"])
  ) %>%
  mutate(
    ir.cases = if_else(
      data_source == uvira_label,
      n.cases / pop.uvira,
      n.cases / pop.serochit
    ),
    ir.cases.lo = if_else(
      data_source == uvira_label,
      (n.cases + qnorm(0.025) * sqrt(n.cases)) /
        pop.uvira,
      (n.cases + qnorm(0.025) * sqrt(n.cases)) /
        pop.serochit
    ),
    ir.cases.hi = if_else(
      data_source == uvira_label,
      (n.cases + qnorm(.975) * sqrt(n.cases)) /
        pop.uvira,
      (n.cases + qnorm(.975) * sqrt(n.cases)) /
        pop.serochit
    ),
    ir.suspected = if_else(
      data_source == uvira_label,
      n.suspected / pop.uvira,
      n.suspected / pop.serochit
    ),
    ir.suspected.lo = if_else(
      data_source == uvira_label,
      (n.suspected + qnorm(0.025) * sqrt(n.suspected)) /
        pop.uvira,
      (n.suspected + qnorm(0.025) * sqrt(n.suspected)) /
        pop.serochit
    ),
    ir.suspected.hi = if_else(
      data_source == uvira_label,
      (n.suspected + qnorm(.975) * sqrt(n.suspected)) /
        pop.uvira,
      (n.suspected + qnorm(.975) * sqrt(n.suspected)) /
        pop.serochit
    ),
    across(
      starts_with("ir"),
      ~ .x / as.numeric(end_date - start_date) * 365 * inc_modifier
    )
  )

values$suspected_uvira_ir <- signif(temp$ir.suspected[temp$data_source ==
  uvira_label], 3)
values$suspected_uvira_ir <- paste0(
  values$suspected_uvira_ir,
  " (95% CI ",
  signif(temp$ir.suspected.lo[temp$data_source ==
    uvira_label], 3),
  ", ",
  signif(temp$ir.suspected.hi[temp$data_source ==
    uvira_label], 3),
  ")"
)

values$suspected_sita_ir <- signif(temp$ir.suspected[temp$data_source ==
  serochit_label], 3)
values$suspected_sita_ir <- paste0(
  values$suspected_sita_ir,
  " (95% CI ",
  signif(temp$ir.suspected.lo[temp$data_source ==
    serochit_label], 3),
  ", ",
  signif(temp$ir.suspected.hi[temp$data_source ==
    serochit_label], 3),
  ")"
)

values$cases_uvira_ir <- signif(temp$ir.cases[temp$data_source == uvira_label], 3)
values$cases_uvira_ir <- paste0(
  values$cases_uvira_ir,
  " (95% CI ",
  signif(temp$ir.cases.lo[temp$data_source ==
    uvira_label], 3),
  ", ",
  signif(temp$ir.cases.hi[temp$data_source ==
    uvira_label], 3),
  ")"
)

values$cases_sita_ir <- signif(temp$ir.cases[temp$data_source == serochit_label], 3)
values$cases_sita_ir <- paste0(
  values$cases_sita_ir,
  " (95% CI ",
  signif(temp$ir.cases.lo[temp$data_source ==
    serochit_label], 3),
  ", ",
  signif(temp$ir.cases.hi[temp$data_source ==
    serochit_label], 3),
  ")"
)

values$tested_sita <- clinical.data %>%
  filter(data_source == serochit_label &
    rdt_result %in% c("Negative", "Positive")) %>%
  nrow()
values$positive_sita <- clinical.data %>%
  filter(data_source == serochit_label &
    rdt_result %in% c("Positive")) %>%
  nrow()
values$tested_uvira <- clinical.data %>%
  filter(data_source == uvira_label &
    rdt_result %in% c("Negative", "Positive")) %>%
  nrow()
values$denom_tested_uvira <- nrow(filter(
  clinical.data,
  data_source == uvira_label &
    date_arrival >= as.Date("2016-04-07")
))
values$positive_uvira <- clinical.data %>%
  filter(data_source == uvira_label &
    rdt_result %in% c("Positive")) %>%
  nrow()
values$min_tested_uvira <- clinical.data %>%
  filter(date_arrival >= as.Date("2016-04-07") &
    data_source == uvira_label) %>%
  group_by(
    year = floor_date(date_arrival, "year"),
    tested = if_else(is.na(rdt_result), 0, 1)
  ) %>%
  summarize(n = n()) %>%
  mutate(
    prop =
      prop.table(n)
  ) %>%
  filter(tested == 1) %>%
  ungroup() %>%
  filter(prop == min(prop)) %>%
  pull() %>%
  signif(., 3) *
  100
values$max_tested_uvira <- clinical.data %>%
  filter(date_arrival >= as.Date("2016-04-07") &
    data_source == uvira_label) %>%
  group_by(
    year = floor_date(date_arrival, "year"),
    tested = if_else(is.na(rdt_result), 0, 1)
  ) %>%
  summarize(n = n()) %>%
  mutate(
    prop =
      prop.table(n)
  ) %>%
  filter(tested == 1) %>%
  ungroup() %>%
  filter(prop == max(prop)) %>%
  pull() %>%
  signif(., 3) *
  100

z <- qnorm(0.975)


weekly.site <- clinical.data %>%
    filter(!is.na(site)) %>% 
    mutate(week = floor_date(date_arrival, "week")) %>%
    group_by(data_source, week, site) %>%
    summarize(n.suspected = n(),
              n.tested = sum(rdt_result %in% c("Positive", "Negative")),
              n.positive = sum(rdt_result == "Positive", na.rm = TRUE),
              .groups = "drop")

weekly.site = weekly.site %>% arrange(site, week) %>% mutate(missing = n.tested == 0)
site.weeks <- tibble()

for(i in unique(weekly.site$site)){
    
    if(all(!weekly.site$missing[weekly.site$site==i])){
        site.weeks <- weekly.site %>% filter(site == i) %>% 
            bind_rows(site.weeks)
        next
    }
    
    rle.site.weeks <- rle(weekly.site$missing[weekly.site$site==i])
    site.weeks <- weekly.site %>% 
        filter(site == i) %>% 
        mutate(consecutive = rep(rle.site.weeks$lengths, rle.site.weeks$lengths),
               consecutive_group = rep(1:length(rle.site.weeks$lengths), rle.site.weeks$lengths)) %>%
        bind_rows(site.weeks)
}

site.weeks <- site.weeks %>%
    mutate(
        missing.year = missing & consecutive >= 4,   # >=4 consecutive missing weeks -> annual average
        missing.month = missing & consecutive < 4    # <4 consecutive missing weeks -> recent 4-week average
    ) %>%
    arrange(site, week) %>%
    tibble::rowid_to_column()

impute.data <- site.weeks

missing.rows.year <- site.weeks$rowid[site.weeks$missing.year]
missing.rows.month <- site.weeks$rowid[site.weeks$missing.month]

if (length(missing.rows.year) > 0) {
    for (j in seq_along(missing.rows.year)) {
        row_j <- site.weeks[site.weeks$rowid == missing.rows.year[j], ]
        hist <- site.weeks[site.weeks$rowid < missing.rows.year[j] &
                               !site.weeks$missing &
                               site.weeks$site == row_j$site &
                               site.weeks$week >= row_j$week - 365, ]
        if (nrow(hist) > 0) {
            agg <- hist %>% summarize(n.positive = sum(n.positive), n.tested = sum(n.tested))
            impute.data[impute.data$rowid == missing.rows.year[j], c("n.positive", "n.tested")] <- agg
        }
    }
}

if (length(missing.rows.month) > 0) {
    for (j in seq_along(missing.rows.month)) {
        row_j <- site.weeks[site.weeks$rowid == missing.rows.month[j], ]
        hist <- site.weeks[site.weeks$rowid < missing.rows.month[j] &
                               !site.weeks$missing &
                               site.weeks$site == row_j$site &
                               site.weeks$week >= row_j$week - weeks(4), ]
        if (nrow(hist) > 0) {
            agg <- hist %>% summarize(n.positive = sum(n.positive), n.tested = sum(n.tested))
            impute.data[impute.data$rowid == missing.rows.month[j], c("n.positive", "n.tested")] <- agg
        }
    }
}

# exclude missing estimates resulting from no cases being tested in the preceding year
impute.data <- impute.data %>% anti_join(
    impute.data %>% filter(missing, n.tested == 0)
) 

impute.data <- impute.data %>%
    mutate(phat = n.positive/n.tested,
           denom = 1 + z^2/n.tested,
           wilson.half = (z/denom)*sqrt(phat*(1-phat)/n.tested + z^2/(4*n.tested^2)),
           var.p = (wilson.half/z)^2,
           n.cases.site = phat * n.suspected,
           var.cases.site = n.suspected*phat*(1-phat) + n.suspected^2*var.p)

weekly.implied <- impute.data %>%
    group_by(data_source, week) %>%
    summarize(n.cases.implied = sum(n.cases.site),
              var.cases.implied = sum(var.cases.site),
              .groups = "drop")

temp.implied <- weekly.implied %>%
    group_by(data_source) %>%
    summarize(n.cases = sum(n.cases.implied), var.cases = sum(var.cases.implied)) %>%
    left_join(temp %>% select(data_source, start_date, end_date)) %>%
    mutate(se.cases = sqrt(var.cases),
           ir.cases.implied = if_else(data_source == uvira_label, n.cases/pop.uvira, n.cases/pop.serochit),
           ir.cases.implied.lo = if_else(data_source == uvira_label, pmax(0, n.cases-z*se.cases)/pop.uvira, pmax(0, n.cases-z*se.cases)/pop.serochit),
           ir.cases.implied.hi = if_else(data_source == uvira_label, (n.cases+z*se.cases)/pop.uvira, (n.cases+z*se.cases)/pop.serochit),
           across(starts_with("ir.cases.implied"), ~.x/as.numeric(end_date-start_date)*365*inc_modifier))

values$cases_implied_sita_ir <- signif(temp.implied$ir.cases.implied[temp.implied$data_source==serochit_label], 3)
values$cases_implied_sita_ir <- paste0(values$cases_implied_sita_ir, " (95% CI ",
                                       signif(temp.implied$ir.cases.implied.lo[temp.implied$data_source==serochit_label], 3), ", ",
                                       signif(temp.implied$ir.cases.implied.hi[temp.implied$data_source==serochit_label], 3), ")")

values$cases_implied_uvira_ir <- signif(temp.implied$ir.cases.implied[temp.implied$data_source==uvira_label], 3)
values$cases_implied_uvira_ir <- paste0(values$cases_implied_uvira_ir, " (95% CI ",
                                        signif(temp.implied$ir.cases.implied.lo[temp.implied$data_source==uvira_label], 3), ", ",
                                        signif(temp.implied$ir.cases.implied.hi[temp.implied$data_source==uvira_label], 3), ")")
### ---- PRINT: Sample description values ----
# ... values <- list() block ...

cat(
  paste0(
    "Suspected cases Sitakunda: ",
    values$suspected_sita,
    "\n",
    "Suspected IR Sitakunda: ",
    values$suspected_sita_ir,
    "\n",
    "Suspected cases Uvira: ",
    values$suspected_uvira,
    "\n",
    "Suspected IR Uvira: ",
    values$suspected_uvira_ir,
    "\n",
    "Confirmed IR Sitakunda: ",
    values$cases_sita_ir,
    "\n",
    "Confirmed IR Uvira: ",
    values$cases_uvira_ir,
    "\n",
    "Tested Sitakunda: ",
    values$tested_sita,
    "\n",
    "Positive Sitakunda: ",
    values$positive_sita,
    "\n",
    "Tested Uvira: ",
    values$tested_uvira,
    "\n",
    "Positive Uvira: ",
    values$positive_uvira,
    "\n",
    "Min % tested per year Uvira: ",
    values$min_tested_uvira,
    "%\n",
    "Max % tested per year Uvira: ",
    values$max_tested_uvira,
    "%\n",
    "RDT-implied IR for all suspected cases Sitakunda: ",
    "\n", 
    values$cases_implied_sita_ir, 
    "\n",
    "RDT-implied IR for all suspected cases Uvira: ",
    "\n", 
    values$cases_implied_uvira_ir
  )
)

### ---- PRINT: Perc Difference in estimates vs truth ----
message("\n\n=== Perc Difference in estimates vs truth ===\n")

message("-- All strategies except Severe cases only --")
case.data %>%
  filter(strategy %in% select_strategies_plus[select_strategies_plus != "Severe cases only"] &
    under5 == 2 & time == "year" & aggregate == T) %>%
  group_by(data_source, time) %>%
  summarize(
    min.cases = min(bias.cases),
    max.cases = max(bias.cases),
    median.cases = median(bias.cases),
    median.ir = median(bias.ir),
    min.ir = min(bias.ir),
    max.ir = max(bias.ir),
    median.ir.perc = median((ir - true.ir) / true.ir, na.rm = T),
    min.ir.perc = min((ir - true.ir) / true.ir, na.rm = T),
    max.ir.perc = max((ir - true.ir) / true.ir, na.rm = T)
  ) %>%
  mutate(across(is.numeric, ~ signif(.x, 3))) %>%
  print(n = Inf)

message("\n-- Severe cases annual bias (median and range; restricted to years with any data) --")
case.data %>%
  filter(aggregate == T, under5 == 2, strategy == "Severe cases only") %>%
  filter(!is.na(bias.ir)) %>%
  group_by(data_source) %>%
  summarize(
    median = median(bias.ir),
    min = min(bias.ir),
    max = max(bias.ir)
  ) %>%
  mutate(across(is.numeric, ~ signif(.x, 3))) %>%
  unite(min:max, col = "ci", sep = ", ") %>%
  mutate(ci = paste0("(Range: ", ci, ")")) %>%
  unite(median:ci, col = "ci", sep = " ") %>%
  mutate(ci = paste0("Median: ", ci)) %>%
  rename(`Severe cases` = ci) %>%
  print()

message("\nDone.")
