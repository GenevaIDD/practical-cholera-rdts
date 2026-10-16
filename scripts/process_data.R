# Process Clinical Surveillance Data: Sample, IR estimation, Bias calculation

library(tidyverse)
library(lubridate)
# random draws in some sampling strategies (first_n_cases_week, all_once_week)
# depend on the exact sequence of RNG calls up to that point.
# Any change in the data pipeline (e.g. different number of rows) will shift
# the seed state and change results
set.seed(129623)

uvira_label <- "Uvira"
serochit_label <- "Sitakunda"
### 1. Download data ----
data <- read_csv(here::here("data/clinical_data.csv"))
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

### 2. Sampling datasets for volume (all clinical cases of known age) and bias analysis (known age and RDT results) ----
source(here::here("scripts/sampling_funcs.R"))
source(here::here("scripts/cases_per_period_delta_wilson.R"))

sampled.data <- list()
volume.data <- list()
data_sources <- c(serochit_label, uvira_label)

for (i in 1:length(data_sources)) {
  sampled.data[[i]] <- data %>%
    filter(data_source == data_sources[i]) %>%
    filter(rdt_result %in% c("Positive", "Negative")) %>%
    first_n_cases(n = 1) %>%
    first_n_cases(n = 2) %>%
    first_n_cases(n = 3) %>%
    first_n_cases(n = 4) %>%
    first_n_cases(n = 5) %>%
    first_n_cases(n = 6) %>%
    first_n_cases(n = 7) %>%
    first_n_cases(n = 8) %>%
    first_n_cases(n = 9) %>%
    first_n_cases(n = 10) %>%
    first_n_cases(n = 11) %>%
    first_n_cases(n = 12) %>%
    first_n_cases(n = 13) %>%
    first_n_cases(n = 14) %>%
    first_n_cases(n = 15) %>%
    first_n_cases(n = 100) %>%
    first_n_cases_week(n = 1) %>%
    first_n_cases_week(n = 2) %>%
    first_n_cases_week(n = 3) %>%
    first_n_cases_week(n = 4) %>%
    first_n_cases_week(n = 5) %>%
    first_n_cases_week(n = 6) %>%
    first_n_cases_week(n = 7) %>%
    first_n_cases_week(n = 8) %>%
    first_n_cases_week(n = 9) %>%
    first_n_cases_week(n = 10) %>%
    first_n_cases_week(n = 11) %>%
    first_n_cases_week(n = 12) %>%
    first_n_cases_week(n = 13) %>%
    first_n_cases_week(n = 14) %>%
    first_n_cases_week(n = 15) %>%
    first_n_cases_week(n = 100) %>%
    # everyn(n=1) %>%
    everyn(n = 2) %>%
    everyn(n = 3) %>%
    everyn(n = 4) %>%
    everyn(n = 5) %>%
    everyn(n = 6) %>%
    everyn(n = 7) %>%
    everyn(n = 8) %>%
    everyn(n = 9) %>%
    everyn(n = 10) %>%
    everyn(n = 11) %>%
    everyn(n = 12) %>%
    everyn(n = 13) %>%
    everyn(n = 14) %>%
    everyn(n = 15) %>%
    all_once_week() %>%
    severe_cases() %>%
    one_weekday(day = 1) %>%
    one_weekday(day = 2) %>%
    one_weekday(day = 3) %>%
    one_weekday(day = 4) %>%
    one_weekday(day = 5) %>%
    one_weekday(day = 6) %>%
    one_weekday(day = 7) %>%
    all_cases()

  volume.data[[i]] <- data %>%
    filter(data_source == data_sources[i]) %>%
    filter(rdt_result %in% c("Positive", "Negative")) %>%
    first_n_cases(n = 1) %>%
    first_n_cases(n = 2) %>%
    first_n_cases(n = 3) %>%
    first_n_cases(n = 4) %>%
    first_n_cases(n = 5) %>%
    first_n_cases(n = 6) %>%
    first_n_cases(n = 7) %>%
    first_n_cases(n = 8) %>%
    first_n_cases(n = 9) %>%
    first_n_cases(n = 10) %>%
    first_n_cases(n = 11) %>%
    first_n_cases(n = 12) %>%
    first_n_cases(n = 13) %>%
    first_n_cases(n = 14) %>%
    first_n_cases(n = 15) %>%
    first_n_cases(n = 100) %>%
    first_n_cases_week(n = 1) %>%
    first_n_cases_week(n = 2) %>%
    first_n_cases_week(n = 3) %>%
    first_n_cases_week(n = 4) %>%
    first_n_cases_week(n = 5) %>%
    first_n_cases_week(n = 6) %>%
    first_n_cases_week(n = 7) %>%
    first_n_cases_week(n = 8) %>%
    first_n_cases_week(n = 9) %>%
    first_n_cases_week(n = 10) %>%
    first_n_cases_week(n = 11) %>%
    first_n_cases_week(n = 12) %>%
    first_n_cases_week(n = 13) %>%
    first_n_cases_week(n = 14) %>%
    first_n_cases_week(n = 15) %>%
    first_n_cases_week(n = 100) %>%
    # everyn(n=1) %>%
    everyn(n = 2) %>%
    everyn(n = 3) %>%
    everyn(n = 4) %>%
    everyn(n = 5) %>%
    everyn(n = 6) %>%
    everyn(n = 7) %>%
    everyn(n = 8) %>%
    everyn(n = 9) %>%
    everyn(n = 10) %>%
    everyn(n = 11) %>%
    everyn(n = 12) %>%
    everyn(n = 13) %>%
    everyn(n = 14) %>%
    everyn(n = 15) %>%
    all_once_week() %>%
    severe_cases() %>%
    one_weekday(day = 1) %>%
    one_weekday(day = 2) %>%
    one_weekday(day = 3) %>%
    one_weekday(day = 4) %>%
    one_weekday(day = 5) %>%
    one_weekday(day = 6) %>%
    one_weekday(day = 7) %>%
    all_cases()
}

sampled.data <- bind_rows(sampled.data) %>%
  mutate(
    date = week,
    month = floor_date(week, "month"),
    year = floor_date(week, "year")
  )

volume.data <- bind_rows(volume.data) %>%
  mutate(
    date = week,
    month = floor_date(week, "month"),
    year = floor_date(week, "year")
  )

### 3. Save "Truth" data as the number of RDT-tested, suspected cases ----

min.date <- sampled.data %>%
  group_by(data_source) %>%
  filter(date_arrival == min(date_arrival)) %>%
  distinct(min_date2 = date_arrival, data_source)
max.date <- sampled.data %>%
  group_by(data_source) %>%
  filter(date_arrival == max(date_arrival)) %>%
  distinct(max_date2 = date_arrival, data_source) %>%
  mutate(max_date2 = if_else(data_source == "Sitakunda", as.Date("2022-09-01"), as.Date("2023-01-01")))

truth.data <- sampled.data %>%
  filter(sample_all_cases == 1) %>%
  count(data_source, rdt_result, date, under5, site, year) %>%
  group_by(data_source, date, under5, year, site) %>%
  summarize(
    n.cases = sum(n[rdt_result == "Positive"]),
    n.suspected = sum(n)
  ) %>%
  ungroup() %>%
  relocate(data_source, date)

truth.data <- bind_rows(
  truth.data %>%
    mutate(
      time = "week",
      max_date = ceiling_date(date, "week")
    ),
  truth.data %>%
    mutate(max_date = ceiling_date(date, "month")) %>%
    group_by(data_source, under5, date = lubridate::floor_date(date, "month"), year, site) %>%
    summarize(
      time = "month",
      n.cases = sum(n.cases),
      n.suspected = sum(n.suspected),
      max_date = max(max_date)
    ),
  truth.data %>%
    mutate(max_date = ceiling_date(date, "year")) %>%
    group_by(data_source, under5, date = lubridate::floor_date(date, "year"), year, site) %>%
    summarize(
      time = "year",
      n.cases = sum(n.cases),
      n.suspected = sum(n.suspected),
      max_date = max(max_date)
    )
) %>%
  relocate(data_source, site, time, date)

truth.data <- truth.data %>%
  group_by(data_source, time, date, year, site) %>%
  summarize(
    n.cases = sum(n.cases),
    n.suspected = sum(n.suspected),
    under5 = 2,
    max_date = max(max_date)
  ) %>%
  bind_rows(truth.data)

### 4. Calculate Empirical Estimates -----
sampled_strategies <- colnames(sampled.data)[str_detect(colnames(sampled.data), "sample_")]
sampled_strategies_label <- case_when(
  str_detect(sampled_strategies, "cases_week") ~ paste0("First n weekly (n=", str_extract(sampled_strategies, "\\d{1,3}"), ")"),
  str_detect(sampled_strategies, "every") ~ paste0("Every n (n=", str_extract(sampled_strategies, "\\d{1,3}"), ")"),
  str_detect(sampled_strategies, "first") ~ paste0("First n daily (n=", str_extract(sampled_strategies, "\\d{1,3}"), ")"),
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
sampled_strategies_label <- if_else(str_detect(sampled_strategies, "u5"), paste0(sampled_strategies_label, " (age)"), sampled_strategies_label)

empirical.weeks <- list()
empirical.years <- list()
empirical.months <- list()

for (i in 1:length(sampled_strategies)) {
  empirical.weeks[[i]] <- cases_per_period(
    data = sampled.data %>% filter(!!as.symbol(sampled_strategies[i]) == 1),
    period = "week", truth_data = truth.data
  ) %>%
    mutate(
      strategy = sampled_strategies_label[i],
      age_strat = if_else(str_detect(sampled_strategies[i], "u5"), T, F),
      period = "weekly"
    )

  empirical.months[[i]] <- cases_per_period(
    data = sampled.data %>% filter(!!as.symbol(sampled_strategies[i]) == 1),
    period = "month", truth_data = truth.data
  ) %>%
    mutate(
      strategy = sampled_strategies_label[i],
      age_strat = if_else(str_detect(sampled_strategies[i], "u5"), T, F),
      period = "monthly"
    )

  empirical.years[[i]] <- cases_per_period(
    data = sampled.data %>% filter(!!as.symbol(sampled_strategies[i]) == 1),
    period = "year", truth_data = truth.data
  ) %>%
    mutate(
      strategy = sampled_strategies_label[i],
      age_strat = if_else(str_detect(sampled_strategies[i], "u5"), T, F),
      period = "annual"
    )
}

case.data <- bind_rows(
  empirical.weeks,
  empirical.months,
  empirical.years
) %>%
  rename(n.sampled = n.tested)

### 5. Calculate Bias in Annual Estimates when Aggregating from Weekly Estimates -----
z <- 1.96

case.year <- case.data %>%
  filter(time == "week") %>%
  group_by(data_source, year, under5, age_strat, strategy) %>%
  summarize(
    n.sampled = sum(n.sampled, na.rm = TRUE),
    n.positive = sum(n.positive, na.rm = TRUE),
    n.suspected = sum(n.suspected, na.rm = TRUE),
    truth = sum(truth, na.rm = TRUE),
    n.weeks.estimated = sum(!is.na(n.cases)),
    n.cases = if (n.weeks.estimated == 0) NA_real_ else sum(n.cases, na.rm = TRUE), # sum weekly mle totals
    var.cases = if (n.weeks.estimated == 0) NA_real_ else sum(var.cases, na.rm = TRUE), # sum weekly variances (delta-method)
  ) %>%
  mutate(
    se.cases = sqrt(var.cases),
    n.cases.lo = pmax(0, n.cases - z * se.cases),
    n.cases.hi = pmin(n.suspected, n.cases + z * se.cases),
    positivity = if_else(n.suspected > 0, n.cases / n.suspected, NA_real_),
    positivity.lo = if_else(n.suspected > 0, n.cases.lo / n.suspected, NA_real_),
    positivity.hi = if_else(n.suspected > 0, n.cases.hi / n.suspected, NA_real_),
    truth.positivity = if_else(n.suspected > 0, truth / n.suspected, NA_real_),
    aggregate = TRUE,
    aggregate_from = "week",
    time = "year",
    period = "annual",
    date = year
  ) %>%
  select(-se.cases)

case.data <- case.data %>%
  mutate(aggregate = FALSE, aggregate_from = NA_character_) %>%
  bind_rows(
    case.year
  )

### 6. Calculate IR, Bias and % of estimates capturing truth in 95% CI -----
ptime.data <- truth.data %>%
  ungroup() %>%
  filter(under5 == 2) %>%
  distinct(data_source, time, date, year, max_date) %>%
  left_join(min.date) %>%
  left_join(max.date) %>%
  mutate(
    min_date = if_else(date < min_date2, min_date2, date),
    max_date = if_else(max_date > max_date2, max_date2, max_date),
    days_covered = as.numeric(max_date - min_date),
    days_in_month = days_in_month(min_date),
    timerate = case_when(
      time == "week" ~ days_covered / 7,
      time == "month" ~ days_covered / days_in_month,
      time == "year" ~ days_covered / 365.25
    )
  ) %>%
  select(data_source, time, date, year, timerate)

case.data <- case.data %>%
  left_join(ptime.data) %>%
  mutate(
    persontime = case_when(
      data_source == serochit_label & under5 == 2 ~ pop.serochit * timerate,
      data_source == serochit_label & under5 == 0 ~ pop.o5.serochit * timerate,
      data_source == serochit_label & under5 == 1 ~ pop.u5.serochit * timerate,
      data_source == uvira_label & under5 == 2 ~ pop.uvira * timerate,
      data_source == uvira_label & under5 == 0 ~ pop.o5.uvira * timerate,
      data_source == uvira_label & under5 == 1 ~ pop.u5.uvira * timerate
    ),
    inc_modifier = 10000,
    within = n.cases.lo <= truth & n.cases.hi >= truth,
    within = if_else(is.na(within) & is.nan(n.cases) & n.suspected != 0, # treats weeks where no estimate was produced bc of no sampling as NOT covering interval
      FALSE, within
    ),
    ir = n.cases / persontime * inc_modifier,
    ir.lo = n.cases.lo / persontime * inc_modifier,
    ir.hi = n.cases.hi / persontime * inc_modifier,
    true.ir = truth / persontime * inc_modifier,
    within.ir = ir.lo <= true.ir & ir.hi >= true.ir,
    within.ir = if_else(is.na(within.ir) & is.nan(n.cases) & n.suspected != 0,
      FALSE, within.ir
    ),
    bias.cases = n.cases - truth,
    bias.cases.lo = n.cases.lo - truth,
    bias.cases.hi = n.cases.hi - truth,
    bias.ir = ir - true.ir,
    bias.ir.lo = ir.lo - true.ir,
    bias.ir.hi = ir.hi - true.ir
  )

### 7. Bias Comparison ----

bias.age <- case.data %>%
  filter(under5 == 2) %>%
  filter(
    (time == "week" & period == "weekly") |
      (time == "year" & period == "annual" & aggregate == T)
  ) %>%
  filter(str_detect(strategy, "Severe|All cases", negate = T)) %>%
  mutate(strategy = str_remove(strategy, " \\(age\\)")) %>%
  select(data_source, strategy, aggregate, date, year, under5, time, period, truth, n.cases, age_strat, persontime, inc_modifier) %>%
  pivot_wider(names_from = age_strat, values_from = c(n.cases)) %>%
  mutate(
    abs.bias.age.cases = abs(`TRUE` - truth) - abs(`FALSE` - truth),
    abs.bias.age.ir = abs.bias.age.cases / persontime * inc_modifier
  )

bias.aggregate <- case.data %>%
  filter(time == "year", period == "annual", under5 == 2) %>%
  filter(str_detect(strategy, "Severe|All cases", negate = T)) %>%
  filter(str_detect(strategy, " \\(age\\)", negate = T) & age_strat == F) %>%
  select(data_source, strategy, date, year, under5, time, period, truth, n.cases, aggregate, persontime, inc_modifier) %>%
  pivot_wider(names_from = aggregate, values_from = c(n.cases)) %>%
  mutate(
    abs.bias.aggregate.cases = abs(`TRUE` - truth) - abs(`FALSE` - truth),
    abs.bias.aggregate.ir = abs.bias.aggregate.cases / persontime * inc_modifier
  )

### 8. Write datasets ----

# RDT-tested participants with columns indicating whether sampled for strategy
write_csv(sampled.data, here::here("data/sampled_data.csv"))
# Estimated number of estimated, sampled, suspected cases, cholera positivity
write_csv(case.data, here::here("data/case_data_delta.csv"))
# Volume of RDT when sampling all suspected cases
write_csv(volume.data, here::here("data/volume_data.csv"))
# Bias estimates
write_csv(bias.age, here::here("data/bias_age.csv"))
write_csv(bias.aggregate, here::here("data/bias_aggregate.csv"))
