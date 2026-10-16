# Functions used to sample cases

# Sample the first n cases per day with and without age-stratification
first_n_cases <- function(x, n) {
  sample_col_name <- paste0("sample_first_", n, "_cases")
  sample_col_name_u5 <- paste0(sample_col_name, "_u5")

  x <- x %>%
    group_by(date_arrival, site) %>%
    arrange(date_arrival, record_id) %>%
    mutate(
      order = 1, order = cumsum(order),
      !!sample_col_name := if_else(order <= n, 1, 0)
    ) %>%
    ungroup() %>%
    select(-order)

  x <- x %>%
    group_by(date_arrival, under5, site) %>%
    arrange(date_arrival, record_id) %>%
    mutate(
      order = 1, order = cumsum(order),
      !!sample_col_name_u5 := if_else(order <= n, 1, 0)
    ) %>%
    ungroup() %>%
    select(-order)

  return(x)
}

# Sample the first n cases per week with and without age-stratification
first_n_cases_week <- function(x, n) {
  sample_col_name <- paste0("sample_first_", n, "_cases_week")
  sample_col_name_u5 <- paste0(sample_col_name, "_u5")

  week_day <- x %>%
    ungroup() %>%
    distinct(week, site, data_source)

  week_day <- tibble(week_day, sampled_weekday = sample(1:7, nrow(week_day), replace = T))

  x <- x %>%
    mutate(facility = if_else(site %in% c("BITID", "ctc"), 1, 2)) %>%
    left_join(week_day) %>%
    group_by(week, site) %>%
    arrange(week, date_arrival, record_id) %>%
    mutate(
      weekday = wday(date_arrival),
      order = if_else(weekday >= sampled_weekday, 1, 0),
      order = cumsum(order),
      !!sample_col_name := if_else(order <= n & order > 0, 1, 0)
    ) %>%
    ungroup() %>%
    select(-order, -weekday, -sampled_weekday)

  x <- x %>%
    mutate(facility = if_else(site %in% c("BITID", "ctc"), 1, 2)) %>%
    left_join(week_day) %>%
    group_by(week, under5, site) %>%
    arrange(week, date_arrival, record_id) %>%
    mutate(
      weekday = wday(date_arrival),
      order = if_else(weekday >= sampled_weekday, 1, 0),
      order = cumsum(order),
      !!sample_col_name_u5 := if_else(order <= n & order > 0, 1, 0)
    ) %>%
    ungroup() %>%
    select(-order, -weekday, -sampled_weekday)

  return(x)
}

# Sample every n cases, starting with the first case that shows up at a facility, with and without age-stratification
everyn <- function(x, n) {
  sample_col_name <- paste0("sample_every", n)
  sample_col_name_u5 <- paste0(sample_col_name, "_u5")

  x <- x %>%
    group_by(site) %>%
    arrange(date_arrival, record_id) %>%
    mutate(
      !!sample_col_name := 1,
      !!sample_col_name := cumsum(!!as.symbol(sample_col_name)),
      !!sample_col_name := if_else(!!as.symbol(sample_col_name) %% n == 1, 1, 0)
    )

  x <- x %>%
    group_by(under5, site) %>%
    arrange(date_arrival, record_id) %>%
    mutate(
      !!sample_col_name_u5 := 1,
      !!sample_col_name_u5 := cumsum(!!as.symbol(sample_col_name_u5)),
      !!sample_col_name_u5 := if_else(!!as.symbol(sample_col_name_u5) %% n == 1, 1, 0)
    ) %>%
    ungroup()

  return(x)
}

# Sample all cases once a week on a random day
all_once_week <- function(x) {
  week_day <- x %>%
    ungroup() %>%
    distinct(week, site, data_source)

  week_day <- tibble(week_day, sampled_weekday = sample(1:7, nrow(week_day), replace = T))

  x <- x %>%
    mutate(facility = if_else(site %in% c("BITID", "ctc"), 1, 2)) %>%
    left_join(week_day) %>%
    mutate(
      weekday = wday(date_arrival),
      sample_all_once_week = if_else(weekday == sampled_weekday, 1, 0)
    ) %>%
    select(-weekday, -sampled_weekday)

  return(x)
}

# Sample only severe cases, defined as those with "Severe" dehydration or kept for observation/treatment for more than 2 days
severe_cases <- function(x) {
  x <- x %>%
    mutate(sample_severe_cases = case_when(
      dehydration == "Severe" ~ 1,
      los_days > 2 ~ 1,
      TRUE ~ 0
    ))

  return(x)
}

# Sample all cases on one weekday
one_weekday <- function(x, day = 1) {
  sample_name <- paste0("sample_", as.character(wday(day, label = TRUE))) %>% str_to_lower()

  x <- x %>%
    mutate(
      weekday = wday(date_arrival),
      !!sample_name := if_else(weekday == day, 1, 0)
    ) %>%
    select(-weekday)

  return(x)
}

# Sample all cases
all_cases <- function(x) {
  x <- x %>%
    mutate(sample_all_cases = 1)

  return(x)
}
