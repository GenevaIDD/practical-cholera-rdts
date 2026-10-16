# From repository root: Rscript scripts/budget_matched_by_caseload.R [repetitions] [cap]
# Weekly errors at matched budgets split by weekly caseload; used by notebooks/technical_supplement.qmd
suppressPackageStartupMessages({library(dplyr); library(readr)})
args <- commandArgs(trailingOnly = TRUE)
repetitions <- if (length(args)) as.integer(args[1]) else 200L
cap <- if (length(args) >= 2) as.integer(args[2]) else 3L
source("scripts/budget_matched_funcs.R")
source("scripts/supplement_funcs.R")
d <- read_csv("data/sampled_data.csv", show_col_types = FALSE)
res <- bm_by_caseload(d, cap = cap, repetitions = repetitions)
write_csv(res, "data/budget_matched/by_caseload.csv")
print(as.data.frame(res), digits = 3)
