# Cholera surveillance with rapid diagnostic tests: sampling approaches and threats to inference

Code and de-identified data to reproduce the analyses, figures and tables in the manuscript. The analyses use cholera rapid diagnostic test (RDT) results from suspected cases at health facilities in Sitakunda, Bangladesh (2021–2022) and Uvira, Democratic Republic of the Congo (2016–2022). They compare how well different strategies for choosing which suspected cases to test recover the true number of RDT-positive cases.

## Repository layout

```
data/        de-identified line lists, population denominators, saved analysis outputs
scripts/     R code: data processing, sampling functions, figures and tables
notebooks/   Quarto supplements (sampling-scheme note, technical supplement)
figures/     created by scripts/manuscript.R
tables/      created by scripts/manuscript.R
```

## Reproducing the analyses

Open `cholera_rdt_sampling.Rproj` (or run from the repository root; paths use `here::here()`).

| Step | Command | Output | Time* |
|------|---------|--------|-------|
| 1. Apply sampling strategies and estimate cases | `Rscript scripts/process_data.R` | `data/sampled_data.csv`, `data/volume_data.csv`, `data/case_data_delta.csv`, `data/bias_*.csv` | ~3 min |
| 2. Main and supplemental figures and tables | `Rscript scripts/manuscript.R` | `figures/`, `tables/` | <1 min |
| 3. Budget-matched comparison (200 repetitions) | `Rscript scripts/budget_matched_comparison.R` | `data/budget_matched/` | long |
| 4. Budget-matched results by caseload | `Rscript scripts/budget_matched_by_caseload.R` | `data/budget_matched/by_caseload.csv` | long |
| 5. Supplements | `quarto render notebooks/sampling_schemes_note.qmd`<br>`quarto render notebooks/technical_supplement.qmd` | HTML/PDF | ~1 min |

\*On an Apple M-series laptop.

`data/case_data_delta.csv` (~50 MB) is not tracked; step 1 regenerates it. The other outputs of steps 1, 3 and 4 are included so later steps can run on their own. `process_data.R` sets a fixed seed, and rerunning it on the included data reproduces the included outputs exactly. Figure points use unseeded jitter, so PNG/PDF files differ slightly between runs; the estimates do not.

Checks: `Rscript scripts/test_budget_matched.R`.

`scripts/fig_sampling_schematic.R` draws the illustrative schematic of the sampling strategies; it uses no data. `scripts/listed_arrival_order.R` holds sensitivity functions that rank cases by file row order instead of recorded arrival order.

### R packages

Tested with R 4.5.3 and: dplyr 1.2.1, tidyr 1.3.2, readr 2.2.0, purrr 1.2.2, lubridate 1.9.5, ggplot2 4.0.3, here 1.0.2, gtsummary 2.5.1, flextable 0.9.11, ggh4x 0.3.1, ggtext 0.2.0, reactable 0.4.5, webshot2 0.1.2, patchwork 1.3.2, zoo 1.8.15, boot 1.3.32, lme4 2.0.1, knitr 1.51. The supplements also need Quarto.

```r
install.packages(c("tidyverse", "here", "gtsummary", "flextable", "ggh4x", "ggtext",
                   "reactable", "webshot2", "patchwork", "zoo", "boot", "lme4", "knitr"))
```

## Data

### `data/clinical_data.csv`: one row per suspected cholera case

| Column | Description |
|--------|-------------|
| `record_id` | Study-specific sequential ID. It follows recorded arrival order within each site (by visit date, then record time in Sitakunda or record sequence in Uvira), so sorting by `date_arrival, record_id` gives the arrival order the analyses use. It does not link to any other data source. |
| `data_source` | `Sitakunda` or `Uvira` |
| `site` | Facility: `BITID`, `Sitakunda Upazila Health Complex` (Sitakunda); `ctc`, `ctu` (Uvira cholera treatment centre/unit) |
| `date_arrival` | Visit date |
| `week` | Epidemiological week start (Sunday) |
| `under5` | 1 if younger than 5 years |
| `age_group` | Years: `1-4`, `5-14`, `15-44`, `45+` |
| `dehydration` | Dehydration status as recorded (category labels differ between sites) |
| `los_days` | Days from visit to outcome (discharge, etc.); `NA` if not recorded. Values are kept as recorded, including a few implausible ones. |
| `rdt_result` | `Positive`, `Negative`, `Invalid`, or `NA` (not tested) |

`data/sampled_data.csv` and `data/volume_data.csv` hold the same columns for RDT-tested cases (positive or negative), plus one 0/1 column per sampling strategy (`sample_*`) showing whether the case would have been tested. The two files differ only in their random draws for the random-day strategies. `process_data.R` writes both.

### Population denominators

- `census_data_sex_age_sitakunda.csv`: Sitakunda Upazila population by age, 2011 Bangladesh census.
- `uvira_pop_sum.csv`: Uvira population by age group.

### De-identification

The public line lists come from the study databases. To lower re-identification risk:

- Original record IDs and record timestamps were replaced with a sequential arrival-order ID.
- Exact age was replaced with age groups (plus the under-5 indicator).
- Sex, outcome type, and outcome dates were removed. Length of stay (`los_days`) is kept because the "severe cases" strategy uses it.
- No names, addresses, residence locations, contact details or vaccination data are included.

Visit date and facility are kept because the sampling strategies (first *n* per day, every *n*-th case, single weekday) depend on them. Every analysis output reproduces exactly from these files. Because of these changes, Table 1 and Table S2 report age groups rather than median age and omit sex; the published versions of those tables used the full data.

## Citation

<!-- TODO: add manuscript citation / DOI once available -->

## License

Code and data are released under CC0 1.0 Universal; see `LICENSE`.
