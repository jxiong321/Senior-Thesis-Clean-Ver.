# ------------------------------------------------------------
# 04_did.R
# Difference-in-Differences (DiD) models on matched data
# - 2v3 sample: Full matching
# - 3v4 sample: Nearest-neighbor matching (ratio=3)
# Runs DiD with/without gender interaction, weighted & unweighted
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(broom)
})

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ---- 0) Load matched long data ----
dat_2v3_with <- readRDS("output/data/matched_long_2v3_with_gender_full.rds")
dat_2v3_no   <- readRDS("output/data/matched_long_2v3_no_gender_full.rds")
dat_3v4_with <- readRDS("output/data/matched_long_3v4_with_gender_nearest.rds")
dat_3v4_no   <- readRDS("output/data/matched_long_3v4_no_gender_nearest.rds")

# ---- 1) Helper to run DiD ----
run_did <- function(df, outcome = "self_efficacy", weight_var = NULL,
                    triple_gender = FALSE, tag = "model") {
  
  fmla <- as.formula(
    if (triple_gender) {
      paste0(outcome, " ~ treated*post*female")
    } else {
      paste0(outcome, " ~ treated*post")
    }
  )
  
  if (!is.null(weight_var) && weight_var %in% names(df)) {
    fit <- lm(fmla, data = df, weights = df[[weight_var]])
  } else {
    fit <- lm(fmla, data = df)
  }
  
  res <- broom::tidy(fit, conf.int = TRUE)
  
  # Save table
  write_csv(res, file.path("output/tables", paste0("did_results_", tag, ".csv")))
  
  return(fit)
}

# ---- 2) Run DiD models ----
fits <- list()

# 2v3 full matching
fits$`2v3_with_gender_basic`   <- run_did(dat_2v3_with, triple_gender = FALSE, tag = "2v3_with_basic")
fits$`2v3_with_gender_triple`  <- run_did(dat_2v3_with, triple_gender = TRUE,  tag = "2v3_with_triple")
fits$`2v3_no_gender_basic`     <- run_did(dat_2v3_no,   triple_gender = FALSE, tag = "2v3_no_basic")

# 3v4 nearest-neighbor matching
fits$`3v4_with_gender_basic`   <- run_did(dat_3v4_with, triple_gender = FALSE, tag = "3v4_with_basic")
fits$`3v4_with_gender_triple`  <- run_did(dat_3v4_with, triple_gender = TRUE,  tag = "3v4_with_triple")
fits$`3v4_no_gender_basic`     <- run_did(dat_3v4_no,   triple_gender = FALSE, tag = "3v4_no_basic")

message("DiD complete. See output/tables/did_results_*.csv")
