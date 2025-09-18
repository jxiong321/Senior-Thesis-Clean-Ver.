# ------------------------------------------------------------
# 03_matching.R  — Minimal, restart-from-scratch version
# 2v3: Full matching       |  3v4: Nearest-neighbor (ratio = 3)
# Drops to complete cases. Exports: matched data, long data,
# balance tables, love plots (with/without gender variants).
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(MatchIt)
  library(cobalt)
  library(ggplot2)
  library(readr)
})

options(stringsAsFactors = FALSE)
dir.create("output/data",    recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
df_2v3 <- readRDS("data/cleaned/df_2v3.rds")
df_3v4 <- readRDS("data/cleaned/df_3v4.rds")

# We assume these exist (as in your earlier scripts)
stopifnot(all(c("treated","weight_panel") %in% names(df_2v3)))
stopifnot(all(c("treated","weight_panel") %in% names(df_3v4)))

# Add a stable row id
df_2v3 <- df_2v3 %>% mutate(orig_row = dplyr::row_number())
df_3v4 <- df_3v4 %>% mutate(orig_row = dplyr::row_number())

# Core covariates for matching (adjust names here if your columns differ)
core_covs <- c(
  "math_self_efficacy_by", "math_score_by", "verbal_score_by",
  "race", "sex", "parent_educ", "income", "urbanicity"
)
keep_existing <- function(df, v) intersect(v, names(df))

# Drop to complete cases on: treated, weight, covariates, and BY/F1 outcomes
clean_for_matching <- function(df) {
  covs <- keep_existing(df, core_covs)
  req  <- c("treated", "weight_panel",
            "math_self_efficacy_by", "math_self_efficacy_f1")
  df %>% filter(if_all(all_of(c(req, covs)), ~ !is.na(.)))
}

df_2v3_cc <- clean_for_matching(df_2v3)
df_3v4_cc <- clean_for_matching(df_3v4)

# ---- Matching runner (minimal) ----
# method = "full" (needs optmatch installed) or "nearest" (uses ratio)
run_matching <- function(df_cc, tag, method = c("full","nearest"), ratio_nn = 3L) {
  method <- match.arg(method)
  
  # For full matching, MatchIt calls optmatch under the hood
  if (method == "full" && !requireNamespace("optmatch", quietly = TRUE)) {
    stop("For method='full', please install.packages('optmatch') and re-run.")
  }
  
  vars_all <- keep_existing(df_cc, core_covs)
  if (length(vars_all) == 0L) stop("No matching covariates found in data for tag=", tag)
  
  # Helper: run one variant (suffix = "with_gender" or "no_gender")
  run_one <- function(vars, suffix) {
    # Build matching frame as plain data.frame; carry orig_row as rownames
    df_match <- df_cc %>% select(treated, all_of(vars), orig_row) %>% as.data.frame()
    rownames(df_match) <- as.character(df_match$orig_row)
    df_match$orig_row <- NULL
    
    # Build args list (do NOT pass ratio when not needed)
    args <- list(
      formula  = treated ~ .,
      data     = df_match,
      method   = method,
      distance = "logit"
    )
    if (method == "nearest") args$ratio <- ratio_nn
    
    m <- do.call(MatchIt::matchit, args)
    
    # Balance + love plot
    bt <- cobalt::bal.tab(m, un = TRUE)
    write_csv(as.data.frame(bt$Balance),
              file.path("output/tables", paste0("balance_", tag, "_", suffix, ".csv")))
    
    lp <- cobalt::love.plot(m, stats = "m", abs = TRUE, line = TRUE) +
      ggtitle(paste0("Love Plot (", tag, " | ", suffix, " | ", method, ")"))
    ggsave(file.path("output/figures",
                     paste0("loveplot_", tag, "_", suffix, "_", method, ".png")),
           lp, width = 9, height = 6, dpi = 300)
    
    # Matched data (+ reattach fields needed later)
    md <- MatchIt::match.data(m)
    md$orig_row <- suppressWarnings(as.integer(rownames(md)))
    
    matched <- md %>%
      left_join(
        df_cc %>% select(orig_row, weight_panel,
                         female = any_of("female"),
                         math_self_efficacy_f1, math_score_by),
        by = "orig_row"
      )
    
    saveRDS(matched,
            file.path("output/data",
                      paste0("matched_", tag, "_", suffix, "_", method, ".rds")))
    
    # Long panel on self-efficacy (BY vs F1) for easy DiD
    matched_long <- matched %>%
      pivot_longer(c(math_self_efficacy_by, math_self_efficacy_f1),
                   names_to = "timepoint", values_to = "self_efficacy") %>%
      mutate(post = if_else(timepoint == "math_self_efficacy_f1", 1L, 0L))
    
    saveRDS(matched_long,
            file.path("output/data",
                      paste0("matched_long_", tag, "_", suffix, "_", method, ".rds")))
    
    invisible(m)
  }
  
  # Variant A: WITH gender vars
  run_one(vars_all, "with_gender")
  
  # Variant B: WITHOUT gender vars
  vars_no_gender <- setdiff(vars_all, keep_existing(df_cc, c("sex","female")))
  if (length(vars_no_gender) > 0L) {
    run_one(vars_no_gender, "no_gender")
  } else {
    message("(", tag, ") No covariates left after dropping sex/female; skipping no_gender.")
  }
  
  invisible(NULL)
}

# ---- Run exactly as specified ----
# 2v3 => Full matching
run_matching(df_2v3_cc, tag = "2v3", method = "full",    ratio_nn = 3L)

# 3v4 => Nearest-neighbor matching (ratio = 3)
run_matching(df_3v4_cc, tag = "3v4", method = "nearest", ratio_nn = 3L)

message("Done. Outputs in output/data, output/tables, output/figures.")

