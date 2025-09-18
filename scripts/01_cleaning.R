# ------------------------------------------------------------
# 01_cleaning.R
# Clean & prepare ELS:2002 student panel for DiD analysis
# Author: Jess Xiong   |  Rev: <TODAY>
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  # psych, here were removed to keep deps light; add back if you need them
})

options(stringsAsFactors = FALSE)

# ---- 0) Paths & small helpers --------------------------------
j <- function(...) normalizePath(file.path(...), mustWork = FALSE)
dir.create("data/cleaned", showWarnings = FALSE, recursive = TRUE)

#get raw file
raw_file <- "data/raw/raw_data.rdata"

if (!file.exists(raw_file)) {
  stop("Raw file not found: ", raw_file,
       "\nPut your ELS .rdata under data/raw/ and update 'raw_file' above.")
}

# Load .rdata into a temporary env so we know the object name
e <- new.env(parent = emptyenv())
load(raw_file, envir = e)
obj_name <- ls(envir = e)
if (length(obj_name) != 1L) {
  stop("Expected exactly one object in ", basename(raw_file),
       " but found: ", paste(obj_name, collapse = ", "))
}
stu <- get(obj_name, envir = e); rm(e, obj_name)

# ---- 1) Global NA recode: convert values (–9, –8, –7, –6, –4) from dataset to NA ----------------
na_codes <- c(-9, -8, -7, -6, -4)
stu <- stu %>%
  mutate(across(everything(), ~ ifelse(.x %in% na_codes, NA, .x)))

# ---- 2) Compute variable indices --------------------------------
GEN  <- c("BYS89E","BYS89J","BYS89O","BYS89S","BYS89V")
VERB <- c("BYS89C","BYS89F","BYS89M")
M10  <- c("BYS89B","BYS89L","BYS89U")
M12  <- c("F1S18B","F1S18C","F1S18E")
BASE_SE <- "BYS88A"  # base-year math self-efficacy (BYS88A)

missing_items <- setdiff(c(GEN, VERB, M10, M12, BASE_SE), names(stu))
if (length(missing_items)) {
  warning("Missing expected item columns: ", paste(missing_items, collapse = ", "),
          "\nCheck your raw file or update the item lists above.")
}

stu <- stu %>%
  mutate(
    growth_mindset   = .data[[BASE_SE]],
    gen_challenge    = rowMeans(across(all_of(intersect(GEN,  names(stu)))),  na.rm = TRUE),
    verb_challenge   = rowMeans(across(all_of(intersect(VERB, names(stu)))),  na.rm = TRUE),
    math10_challenge = rowMeans(across(all_of(intersect(M10,  names(stu)))),  na.rm = TRUE),
    math12_challenge = rowMeans(across(all_of(intersect(M12,  names(stu)))),  na.rm = TRUE)
  )

# ---- 3) Select and RENAME variables for readability ----------
stu <- stu %>%
  select(
    # originals to keep (edit if needed)
    F1PNLWT, F1A07B, bytxmirr, F1TXM1IR, !!BASE_SE, F1MATHSE, bytxrirr,
    byrace, bysex, bypared, byincome, byurban, F2B15, F1RGPP2, F1HIMATH,
    # the newly created indices
    growth_mindset, gen_challenge, verb_challenge, math10_challenge, math12_challenge
  ) %>%
  rename(
    weight_panel          = F1PNLWT,
    math_req_yrs          = F1A07B,
    math_score_by         = bytxmirr,
    math_score_f1         = F1TXM1IR,
    math_self_efficacy_by = !!BASE_SE,
    math_self_efficacy_f1 = F1MATHSE,
    verbal_score_by       = bytxrirr,
    race                  = byrace,
    sex                   = bysex,
    parent_educ           = bypared,
    income                = byincome,
    urbanicity            = byurban,
    intended_major_f2     = F2B15,
    cum_gpa               = F1RGPP2
  )

# ---- 4) GPA buckets ------------------------------------------
stu <- stu %>%
  mutate(gpa_bucket = cut(
    cum_gpa,
    breaks = c(-Inf, 1.01, 1.51, 2.01, 2.51, 3.01, 3.51, Inf),
    labels = c("0.00–1.00","1.01–1.50","1.51–2.00",
               "2.01–2.50","2.51–3.00","3.01–3.50","3.51–4.00")
  ))

# ---- 5) Socio-economic dummies & labelled factors -------------
stu <- stu %>%
  mutate(
    female   = if_else(sex == 2, 1, 0, missing = NA_real_),
    urban    = if_else(urbanicity == 1, 1, 0, missing = NA_real_),
    suburban = if_else(urbanicity == 2, 1, 0, missing = NA_real_),
    rural    = if_else(urbanicity == 3, 1, 0, missing = NA_real_),
    
    inc_q1 = if_else(income %in% 1:8,   1, 0, missing = NA_real_),   # ≤ $35 K
    inc_q2 = if_else(income == 9,       1, 0, missing = NA_real_),   # $35–50 K
    inc_q3 = if_else(income %in% 10:11, 1, 0, missing = NA_real_),   # $50–100 K
    inc_q4 = if_else(income %in% 12:13, 1, 0, missing = NA_real_),   # ≥ $100 K
    
    paredu_hs_or_less   = if_else(parent_educ %in% 1:2, 1, 0, missing = NA_real_),
    paredu_lt_four_year = if_else(parent_educ %in% 3:4, 1, 0, missing = NA_real_),
    paredu_bachelors    = if_else(parent_educ == 6,     1, 0, missing = NA_real_),
    paredu_gt_bachelors = if_else(parent_educ %in% 7:8, 1, 0, missing = NA_real_)
  ) %>%
  mutate(
    gender_f = factor(female, levels = c(0,1), labels = c("Male","Female")),
    race = factor(dplyr::case_when(
      race == 7        ~ "White, NH",
      race == 3        ~ "Black, NH",
      race %in% c(4,5) ~ "Hispanic",
      race == 2        ~ "Asian/PI, NH",
      race == 1        ~ "AIAN, NH",
      race == 6        ~ "Multi-race, NH",
      TRUE             ~ NA_character_
    ),
    levels = c("White, NH","Black, NH","Hispanic",
               "Asian/PI, NH","AIAN, NH","Multi-race, NH")),
    
    paredu_cat = factor(dplyr::case_when(
      parent_educ %in% 1:2 ~ "HS or less",
      parent_educ %in% 3:4 ~ "< 4-yr degree",
      parent_educ == 6     ~ "Bachelor",
      parent_educ %in% 7:8 ~ "> Bachelor",
      TRUE                 ~ NA_character_
    ), levels = c("HS or less","< 4-yr degree","Bachelor","> Bachelor")),
    
    income_q = factor(dplyr::case_when(
      income %in% 1:8   ~ "≤ $35 K",
      income == 9       ~ "$35–50 K",
      income %in% 10:11 ~ "$50–100 K",
      income %in% 12:13 ~ "≥ $100 K",
      TRUE              ~ NA_character_
    ), levels = c("≤ $35 K","$35–50 K","$50–100 K","≥ $100 K")),
    
    urbanicity_cat = factor(dplyr::case_when(
      urbanicity == 1 ~ "Urban",
      urbanicity == 2 ~ "Suburban",
      urbanicity == 3 ~ "Rural",
      TRUE            ~ NA_character_
    ), levels = c("Urban","Suburban","Rural"))
  )

# ---- 6) Major-group variable from F2B15 -----------------------
stu <- stu %>%
  mutate(
    major_group = dplyr::case_when(
      intended_major_f2 %in% c(4,5,6) ~ "PEMC",
      intended_major_f2 == 2          ~ "Bio",
      intended_major_f2 %in% 1:16     ~ "Other",
      TRUE                            ~ NA_character_
    ) %>% factor(levels = c("PEMC","Bio","Other")),
    pemc_major  = if_else(major_group == "PEMC", 1, 0, missing = NA_real_),
    bio_major   = if_else(major_group == "Bio",  1, 0, missing = NA_real_),
    other_major = if_else(major_group == "Other",1, 0, missing = NA_real_)
  )

# ---- 7) DiD treatment vs control subsets ----------------------
df_2v3 <- stu %>%
  filter(math_req_yrs %in% c(5,6)) %>%
  mutate(treated = if_else(math_req_yrs == 6, 1, 0, missing = NA_real_))

df_3v4 <- stu %>%
  filter(math_req_yrs %in% c(6,7)) %>%
  mutate(treated = if_else(math_req_yrs == 7, 1, 0, missing = NA_real_))

to_long <- function(df) {
  df %>%
    pivot_longer(
      cols = c(math_self_efficacy_by, math_self_efficacy_f1),
      names_to = "timepoint",
      values_to = "self_efficacy"
    ) %>%
    mutate(post = if_else(timepoint == "math_self_efficacy_f1", 1, 0, missing = NA_real_))
}

#Long format - for DiD
df_2v3_long <- to_long(df_2v3)
df_3v4_long <- to_long(df_3v4)

# ---- 8) Save cleaned objects ----------------------------------
dir.create("data/cleaned", showWarnings = FALSE, recursive = TRUE)

saveRDS(stu,         "data/cleaned/els_clean.rds")
saveRDS(df_2v3,      "data/cleaned/df_2v3.rds")
saveRDS(df_3v4,      "data/cleaned/df_3v4.rds")
saveRDS(df_2v3_long, "data/cleaned/df_2v3_long.rds")
saveRDS(df_3v4_long, "data/cleaned/df_3v4_long.rds")

message("1_cleaning.R complete. Files written to data/cleaned/")
