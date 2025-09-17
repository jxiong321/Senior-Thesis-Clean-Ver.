# ------------------------------------------------------------
# 02_eda.R
# Exploratory Data Analysis: weighted descriptives & outcomes
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(survey)
  library(kableExtra)
})

# ---- 0) Load cleaned data -----------------------------------
stu    <- readRDS("data/cleaned/els_clean.rds")
df_2v3 <- readRDS("data/cleaned/df_2v3.rds")
df_3v4 <- readRDS("data/cleaned/df_3v4.rds")

# ---- 1) Survey designs --------------------------------------
des_stu <- svydesign(ids = ~0, weights = ~weight_panel, data = stu)
des_2yr <- svydesign(ids = ~0, weights = ~weight_panel, data = filter(df_2v3, treated == 0))
des_3yr <- svydesign(ids = ~0, weights = ~weight_panel, data = filter(df_2v3, treated == 1))
des_4yr <- svydesign(ids = ~0, weights = ~weight_panel, data = filter(df_3v4, treated == 1))

# ============================================================
# A. DEMOGRAPHIC DESCRIPTIVES
# ============================================================

# helpers: weighted % for factors, weighted mean for continuous
w_pct <- function(des, var){
  tb <- svytable(as.formula(paste0("~", var)), design = des, na.action = na.omit)
  tibble(Level = names(tb), Value = as.numeric(100 * tb / sum(tb)))
}
w_mean <- function(des, var){ tibble(Level = "Mean", Value = as.numeric(svymean(as.formula(paste0("~", var)), des, na.rm = TRUE))) }

demo_vars <- list(
  Gender        = "female",
  `Race/Eth.`   = "race",
  `Parent Ed.`  = "paredu_cat",   
  `HH Income`   = "income_q",
  `Math Score`  = "math_score_by",
  `Verbal Score`= "verbal_score_by",
  Urbanicity    = "urbanicity_cat"
)

demo_long <- imap_dfr(demo_vars, function(var, nice){
  if(is.numeric(stu[[var]])){
    bind_rows(
      w_mean(des_stu, var) %>% mutate(Group="Full"),
      w_mean(des_2yr, var) %>% mutate(Group="2-Year"),
      w_mean(des_3yr, var) %>% mutate(Group="3-Year"),
      w_mean(des_4yr, var) %>% mutate(Group="4-Year")
    ) %>% mutate(Variable = nice)
  } else {
    bind_rows(
      w_pct(des_stu, var) %>% mutate(Group="Full"),
      w_pct(des_2yr, var) %>% mutate(Group="2-Year"),
      w_pct(des_3yr, var) %>% mutate(Group="3-Year"),
      w_pct(des_4yr, var) %>% mutate(Group="4-Year")
    ) %>% mutate(Variable = nice)
  }
})

# pivot wide for LaTeX export
demo_wide <- demo_long %>%
  pivot_wider(names_from = Group, values_from = Value)

latex_demo <- kbl(demo_wide, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position","repeat_header"))
writeLines(latex_demo, "output/tables/demo_table.tex")

# ============================================================
# B. OUTCOME DESCRIPTIVES
# ============================================================
outcome_vars <- list(
  `Challenge (General)`   = "gen_challenge",
  `Challenge (Verbal)`    = "verb_challenge",
  `Challenge (Math 10)`   = "math10_challenge",
  `Challenge (Math 12)`   = "math12_challenge",
  `Growth Mindset`        = "growth_mindset"
)

out_long <- purrr::imap_dfr(outcome_vars, function(var, nice){
  dplyr::bind_rows(
    w_mean(des_stu, var) %>% mutate(Group = "Full"),
    w_mean(des_2yr, var) %>% mutate(Group = "2-Year"),
    w_mean(des_3yr, var) %>% mutate(Group = "3-Year"),
    w_mean(des_4yr, var) %>% mutate(Group = "4-Year")
  ) %>% mutate(Variable = nice, Level = "Mean")
})

out_wide <- out_long %>%
  tidyr::pivot_wider(names_from = Group, values_from = Value) %>%
  dplyr::arrange(factor(Variable, levels = names(outcome_vars)))

latex_out <- kbl(out_wide, format = "latex", booktabs = TRUE,
                 caption = "Weighted Outcome Means (Challenge Indices & Growth Mindset)") %>%
  kable_styling(latex_options = c("hold_position","repeat_header"))
writeLines(latex_out, "output/tables/outcome_table.tex")

message("Outcome Descriptives complete: wrote output/tables/demo_table.tex and outcome_table.tex")

# ============================================================
# C. COURSE-TAKING PATTERNS (highest math course, by req. years, gender, etc.)
# ============================================================

# ---- C1) Labels for highest math course (F1HIMATH) -----------------------
course_levels <- c(
  "No math course",
  "Pre-algebra/general math",
  "Algebra I",
  "Geometry",
  "Algebra II",
  "Trig, Pre-calc, or Calculus",
  "Other/Unknown"
)

stu <- stu %>%
  mutate(
    F1HIMATH_label = case_when(
      F1HIMATH == 1 ~ "No math course",
      F1HIMATH == 2 ~ "Pre-algebra/general math",
      F1HIMATH == 3 ~ "Algebra I",
      F1HIMATH == 4 ~ "Geometry",
      F1HIMATH == 5 ~ "Algebra II",
      F1HIMATH == 6 ~ "Trig, Pre-calc, or Calculus",
      TRUE ~ "Other/Unknown"
    ) %>% factor(levels = course_levels),
    gender_label = factor(female, levels = c(0,1), labels = c("Male","Female"))
  )

# Subset to 2/3/4-year requirement groups (5 = 2y, 6 = 3y, 7 = 4y)
df_subset <- stu %>%
  filter(math_req_yrs %in% c(5,6,7)) %>%
  mutate(
    math_years_label = case_when(
      math_req_yrs == 5 ~ "2 Years",
      math_req_yrs == 6 ~ "3 Years",
      math_req_yrs == 7 ~ "4 Years"
    )
  )

# ---- C2) Highest course by requirement group: weighted % + χ² ------------
# Weighted percentages using survey design on the full student dataset
# Note: svytable gives weighted counts; we convert to row % within each group.
des_subset <- svydesign(ids = ~0, weights = ~weight_panel, data = df_subset)

wt_tab <- svytable(~ F1HIMATH_label + math_years_label, design = des_subset)
wt_prop <- prop.table(wt_tab, margin = 2) * 100  # % within math_years_label

# Unweighted χ² for simple significance flag (if you want weighted χ²: svychisq)
ct_unw <- table(df_subset$F1HIMATH_label, df_subset$math_years_label)
chisq_course_req <- suppressWarnings(chisq.test(ct_unw))

# Standardized residuals (unweighted) to assign stars per cell (optional)
stdres <- chisq_course_req$stdres
residuals_long <- as.data.frame(as.table(stdres)) %>%
  rename(F1HIMATH_label = Var1, math_years_label = Var2, residual = Freq) %>%
  mutate(star = case_when(
    abs(residual) > 3 ~ "**",   # very strong
    abs(residual) > 2 ~ "*",
    TRUE ~ ""
  ))

# Pretty LaTeX table of weighted percentages with stars
dist_tbl <- as.data.frame(as.table(wt_prop)) %>%
  rename(F1HIMATH_label = F1HIMATH_label, math_years_label = math_years_label, percent = Freq) %>%
  left_join(residuals_long, by = c("F1HIMATH_label","math_years_label")) %>%
  mutate(percent_star = sprintf("%.1f%s", percent, coalesce(star,""))) %>%
  select(F1HIMATH_label, math_years_label, percent_star) %>%
  tidyr::pivot_wider(names_from = math_years_label, values_from = percent_star)

latex_course_req <- kbl(dist_tbl, "latex", booktabs = TRUE,
                        caption = "Highest Math Course by Math Requirement Years (weighted %)") %>%
  kable_styling(latex_options = c("hold_position","repeat_header"))
writeLines(latex_course_req, "output/tables/course_by_requirement.tex")

# ---- C3) Plot: weighted % by course, colored by requirement years --------
# Convert wt_prop to data frame for plotting
library(ggplot2)

plot_df <- as.data.frame(as.table(wt_prop)) %>%
  rename(F1HIMATH_label = F1HIMATH_label, math_years_label = math_years_label, percent = Freq)

p1 <- ggplot(plot_df, aes(x = F1HIMATH_label, y = percent, fill = math_years_label)) +
  geom_col(position = position_dodge(width = 0.75)) +
  labs(x = "Highest Math Course Taken", y = "Weighted Percentage",
       fill = "Math Years Required") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")
ggsave("output/figures/course_by_requirement.png", p1, width = 10, height = 6, dpi = 300)

# ---- C4) Base-year math score by highest course (mean + SD bars) ---------
math_score_summary <- stu %>%
  filter(!is.na(F1HIMATH_label), !is.na(math_score_by)) %>%
  group_by(F1HIMATH_label) %>%
  summarise(
    mean_score = mean(math_score_by, na.rm = TRUE),
    sd_score   = sd(math_score_by,   na.rm = TRUE),
    n = n(),
    se_score = sd_score / sqrt(n),
    .groups = "drop"
  )

p2 <- ggplot(math_score_summary, aes(x = F1HIMATH_label, y = mean_score)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_score - sd_score, ymax = mean_score + sd_score), width = 0.2) +
  labs(x = "Highest Math Course Taken", y = "Base-Year Math Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("output/figures/math_score_by_course.png", p2, width = 10, height = 6, dpi = 300)

# ---- C5) Gender distribution within each course --------------------------
gender_dist <- stu %>%
  filter(!is.na(F1HIMATH_label), !is.na(gender_label)) %>%
  group_by(F1HIMATH_label, gender_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(F1HIMATH_label) %>%
  mutate(percent = 100 * n / sum(n))

gender_table <- gender_dist %>%
  select(F1HIMATH_label, gender_label, percent) %>%
  tidyr::pivot_wider(names_from = gender_label, values_from = percent)

latex_gender_course <- kbl(gender_table, "latex", booktabs = TRUE, digits = 1,
                           caption = "Gender Distribution by Highest Math Course (row %)") %>%
  kable_styling(latex_options = c("hold_position","repeat_header"))
writeLines(latex_gender_course, "output/tables/gender_by_course.tex")

library(scales)

p3 <- ggplot(gender_dist, aes(x = F1HIMATH_label, y = percent, fill = gender_label)) +
  geom_col(position = position_dodge(width = 0.75)) +
  labs(title = "Gender Distribution by Highest Math Course",
       x = "Highest Math Course", y = "Percentage", fill = "Gender") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")
ggsave("output/figures/gender_by_course.png", p3, width = 10, height = 6, dpi = 300)

# ---- C6) Self-efficacy change (Grade 10 -> 12) by course & gender --------
stu <- stu %>%
  mutate(efficacy_change = math_self_efficacy_f1 - math_self_efficacy_by)

eff_change <- stu %>%
  filter(!is.na(F1HIMATH_label), !is.na(efficacy_change), !is.na(gender_label)) %>%
  group_by(F1HIMATH_label, gender_label) %>%
  summarise(mean_change = mean(efficacy_change, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(eff_change, aes(x = F1HIMATH_label, y = mean_change, color = gender_label, group = gender_label)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  geom_line(linewidth = 1, position = position_dodge(0.4)) +
  labs(title = "Change in Math Self-Efficacy (BY → F1) by Course and Gender",
       x = "Highest Math Course", y = "Average Change", color = "Gender") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("output/figures/efficacy_change_by_course_gender.png", p4, width = 10, height = 6, dpi = 300)

# ---- C7) Ability deciles (BY math) and self-efficacy over time ------------
stu_dec <- stu %>%
  filter(!is.na(math_score_by), !is.na(math_self_efficacy_by), !is.na(math_self_efficacy_f1)) %>%
  mutate(
    math_decile = ntile(math_score_by, 10),
    sex_label = if_else(female == 1, "Female", "Male")
  ) %>%
  group_by(math_decile, sex_label) %>%
  summarise(
    mean_by = mean(math_self_efficacy_by, na.rm = TRUE),
    mean_f1 = mean(math_self_efficacy_f1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(c(mean_by, mean_f1), names_to = "timepoint", values_to = "self_efficacy") %>%
  mutate(timepoint = recode(timepoint, mean_by = "Grade 10", mean_f1 = "Grade 12"))

p5 <- ggplot(stu_dec, aes(x = math_decile, y = self_efficacy, color = sex_label, linetype = timepoint)) +
  geom_point(size = 2, position = position_dodge(0.4)) +
  geom_line(size = 1, position = position_dodge(0.4), aes(group = interaction(sex_label, timepoint))) +
  scale_x_continuous(breaks = 1:10, labels = paste0("Q", 1:10)) +
  labs(title = "Math Self-Efficacy by Ability Decile and Gender (BY vs F1)",
       x = "Math Ability Decile (BY)", y = "Self-Efficacy",
       color = "Gender", linetype = "Grade") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
ggsave("output/figures/efficacy_by_decile_gender.png", p5, width = 10, height = 6, dpi = 300)


message("Course taking patterns complete: wrote output to output/figures")
