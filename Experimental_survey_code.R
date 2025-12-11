# Experimental_survey_code.R
# Converted from sajobun_r_code_guide.md to runnable R code
# Purpose: data cleaning, validation, scoring, and basic analysis workflow

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(psych)
})

# -----------------------------------------------------------------------------
# STEP 1: Load coded survey data
# -----------------------------------------------------------------------------
# Replace with your file path or use file.choose()
input_path <- NULL
if (is.null(input_path)) {
  df <- read_excel(file.choose())
} else {
  df <- read_excel(input_path)
}

# Inspect structure
print(head(df))
print(str(df))
print(dim(df))

# -----------------------------------------------------------------------------
# STEP 2: Error / missing detection
# -----------------------------------------------------------------------------
# Locate "X" entries (likely input errors)
x_location <- which(df == "X", arr.ind = TRUE)
print(x_location)

# Count missing values by column
na_count <- colSums(is.na(df))
print(na_count[na_count > 0])

# Per-respondent error/missing counts
df <- df %>%
  mutate(
    errors = rowSums(. == "X", na.rm = TRUE),
    missing = rowSums(is.na(.))
  )

problem_respondents <- df %>% filter(errors > 0 | missing > 5)
print(problem_respondents)

# -----------------------------------------------------------------------------
# STEP 3: Cleaning options
# -----------------------------------------------------------------------------
# Option 1: Listwise deletion (errors == 0, missing <= 3)
df_listwise <- df %>%
  filter(errors == 0, missing <= 3)

# Option 2: Pairwise-friendly set (keep rows without "X"; NA retained)
df_pairwise <- df %>%
  filter(errors == 0)

message("Option1 (listwise): ", nrow(df), " → ", nrow(df_listwise), " rows")
message("Option2 (pairwise): ", nrow(df_pairwise), " rows retained")

# Choose working dataset (pairwise by default)
df_clean <- df_pairwise

# -----------------------------------------------------------------------------
# STEP 4: Type conversion and range checks
# -----------------------------------------------------------------------------
# Convert X to NA
for (col in names(df_clean)) {
  df_clean[[col]][df_clean[[col]] == "X"] <- NA
}

# Convert selected columns to numeric; adjust demographic_cols if needed
demographic_cols <- c(1, 2, 3, 4)  # keep as character/factor
df_clean <- df_clean %>%
  mutate(across(-demographic_cols, ~ suppressWarnings(as.numeric(.))))

# Range check helper (default 0-5 likert)
range_check <- function(data, start_col = 5, min_val = 0, max_val = 5) {
  for (i in seq.int(start_col, ncol(data))) {
    outliers <- data[[i]][data[[i]] < min_val | data[[i]] > max_val]
    if (length(outliers) > 0) {
      cat("Out-of-range values in", names(data)[i], ":", unique(outliers), "\n")
    }
  }
}
range_check(df_clean, start_col = 5, min_val = 0, max_val = 5)

# -----------------------------------------------------------------------------
# STEP 5: Scale scoring examples
# -----------------------------------------------------------------------------
# Adjust column indices to match your survey
rumination_items <- 5:14
rumination_mean <- rowMeans(df_clean[, rumination_items], na.rm = TRUE)
rumination_sum  <- rowSums(df_clean[, rumination_items], na.rm = TRUE)

# Example DASS-21 subscales (replace indices with your mapping)
dass_dep <- c(15, 18, 21, 24)
dass_anx <- c(16, 19, 22, 25)
dass_str <- c(17, 20, 23, 26)

(df_clean <- df_clean %>%
  mutate(
    Rumination_mean = rumination_mean,
    Rumination_sum  = rumination_sum,
    Depression      = rowMeans(across(all_of(dass_dep)), na.rm = TRUE),
    Anxiety         = rowMeans(across(all_of(dass_anx)), na.rm = TRUE),
    Stress          = rowMeans(across(all_of(dass_str)), na.rm = TRUE)
  ))

# -----------------------------------------------------------------------------
# STEP 6: Reliability checks
# -----------------------------------------------------------------------------
# Cronbach's alpha for rumination items and DASS subscales
alpha(df_clean[, rumination_items])
alpha(df_clean[, dass_dep])
alpha(df_clean[, dass_anx])
alpha(df_clean[, dass_str])

# -----------------------------------------------------------------------------
# STEP 7: Descriptive statistics
# -----------------------------------------------------------------------------
summary_table <- df_clean %>%
  summarise(
    N = n(),
    Rumination_M = mean(Rumination_mean, na.rm = TRUE),
    Rumination_SD = sd(Rumination_mean, na.rm = TRUE),
    Rumination_Min = min(Rumination_mean, na.rm = TRUE),
    Rumination_Max = max(Rumination_mean, na.rm = TRUE),
    Depression_M = mean(Depression, na.rm = TRUE),
    Depression_SD = sd(Depression, na.rm = TRUE),
    Anxiety_M = mean(Anxiety, na.rm = TRUE),
    Anxiety_SD = sd(Anxiety, na.rm = TRUE),
    Stress_M = mean(Stress, na.rm = TRUE),
    Stress_SD = sd(Stress, na.rm = TRUE)
  )
print(summary_table)

# Stratified example (replace "성별" column name if different)
if ("성별" %in% names(df_clean)) {
  df_clean %>%
    group_by(성별) %>%
    summarise(
      N = n(),
      Depression_M = mean(Depression, na.rm = TRUE),
      Depression_SD = sd(Depression, na.rm = TRUE)
    ) %>%
    print()
}

# -----------------------------------------------------------------------------
# STEP 8: Basic inferential examples
# -----------------------------------------------------------------------------
# Example: Wilcoxon test by a binary factor (replace "종교")
if ("종교" %in% names(df_clean)) {
  try(print(wilcox.test(Depression ~ 종교, data = df_clean)), silent = TRUE)
}

# Example correlation between Rumination and Depression
try(print(cor.test(df_clean$Rumination_mean, df_clean$Depression)), silent = TRUE)

# -----------------------------------------------------------------------------
# STEP 9: Save cleaned data
# -----------------------------------------------------------------------------
write.csv(df_clean, "final_cleaned_data.csv", row.names = FALSE)

message("Cleaning and basic analysis complete. Saved to final_cleaned_data.csv")
