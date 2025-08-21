# transform_covariates.R
library(dplyr)

# Define which reference class to use for each comparison
comparison_source <- list(
  "1 v. 2" = "RefClass1",
  "1 v. 3" = "RefClass1",
  "1 v. 4" = "RefClass1",
  "2 v. 3" = "RefClass2",
  "2 v. 4" = "RefClass2",
  "3 v. 4" = "RefClass3"
)

# Initialize results data frame
results <- data.frame(
  Comparison = character(),
  Covariate = character(),
  LogOdds_Est = numeric(),
  Odds_Ratio = numeric(),
  SE = numeric(),
  Lower_95CI = numeric(),
  Upper_95CI = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Process scraped_data
for (i in 1:nrow(scraped_data)) {
  ref_class <- if (i <= 12) "RefClass1" else if (i <= 24) "RefClass2" else "RefClass3"
  row <- scraped_data[i, ]
  
  comp <- case_when(
    ref_class == "RefClass1" & row$paramHeader == "C#2.ON" ~ "1 v. 2",
    ref_class == "RefClass1" & row$paramHeader == "C#3.ON" ~ "1 v. 3",
    ref_class == "RefClass1" & row$paramHeader == "C#4.ON" ~ "1 v. 4",
    ref_class == "RefClass2" & row$paramHeader == "C#1.ON" ~ "1 v. 2",
    ref_class == "RefClass2" & row$paramHeader == "C#3.ON" ~ "2 v. 3",
    ref_class == "RefClass2" & row$paramHeader == "C#4.ON" ~ "2 v. 4",
    ref_class == "RefClass3" & row$paramHeader == "C#1.ON" ~ "1 v. 3",
    ref_class == "RefClass3" & row$paramHeader == "C#2.ON" ~ "2 v. 3",
    ref_class == "RefClass3" & row$paramHeader == "C#4.ON" ~ "3 v. 4",
    TRUE ~ NA_character_
  )
  
  if (is.na(comp)) next
  if (ref_class != comparison_source[[comp]]) next
  
  covariate <- case_when(
    row$param == "FIRSTGEN" ~ "Generation Status",
    row$param == "WHITE" ~ "European American",
    row$param == "LATINX" ~ "Latino/a",
    row$param == "ASIAN" ~ "Asian",
    TRUE ~ row$param
  )
  
  logodds_est <- row$est
  odds_ratio <- row$odds_est
  se <- row$odds_se
  lower_ci <- row$odds_lower_2.5ci
  upper_ci <- row$odds_upper_2.5ci
  
  if ((comp == "1 v. 2" && ref_class == "RefClass2") ||
      (comp == "1 v. 3" && ref_class == "RefClass3") ||
      (comp == "2 v. 3" && ref_class == "RefClass3") ||
      (comp == "1 v. 4")) {
    logodds_est <- -logodds_est
    odds_ratio <- 1 / odds_ratio
    lower_ci_temp <- 1 / row$odds_upper_2.5ci
    upper_ci <- 1 / row$odds_lower_2.5ci
    lower_ci <- lower_ci_temp
    se <- row$odds_se
  }
  
  results <- rbind(results, data.frame(
    Comparison = comp,
    Covariate = covariate,
    LogOdds_Est = logodds_est,
    Odds_Ratio = odds_ratio,
    SE = se,
    Lower_95CI = lower_ci,
    Upper_95CI = upper_ci,
    P_Value = row$pval,
    stringsAsFactors = FALSE
  ))
}

# Sort and add significance indicators
results <- results %>%
  arrange(
    factor(Covariate, levels = c("Generation Status", "Asian", "Latino/a", "European American")),
    factor(Comparison, levels = c("1 v. 2", "1 v. 3", "1 v. 4", "2 v. 3", "2 v. 4", "3 v. 4"))
  ) %>%
  mutate(
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    LogOdds_Est = paste0(LogOdds_Est, Significance)
  ) %>%
  select(-Significance)