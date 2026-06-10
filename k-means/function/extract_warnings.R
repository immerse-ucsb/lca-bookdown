
# ✅ Extract warnings and counts
warnings_table_data <- final_data %>%
  select(File_Name, Warnings) %>%
  unnest(Warnings) %>%
  filter(!is.na(Warnings)) %>%
  mutate(
    Warning_Count = str_extract(Warnings, "\\d+(?= WARNING\\(S\\) FOUND)") %>% as.numeric()
  ) 

# ✅ Keep only the highest valid warning count per file
warnings_table_data <- warnings_table_data %>%
  group_by(File_Name) %>%
  mutate(Warning_Count = max(Warning_Count, na.rm = TRUE)) %>%
  ungroup()

# ✅ Ensure Warning_Count is displayed **ONCE per file** in the second column
warnings_table_data <- warnings_table_data %>%
  mutate(
    Warning_Summary = ifelse(duplicated(File_Name), "", 
                             paste0("There are ", Warning_Count, " warnings in the output file.")),
    File_Name = ifelse(duplicated(File_Name), "", File_Name)  # ✅ Only show Output File name once
  ) %>%
  select(File_Name, Warning_Summary, Warnings)  # ✅ Reorder columns

# ✅ Remove "X WARNING(S) FOUND IN THE INPUT INSTRUCTIONS" from warnings column
warnings_table_data <- warnings_table_data %>%
  mutate(Warnings = str_replace(Warnings, "\\d+ WARNING\\(S\\) FOUND IN THE INPUT INSTRUCTIONS", ""))
