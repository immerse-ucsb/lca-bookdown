extract_warnings <- function(final_data) {
  
  warnings_table_data <- final_data %>%
    select(File_Name, Warnings) %>%
    unnest(Warnings) %>%
    filter(!is.na(Warnings)) %>%
    mutate(
      Warning_Count = str_extract(Warnings, "\\d+(?= WARNING\\(S\\) FOUND)") %>% as.numeric()
    ) 
  
  warnings_table_data <- warnings_table_data %>%
    group_by(File_Name) %>%
    mutate(Warning_Count = max(Warning_Count, na.rm = TRUE)) %>%
    ungroup()
  
  warnings_table_data <- warnings_table_data %>%
    mutate(
      Warning_Summary = ifelse(duplicated(File_Name), "", 
                               paste0("There are ", Warning_Count, " warnings in the output file.")),
      File_Name = ifelse(duplicated(File_Name), "", File_Name)
    ) %>%
    select(File_Name, Warning_Summary, Warnings)
  
  warnings_table_data <- warnings_table_data %>%
    mutate(Warnings = str_replace(Warnings, "\\d+ WARNING\\(S\\) FOUND IN THE INPUT INSTRUCTIONS", ""))
  
  
  if (nrow(warnings_table_data) > 0) {
    warnings_table <- warnings_table_data %>%
      gt() %>%
      tab_header(title = md("**Model Warnings**")) %>%
      cols_label(
        File_Name = "Output File",
        Warning_Summary = "# of Warnings",
        Warnings = "Warning Message(s)"
      ) %>%
      cols_align(align = "left", columns = everything()) %>%
      
      # Set column widths to allow wrapping
      cols_width(
        File_Name ~ px(150),
        Warning_Summary ~ px(150),
        Warnings ~ px(400)
      ) %>%
      
      
      # Attempt to wrap long text for HTML output
      text_transform(
        locations = cells_body(columns = Warnings),
        fn = function(x) {
          if (knitr::is_html_output()) {
            paste0("<div style='white-space: normal; word-wrap: break-word;'>", x, "</div>")
          } else {
            # For PDF, rely on the fixed column width
            x
          }
        }
      ) %>%
      tab_options(table.width = pct(100))
    
    return(warnings_table)
    
  } else {
    return(cat("No warnings detected in any output files.\n"))
  }
  
}
