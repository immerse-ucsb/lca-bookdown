# error_visualization.R
process_error_data <- function(final_data) {
  error_table_data <- final_data %>%
    select(File_Name, Class_Model, Errors) %>%
    mutate(Error_Message = map_chr(Errors, ~ {
      if (is.null(.x) || (is_tibble(.x) && all(is.na(.x$Error_Message)))) {
        NA_character_
      } else if (is_tibble(.x)) {
        paste(unlist(.x$Error_Message), collapse = "\n\n")  # Double newline for space
      } else if (is.list(.x)) {
        paste(unlist(.x), collapse = "\n\n")  # Double newline for space
      } else {
        NA_character_
      }
    })) %>%
    select(File_Name, Class_Model, Error_Message) %>%
    filter(!is.na(Error_Message))
  
  # Visualize Errors If Any Exist
  if (nrow(error_table_data) > 0) {
    error_table <- error_table_data %>%
      gt() %>%
      tab_header(title = md("**Model Estimation Errors**")) %>%
      cols_label(
        File_Name     = "Output File",
        Class_Model   = "Model Type",
        Error_Message = "Error Message"
      ) %>%
      cols_align(align = "left", columns = everything()) %>%
      cols_width(
        File_Name     ~ px(150),
        Class_Model   ~ px(100),
        Error_Message ~ px(400)
      ) %>%
      tab_options(table.width = px(600)) %>%
      fmt(
        columns = "Error_Message",
        fns = function(x) gsub("\n", "<br>", x)  # Convert \n to HTML breaks
      )
    
   return(error_table)   # Display the table
  } else {
    return(cat("No errors detected in any output files.\n"))
  }
}
