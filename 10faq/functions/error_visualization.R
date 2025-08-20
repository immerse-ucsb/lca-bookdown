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
  
  return(error_table_data)
}
