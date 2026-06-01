process_error_data <- function(final_data) {
  error_table_data <- final_data %>%
    select(File_Name, Errors) %>%
    mutate(Errors = map(Errors, ~ {
      if (is.null(.x)) {
        tibble(Error_Message = NA_character_)  # ✅ Handle NULL cases
      } else if (is_tibble(.x)) {
        tibble(Error_Message = unlist(.x$Error_Message))  # ✅ Extract message from tibble
      } else if (is.list(.x) && all(map_lgl(.x, is.character))) {
        tibble(Error_Message = unlist(.x))  # ✅ Convert list of characters to tibble
      } else if (is.character(.x)) {
        tibble(Error_Message = .x)  # ✅ Convert single character to tibble
      } else {
        tibble(Error_Message = NA_character_)  # ✅ Catch unexpected cases
      }
    })) %>%
    unnest(Errors) %>%
    filter(!is.na(Error_Message))  # ✅ Remove empty errors
  
  return(error_table_data)
}


