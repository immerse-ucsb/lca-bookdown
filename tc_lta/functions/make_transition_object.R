#— file: functions/make_transition_object.R ——————————————
make_transition_object <- function(df, n1, n2, n3) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  # Convert to factors if needed
  df <- df %>%
    mutate(
      across(all_of(c(n1, n2, n3)), ~ factor(.x, levels = 1:4))
    ) %>%
    drop_na(all_of(c(n1, n2, n3)))
  
  timepoints <- c(n1, n2, n3)
  transitions <- map2(timepoints[-length(timepoints)], timepoints[-1], ~{
    df %>%
      count(from = .data[[.x]], to = .data[[.y]], name = "n") %>%
      complete(from = factor(1:4), to = factor(1:4), fill = list(n = 0)) %>%
      group_by(from) %>%
      mutate(probability = n / sum(n)) %>%
      ungroup() %>%
      mutate(
        from = paste(.x, from, sep = "."),
        to   = paste(.y, to, sep = ".")
      ) %>%
      select(from, to, probability)
  }) %>% bind_rows()
  
  # Compute class proportions for each timepoint
  proportions <- map_dfr(timepoints, function(tp) {
    df %>%
      count(class = .data[[tp]], name = "n") %>%
      complete(class = factor(1:4), fill = list(n = 0)) %>%
      mutate(
        variable    = tp,
        proportion  = n / sum(n),
        class       = as.integer(as.character(class))
      ) %>%
      select(variable, class, proportion)
  })
  
  list(
    class_counts = list(
      transitionProbs = transitions,
      modelEstimated  = proportions
    )
  )
}
#———————————————————————————————————————————————————————————————
