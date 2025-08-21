plot_gmm <- function(file_path) {
  model <- readModels(file_path, quiet = TRUE)
  growth_params <- model$parameters$unstandardized %>%
    filter(param %in% c("I", "S"), str_detect(paramHeader, "Means")) %>%
    select(LatentClass, param, est) %>%
    pivot_wider(names_from = param, values_from = est) %>%
    mutate(LatentClass = paste("Class", LatentClass))
  
  plot_data <- growth_params %>%
    mutate(Grade2 = I, Grade3 = I + S, Grade4 = I + S * 2, Grade5 = I + S * 3) %>%
    pivot_longer(cols = Grade2:Grade5, names_to = "Grade", values_to = "NCE") %>%
    mutate(Grade = as.numeric(str_extract(Grade, "\\d+")))
  
  ggplot(plot_data, aes(x = Grade, y = NCE, color = LatentClass, lty = LatentClass)) +
    geom_line() + geom_point(size = 4) +
    labs(title = "GMM Reading Trajectories", y = "NCE Score") +
    scale_x_continuous(breaks = 2:5, labels = c("2", "3", "4", "5")) +
    theme_cowplot() +
    theme(text = element_text(family = "serif", size = 12), 
          legend.position = "top", 
          legend.title = element_blank())
}