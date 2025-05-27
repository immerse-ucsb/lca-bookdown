library(cowplot)

poLCA_stacked <- function(model_name, category_labels) {
  # Prepare the plot data
  pp_plots <- data.frame(model_name$parameters$probability.scale) %>% 
    mutate(LatentClass = sub("^","Class ", LatentClass)) %>% 
    dplyr::select(est, LatentClass, param, category) %>% 
    pivot_wider(names_from = LatentClass, values_from = est) %>% 
    relocate(category, .after = last_col()) %>% 
    relocate(param, .after = last_col())

  c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>% 
    rename("cs" = 1) %>% 
    mutate(cs = round(cs * 100, 2))

  colnames(pp_plots)[1:nrow(c_size)] <- paste0(
    colnames(pp_plots)[1:nrow(c_size)], 
    glue(" ({c_size[1:nrow(c_size),]}%)")
  )

  # Prepare the plot data for ggplot
  plot_data <- pp_plots %>% 
    pivot_longer(cols = starts_with("class"), 
                 names_to = "Class", 
                 values_to = "Value") %>% 
    mutate(category = recode(category, !!!category_labels))

  # Get the model title
  name <- model_name$input$title

  # Create and return the plot
  ggplot(plot_data, aes(x = param, y = Value, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = sub("^0\\.", ".", sprintf("%.2f", Value))), 
          position = position_stack(vjust = 0.5), 
          size = 3, color = "white") +
    facet_wrap(~ Class) +  # Facet by Class
    labs(title = paste("Class Probabilities by Item and Category"), 
         x = "Item", 
         y = "Probability") +
    scale_fill_brewer(palette = "Set1", name = "Category") +  # Color palette for categories
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_cowplot()
}

poLCA_grouped <- function(model_name, category_labels) {
  # Prepare the plot data
  pp_plots <- data.frame(model_name$parameters$probability.scale) %>% 
    mutate(LatentClass = sub("^","Class ", LatentClass)) %>% 
    dplyr::select(est, LatentClass, param, category) %>% 
    pivot_wider(names_from = LatentClass, values_from = est) %>% 
    relocate(category, .after = last_col()) %>% 
    relocate(param, .after = last_col())

  c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>% 
    rename("cs" = 1) %>% 
    mutate(cs = round(cs * 100, 2))

  colnames(pp_plots)[1:nrow(c_size)] <- paste0(
    colnames(pp_plots)[1:nrow(c_size)], 
    glue(" ({c_size[1:nrow(c_size),]}%)")
  )

  # Prepare the plot data for ggplot
  plot_data <- pp_plots %>% 
    pivot_longer(cols = starts_with("class"), 
                 names_to = "Class", 
                 values_to = "Value") %>% 
    mutate(category = recode(category, !!!category_labels))

  # Get the model title
  name <- model_name$input$title

  # Create and return the plot
  plot_data %>%
  ggplot(aes(x = param, y = Value, fill = category, group = category)) + 
  geom_bar(stat = "identity", position = "dodge") +  # Change position to dodge for grouped bars
  geom_text(aes(label = sub("^0\\.", ".", sprintf("%.2f", Value))), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +  # Add text labels above bars without leading zeros
  facet_wrap(~ Class) +  # Facet by Class
  labs(title = "Class Probabilities by Item and Category", 
       x = "Item", 
       y = "Probability") +
  scale_fill_brewer(palette = "Set1", name = "Category") +  # Color palette for categories
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_cowplot()
}
