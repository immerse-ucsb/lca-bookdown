library(MplusAutomation)
library(tidyverse)
library(reshape2)
library(cowplot)
library(glue)

plot_lta <- function(model_name, time_point = "c1", items = NULL) {
  probs <- data.frame(model_name$parameters$probability.scale)
  pp_plots <- probs %>%
    mutate(LatentClass = sub("^", "Class ", LatentClass)) %>%
    filter(grepl(paste0("^Class ", toupper(time_point), "#"), LatentClass)) %>%
    filter(category == 2) %>%
    dplyr::select(est, LatentClass, param)
  
  if (!is.null(items)) {
    target_params <- items
    pp_plots <- pp_plots %>%
      filter(param %in% target_params)
  } else {
    unique_items <- unique(pp_plots$param)[1:10]
    pp_plots <- pp_plots %>%
      filter(param %in% unique_items)
  }
  
  if (nrow(pp_plots) == 0) {
    stop("No data remains after filtering. Check LatentClass, category, or item names.")
  }
  
  pp_plots <- pp_plots %>%
    pivot_wider(names_from = LatentClass, values_from = est) %>%
    relocate(param, .after = last_col())
  
  class_indices <- 1:4
  c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion[class_indices]) %>%
    rename("cs" = 1) %>%
    mutate(cs = round(cs * 100, 2))
  
  colnames(pp_plots)[1:4] <- paste0(colnames(pp_plots)[1:4], glue(" ({c_size[1:4,]}%)"))
  
  plot_data <- pp_plots %>%
    rename("param" = ncol(pp_plots)) %>%
    reshape2::melt(id.vars = "param") %>%
    mutate(param = fct_inorder(param))
  
  levels(plot_data$param) <- paste0("Item ", 1:10)
  
  name <- model_name$input$title
  
  p <- plot_data %>%
    ggplot(
      aes(
        x = param,
        y = value,
        shape = variable,
        colour = variable,
        lty = variable,
        group = variable
      )
    ) +
    geom_point(size = 4) +
    geom_line() +
    ylim(0, 1) +
    scale_x_discrete("") +
    labs(
      title = glue("{name} Invariant Probability Plot"),
      y = "Probability"
    ) +
    theme_cowplot() +
    theme(
      text = element_text(family = "serif", size = 12),
      legend.text = element_text(family = "serif", size = 12),
      legend.key.width = unit(0, "line"),
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      )
    )
  
  return(p)
}