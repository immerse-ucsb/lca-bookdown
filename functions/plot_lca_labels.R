
    library(MplusAutomation)
    library(tidyverse)
    library(reshape2)
    library(cowplot)
    library(glue)
    


plot_lca_labels <-
  function(model_name, item_labels, class_labels, title) {

    
pp_plots <- data.frame(model_name$parameters$probability.scale) %>% 
  mutate(LatentClass = sub("^","Class ", LatentClass)) %>% 
  filter(category == 2) %>% 
  dplyr::select(est, LatentClass, param) %>% 
  pivot_wider(names_from = LatentClass, values_from = est) %>% 
  relocate(param, .after = last_col()) 

colnames(pp_plots) <- class_labels

c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>% 
  rename("cs" = 1) %>% 
  mutate(cs = round(cs * 100, 2))

colnames(pp_plots) <- paste0(colnames(pp_plots[,1:ncol(pp_plots)-1]), glue(" ({c_size[1:ncol(pp_plots)-1,]}%)")) 

plot_data <- pp_plots %>% 
  rename("param" = ncol(pp_plots)) %>% 
  reshape2::melt(id.vars = "param") %>% 
  mutate(param = fct_inorder(param))

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
  geom_point(size = 4) + geom_line() +
  ylim(0,1) +
  scale_x_discrete(labels = item_labels) +
  labs(title = title, y = "Probability", x="") +
  theme_cowplot() +
  scale_colour_grey(start=0, end=0) +
  theme(
    text = element_text(family = "serif", size = 15),
    axis.text = element_text(family = "serif", size = 15),
    legend.key.width = unit(0, "line"),
    legend.title = element_blank(),
    legend.position = "top",    
    axis.text.x = element_text(
      vjust = 1
    )
  )
p
return(p)
}