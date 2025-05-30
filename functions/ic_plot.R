
ic_plot <- function(x) {

# Calculate Indices Derived from the Log Likelihood (LL)
enum_fit <- x %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  separate(Title, c("Model", "Class"), sep = "with") %>% 
  mutate(SIC = -.5 * BIC) %>%
  drop_na(SIC) %>% 
  group_by(Model) %>% 
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  ungroup() %>% 
  unite(Title, c("Model", "Class"), sep = "with", remove = TRUE) %>% 
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  mutate(Title = str_to_title(Title)) %>% 
  arrange(Title)

enum_fit %>% 
  dplyr::select(1:7) %>% 
  separate(Title, c("Model", "Class"), sep = "With") %>% 
  separate(Class, "Class", sep = "Classes") %>% 
  pivot_longer(`BIC`:`AWE`,
               names_to = "Index",
               values_to = "ic_value") %>%  
mutate(Index = factor(Index,
                      levels = c ("AWE", "CAIC", "BIC", "aBIC"))) %>% 
  ggplot(aes(x = Class, y = ic_value,
             color = Index, shape = Index,
             group = Index, lty = Index)) +
  geom_point(size = 2.0) + geom_line(linewidth = .8) +
  scale_colour_grey(end = .5) +
  theme_cowplot() +
  labs(x = "Number of Profiles", y = "Information Criteria Value") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  facet_wrap(~Model, scales = "free_y")

}