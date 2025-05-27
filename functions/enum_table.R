enum_table <- function(x, rows) {


# Extract model fit data
enum_extract <- LatexSummaryTable(x,                                 
                keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                           "BLRT_PValue", "T11_VLMR_PValue","Observations")) 

# Calculate Indices Derived from the Log Likelihood (LL)
enum_fit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  mutate(SIC = -.5 * BIC) %>%
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  arrange(Parameters)

fit_table <- enum_fit %>%
  gt() %>%
  tab_header(title = md("**Model Fit Summary Table**")) %>%
  cols_label(
    Title = "Classes",
    Parameters = md("Par"),
    LL = md("*LL*"),
    BLRT_PValue = "BLRT",
    T11_VLMR_PValue = "VLMR",    
    BF = md("BF"),
    cmPk = md("*cmPk*")) %>%
  tab_footnote(
    footnote = md(
    "*Note.* Par = Parameters; *LL* = model log likelihood;
      BIC = Bayesian information criterion;
      aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
      AWE = approximate weight of evidence criterion;
      BLRT = bootstrapped likelihood ratio test p-value;
      cmPk = approximate correct model probability."), 
    locations = cells_title()) %>% 
  tab_options(column_labels.font.weight = "bold") %>% 
  fmt_number(10,decimals = 2,
             drop_trailing_zeros=TRUE,
             suffixing = TRUE) %>% 
  fmt_number(c(3:7,11), 
             decimals = 2) %>% 
  sub_missing(1:11,
              missing_text = "--") %>% 
  fmt(c(8,9, 11),
    fns = function(x) 
    ifelse(x<0.001, "<.001",
           scales::number(x, accuracy = 0.001))) %>%
  fmt(10, fns = function(x) 
    ifelse(x>100, ">100",
           scales::number(x, accuracy = .1))) %>%  
  tab_style(
    style = list(
      cell_text(weight = "bold")

            ),
    locations = list(cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows]) # Change this to the number of classes you estimated
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows])
     ),    
    cells_body(
     columns = BF,
     row =  BF > 10),
    cells_body( 
     columns =  T11_VLMR_PValue,
     row =  ifelse(T11_VLMR_PValue < .001 & lead(T11_VLMR_PValue) > .05, T11_VLMR_PValue < .001, NA)),
    cells_body(
     columns =  BLRT_PValue,
     row =  ifelse(BLRT_PValue < .001 & lead(BLRT_PValue) > .05, BLRT_PValue < .001, NA))
  )
) 

fit_table


}


seeRows <- function(model) {

enum_extract <- LatexSummaryTable(
  model,
  keepCols = c(
    "Title",
    "Parameters",
    "LL",
    "BIC",
    "aBIC",
    "BLRT_PValue",
    "T11_VLMR_PValue",
    "Observations"
  ),
  sortBy = "Title"
) #%>% slice_head(n=4) # Select first four models (Class 1 through 4)


allFit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  mutate(SIC = -.5 * BIC) %>%
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  arrange(Parameters)
  
as.data.frame(allFit)

}

ic_plot <- function(x) {

library(cowplot)

# Extract model fit data
enum_extract <- LatexSummaryTable(x,                                 
                keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                           "BLRT_PValue", "T11_VLMR_PValue","Observations")) 

# Calculate Indices Derived from the Log Likelihood (LL)
allFit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  mutate(SIC = -.5 * BIC) %>%
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  arrange(Parameters)

allFit %>%
  dplyr::select(2:7) %>%
  rowid_to_column() %>%
  pivot_longer(`BIC`:`AWE`,
               names_to = "Index",
               values_to = "ic_value") %>%
  mutate(Index = factor(Index,
                        levels = c ("AWE", "CAIC", "BIC", "aBIC"))) %>%
  ggplot(aes(
    x = rowid,
    y = ic_value,
    color = Index,
    shape = Index,
    group = Index,
    lty = Index
  )) +
  geom_point(size = 2.0) + geom_line(size = .8) +
  scale_x_continuous(breaks = 1:nrow(allFit)) +
  scale_colour_grey(end = .5) +
  theme_cowplot() +
  labs(x = "Number of Classes", y = "Information Criteria Value", title = "Information Criteria") +
  theme(
    text = element_text(family = "Times", size = 12),
    legend.text = element_text(family="Times", size=12),
    legend.key.width = unit(3, "line"),
    legend.title = element_blank(),
    legend.position = "top"  
  )



}

