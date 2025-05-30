enum_table <- function(x, rows_m1, rows_m2, rows_m3, rows_m4, rows_m5, rows_m6) {

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
 tab_row_group(
    group = "Model 1",
    rows = rows_m1) %>%
  tab_row_group(
    group = "Model 2",
    rows = rows_m2) %>%
  tab_row_group(
    group = "Model 3",
    rows = rows_m3) %>%    
  tab_row_group(
    group = "Model 4",
    rows = rows_m4) %>%
  tab_row_group(
    group = "Model 5",
    rows = rows_m5) %>%
  tab_row_group(
    group = "Model 6",
    rows = rows_m6) %>%    
  row_group_order(
      groups = c("Model 1","Model 2", "Model 3","Model 4", "Model 5", "Model 6")
      ) %>%  
  tab_style(
    style = list(
      cell_text(weight = "bold")
      ),
    locations = list(cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m1]) # Model 1
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m1])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m1])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m1])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m1])
     ),   
    cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m2]) # Model 2
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m2])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m2])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m2])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m2])
     ),  
    cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m3]) # Model 3
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m3])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m3])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m3])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m3])
     ),        
    cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m4]) # Model 4
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m4])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m4])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m4])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m4])
     ),   
    cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m5]) # Model 5
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m5])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m5])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m5])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m5])
     ), 
    cells_body(
     columns = BIC,
     row = BIC == min(BIC[rows_m6]) # Model 6
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[rows_m6])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[rows_m6])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[rows_m6])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[rows_m6])
     ), 
    cells_body(
     columns = BF, 
     row =  BF > 10),
    cells_body(
     columns =  BLRT_PValue,
     row =  ifelse(BLRT_PValue < .05 & lead(BLRT_PValue) > .05, BLRT_PValue < .05, NA)),
    cells_body(
     columns =  T11_VLMR_PValue,
     row =  ifelse(T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue) > .05, T11_VLMR_PValue < .05, NA))
  )
)

fit_table


}


enum_fit <- function(x) {

# Extract model fit data
enum_extract <- LatexSummaryTable(x,                                 
                keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                           "BLRT_PValue", "T11_VLMR_PValue","Observations")) 

# Calculate Indices Derived from the Log Likelihood (LL)
enum_fit <- enum_extract %>%
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
  arrange(Title) %>% 
  mutate(row = row_number()) %>%
  select(row, everything())
  
  enum_fit

}