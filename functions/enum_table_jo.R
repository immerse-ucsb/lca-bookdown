
fit_table_jo <- function(c1, c2, row1, row2) {
  
  # Define rows for row groups (assuming 6 models per time)
  rows_m1 <- row1
  rows_m2 <- row2  

  # Extract fit data
  enum_extract1 <- LatexSummaryTable(c1,                                 
                                     keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                                                "BLRT_PValue", "T11_VLMR_PValue","Observations"))
  
  enum_extract2 <- LatexSummaryTable(c2,                                 
                                     keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                                                "BLRT_PValue", "T11_VLMR_PValue","Observations"))
  
  # Calculate derived indices function
  calc_indices <- function(df) {
    df %>% 
      mutate(
        aBIC = -2*LL + Parameters * log((Observations + 2) / 24),
        CAIC = -2*LL + Parameters * (log(Observations) + 1),
        AWE  = -2*LL + 2 * Parameters * (log(Observations) + 1.5),
        SIC = -0.5 * BIC,
        expSIC = exp(SIC - max(SIC)),
        BF = exp(SIC - lead(SIC)),
        cmPk = expSIC / sum(expSIC)
      ) %>% 
      select(1:5, 9:10, 6:7, 13, 14) %>% 
      arrange(Parameters)
  }
  
  allFit1 <- calc_indices(enum_extract1)
  allFit2 <- calc_indices(enum_extract2)
  
  # Join tables
  allFit <- full_join(allFit1, allFit2)
  
  # Format table with gt
  tbl <- allFit %>% 
    mutate(Title = str_remove(Title, "_Time*")) %>% 
    gt() %>%
    tab_header(title = md("**Model Fit Summary Table**")) %>% 
    cols_label(
      Title = "Classes",
      Parameters = md("Par"),
      LL = md("*LL*"),
      T11_VLMR_PValue = "VLMR",
      BLRT_PValue = "BLRT",
      BF = md("BF"),
      cmPk = md("*cmP_k*")
    ) %>%
    tab_footnote(
      footnote = md(
        "*Note.* Par = Parameters; *LL* = model log likelihood;
        BIC = Bayesian information criterion;
        aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
        AWE = approximate weight of evidence criterion;
        BLRT = bootstrapped likelihood ratio test p-value;
        VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
        cmPk = approximate correct model probability."
      ), 
      locations = cells_title()
    ) %>% 
    tab_options(column_labels.font.weight = "bold") %>% 
    fmt_number(10, decimals = 2, drop_trailing_zeros = TRUE, suffixing = TRUE) %>% 
    fmt_number(c(3:9,11), decimals = 2) %>% 
    fmt_missing(1:11, missing_text = "--") %>% 
    fmt(c(8:9,11),
        fns = function(x) ifelse(x < 0.001, "<.001", scales::number(x, accuracy = 0.01))
    ) %>%
    fmt(10, fns = function(x) ifelse(x > 100, ">100", scales::number(x, accuracy = 0.1))) %>%
    tab_row_group(group = "LCA 1", rows = rows_m1) %>%
    tab_row_group(group = "LCA 2", rows = rows_m2) %>% 
    row_group_order(groups = c("LCA 1", "LCA 2")) %>% 
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = list(
        cells_body(columns = BIC, row = BIC == min(BIC[rows_m1])),
        cells_body(columns = aBIC, row = aBIC == min(aBIC[rows_m1])),
        cells_body(columns = CAIC, row = CAIC == min(CAIC[rows_m1])),
        cells_body(columns = AWE, row = AWE == min(AWE[rows_m1])),
        cells_body(columns = cmPk, row = cmPk == max(cmPk[rows_m1])),
        cells_body(columns = BIC, row = BIC == min(BIC[rows_m2])),
        cells_body(columns = aBIC, row = aBIC == min(aBIC[rows_m2])),
        cells_body(columns = CAIC, row = CAIC == min(CAIC[rows_m2])),
        cells_body(columns = AWE, row = AWE == min(AWE[rows_m2])),
        cells_body(columns = cmPk, row = cmPk == max(cmPk[rows_m2])),
        cells_body(columns = BF,  row = BF > 10),
        cells_body(columns = BLRT_PValue, row = ifelse(BLRT_PValue < .05 & lead(BLRT_PValue) > .05, TRUE, NA)),
        cells_body(columns = T11_VLMR_PValue, row = ifelse(T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue) > .05, TRUE, NA))
      )
    )
  
  return(tbl)
}