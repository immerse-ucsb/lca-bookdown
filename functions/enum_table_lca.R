fit_table_lca <- function(x, final_data) {
 
  # Extract fit indices from Mplus
  enum_extract <- LatexSummaryTable(
    x,
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
  )
  
  # Add calculated fit indices
  allFit <- enum_extract %>%
    mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1),
           AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5),
           SIC = -0.5 * BIC,
           expSIC = exp(SIC - max(SIC)),
           BF = exp(SIC - lead(SIC)),
           cmPk = expSIC / sum(expSIC)) %>%
    dplyr::select(Title, Parameters, LL, BIC, aBIC, CAIC, AWE, 
                  BLRT_PValue, T11_VLMR_PValue, BF, cmPk) %>%
    arrange(Parameters)
  
  # Merge with external final_data
  merged_table <- allFit %>%
    mutate(Title = str_trim(Title)) %>%
    left_join(
      final_data %>%
        select(Class_Model, Perc_Convergence, Replicated_LL_Perc, Smallest_Class, Smallest_Class_Perc),
      by = c("Title" = "Class_Model")
    ) %>%
    mutate(
      Smallest_Class = coalesce(Smallest_Class, final_data$Smallest_Class[match(Title, final_data$Class_Model)]),
      Smallest_Class_Combined = paste0(Smallest_Class, "\u00A0(", Smallest_Class_Perc, "%)")
    ) %>%
    relocate(Perc_Convergence, Replicated_LL_Perc, .after = LL) %>%
    select(
      Title, Parameters, LL, Perc_Convergence, Replicated_LL_Perc,
      BIC, aBIC, CAIC, AWE,
      T11_VLMR_PValue, BLRT_PValue,
      Smallest_Class_Combined, BF, cmPk
    )
  
  # Create gt table
  fit_table <- merged_table %>%
    select(Title, Parameters, LL, Perc_Convergence, Replicated_LL_Perc, 
           BIC, aBIC, CAIC, AWE, 
           T11_VLMR_PValue, BLRT_PValue, 
           Smallest_Class_Combined) %>% 
    gt() %>%
    tab_header(title = md("**Model Fit Summary Table**")) %>%
    tab_spanner(label = "Model Fit Indices", columns = c(BIC, aBIC, CAIC, AWE)) %>%
    tab_spanner(label = "LRTs", columns = c(T11_VLMR_PValue, BLRT_PValue)) %>%
    tab_spanner(label = md("Smallest\u00A0Class"), columns = c(Smallest_Class_Combined)) %>%
    cols_label(
      Title = "Classes",
      Parameters = md("Par"),
      LL = md("*LL*"),
      Perc_Convergence = "% Converged",
      Replicated_LL_Perc = "% Replicated",
      BIC = "BIC",
      aBIC = "aBIC",
      CAIC = "CAIC",
      AWE = "AWE",
      T11_VLMR_PValue = "VLMR",
      BLRT_PValue = "BLRT",
      Smallest_Class_Combined = "n (%)"
    ) %>%
    tab_footnote(
      footnote = md(
        "*Note.* Par = Parameters; *LL* = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
*cmPk* = approximate correct model probability."
      ),
      locations = cells_title()
    ) %>%
    tab_options(column_labels.font.weight = "bold") %>%
    fmt_number(
      columns = c(3, 6:9), 
      decimals = 2
    ) %>%
    sub_missing(1:11, missing_text = "--") %>%
    fmt(
      c(T11_VLMR_PValue, BLRT_PValue),
      fns = function(x)
        ifelse(x < 0.001, "<.001", scales::number(x, accuracy = .01))
    ) %>%
    fmt_percent(
      columns = c(Perc_Convergence, Replicated_LL_Perc),
      decimals = 0,
      scale_values = FALSE
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = list(
        cells_body(columns = BIC, row = BIC == min(BIC, na.rm = TRUE)),
        cells_body(columns = aBIC, row = aBIC == min(aBIC, na.rm = TRUE)),
        cells_body(columns = CAIC, row = CAIC == min(CAIC, na.rm = TRUE)),
        cells_body(columns = AWE, row = AWE == min(AWE, na.rm = TRUE)),
        cells_body(columns = T11_VLMR_PValue, 
                   row = ifelse(T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue) > .05, T11_VLMR_PValue < .05, NA)),
        cells_body(columns = BLRT_PValue, 
                   row = ifelse(BLRT_PValue < .05 & lead(BLRT_PValue) > .05, BLRT_PValue < .05, NA))
      )
    )
  
  return(fit_table)
}
