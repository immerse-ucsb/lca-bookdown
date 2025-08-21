#| label: "create-flextable"
#| echo: true
#| message: false
#| warning: false

create_flextable <- function(data_table, sample_size) {
  
  # Keep structure consistent with your other tables
  data_table <- data_table %>%
    select(
      Class_Model, Best_LogLikelihood, npar, 
      Random_Start1, Random_Start2, 
      Converged, Perc_Convergence, 
      Replicated_LL, Replicated_LL_Perc, 
      Smallest_Class, Smallest_Class_Perc
    ) %>%
    as.data.frame()
  
  # Create the flextable
  summary_table <- flextable(data_table) %>%
    set_header_labels(
      Class_Model           = "Model",
      Best_LogLikelihood    = "Best LL",
      npar                  = "npar",
      Random_Start1         = "Initial",
      Random_Start2         = "Final",
      Converged             = "𝒇",
      Perc_Convergence      = "%",
      Replicated_LL         = "𝒇",
      Replicated_LL_Perc    = "%",
      Smallest_Class        = "𝒇",
      Smallest_Class_Perc   = "%"
    ) %>%
    # Apply column widths and basic formatting
    width(j = "Class_Model",           width = 0.7) %>%
    width(j = "Best_LogLikelihood",    width = 0.8) %>%
    width(j = "npar",                  width = 0.4) %>%
    width(j = c("Random_Start1","Random_Start2"), width = 0.5) %>%
    width(j = c("Converged","Perc_Convergence"),  width = 0.5) %>%
    width(j = c("Replicated_LL","Replicated_LL_Perc"), width = 0.5) %>%
    width(j = c("Smallest_Class","Smallest_Class_Perc"), width = 0.5) %>%
    align(align = "center", part = "all") %>%
    valign(valign = "bottom", part = "header") %>%
    font(fontname = "Avenir Next", part = "all")
  
  # 🚀 Add superheader row
  summary_table <- add_header_row(
    summary_table,
    values    = c(glue("N = {sample_size}"), "Random Starts", "Final starts converging", "LL Replication", "Smallest Class"),
    colwidths = c(3, 2, 2, 2, 2)
  ) %>%
    align(j = 1, align = "left", part = "header") %>%
    hline(i = 1, j = 1:11, border = fp_border(color="transparent", width=0), part = "header")
  
  # Format numeric columns and superheader background
  summary_table <- summary_table %>%
    colformat_num(j = c("Perc_Convergence","Replicated_LL_Perc","Smallest_Class_Perc"), suffix="%") %>%
    bg(i = 1, bg = "#f0f0f0", part = "header")
  
  # Alternating body rows
  total_rows  <- nrow(data_table)
  color_rows  <- rep(FALSE, total_rows)
  color_rows[seq(1, total_rows, by = 2)] <- TRUE
  summary_table <- bg(summary_table, i = color_rows, bg = "#f0f0f0", part = "body")
  
  # --- Heatmap for Perc_Convergence ---
  coral_shades <- colorRampPalette(c("#E44D26","#FFECE6"))
  perc_min <- min(data_table$Perc_Convergence, na.rm=TRUE)
  perc_max <- max(data_table$Perc_Convergence, na.rm=TRUE)
  if (perc_min==perc_max) {
    perc_scaled <- rep(0.5, total_rows)
  } else {
    perc_scaled <- (data_table$Perc_Convergence - perc_min)/(perc_max-perc_min)
  }
  perc_colors <- coral_shades(100)[as.numeric(cut(perc_scaled, breaks=100))]
  font_colors <- ifelse(data_table$Perc_Convergence < 50, "white", "black")
  # override true 100% rows
  perc_colors[data_table$Perc_Convergence==100 & color_rows] <- "#f0f0f0"
  perc_colors[data_table$Perc_Convergence==100 & !color_rows] <- "#ffffff"
  font_colors[data_table$Perc_Convergence==100] <- "black"
  summary_table <- summary_table %>%
    bg(j="Perc_Convergence", bg=perc_colors, part="body") %>%
    color(j="Perc_Convergence", color=font_colors, part="body")
  
  # --- Heatmap for Replicated_LL_Perc ---
  coral2 <- colorRampPalette(c("#E44D26","#FFECE6"))
  ll_min <- min(data_table$Replicated_LL_Perc[data_table$Replicated_LL_Perc<100], na.rm=TRUE)
  ll_max <- max(data_table$Replicated_LL_Perc[data_table$Replicated_LL_Perc<100], na.rm=TRUE)
  if (ll_min==ll_max) {
    ll_scaled <- rep(0.5, total_rows)
  } else {
    ll_scaled <- (data_table$Replicated_LL_Perc - ll_min)/(ll_max-ll_min)
  }
  ll_colors <- coral2(100)[as.numeric(cut(ll_scaled, breaks=100))]
  font_colors_ll <- ifelse(data_table$Replicated_LL_Perc < 50, "white", "black")
  ll_colors[data_table$Replicated_LL_Perc==100 & color_rows] <- "#f0f0f0"
  ll_colors[data_table$Replicated_LL_Perc==100 & !color_rows] <- "#ffffff"
  font_colors_ll[data_table$Replicated_LL_Perc==100] <- "black"
  summary_table <- summary_table %>%
    bg(j="Replicated_LL_Perc", bg=ll_colors, part="body") %>%
    color(j="Replicated_LL_Perc", color=font_colors_ll, part="body")
  
  return(summary_table)
}
