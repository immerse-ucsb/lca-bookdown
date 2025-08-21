plot_spaghetti <- function(file_path, data_path) {
  model <- readModels(file_path, quiet = TRUE)
  output <- readLines(file_path)
  save_idx <- grep("SAVEDATA INFORMATION", output, ignore.case = TRUE)
  order_idx <- grep("Order and format of variables", output[save_idx:length(output)], ignore.case = TRUE) + save_idx - 1
  vars <- output[(order_idx + 1):length(output)]
  col_names <- str_trim(str_extract(vars, "^\\s*\\S+"))
  col_names <- col_names[!is.na(col_names) & nchar(col_names) > 0]
  
  # Read data, handle missing values
  data <- read.table(data_path, header = FALSE, na.strings = c("999", "NA", "", "*"))  # Add "*" for Mplus flags
  col_names <- col_names[1:ncol(data)]
  
  colnames(data) <- col_names
  series_vars <- col_names[grep("^E_NCE\\d+$", col_names)]
  if (length(series_vars) == 0) stop("No E_NCE variables found: ", paste(col_names, collapse = ", "))
  class_col <- "C"
  
  # Convert to numeric, filter invalid entries
  data <- data %>%
    mutate(across(all_of(series_vars), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
    filter(if_all(all_of(series_vars), ~ !is.na(.) & is.finite(.)))
  
  if (nrow(data) == 0) stop("No valid data after filtering NAs.")
  
  data_long <- data %>%
    mutate(ID = row_number()) %>%
    pivot_longer(cols = all_of(series_vars), names_to = "Grade", values_to = "NCE") %>%
    mutate(Grade = as.numeric(str_extract(Grade, "\\d+")),
           NCE = as.numeric(NCE),
           Class = paste("Class", .data[[class_col]])) %>%
    filter(!is.na(NCE) & is.finite(NCE))
  
  class_props <- model$class_counts$mostLikely$proportion
  alpha_vals <- scales::rescale(1 - class_props, to = c(0.1, 0.5))
  
  ggplot(data_long, aes(x = Grade, y = NCE, group = interaction(Class, ID), color = Class)) +
    geom_line(aes(alpha = Class, size = Class)) +
    stat_summary(aes(group = 1), fun = "mean", geom = "line", color = "black", size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Class 1" = "#E41A1C",  # Red
                                  "Class 2" = "#377EB8",  # Blue
                                  "Class 3" = "#FFFF33",  # Yellow
                                  "Class 4" = "#984EA3"),  # Purple
                       guide = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    scale_alpha_manual(values = setNames(alpha_vals, paste("Class", 1:4))) +
    scale_size_manual(values = c("Class 1" = 0.8, "Class 2" = 0.3, "Class 3" = 0.8, "Class 4" = 0.3)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    labs(title = "Spaghetti Plot of GMM Trajectories", y = "NCE Score") +
    scale_x_continuous(breaks = 2:5, labels = c("2", "3", "4", "5")) +
    theme_cowplot() +
    theme(text = element_text(family = "serif", size = 12), 
          legend.position = "top", 
          legend.title = element_blank())
}