library(dplyr)
library(tidyr)
library(flextable)
library(stringr)

# ----- STEP 1: Extract, count, and compute replication percentage -----
ll_data <- final_data %>%
  select(File_Name, Class_Model, LogLikelihoods) %>%
  unnest(LogLikelihoods) %>%
  count(File_Name, Class_Model, LogLikelihoods, name = "ReplicationCount") %>%
  arrange(File_Name, desc(LogLikelihoods)) %>%
  group_by(Class_Model) %>%
  mutate(ReplicationPerc = round((ReplicationCount / sum(ReplicationCount)) * 100, 1)) %>%
  ungroup()

# ----- STEP 2: Pivot wider (LogLikelihoods, ReplicationCount, ReplicationPerc) -----
ll_replication_df <- ll_data %>%
  select(-File_Name) %>%
  group_by(Class_Model) %>%
  mutate(id = row_number()) %>%
  pivot_wider(
    names_from = Class_Model,
    values_from = c(LogLikelihoods, ReplicationCount, ReplicationPerc),
    names_glue = "{.value}_{Class_Model}"
  ) %>%
  ungroup() %>%
  select(-id)

# ----- STEP 3: Forcibly reorder columns by class number (LL → N → Perc) -----

# 1) Get original pivoted names
orig_cols <- colnames(ll_replication_df)

# 2) Parse out the class number from each column name
#    e.g. "LogLikelihoods_1-Class" => "1"
#         "ReplicationCount_2-Class" => "2"
#         "ReplicationPerc_3-Class" => "3"
class_num <- as.numeric(str_extract(orig_cols, "(?<=_)(\\d+)(?=-Class)"))

# 3) Parse out the column type (LL, N, or Perc)
#    e.g. "LogLikelihoods_" => "LL", "ReplicationCount_" => "N", "ReplicationPerc_" => "Perc"
col_type <- case_when(
  grepl("^LogLikelihoods_", orig_cols) ~ "LL",
  grepl("^ReplicationCount_", orig_cols) ~ "N",
  grepl("^ReplicationPerc_", orig_cols) ~ "Perc"
)

# 4) Combine into a data frame for sorting
col_info <- data.frame(
  orig_name = orig_cols,
  class_num = class_num,
  col_type = col_type,
  stringsAsFactors = FALSE
)

# 5) Define a custom order for col_type: LL < N < Perc
type_order <- c("LL" = 1, "N" = 2, "Perc" = 3)

# 6) Sort columns by (class_num, type_order)
col_info <- col_info %>%
  arrange(class_num, type_order[col_type])

# 7) Now rename them in final order => LL_cX, n_cX, perc_cX
#    We'll do this in a single pass:
new_names <- mapply(function(cnum, ctype) {
  if (ctype == "LL") {
    paste0("LL_c", cnum)
  } else if (ctype == "N") {
    paste0("n_c", cnum)
  } else {
    paste0("perc_c", cnum)
  }
}, col_info$class_num, col_info$col_type)

# 8) Apply this new order & rename to ll_replication_df
ll_replication_df <- ll_replication_df[col_info$orig_name]  # reorder columns
colnames(ll_replication_df) <- new_names                    # rename columns

# ----- STEP 4: Replace NAs with "—" -----
ll_replication_df <- ll_replication_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "—", .)))

# ----- STEP 5: Create the flextable -----
all_cols <- names(ll_replication_df)
num_cols <- length(all_cols)
num_classes <- num_cols / 3  # 3 columns per class (LL, n, perc)

# Build header labels => "LL", "N", or "%"
header_labels <- setNames(
  ifelse(grepl("^LL_", all_cols), "LL",
         ifelse(grepl("^n_", all_cols), "N", "%")),
  all_cols
)

ll_replication_table_all <- flextable(ll_replication_df) %>%
  set_header_labels(.list = header_labels) %>%
  align(align = "center", part = "all") %>%
  font(fontname = "Avenir Next", part = "all") %>%
  fontsize(size = 10, part = "body") %>%
  fontsize(size = 11, part = "header") %>%
  bold(part = "header", bold = FALSE)

# Adjust widths for each column type
ll_cols <- grep("^LL_", all_cols, value = TRUE)
n_cols  <- grep("^n_", all_cols, value = TRUE)
p_cols  <- grep("^perc_", all_cols, value = TRUE)

ll_replication_table_all <- ll_replication_table_all %>%
  width(j = ll_cols, width = 0.7) %>%
  width(j = n_cols, width = 0.3) %>%
  width(j = p_cols, width = 0.3) %>%
  set_table_properties(layout = "fixed", width = 1) %>%
  padding(part = "header", padding = 3) %>%
  padding(part = "body", padding = 3)

# Italicize the LL, N, and % in the header
for (col in ll_cols) {
  ll_replication_table_all <- compose(
    ll_replication_table_all, 
    part = "header", j = col, 
    value = as_paragraph(as_i("LL"))
  )
}
for (col in n_cols) {
  ll_replication_table_all <- compose(
    ll_replication_table_all, 
    part = "header", j = col, 
    value = as_paragraph(as_i("N"))
  )
}
for (col in p_cols) {
  ll_replication_table_all <- compose(
    ll_replication_table_all, 
    part = "header", j = col, 
    value = as_paragraph(as_i("%"))
  )
}

# Superheader => each class has 3 columns
super_headers <- paste0(seq_len(num_classes), "-Class")
ll_replication_table_all <- add_header_row(
  ll_replication_table_all,
  values = super_headers,
  colwidths = rep(3, num_classes)
)

# Remove line under the superheader
ll_replication_table_all <- hline(
  ll_replication_table_all, i = 1, 
  border = fp_border(color = "transparent"), 
  part = "header"
)

ll_replication_table_all <- ll_replication_table_all %>%
  set_table_properties(layout = "fixed") %>%
  flextable::keep_with_next()

# Return the final flextable
return(ll_replication_table_all)
