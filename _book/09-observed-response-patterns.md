



# (PART) Understanding the Data {-}

# Observed Response Patterns

------------------------------------------------------------------------

## Load Packages


``` r
library(naniar)
library(tidyverse)
library(haven)
library(glue)
library(MplusAutomation)
library(here)
library(janitor)
library(gt)
library(tidyLPA)
library(pisaUSA15)
library(cowplot)
library(filesstrings)
library(patchwork)
library(RcppAlgos)
```

------------------------------------------------------------------------

## Prepare Data


``` r
df_bully <- read_csv(here("data", "crdc_lca_data.csv")) %>% 
  clean_names() %>% 
  dplyr::select(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte) 
```

------------------------------------------------------------------------

Continuing the LCA example (\@ref(lca-enum)) in this bookdown, save response frequencies for the 3-class model with `response is _____.dat` under `SAVEDATA.`


``` r

patterns  <- mplusObject(
  
  TITLE = "C3 LCA - Save response patterns", 
  
  VARIABLE = 
  "categorical = report_dis-law_fte; 
   usevar =  report_dis-law_fte;
   classes = c(3);",
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 0;
    processors = 10;
    optseed = 802779;",
  
  SAVEDATA = 
   "File=savedata.dat;
    Save=cprob;
    
    ! Code to save response frequency data 
    
    response is resp_patterns.dat;",
  
  OUTPUT = "residual patterns tech11 tech14",
  
  usevariables = colnames(df_bully),
  rdata = df_bully)

patterns_fit <- mplusModeler(patterns,
                dataout=here("mplus", "bully.dat"),
                modelout=here("mplus", "patterns.inp") ,
                check=TRUE, run = TRUE, hashfilename = FALSE)
```

------------------------------------------------------------------------

Read in observed response pattern data and relabel the columns


``` r
# Read in response frequency data that we just created:
patterns <- read_table(here("mplus", "resp_patterns.dat"),
                        col_names=FALSE, na = "*") 

# Extract the column names
names <- names(readModels(here("mplus", "patterns.out"))[['savedata']]) 

# Add the names back to the dataset
colnames(patterns) <- c("Frequency", names)  
```

Create a table with the top 5 unconditional response pattern, then top of conditional response pattern for each modal class assignment


``` r
# Order responses by highest frequency
order_highest <- patterns %>% 
  arrange(desc(Frequency)) 

# Loop `patterns` data to list top 5 conditional response patterns for each class
loop_cond  <- lapply(1:max(patterns$C), function(k) {       
order_cond <- patterns %>%                    
  filter(C == k) %>%                    
  arrange(desc(Frequency)) %>%                
  head(5)                                     
  })                                          

# Convert loop into data frame
table_data <- as.data.frame(bind_rows(loop_cond))

# Combine unconditional and conditional responses patterns
response_patterns <-  rbind(order_highest[1:5,], table_data) 
```

Finally, use `{gt}` to make a nicely formatted table


``` r
resp_table <- response_patterns %>% 
  gt() %>%
    tab_header(
    title = "Observed Response Patterns",
    subtitle = html("Response patterns, estimated frequencies, estimated posterior class probabilities and modal assignments")) %>% 
    tab_source_note(
    source_note = md("Data Source: **Civil Rights Data Collection (CRDC)**")) %>%
    cols_label(
      Frequency = html("<i>f</i><sub>r</sub>"),
    REPORT_D = "Harrassment: Disability",
    REPORT_R = "Harrassment: Race",
    REPORT_S = "Harrassment: Sex",
    COUNSELO = "Staff: Counselor",
    PSYCH_FT = "Staff: Psychologist",
    LAW_FTE = "Staff: Law Enforcement",
    CPROB1 = html("P<sub><i>k</i></sub>=1"),
    CPROB2 = html("P<sub><i>k</i></sub>=2"),
    CPROB3 = html("P<sub><i>k</i></sub>=3"),
    C = md("*k*")) %>% 
  tab_row_group(
    label = "Unconditional response patterns",
    rows = 1:5) %>%
  tab_row_group(
    label = md("*k* = 1 Conditional response patterns"),
    rows = 6:10) %>% #EDIT THESE VALUES BASED ON THE LAST COLUMN
  tab_row_group(
    label = md("*k* = 2 Conditional response patterns"),
    rows = 11:15)  %>% #EDIT THESE VALUES BASED ON THE LAST COLUMN
  tab_row_group(
    label = md("*k* = 3 Conditional response patterns"),
    rows = 16:20)  %>% #EDIT THESE VALUES BASED ON THE LAST COLUMN  
    row_group_order(
      groups = c("Unconditional response patterns",
                 md("*k* = 1 Conditional response patterns"),
                 md("*k* = 2 Conditional response patterns"),
                 md("*k* = 3 Conditional response patterns"))) %>% 
    tab_footnote(
    footnote = html(
      "<i>Note.</i> <i>f</i><sub>r</sub> = response pattern frequency; P<sub><i>k</i></sub> = posterior class probabilities"
    )
  ) %>% 
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "Times New Roman")

resp_table
```


```{=html}
<div id="erpcjfmffl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#erpcjfmffl table {
  font-family: 'Times New Roman';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#erpcjfmffl thead, #erpcjfmffl tbody, #erpcjfmffl tfoot, #erpcjfmffl tr, #erpcjfmffl td, #erpcjfmffl th {
  border-style: none;
}

#erpcjfmffl p {
  margin: 0;
  padding: 0;
}

#erpcjfmffl .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#erpcjfmffl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#erpcjfmffl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#erpcjfmffl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#erpcjfmffl .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erpcjfmffl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erpcjfmffl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erpcjfmffl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#erpcjfmffl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#erpcjfmffl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#erpcjfmffl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#erpcjfmffl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#erpcjfmffl .gt_spanner_row {
  border-bottom-style: hidden;
}

#erpcjfmffl .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#erpcjfmffl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#erpcjfmffl .gt_from_md > :first-child {
  margin-top: 0;
}

#erpcjfmffl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#erpcjfmffl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#erpcjfmffl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#erpcjfmffl .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#erpcjfmffl .gt_row_group_first td {
  border-top-width: 2px;
}

#erpcjfmffl .gt_row_group_first th {
  border-top-width: 2px;
}

#erpcjfmffl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erpcjfmffl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#erpcjfmffl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#erpcjfmffl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erpcjfmffl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erpcjfmffl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#erpcjfmffl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#erpcjfmffl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#erpcjfmffl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erpcjfmffl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erpcjfmffl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#erpcjfmffl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erpcjfmffl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#erpcjfmffl .gt_left {
  text-align: left;
}

#erpcjfmffl .gt_center {
  text-align: center;
}

#erpcjfmffl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#erpcjfmffl .gt_font_normal {
  font-weight: normal;
}

#erpcjfmffl .gt_font_bold {
  font-weight: bold;
}

#erpcjfmffl .gt_font_italic {
  font-style: italic;
}

#erpcjfmffl .gt_super {
  font-size: 65%;
}

#erpcjfmffl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#erpcjfmffl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#erpcjfmffl .gt_indent_1 {
  text-indent: 5px;
}

#erpcjfmffl .gt_indent_2 {
  text-indent: 10px;
}

#erpcjfmffl .gt_indent_3 {
  text-indent: 15px;
}

#erpcjfmffl .gt_indent_4 {
  text-indent: 20px;
}

#erpcjfmffl .gt_indent_5 {
  text-indent: 25px;
}

#erpcjfmffl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#erpcjfmffl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal" style>Observed Response Patterns</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Response patterns, estimated frequencies, estimated posterior class probabilities and modal assignments</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Frequency"><i>f</i><sub>r</sub></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="REPORT_D">Harrassment: Disability</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="REPORT_R">Harrassment: Race</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="REPORT_S">Harrassment: Sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="COUNSELO">Staff: Counselor</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="PSYCH_FT">Staff: Psychologist</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LAW_FTE">Staff: Law Enforcement</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CPROB1">P<sub><i>k</i></sub>=1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CPROB2">P<sub><i>k</i></sub>=2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CPROB3">P<sub><i>k</i></sub>=3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="C"><span class='gt_from_md'><em>k</em></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="Unconditional response patterns">Unconditional response patterns</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Unconditional response patterns  Frequency" class="gt_row gt_center">525</td>
<td headers="Unconditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  CPROB1" class="gt_row gt_center">0.023</td>
<td headers="Unconditional response patterns  CPROB2" class="gt_row gt_center">0.002</td>
<td headers="Unconditional response patterns  CPROB3" class="gt_row gt_center">0.976</td>
<td headers="Unconditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="Unconditional response patterns  Frequency" class="gt_row gt_center">299</td>
<td headers="Unconditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  CPROB1" class="gt_row gt_center">0.139</td>
<td headers="Unconditional response patterns  CPROB2" class="gt_row gt_center">0.007</td>
<td headers="Unconditional response patterns  CPROB3" class="gt_row gt_center">0.854</td>
<td headers="Unconditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="Unconditional response patterns  Frequency" class="gt_row gt_center">293</td>
<td headers="Unconditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  CPROB1" class="gt_row gt_center">0.146</td>
<td headers="Unconditional response patterns  CPROB2" class="gt_row gt_center">0.004</td>
<td headers="Unconditional response patterns  CPROB3" class="gt_row gt_center">0.850</td>
<td headers="Unconditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="Unconditional response patterns  Frequency" class="gt_row gt_center">251</td>
<td headers="Unconditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  CPROB1" class="gt_row gt_center">0.541</td>
<td headers="Unconditional response patterns  CPROB2" class="gt_row gt_center">0.009</td>
<td headers="Unconditional response patterns  CPROB3" class="gt_row gt_center">0.449</td>
<td headers="Unconditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr><td headers="Unconditional response patterns  Frequency" class="gt_row gt_center">75</td>
<td headers="Unconditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="Unconditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  LAW_FTE" class="gt_row gt_center">1</td>
<td headers="Unconditional response patterns  CPROB1" class="gt_row gt_center">0.959</td>
<td headers="Unconditional response patterns  CPROB2" class="gt_row gt_center">0.011</td>
<td headers="Unconditional response patterns  CPROB3" class="gt_row gt_center">0.030</td>
<td headers="Unconditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="&lt;span class='gt_from_md'&gt;&lt;em&gt;k&lt;/em&gt; = 1 Conditional response patterns&lt;/span&gt;"><span class='gt_from_md'><em>k</em> = 1 Conditional response patterns</span></th>
    </tr>
    <tr class="gt_row_group_first"><td headers="*k* = 1 Conditional response patterns  Frequency" class="gt_row gt_center">251</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  CPROB1" class="gt_row gt_center">0.541</td>
<td headers="*k* = 1 Conditional response patterns  CPROB2" class="gt_row gt_center">0.009</td>
<td headers="*k* = 1 Conditional response patterns  CPROB3" class="gt_row gt_center">0.449</td>
<td headers="*k* = 1 Conditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr><td headers="*k* = 1 Conditional response patterns  Frequency" class="gt_row gt_center">75</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  LAW_FTE" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  CPROB1" class="gt_row gt_center">0.959</td>
<td headers="*k* = 1 Conditional response patterns  CPROB2" class="gt_row gt_center">0.011</td>
<td headers="*k* = 1 Conditional response patterns  CPROB3" class="gt_row gt_center">0.030</td>
<td headers="*k* = 1 Conditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr><td headers="*k* = 1 Conditional response patterns  Frequency" class="gt_row gt_center">72</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  CPROB1" class="gt_row gt_center">0.803</td>
<td headers="*k* = 1 Conditional response patterns  CPROB2" class="gt_row gt_center">0.088</td>
<td headers="*k* = 1 Conditional response patterns  CPROB3" class="gt_row gt_center">0.108</td>
<td headers="*k* = 1 Conditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr><td headers="*k* = 1 Conditional response patterns  Frequency" class="gt_row gt_center">38</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  CPROB1" class="gt_row gt_center">0.431</td>
<td headers="*k* = 1 Conditional response patterns  CPROB2" class="gt_row gt_center">0.139</td>
<td headers="*k* = 1 Conditional response patterns  CPROB3" class="gt_row gt_center">0.430</td>
<td headers="*k* = 1 Conditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr><td headers="*k* = 1 Conditional response patterns  Frequency" class="gt_row gt_center">34</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 1 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  LAW_FTE" class="gt_row gt_center">1</td>
<td headers="*k* = 1 Conditional response patterns  CPROB1" class="gt_row gt_center">0.789</td>
<td headers="*k* = 1 Conditional response patterns  CPROB2" class="gt_row gt_center">0.027</td>
<td headers="*k* = 1 Conditional response patterns  CPROB3" class="gt_row gt_center">0.184</td>
<td headers="*k* = 1 Conditional response patterns  C" class="gt_row gt_center">1</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="&lt;span class='gt_from_md'&gt;&lt;em&gt;k&lt;/em&gt; = 2 Conditional response patterns&lt;/span&gt;"><span class='gt_from_md'><em>k</em> = 2 Conditional response patterns</span></th>
    </tr>
    <tr class="gt_row_group_first"><td headers="*k* = 2 Conditional response patterns  Frequency" class="gt_row gt_center">24</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_R" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  CPROB1" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB2" class="gt_row gt_center">0.561</td>
<td headers="*k* = 2 Conditional response patterns  CPROB3" class="gt_row gt_center">0.439</td>
<td headers="*k* = 2 Conditional response patterns  C" class="gt_row gt_center">2</td></tr>
    <tr><td headers="*k* = 2 Conditional response patterns  Frequency" class="gt_row gt_center">20</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_R" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  CPROB1" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB2" class="gt_row gt_center">0.981</td>
<td headers="*k* = 2 Conditional response patterns  CPROB3" class="gt_row gt_center">0.019</td>
<td headers="*k* = 2 Conditional response patterns  C" class="gt_row gt_center">2</td></tr>
    <tr><td headers="*k* = 2 Conditional response patterns  Frequency" class="gt_row gt_center">19</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_R" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  CPROB1" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB2" class="gt_row gt_center">0.992</td>
<td headers="*k* = 2 Conditional response patterns  CPROB3" class="gt_row gt_center">0.008</td>
<td headers="*k* = 2 Conditional response patterns  C" class="gt_row gt_center">2</td></tr>
    <tr><td headers="*k* = 2 Conditional response patterns  Frequency" class="gt_row gt_center">18</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_R" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  CPROB1" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB2" class="gt_row gt_center">0.967</td>
<td headers="*k* = 2 Conditional response patterns  CPROB3" class="gt_row gt_center">0.033</td>
<td headers="*k* = 2 Conditional response patterns  C" class="gt_row gt_center">2</td></tr>
    <tr><td headers="*k* = 2 Conditional response patterns  Frequency" class="gt_row gt_center">12</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_R" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  LAW_FTE" class="gt_row gt_center">1</td>
<td headers="*k* = 2 Conditional response patterns  CPROB1" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB2" class="gt_row gt_center">1.000</td>
<td headers="*k* = 2 Conditional response patterns  CPROB3" class="gt_row gt_center">0.000</td>
<td headers="*k* = 2 Conditional response patterns  C" class="gt_row gt_center">2</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="&lt;span class='gt_from_md'&gt;&lt;em&gt;k&lt;/em&gt; = 3 Conditional response patterns&lt;/span&gt;"><span class='gt_from_md'><em>k</em> = 3 Conditional response patterns</span></th>
    </tr>
    <tr class="gt_row_group_first"><td headers="*k* = 3 Conditional response patterns  Frequency" class="gt_row gt_center">525</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  CPROB1" class="gt_row gt_center">0.023</td>
<td headers="*k* = 3 Conditional response patterns  CPROB2" class="gt_row gt_center">0.002</td>
<td headers="*k* = 3 Conditional response patterns  CPROB3" class="gt_row gt_center">0.976</td>
<td headers="*k* = 3 Conditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="*k* = 3 Conditional response patterns  Frequency" class="gt_row gt_center">299</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">1</td>
<td headers="*k* = 3 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  CPROB1" class="gt_row gt_center">0.139</td>
<td headers="*k* = 3 Conditional response patterns  CPROB2" class="gt_row gt_center">0.007</td>
<td headers="*k* = 3 Conditional response patterns  CPROB3" class="gt_row gt_center">0.854</td>
<td headers="*k* = 3 Conditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="*k* = 3 Conditional response patterns  Frequency" class="gt_row gt_center">293</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  COUNSELO" class="gt_row gt_center">1</td>
<td headers="*k* = 3 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  CPROB1" class="gt_row gt_center">0.146</td>
<td headers="*k* = 3 Conditional response patterns  CPROB2" class="gt_row gt_center">0.004</td>
<td headers="*k* = 3 Conditional response patterns  CPROB3" class="gt_row gt_center">0.850</td>
<td headers="*k* = 3 Conditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="*k* = 3 Conditional response patterns  Frequency" class="gt_row gt_center">36</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_S" class="gt_row gt_center">1</td>
<td headers="*k* = 3 Conditional response patterns  COUNSELO" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  LAW_FTE" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  CPROB1" class="gt_row gt_center">0.117</td>
<td headers="*k* = 3 Conditional response patterns  CPROB2" class="gt_row gt_center">0.060</td>
<td headers="*k* = 3 Conditional response patterns  CPROB3" class="gt_row gt_center">0.823</td>
<td headers="*k* = 3 Conditional response patterns  C" class="gt_row gt_center">3</td></tr>
    <tr><td headers="*k* = 3 Conditional response patterns  Frequency" class="gt_row gt_center">27</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_D" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_R" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  REPORT_S" class="gt_row gt_center">0</td>
<td headers="*k* = 3 Conditional response patterns  COUNSELO" class="gt_row gt_center">NA</td>
<td headers="*k* = 3 Conditional response patterns  PSYCH_FT" class="gt_row gt_center">NA</td>
<td headers="*k* = 3 Conditional response patterns  LAW_FTE" class="gt_row gt_center">NA</td>
<td headers="*k* = 3 Conditional response patterns  CPROB1" class="gt_row gt_center">0.236</td>
<td headers="*k* = 3 Conditional response patterns  CPROB2" class="gt_row gt_center">0.006</td>
<td headers="*k* = 3 Conditional response patterns  CPROB3" class="gt_row gt_center">0.758</td>
<td headers="*k* = 3 Conditional response patterns  C" class="gt_row gt_center">3</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="11"><span class='gt_from_md'>Data Source: <strong>Civil Rights Data Collection (CRDC)</strong></span></td>
    </tr>
  </tfoot>
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="11"> <i>Note.</i> <i>f</i><sub>r</sub> = response pattern frequency; P<sub><i>k</i></sub> = posterior class probabilities</td>
    </tr>
  </tfoot>
</table>
</div>
```


------------------------------------------------------------------------

Save table:


``` r
gtsave(resp_table, here("figures","resp_table.png"))
```

<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
