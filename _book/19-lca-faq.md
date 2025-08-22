

# Ten Frequently Asked Questions (Nylund-Gibson 7 Choi, 2018)

------------------------------------------------------------------------

## Example: Positive Youth Development Inventory Analysis

------------------------------------------------------------------------

- The original paper illustrated the modeling ideas described in this article using seven items (see Table 2) from the Positive Youth Development Inventory (PYDI) Contribution subscale (Arnold, Nott, & Meinhold, 2012) that were administered to 1629 college and university students.
- The PYDI measures behavioral, psychological, and social characteristics that are theorized to indicate positive youth development, and the Contribution Subscale specifically measures the degree to which youth express values and behaviors associated with channeling their positive psychosocial strengths to contribute meaningfully to their local community (Lerner et al., 2005).
- Citation: [Nylund-Gibson, K., & Choi, A. Y. (2018). Ten frequently asked questions about latent class analysis. *Translational Issues in Psychological Science*, *4*(4), 440–461.](https://doi.org/10.1037/tps0000176)

------------------------------------------------------------------------

## Load packages


``` r
library(tidyverse)
library(haven)
library(glue)
library(MplusAutomation)
library(here)
library(janitor)
library(gt)
library(cowplot)
library(DiagrammeR) 
library(webshot2)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(flextable)
library(officer)
library(glue)
library(htmltools)
```

------------------------------------------------------------------------

## Variable Description



------------------------------------------------------------------------

- The original research question was to examine whether this construct of positive contribution comprised qualitatively distinct subtypes among college and university students, and to further examine whether such subtypes (if indeed were present) were meaningfully associated with a demographic predictor and a wellbeing-related outcome.



------------------------------------------------------------------------

## Prepare Data


``` r
df_qa <- read_csv(here("10faq", "data", "lca10faq.csv"))
```

------------------------------------------------------------------------

## Descriptive Statistics


``` r
# Set up data to find proportions of binary indicators
ds <- df_qa %>% 
  pivot_longer(c(PYDI1Ab, PYDI2Ab, PYDI3Ab, PYDI4Ab, PYDI5Ab, PYDI6Ab, PYDI7Ab),
               names_to = "variable") 

# Create table of variables and counts, then find proportions and round to 3 decimal places
prop_df <- ds %>%
  count(variable, value) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(prop = round(prop, 3))


# Make it a gt() table
prop_table <- prop_df %>% 
  gt(groupname_col = "variable", rowname_col = "value") %>%
  tab_stubhead(label = md("*Values*")) %>%
  tab_header(
    md(
      "Variable Proportions"
    )
  ) %>%
  cols_label(
    variable = md("*Variable*"),
    value = md("*Value*"),
    n = md("*N*"),
    prop = md("*Proportion*")
  ) 
  
prop_table
```


```{=html}
<div id="ilulkikets" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ilulkikets table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ilulkikets thead, #ilulkikets tbody, #ilulkikets tfoot, #ilulkikets tr, #ilulkikets td, #ilulkikets th {
  border-style: none;
}

#ilulkikets p {
  margin: 0;
  padding: 0;
}

#ilulkikets .gt_table {
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

#ilulkikets .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ilulkikets .gt_title {
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

#ilulkikets .gt_subtitle {
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

#ilulkikets .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ilulkikets .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilulkikets .gt_col_headings {
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

#ilulkikets .gt_col_heading {
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

#ilulkikets .gt_column_spanner_outer {
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

#ilulkikets .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ilulkikets .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ilulkikets .gt_column_spanner {
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

#ilulkikets .gt_spanner_row {
  border-bottom-style: hidden;
}

#ilulkikets .gt_group_heading {
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

#ilulkikets .gt_empty_group_heading {
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

#ilulkikets .gt_from_md > :first-child {
  margin-top: 0;
}

#ilulkikets .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ilulkikets .gt_row {
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

#ilulkikets .gt_stub {
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

#ilulkikets .gt_stub_row_group {
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

#ilulkikets .gt_row_group_first td {
  border-top-width: 2px;
}

#ilulkikets .gt_row_group_first th {
  border-top-width: 2px;
}

#ilulkikets .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilulkikets .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ilulkikets .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ilulkikets .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilulkikets .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilulkikets .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ilulkikets .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ilulkikets .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ilulkikets .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ilulkikets .gt_footnotes {
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

#ilulkikets .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilulkikets .gt_sourcenotes {
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

#ilulkikets .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ilulkikets .gt_left {
  text-align: left;
}

#ilulkikets .gt_center {
  text-align: center;
}

#ilulkikets .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ilulkikets .gt_font_normal {
  font-weight: normal;
}

#ilulkikets .gt_font_bold {
  font-weight: bold;
}

#ilulkikets .gt_font_italic {
  font-style: italic;
}

#ilulkikets .gt_super {
  font-size: 65%;
}

#ilulkikets .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ilulkikets .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ilulkikets .gt_indent_1 {
  text-indent: 5px;
}

#ilulkikets .gt_indent_2 {
  text-indent: 10px;
}

#ilulkikets .gt_indent_3 {
  text-indent: 15px;
}

#ilulkikets .gt_indent_4 {
  text-indent: 20px;
}

#ilulkikets .gt_indent_5 {
  text-indent: 25px;
}

#ilulkikets .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ilulkikets div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'>Variable Proportions</span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"><span class='gt_from_md'><em>Values</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><em>N</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="prop"><span class='gt_from_md'><em>Proportion</em></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI1Ab">PYDI1Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI1Ab stub_1_1 n" class="gt_row gt_right">581</td>
<td headers="PYDI1Ab stub_1_1 prop" class="gt_row gt_right">0.355</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI1Ab stub_1_2 n" class="gt_row gt_right">1048</td>
<td headers="PYDI1Ab stub_1_2 prop" class="gt_row gt_right">0.640</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI1Ab stub_1_3 n" class="gt_row gt_right">9</td>
<td headers="PYDI1Ab stub_1_3 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI2Ab">PYDI2Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_4" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI2Ab stub_1_4 n" class="gt_row gt_right">116</td>
<td headers="PYDI2Ab stub_1_4 prop" class="gt_row gt_right">0.071</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI2Ab stub_1_5 n" class="gt_row gt_right">1513</td>
<td headers="PYDI2Ab stub_1_5 prop" class="gt_row gt_right">0.924</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI2Ab stub_1_6 n" class="gt_row gt_right">9</td>
<td headers="PYDI2Ab stub_1_6 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI3Ab">PYDI3Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_7" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI3Ab stub_1_7 n" class="gt_row gt_right">212</td>
<td headers="PYDI3Ab stub_1_7 prop" class="gt_row gt_right">0.129</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI3Ab stub_1_8 n" class="gt_row gt_right">1417</td>
<td headers="PYDI3Ab stub_1_8 prop" class="gt_row gt_right">0.865</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI3Ab stub_1_9 n" class="gt_row gt_right">9</td>
<td headers="PYDI3Ab stub_1_9 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI4Ab">PYDI4Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_10" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI4Ab stub_1_10 n" class="gt_row gt_right">109</td>
<td headers="PYDI4Ab stub_1_10 prop" class="gt_row gt_right">0.067</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI4Ab stub_1_11 n" class="gt_row gt_right">1520</td>
<td headers="PYDI4Ab stub_1_11 prop" class="gt_row gt_right">0.928</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI4Ab stub_1_12 n" class="gt_row gt_right">9</td>
<td headers="PYDI4Ab stub_1_12 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI5Ab">PYDI5Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_13" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI5Ab stub_1_13 n" class="gt_row gt_right">219</td>
<td headers="PYDI5Ab stub_1_13 prop" class="gt_row gt_right">0.134</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI5Ab stub_1_14 n" class="gt_row gt_right">1410</td>
<td headers="PYDI5Ab stub_1_14 prop" class="gt_row gt_right">0.861</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI5Ab stub_1_15 n" class="gt_row gt_right">9</td>
<td headers="PYDI5Ab stub_1_15 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI6Ab">PYDI6Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_16" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI6Ab stub_1_16 n" class="gt_row gt_right">82</td>
<td headers="PYDI6Ab stub_1_16 prop" class="gt_row gt_right">0.050</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI6Ab stub_1_17 n" class="gt_row gt_right">1547</td>
<td headers="PYDI6Ab stub_1_17 prop" class="gt_row gt_right">0.944</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI6Ab stub_1_18 n" class="gt_row gt_right">9</td>
<td headers="PYDI6Ab stub_1_18 prop" class="gt_row gt_right">0.005</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="PYDI7Ab">PYDI7Ab</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_19" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="PYDI7Ab stub_1_19 n" class="gt_row gt_right">120</td>
<td headers="PYDI7Ab stub_1_19 prop" class="gt_row gt_right">0.073</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="PYDI7Ab stub_1_20 n" class="gt_row gt_right">1509</td>
<td headers="PYDI7Ab stub_1_20 prop" class="gt_row gt_right">0.921</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_right gt_stub">9999</th>
<td headers="PYDI7Ab stub_1_21 n" class="gt_row gt_right">9</td>
<td headers="PYDI7Ab stub_1_21 prop" class="gt_row gt_right">0.005</td></tr>
  </tbody>
  
  
</table>
</div>
```


------------------------------------------------------------------------

## Enumeration

This code uses the `mplusObject` function in the `MplusAutomation` package and saves all model runs in the `enum` folder.


``` r
lca_6  <- lapply(1:6, function(k) {
  lca_enum  <- mplusObject(
      
    TITLE = glue("{k}-Class"), 
  
    VARIABLE = glue(
    "categorical = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab; 
     MISSING ARE ALL (9999);
     usevar = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab;
     classes = c({k}); "),
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 500 100; 
    processors = 10;",
  
  OUTPUT = "sampstat residual tech1 tech11 tech14;",
  
  PLOT = 
    "type = plot3; 
    series = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab(*);",
  
  usevariables = colnames(df_qa),
  rdata = df_qa)

lca_enum_fit <- mplusModeler(lca_enum, 
                     dataout=glue(here("10faq", "enum", "lca10faq.dat")),
                     modelout=glue(here("10faq", "enum", "c{k}_lca10faq.inp")),
                     check=TRUE, run = TRUE, hashfilename = FALSE)
})
```

------------------------------------------------------------------------

### Examine and Extract Mplus files

Check all models for:

1. Warnings  
2. Errors  
3. Convergence and Loglikelihood Replication Information


``` r
source(here("functions", "extract_mplus_info.R"))

# Define the directory where all of the .out files are located.
output_dir <- here("10faq", "enum")

# Get all .out files
output_files <- list.files(output_dir, pattern = "\\.out$", full.names = TRUE)

# Process all .out files into one dataframe
final_data <- map_dfr(output_files, extract_mplus_info_extended)

# Extract Sample_Size from final_data
sample_size <- unique(final_data$Sample_Size)
```

#### Examine Mplus Warnings


``` r
source(here("functions", "extract_warnings.R"))

warnings_table <- extract_warnings(final_data)
warnings_table
```


```{=html}
<div id="opuysypzhi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#opuysypzhi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#opuysypzhi thead, #opuysypzhi tbody, #opuysypzhi tfoot, #opuysypzhi tr, #opuysypzhi td, #opuysypzhi th {
  border-style: none;
}

#opuysypzhi p {
  margin: 0;
  padding: 0;
}

#opuysypzhi .gt_table {
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
  width: 100%;
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

#opuysypzhi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#opuysypzhi .gt_title {
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

#opuysypzhi .gt_subtitle {
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

#opuysypzhi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#opuysypzhi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opuysypzhi .gt_col_headings {
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

#opuysypzhi .gt_col_heading {
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

#opuysypzhi .gt_column_spanner_outer {
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

#opuysypzhi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#opuysypzhi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#opuysypzhi .gt_column_spanner {
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

#opuysypzhi .gt_spanner_row {
  border-bottom-style: hidden;
}

#opuysypzhi .gt_group_heading {
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

#opuysypzhi .gt_empty_group_heading {
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

#opuysypzhi .gt_from_md > :first-child {
  margin-top: 0;
}

#opuysypzhi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#opuysypzhi .gt_row {
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

#opuysypzhi .gt_stub {
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

#opuysypzhi .gt_stub_row_group {
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

#opuysypzhi .gt_row_group_first td {
  border-top-width: 2px;
}

#opuysypzhi .gt_row_group_first th {
  border-top-width: 2px;
}

#opuysypzhi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opuysypzhi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#opuysypzhi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#opuysypzhi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opuysypzhi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opuysypzhi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#opuysypzhi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#opuysypzhi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#opuysypzhi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opuysypzhi .gt_footnotes {
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

#opuysypzhi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opuysypzhi .gt_sourcenotes {
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

#opuysypzhi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opuysypzhi .gt_left {
  text-align: left;
}

#opuysypzhi .gt_center {
  text-align: center;
}

#opuysypzhi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#opuysypzhi .gt_font_normal {
  font-weight: normal;
}

#opuysypzhi .gt_font_bold {
  font-weight: bold;
}

#opuysypzhi .gt_font_italic {
  font-style: italic;
}

#opuysypzhi .gt_super {
  font-size: 65%;
}

#opuysypzhi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#opuysypzhi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#opuysypzhi .gt_indent_1 {
  text-indent: 5px;
}

#opuysypzhi .gt_indent_2 {
  text-indent: 10px;
}

#opuysypzhi .gt_indent_3 {
  text-indent: 15px;
}

#opuysypzhi .gt_indent_4 {
  text-indent: 20px;
}

#opuysypzhi .gt_indent_5 {
  text-indent: 25px;
}

#opuysypzhi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#opuysypzhi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" style="table-layout:fixed;width:100%;" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:150px;"/>
    <col style="width:150px;"/>
    <col style="width:400px;"/>
  </colgroup>
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Warnings</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warning_Summary"># of Warnings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warnings">Warning Message(s)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c1_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 4 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   TECH11 option is not available for TYPE=MIXTURE with only one class.   Request for TECH11 is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   TECH14 option is not available for TYPE=MIXTURE with only one class.   Request for TECH14 is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c2_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_lca10faq.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  9    </div></td></tr>
  </tbody>
  
  
</table>
</div>
```


#### Examine Mplus Errors


``` r
source(here("functions", "error_visualization.R"))

# Process errors
error_table <- process_error_data(final_data)
error_table
```


```{=html}
<div id="hagqhomvds" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hagqhomvds table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hagqhomvds thead, #hagqhomvds tbody, #hagqhomvds tfoot, #hagqhomvds tr, #hagqhomvds td, #hagqhomvds th {
  border-style: none;
}

#hagqhomvds p {
  margin: 0;
  padding: 0;
}

#hagqhomvds .gt_table {
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
  width: 600px;
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

#hagqhomvds .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hagqhomvds .gt_title {
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

#hagqhomvds .gt_subtitle {
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

#hagqhomvds .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hagqhomvds .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hagqhomvds .gt_col_headings {
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

#hagqhomvds .gt_col_heading {
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

#hagqhomvds .gt_column_spanner_outer {
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

#hagqhomvds .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hagqhomvds .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hagqhomvds .gt_column_spanner {
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

#hagqhomvds .gt_spanner_row {
  border-bottom-style: hidden;
}

#hagqhomvds .gt_group_heading {
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

#hagqhomvds .gt_empty_group_heading {
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

#hagqhomvds .gt_from_md > :first-child {
  margin-top: 0;
}

#hagqhomvds .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hagqhomvds .gt_row {
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

#hagqhomvds .gt_stub {
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

#hagqhomvds .gt_stub_row_group {
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

#hagqhomvds .gt_row_group_first td {
  border-top-width: 2px;
}

#hagqhomvds .gt_row_group_first th {
  border-top-width: 2px;
}

#hagqhomvds .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hagqhomvds .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hagqhomvds .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hagqhomvds .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hagqhomvds .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hagqhomvds .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hagqhomvds .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hagqhomvds .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hagqhomvds .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hagqhomvds .gt_footnotes {
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

#hagqhomvds .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hagqhomvds .gt_sourcenotes {
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

#hagqhomvds .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hagqhomvds .gt_left {
  text-align: left;
}

#hagqhomvds .gt_center {
  text-align: center;
}

#hagqhomvds .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hagqhomvds .gt_font_normal {
  font-weight: normal;
}

#hagqhomvds .gt_font_bold {
  font-weight: bold;
}

#hagqhomvds .gt_font_italic {
  font-style: italic;
}

#hagqhomvds .gt_super {
  font-size: 65%;
}

#hagqhomvds .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hagqhomvds .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hagqhomvds .gt_indent_1 {
  text-indent: 5px;
}

#hagqhomvds .gt_indent_2 {
  text-indent: 10px;
}

#hagqhomvds .gt_indent_3 {
  text-indent: 15px;
}

#hagqhomvds .gt_indent_4 {
  text-indent: 20px;
}

#hagqhomvds .gt_indent_5 {
  text-indent: 25px;
}

#hagqhomvds .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hagqhomvds div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" style="table-layout:fixed;width:600px;" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:150px;"/>
    <col style="width:100px;"/>
    <col style="width:400px;"/>
  </colgroup>
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Estimation Errors</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Class_Model">Model Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Error_Message">Error Message</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c2_lca10faq.out</td>
<td headers="Class_Model" class="gt_row gt_left">2-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.  </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_lca10faq.out</td>
<td headers="Class_Model" class="gt_row gt_left">3-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_lca10faq.out</td>
<td headers="Class_Model" class="gt_row gt_left">4-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_lca10faq.out</td>
<td headers="Class_Model" class="gt_row gt_left">5-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_lca10faq.out</td>
<td headers="Class_Model" class="gt_row gt_left">6-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
  </tbody>
  
  
</table>
</div>
```



#### Examine Convergence and Loglikelihood Replications


``` r
source(here("functions", "summary_table.R"))

# **Print Table with Superheader & Heatmap**
summary_table <- create_flextable(final_data, sample_size)
summary_table
```


```{=html}
<div class="tabwid"><style>.cl-c601c0ba{}.cl-c5fb3d9e{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c5fb3da8{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c5fb3da9{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-c5fdcc12{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c5fdcc13{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c5fdcc1c{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c5fde4fe{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde508{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde509{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde512{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde513{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde514{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde515{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde51c{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde51d{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde51e{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde51f{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde526{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde527{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde528{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde529{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde530{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde531{width:0.5in;background-color:rgba(255, 236, 230, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde532{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde533{width:0.5in;background-color:rgba(234, 117, 86, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde534{width:0.5in;background-color:rgba(233, 109, 76, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde53a{width:0.5in;background-color:rgba(249, 203, 191, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde53b{width:0.5in;background-color:rgba(229, 86, 49, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde544{width:0.5in;background-color:rgba(244, 176, 158, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde545{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde546{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde54e{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde54f{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c5fde550{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-c601c0ba'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-c5fde4fe"><p class="cl-c5fdcc12"><span class="cl-c5fb3d9e">N</span><span class="cl-c5fb3da8"> = </span><span class="cl-c5fb3da8">1629</span></p></th><th  colspan="2"class="cl-c5fde512"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Random Starts</span></p></th><th  colspan="2"class="cl-c5fde512"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Final starting value sets converging</span></p></th><th  colspan="2"class="cl-c5fde512"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">LL Replication</span></p></th><th  colspan="2"class="cl-c5fde512"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-c5fde513"><p class="cl-c5fdcc12"><span class="cl-c5fb3da8">Model</span></p></th><th class="cl-c5fde514"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Best LL</span></p></th><th class="cl-c5fde515"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">npar</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Initial</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">Final</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3d9e">f</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">%</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3d9e">f</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">%</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3d9e">f</span></p></th><th class="cl-c5fde51c"><p class="cl-c5fdcc13"><span class="cl-c5fb3da8">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c5fde51d"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">1-Class</span></p></td><td class="cl-c5fde51e"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,905.892</span></p></td><td class="cl-c5fde51f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">7</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100.0%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">1,629</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c5fde527"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">2-Class</span></p></td><td class="cl-c5fde528"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,439.483</span></p></td><td class="cl-c5fde529"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">15</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde531"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100%</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde532"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100.0%</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">331</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">20.3%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c5fde51d"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">3-Class</span></p></td><td class="cl-c5fde51e"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,394.950</span></p></td><td class="cl-c5fde51f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">23</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">54</span></p></td><td class="cl-c5fde533"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da9">54%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">54</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100.0%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">90</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">5.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c5fde527"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">4-Class</span></p></td><td class="cl-c5fde528"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,379.436</span></p></td><td class="cl-c5fde529"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">31</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">51</span></p></td><td class="cl-c5fde534"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da9">51%</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">41</span></p></td><td class="cl-c5fde53a"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">80.4%</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">88</span></p></td><td class="cl-c5fde530"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">5.4%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c5fde51d"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">5-Class</span></p></td><td class="cl-c5fde51e"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,370.534</span></p></td><td class="cl-c5fde51f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">39</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">42</span></p></td><td class="cl-c5fde53b"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da9">42%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">27</span></p></td><td class="cl-c5fde544"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">64.3%</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">35</span></p></td><td class="cl-c5fde526"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">2.1%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c5fde545"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">6-Class</span></p></td><td class="cl-c5fde546"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">-3,364.745</span></p></td><td class="cl-c5fde54e"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">47</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">500</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">100</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">38</span></p></td><td class="cl-c5fde550"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da9">38%</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">2</span></p></td><td class="cl-c5fde550"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da9">5.3%</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">6</span></p></td><td class="cl-c5fde54f"><p class="cl-c5fdcc1c"><span class="cl-c5fb3da8">0.4%</span></p></td></tr></tbody></table></div>
```


#### Check for Loglikelihood Replication

Visualize and examine loglikelihood replication values for each ouptut file individually


``` r
# Load the function for separate plots
source(here("functions", "ll_replication_plots.R"))

# Generate individual log-likelihood replication tables
ll_replication_tables<- generate_ll_replication_plots(final_data)
ll_replication_tables
```

$c1_lca10faq.out
<div id="pdjnilljym" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#pdjnilljym table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pdjnilljym thead, #pdjnilljym tbody, #pdjnilljym tfoot, #pdjnilljym tr, #pdjnilljym td, #pdjnilljym th {
  border-style: none;
}

#pdjnilljym p {
  margin: 0;
  padding: 0;
}

#pdjnilljym .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#pdjnilljym .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pdjnilljym .gt_title {
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

#pdjnilljym .gt_subtitle {
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

#pdjnilljym .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pdjnilljym .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdjnilljym .gt_col_headings {
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

#pdjnilljym .gt_col_heading {
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

#pdjnilljym .gt_column_spanner_outer {
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

#pdjnilljym .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pdjnilljym .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pdjnilljym .gt_column_spanner {
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

#pdjnilljym .gt_spanner_row {
  border-bottom-style: hidden;
}

#pdjnilljym .gt_group_heading {
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

#pdjnilljym .gt_empty_group_heading {
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

#pdjnilljym .gt_from_md > :first-child {
  margin-top: 0;
}

#pdjnilljym .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pdjnilljym .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#pdjnilljym .gt_stub {
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

#pdjnilljym .gt_stub_row_group {
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

#pdjnilljym .gt_row_group_first td {
  border-top-width: 2px;
}

#pdjnilljym .gt_row_group_first th {
  border-top-width: 2px;
}

#pdjnilljym .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdjnilljym .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pdjnilljym .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pdjnilljym .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdjnilljym .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdjnilljym .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pdjnilljym .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pdjnilljym .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pdjnilljym .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdjnilljym .gt_footnotes {
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

#pdjnilljym .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdjnilljym .gt_sourcenotes {
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

#pdjnilljym .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdjnilljym .gt_left {
  text-align: left;
}

#pdjnilljym .gt_center {
  text-align: center;
}

#pdjnilljym .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pdjnilljym .gt_font_normal {
  font-weight: normal;
}

#pdjnilljym .gt_font_bold {
  font-weight: bold;
}

#pdjnilljym .gt_font_italic {
  font-style: italic;
}

#pdjnilljym .gt_super {
  font-size: 65%;
}

#pdjnilljym .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pdjnilljym .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pdjnilljym .gt_indent_1 {
  text-indent: 5px;
}

#pdjnilljym .gt_indent_2 {
  text-indent: 10px;
}

#pdjnilljym .gt_indent_3 {
  text-indent: 15px;
}

#pdjnilljym .gt_indent_4 {
  text-indent: 20px;
}

#pdjnilljym .gt_indent_5 {
  text-indent: 25px;
}

#pdjnilljym .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#pdjnilljym div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 1-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c1_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,905.892</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c2_lca10faq.out
<div id="tejwetkxzp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#tejwetkxzp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tejwetkxzp thead, #tejwetkxzp tbody, #tejwetkxzp tfoot, #tejwetkxzp tr, #tejwetkxzp td, #tejwetkxzp th {
  border-style: none;
}

#tejwetkxzp p {
  margin: 0;
  padding: 0;
}

#tejwetkxzp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#tejwetkxzp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tejwetkxzp .gt_title {
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

#tejwetkxzp .gt_subtitle {
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

#tejwetkxzp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tejwetkxzp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tejwetkxzp .gt_col_headings {
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

#tejwetkxzp .gt_col_heading {
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

#tejwetkxzp .gt_column_spanner_outer {
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

#tejwetkxzp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tejwetkxzp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tejwetkxzp .gt_column_spanner {
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

#tejwetkxzp .gt_spanner_row {
  border-bottom-style: hidden;
}

#tejwetkxzp .gt_group_heading {
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

#tejwetkxzp .gt_empty_group_heading {
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

#tejwetkxzp .gt_from_md > :first-child {
  margin-top: 0;
}

#tejwetkxzp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tejwetkxzp .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#tejwetkxzp .gt_stub {
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

#tejwetkxzp .gt_stub_row_group {
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

#tejwetkxzp .gt_row_group_first td {
  border-top-width: 2px;
}

#tejwetkxzp .gt_row_group_first th {
  border-top-width: 2px;
}

#tejwetkxzp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tejwetkxzp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tejwetkxzp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tejwetkxzp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tejwetkxzp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tejwetkxzp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tejwetkxzp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tejwetkxzp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tejwetkxzp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tejwetkxzp .gt_footnotes {
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

#tejwetkxzp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tejwetkxzp .gt_sourcenotes {
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

#tejwetkxzp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tejwetkxzp .gt_left {
  text-align: left;
}

#tejwetkxzp .gt_center {
  text-align: center;
}

#tejwetkxzp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tejwetkxzp .gt_font_normal {
  font-weight: normal;
}

#tejwetkxzp .gt_font_bold {
  font-weight: bold;
}

#tejwetkxzp .gt_font_italic {
  font-style: italic;
}

#tejwetkxzp .gt_super {
  font-size: 65%;
}

#tejwetkxzp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tejwetkxzp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tejwetkxzp .gt_indent_1 {
  text-indent: 5px;
}

#tejwetkxzp .gt_indent_2 {
  text-indent: 10px;
}

#tejwetkxzp .gt_indent_3 {
  text-indent: 15px;
}

#tejwetkxzp .gt_indent_4 {
  text-indent: 20px;
}

#tejwetkxzp .gt_indent_5 {
  text-indent: 25px;
}

#tejwetkxzp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#tejwetkxzp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 2-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c2_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,439.483</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c3_lca10faq.out
<div id="vtuiatvrwg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#vtuiatvrwg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vtuiatvrwg thead, #vtuiatvrwg tbody, #vtuiatvrwg tfoot, #vtuiatvrwg tr, #vtuiatvrwg td, #vtuiatvrwg th {
  border-style: none;
}

#vtuiatvrwg p {
  margin: 0;
  padding: 0;
}

#vtuiatvrwg .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#vtuiatvrwg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vtuiatvrwg .gt_title {
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

#vtuiatvrwg .gt_subtitle {
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

#vtuiatvrwg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vtuiatvrwg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vtuiatvrwg .gt_col_headings {
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

#vtuiatvrwg .gt_col_heading {
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

#vtuiatvrwg .gt_column_spanner_outer {
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

#vtuiatvrwg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vtuiatvrwg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vtuiatvrwg .gt_column_spanner {
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

#vtuiatvrwg .gt_spanner_row {
  border-bottom-style: hidden;
}

#vtuiatvrwg .gt_group_heading {
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

#vtuiatvrwg .gt_empty_group_heading {
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

#vtuiatvrwg .gt_from_md > :first-child {
  margin-top: 0;
}

#vtuiatvrwg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vtuiatvrwg .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#vtuiatvrwg .gt_stub {
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

#vtuiatvrwg .gt_stub_row_group {
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

#vtuiatvrwg .gt_row_group_first td {
  border-top-width: 2px;
}

#vtuiatvrwg .gt_row_group_first th {
  border-top-width: 2px;
}

#vtuiatvrwg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vtuiatvrwg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vtuiatvrwg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vtuiatvrwg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vtuiatvrwg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vtuiatvrwg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vtuiatvrwg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vtuiatvrwg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vtuiatvrwg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vtuiatvrwg .gt_footnotes {
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

#vtuiatvrwg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vtuiatvrwg .gt_sourcenotes {
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

#vtuiatvrwg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vtuiatvrwg .gt_left {
  text-align: left;
}

#vtuiatvrwg .gt_center {
  text-align: center;
}

#vtuiatvrwg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vtuiatvrwg .gt_font_normal {
  font-weight: normal;
}

#vtuiatvrwg .gt_font_bold {
  font-weight: bold;
}

#vtuiatvrwg .gt_font_italic {
  font-style: italic;
}

#vtuiatvrwg .gt_super {
  font-size: 65%;
}

#vtuiatvrwg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vtuiatvrwg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vtuiatvrwg .gt_indent_1 {
  text-indent: 5px;
}

#vtuiatvrwg .gt_indent_2 {
  text-indent: 10px;
}

#vtuiatvrwg .gt_indent_3 {
  text-indent: 15px;
}

#vtuiatvrwg .gt_indent_4 {
  text-indent: 20px;
}

#vtuiatvrwg .gt_indent_5 {
  text-indent: 25px;
}

#vtuiatvrwg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#vtuiatvrwg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 3-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c3_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,394.950</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">54.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c4_lca10faq.out
<div id="mfbydvhvps" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#mfbydvhvps table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mfbydvhvps thead, #mfbydvhvps tbody, #mfbydvhvps tfoot, #mfbydvhvps tr, #mfbydvhvps td, #mfbydvhvps th {
  border-style: none;
}

#mfbydvhvps p {
  margin: 0;
  padding: 0;
}

#mfbydvhvps .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#mfbydvhvps .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mfbydvhvps .gt_title {
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

#mfbydvhvps .gt_subtitle {
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

#mfbydvhvps .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mfbydvhvps .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mfbydvhvps .gt_col_headings {
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

#mfbydvhvps .gt_col_heading {
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

#mfbydvhvps .gt_column_spanner_outer {
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

#mfbydvhvps .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mfbydvhvps .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mfbydvhvps .gt_column_spanner {
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

#mfbydvhvps .gt_spanner_row {
  border-bottom-style: hidden;
}

#mfbydvhvps .gt_group_heading {
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

#mfbydvhvps .gt_empty_group_heading {
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

#mfbydvhvps .gt_from_md > :first-child {
  margin-top: 0;
}

#mfbydvhvps .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mfbydvhvps .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#mfbydvhvps .gt_stub {
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

#mfbydvhvps .gt_stub_row_group {
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

#mfbydvhvps .gt_row_group_first td {
  border-top-width: 2px;
}

#mfbydvhvps .gt_row_group_first th {
  border-top-width: 2px;
}

#mfbydvhvps .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mfbydvhvps .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mfbydvhvps .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mfbydvhvps .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mfbydvhvps .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mfbydvhvps .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mfbydvhvps .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mfbydvhvps .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mfbydvhvps .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mfbydvhvps .gt_footnotes {
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

#mfbydvhvps .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mfbydvhvps .gt_sourcenotes {
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

#mfbydvhvps .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mfbydvhvps .gt_left {
  text-align: left;
}

#mfbydvhvps .gt_center {
  text-align: center;
}

#mfbydvhvps .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mfbydvhvps .gt_font_normal {
  font-weight: normal;
}

#mfbydvhvps .gt_font_bold {
  font-weight: bold;
}

#mfbydvhvps .gt_font_italic {
  font-style: italic;
}

#mfbydvhvps .gt_super {
  font-size: 65%;
}

#mfbydvhvps .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mfbydvhvps .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mfbydvhvps .gt_indent_1 {
  text-indent: 5px;
}

#mfbydvhvps .gt_indent_2 {
  text-indent: 10px;
}

#mfbydvhvps .gt_indent_3 {
  text-indent: 15px;
}

#mfbydvhvps .gt_indent_4 {
  text-indent: 20px;
}

#mfbydvhvps .gt_indent_5 {
  text-indent: 25px;
}

#mfbydvhvps .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mfbydvhvps div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 4-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c4_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,379.436</td>
<td headers="Count" class="gt_row gt_center">41.000</td>
<td headers="Percentage" class="gt_row gt_center">80.39%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,381.888</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">1.96%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,384.030</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">9.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">17.65%</td></tr>
  </tbody>
  
  
</table>
</div>

$c5_lca10faq.out
<div id="jalunvfrdr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#jalunvfrdr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jalunvfrdr thead, #jalunvfrdr tbody, #jalunvfrdr tfoot, #jalunvfrdr tr, #jalunvfrdr td, #jalunvfrdr th {
  border-style: none;
}

#jalunvfrdr p {
  margin: 0;
  padding: 0;
}

#jalunvfrdr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#jalunvfrdr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jalunvfrdr .gt_title {
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

#jalunvfrdr .gt_subtitle {
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

#jalunvfrdr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jalunvfrdr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jalunvfrdr .gt_col_headings {
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

#jalunvfrdr .gt_col_heading {
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

#jalunvfrdr .gt_column_spanner_outer {
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

#jalunvfrdr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jalunvfrdr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jalunvfrdr .gt_column_spanner {
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

#jalunvfrdr .gt_spanner_row {
  border-bottom-style: hidden;
}

#jalunvfrdr .gt_group_heading {
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

#jalunvfrdr .gt_empty_group_heading {
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

#jalunvfrdr .gt_from_md > :first-child {
  margin-top: 0;
}

#jalunvfrdr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jalunvfrdr .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#jalunvfrdr .gt_stub {
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

#jalunvfrdr .gt_stub_row_group {
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

#jalunvfrdr .gt_row_group_first td {
  border-top-width: 2px;
}

#jalunvfrdr .gt_row_group_first th {
  border-top-width: 2px;
}

#jalunvfrdr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jalunvfrdr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jalunvfrdr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jalunvfrdr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jalunvfrdr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jalunvfrdr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jalunvfrdr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jalunvfrdr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jalunvfrdr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jalunvfrdr .gt_footnotes {
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

#jalunvfrdr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jalunvfrdr .gt_sourcenotes {
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

#jalunvfrdr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jalunvfrdr .gt_left {
  text-align: left;
}

#jalunvfrdr .gt_center {
  text-align: center;
}

#jalunvfrdr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jalunvfrdr .gt_font_normal {
  font-weight: normal;
}

#jalunvfrdr .gt_font_bold {
  font-weight: bold;
}

#jalunvfrdr .gt_font_italic {
  font-style: italic;
}

#jalunvfrdr .gt_super {
  font-size: 65%;
}

#jalunvfrdr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jalunvfrdr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jalunvfrdr .gt_indent_1 {
  text-indent: 5px;
}

#jalunvfrdr .gt_indent_2 {
  text-indent: 10px;
}

#jalunvfrdr .gt_indent_3 {
  text-indent: 15px;
}

#jalunvfrdr .gt_indent_4 {
  text-indent: 20px;
}

#jalunvfrdr .gt_indent_5 {
  text-indent: 25px;
}

#jalunvfrdr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jalunvfrdr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 5-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c5_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,370.534</td>
<td headers="Count" class="gt_row gt_center">27.000</td>
<td headers="Percentage" class="gt_row gt_center">64.29%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,370.866</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">4.76%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,371.068</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,374.447</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,374.941</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,375.957</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,376.015</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,376.365</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,377.039</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,378.025</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,378.516</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">4.76%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,379.088</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,379.586</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,381.974</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.38%</td></tr>
  </tbody>
  
  
</table>
</div>

$c6_lca10faq.out
<div id="xebyqebtiv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#xebyqebtiv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xebyqebtiv thead, #xebyqebtiv tbody, #xebyqebtiv tfoot, #xebyqebtiv tr, #xebyqebtiv td, #xebyqebtiv th {
  border-style: none;
}

#xebyqebtiv p {
  margin: 0;
  padding: 0;
}

#xebyqebtiv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 12px;
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

#xebyqebtiv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xebyqebtiv .gt_title {
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

#xebyqebtiv .gt_subtitle {
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

#xebyqebtiv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xebyqebtiv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xebyqebtiv .gt_col_headings {
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

#xebyqebtiv .gt_col_heading {
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

#xebyqebtiv .gt_column_spanner_outer {
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

#xebyqebtiv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xebyqebtiv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xebyqebtiv .gt_column_spanner {
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

#xebyqebtiv .gt_spanner_row {
  border-bottom-style: hidden;
}

#xebyqebtiv .gt_group_heading {
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

#xebyqebtiv .gt_empty_group_heading {
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

#xebyqebtiv .gt_from_md > :first-child {
  margin-top: 0;
}

#xebyqebtiv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xebyqebtiv .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
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

#xebyqebtiv .gt_stub {
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

#xebyqebtiv .gt_stub_row_group {
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

#xebyqebtiv .gt_row_group_first td {
  border-top-width: 2px;
}

#xebyqebtiv .gt_row_group_first th {
  border-top-width: 2px;
}

#xebyqebtiv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xebyqebtiv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xebyqebtiv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xebyqebtiv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xebyqebtiv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xebyqebtiv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xebyqebtiv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xebyqebtiv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xebyqebtiv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xebyqebtiv .gt_footnotes {
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

#xebyqebtiv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xebyqebtiv .gt_sourcenotes {
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

#xebyqebtiv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xebyqebtiv .gt_left {
  text-align: left;
}

#xebyqebtiv .gt_center {
  text-align: center;
}

#xebyqebtiv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xebyqebtiv .gt_font_normal {
  font-weight: normal;
}

#xebyqebtiv .gt_font_bold {
  font-weight: bold;
}

#xebyqebtiv .gt_font_italic {
  font-style: italic;
}

#xebyqebtiv .gt_super {
  font-size: 65%;
}

#xebyqebtiv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xebyqebtiv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xebyqebtiv .gt_indent_1 {
  text-indent: 5px;
}

#xebyqebtiv .gt_indent_2 {
  text-indent: 10px;
}

#xebyqebtiv .gt_indent_3 {
  text-indent: 15px;
}

#xebyqebtiv .gt_indent_4 {
  text-indent: 20px;
}

#xebyqebtiv .gt_indent_5 {
  text-indent: 25px;
}

#xebyqebtiv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xebyqebtiv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 6-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c6_lca10faq.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,364.745</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.26%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,365.674</td>
<td headers="Count" class="gt_row gt_center">7.000</td>
<td headers="Percentage" class="gt_row gt_center">18.42%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,365.675</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">10.53%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.002</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.027</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.135</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.310</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.311</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.511</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,366.940</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,367.162</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,367.244</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,367.673</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,367.811</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.103</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.160</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.316</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.325</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.369</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.26%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.448</td>
<td headers="Count" class="gt_row gt_center">5.000</td>
<td headers="Percentage" class="gt_row gt_center">13.16%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,368.477</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−3,369.880</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−3,369.961</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.63%</td></tr>
  </tbody>
  
  
</table>
</div>

Visualize and examine loglikelihood replication for each output file together


``` r
ll_replication_table_all <- source(here("functions", "ll_replication_processing.R"), local = TRUE)$value
ll_replication_table_all
```


```{=html}
<div class="tabwid"><style>.cl-c6643aa6{}.cl-c65c60ce{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c65c60d8{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c65c60d9{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c65f2534{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-c65f253e{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-c65f3ef2{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3efc{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3efd{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3efe{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3eff{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3f06{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3f07{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c65f3f08{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-c6643aa6'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">1-Class</span></p></th><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">2-Class</span></p></th><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">3-Class</span></p></th><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">4-Class</span></p></th><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">5-Class</span></p></th><th  colspan="3"class="cl-c65f3ef2"><p class="cl-c65f2534"><span class="cl-c65c60ce">6-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th><th class="cl-c65f3efd"><p class="cl-c65f2534"><span class="cl-c65c60d8">LL</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">N</span></p></th><th class="cl-c65f3efe"><p class="cl-c65f2534"><span class="cl-c65c60d8">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3905.892</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">100</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">100</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3439.483</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">100</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">100</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3394.95</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">54</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">100</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3379.436</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">41</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">80.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3370.534</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">27</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">64.3</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,364.745</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">5.3</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3381.888</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3370.866</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">4.8</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,365.674</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">7</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">18.4</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3384.03</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">9</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">17.6</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3371.068</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,365.675</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">4</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">10.5</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3374.447</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.002</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3374.941</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.027</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3375.957</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.135</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3376.015</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.310</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3376.365</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.311</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3377.039</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.511</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3378.025</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,366.940</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3378.516</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">4.8</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,367.162</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3379.088</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,367.244</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3379.586</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,367.673</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3381.974</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.4</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,367.811</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.103</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.160</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.316</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.325</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.369</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">5.3</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.448</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">5</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">13.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,368.477</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3eff"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,369.880</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f06"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">—</span></p></td><td class="cl-c65f3f07"><p class="cl-c65f253e"><span class="cl-c65c60d9">-3,369.961</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">1</span></p></td><td class="cl-c65f3f08"><p class="cl-c65f253e"><span class="cl-c65c60d9">2.6</span></p></td></tr></tbody></table></div>
```


------------------------------------------------------------------------

### Table of Fit

First, extract data


``` r
output_lca10faq <- readModels(here("10faq", "enum"), filefilter = "lca10faq", quiet = TRUE)

enum_extract <- LatexSummaryTable(output_lca10faq,
    keepCols = c("Title","Parameters","LL","BIC","aBIC",
    "BLRT_PValue","T11_VLMR_PValue","Observations"),
    sortBy = "Title") 

allFit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  mutate(SIC = -.5 * BIC) %>%
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  arrange(Parameters)
```

Add Convergence percentage, LL Replication percentage, and smallest class (%) Columns


``` r
allFit <- allFit %>%
  mutate(Title = str_trim(Title)) %>%
  left_join(
    final_data %>%
      select(Class_Model, Perc_Convergence, Replicated_LL_Perc, 
             Smallest_Class, Smallest_Class_Perc),
    by = c("Title" = "Class_Model")
  ) %>%
  mutate(Smallest_Class = coalesce(Smallest_Class, 
                                   final_data$Smallest_Class[match(Title, final_data$Class_Model)])) %>%
  relocate(Perc_Convergence, Replicated_LL_Perc, .after = LL) %>%
  mutate(Smallest_Class_Combined = paste0(Smallest_Class, "\u00A0(", Smallest_Class_Perc, "%)")) %>%  
  select(-Smallest_Class, -Smallest_Class_Perc)  

allFit <- allFit %>%
  select(
    Title, Parameters, LL,
    Perc_Convergence, Replicated_LL_Perc,  
    BIC, aBIC, CAIC, AWE, 
    T11_VLMR_PValue, BLRT_PValue, 
    Smallest_Class_Combined,  
    BF, cmPk  
  )
```

Then, create table


``` r
fit_table1 <- allFit %>%
  select(Title, Parameters, LL, Perc_Convergence, Replicated_LL_Perc, 
         BIC, aBIC, CAIC, AWE, 
         T11_VLMR_PValue, BLRT_PValue, 
         Smallest_Class_Combined  
  ) %>%
  gt() %>%
  tab_header(title = md("**Model Fit Summary Table**")) %>%
  tab_spanner(label = "Model Fit Indices", columns = c(BIC, aBIC, CAIC, AWE)) %>%
  tab_spanner(label = "LRTs", columns = c(T11_VLMR_PValue, BLRT_PValue)) %>%
  tab_spanner(label = md("Smallest\u00A0Class"), columns = c(Smallest_Class_Combined)) %>%  
  
  cols_label(
    Title = "Classes",
    Parameters = md("npar"),
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
      *cmPk* = approximate correct model probability;
      Smallest K = Number of cases in the smallest class (n (%));
      LL Replicated = Whether the best log-likelihood was replicated."
    ),
    locations = cells_title()
  ) %>%
  tab_options(column_labels.font.weight = "bold") %>%
  fmt_number(
    columns = c(3, 6:9),  
    decimals = 2
  ) %>%
  fmt(
    columns = c(T11_VLMR_PValue, BLRT_PValue),  
    fns = function(x) ifelse(is.na(x), "—", ifelse(x < 0.001, "<.001", scales::number(x, accuracy = .01)))
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
      cells_body(columns = BIC, row = BIC == min(BIC)),
      cells_body(columns = aBIC, row = aBIC == min(aBIC)),
      cells_body(columns = CAIC, row = CAIC == min(CAIC)),
      cells_body(columns = AWE, row = AWE == min(AWE)),
      cells_body(columns = T11_VLMR_PValue, 
                 row = ifelse(T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue) > .05, T11_VLMR_PValue < .05, NA)),
      cells_body(columns = BLRT_PValue, 
                 row = ifelse(BLRT_PValue < .05 & lead(BLRT_PValue) > .05, BLRT_PValue < .05, NA))
    )
  )
```

Print Table


``` r
if (knitr::is_latex_output()) {
  cat("\\includegraphics[width=\\textwidth]{figures/fit_table1.png}\n")
} else {
  fit_table1  
}
```


```{=html}
<div id="bqzpfqmhqu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bqzpfqmhqu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bqzpfqmhqu thead, #bqzpfqmhqu tbody, #bqzpfqmhqu tfoot, #bqzpfqmhqu tr, #bqzpfqmhqu td, #bqzpfqmhqu th {
  border-style: none;
}

#bqzpfqmhqu p {
  margin: 0;
  padding: 0;
}

#bqzpfqmhqu .gt_table {
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

#bqzpfqmhqu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bqzpfqmhqu .gt_title {
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

#bqzpfqmhqu .gt_subtitle {
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

#bqzpfqmhqu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bqzpfqmhqu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqzpfqmhqu .gt_col_headings {
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

#bqzpfqmhqu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
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

#bqzpfqmhqu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bqzpfqmhqu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bqzpfqmhqu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bqzpfqmhqu .gt_column_spanner {
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

#bqzpfqmhqu .gt_spanner_row {
  border-bottom-style: hidden;
}

#bqzpfqmhqu .gt_group_heading {
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

#bqzpfqmhqu .gt_empty_group_heading {
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

#bqzpfqmhqu .gt_from_md > :first-child {
  margin-top: 0;
}

#bqzpfqmhqu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bqzpfqmhqu .gt_row {
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

#bqzpfqmhqu .gt_stub {
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

#bqzpfqmhqu .gt_stub_row_group {
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

#bqzpfqmhqu .gt_row_group_first td {
  border-top-width: 2px;
}

#bqzpfqmhqu .gt_row_group_first th {
  border-top-width: 2px;
}

#bqzpfqmhqu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqzpfqmhqu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bqzpfqmhqu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bqzpfqmhqu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqzpfqmhqu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqzpfqmhqu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bqzpfqmhqu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bqzpfqmhqu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bqzpfqmhqu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqzpfqmhqu .gt_footnotes {
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

#bqzpfqmhqu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqzpfqmhqu .gt_sourcenotes {
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

#bqzpfqmhqu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqzpfqmhqu .gt_left {
  text-align: left;
}

#bqzpfqmhqu .gt_center {
  text-align: center;
}

#bqzpfqmhqu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bqzpfqmhqu .gt_font_normal {
  font-weight: normal;
}

#bqzpfqmhqu .gt_font_bold {
  font-weight: bold;
}

#bqzpfqmhqu .gt_font_italic {
  font-style: italic;
}

#bqzpfqmhqu .gt_super {
  font-size: 65%;
}

#bqzpfqmhqu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bqzpfqmhqu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bqzpfqmhqu .gt_indent_1 {
  text-indent: 5px;
}

#bqzpfqmhqu .gt_indent_2 {
  text-indent: 10px;
}

#bqzpfqmhqu .gt_indent_3 {
  text-indent: 15px;
}

#bqzpfqmhqu .gt_indent_4 {
  text-indent: 20px;
}

#bqzpfqmhqu .gt_indent_5 {
  text-indent: 25px;
}

#bqzpfqmhqu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bqzpfqmhqu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="12" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Fit Summary Table</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Title">Classes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>npar</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="LL"><span class='gt_from_md'><em>LL</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Perc_Convergence">% Converged</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Replicated_LL_Perc">% Replicated</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Model Fit Indices">
        <div class="gt_column_spanner">Model Fit Indices</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="LRTs">
        <div class="gt_column_spanner">LRTs</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="Smallest Class">
        <div class="gt_column_spanner"><span class='gt_from_md'>Smallest Class</span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="BIC">BIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="aBIC">aBIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CAIC">CAIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="AWE">AWE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="T11_VLMR_PValue">VLMR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="BLRT_PValue">BLRT</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Smallest_Class_Combined">n (%)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Title" class="gt_row gt_center">1-Class</td>
<td headers="Parameters" class="gt_row gt_center">7</td>
<td headers="LL" class="gt_row gt_center">−3,905.89</td>
<td headers="Perc_Convergence" class="gt_row gt_center">100%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
<td headers="BIC" class="gt_row gt_center">7,863.56</td>
<td headers="aBIC" class="gt_row gt_center">7,841.32</td>
<td headers="CAIC" class="gt_row gt_center">7,870.55</td>
<td headers="AWE" class="gt_row gt_center">7,936.32</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">—</td>
<td headers="BLRT_PValue" class="gt_row gt_center">—</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">1629 (100%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">2-Class</td>
<td headers="Parameters" class="gt_row gt_center">15</td>
<td headers="LL" class="gt_row gt_center">−3,439.48</td>
<td headers="Perc_Convergence" class="gt_row gt_center">100%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
<td headers="BIC" class="gt_row gt_center">6,989.90</td>
<td headers="aBIC" class="gt_row gt_center">6,942.25</td>
<td headers="CAIC" class="gt_row gt_center">7,004.90</td>
<td headers="AWE" class="gt_row gt_center" style="font-weight: bold;">7,145.84</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">331 (20.3%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">3-Class</td>
<td headers="Parameters" class="gt_row gt_center">23</td>
<td headers="LL" class="gt_row gt_center">−3,394.95</td>
<td headers="Perc_Convergence" class="gt_row gt_center">54%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
<td headers="BIC" class="gt_row gt_center" style="font-weight: bold;">6,960.00</td>
<td headers="aBIC" class="gt_row gt_center" style="font-weight: bold;">6,886.93</td>
<td headers="CAIC" class="gt_row gt_center" style="font-weight: bold;">6,983.00</td>
<td headers="AWE" class="gt_row gt_center">7,199.10</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">90 (5.5%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">4-Class</td>
<td headers="Parameters" class="gt_row gt_center">31</td>
<td headers="LL" class="gt_row gt_center">−3,379.44</td>
<td headers="Perc_Convergence" class="gt_row gt_center">51%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">80%</td>
<td headers="BIC" class="gt_row gt_center">6,988.14</td>
<td headers="aBIC" class="gt_row gt_center">6,889.66</td>
<td headers="CAIC" class="gt_row gt_center">7,019.14</td>
<td headers="AWE" class="gt_row gt_center">7,310.41</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.00</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">88 (5.4%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">5-Class</td>
<td headers="Parameters" class="gt_row gt_center">39</td>
<td headers="LL" class="gt_row gt_center">−3,370.53</td>
<td headers="Perc_Convergence" class="gt_row gt_center">42%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">64%</td>
<td headers="BIC" class="gt_row gt_center">7,029.50</td>
<td headers="aBIC" class="gt_row gt_center">6,905.60</td>
<td headers="CAIC" class="gt_row gt_center">7,068.50</td>
<td headers="AWE" class="gt_row gt_center">7,434.93</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.43</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.19</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">35 (2.1%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">6-Class</td>
<td headers="Parameters" class="gt_row gt_center">47</td>
<td headers="LL" class="gt_row gt_center">−3,364.74</td>
<td headers="Perc_Convergence" class="gt_row gt_center">38%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">5%</td>
<td headers="BIC" class="gt_row gt_center">7,077.09</td>
<td headers="aBIC" class="gt_row gt_center">6,927.78</td>
<td headers="CAIC" class="gt_row gt_center">7,124.09</td>
<td headers="AWE" class="gt_row gt_center">7,565.69</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.47</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.67</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">6 (0.4%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="12"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> Par = Parameters; <em>LL</em> = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
<em>cmPk</em> = approximate correct model probability;
Smallest K = Number of cases in the smallest class (n (%));
LL Replicated = Whether the best log-likelihood was replicated.</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


------------------------------------------------------------------------

### Information Criteria Plot


``` r
# Ensure CAIC exists and is numeric, if it exists
allFit <- allFit %>%
  mutate(CAIC = as.numeric(CAIC))  # Ensure CAIC is numeric

# Ensure CAIC exists and is numeric, if it exists
allFit <- allFit %>%
  mutate(CAIC = as.numeric(CAIC))  # Ensure CAIC is numeric

# Now, pivot the data
allFit %>%
  dplyr::select(Title, BIC, aBIC, CAIC, AWE) %>%
  pivot_longer(
    cols = c(BIC, aBIC, CAIC, AWE),  # Use these columns for pivoting
    names_to = "Index", values_to = "ic_value"
  ) %>%
  mutate(
    Index = factor(Index, levels = c("AWE", "CAIC", "BIC", "aBIC"))  # Ensure proper ordering of Index
  ) %>%
  ggplot(aes(
    x = Title,
    y = ic_value,
    color = Index,
    shape = Index,
    group = Index,
    lty = Index
  )) +
  geom_point(size = 2.0) + 
  geom_line(size = .8) +
  scale_x_discrete() +
  scale_colour_grey(end = .5) +
  theme_cowplot() +
  labs(x = "Number of Classes", y = "Information Criteria Value", title = "Information Criteria") +
  theme(
    text = element_text(family = "serif", size = 12),  # Change font to Avenir Next
    legend.text = element_text(family="serif", size=12),
    legend.key.width = unit(3, "line"),
    legend.title = element_blank(),
    legend.position = "top"  
  )
```

<img src="19-lca-faq_files/figure-html/unnamed-chunk-17-1.png" width="672" />

------------------------------------------------------------------------

### Compare Class Solutions

Compare probability plots for $K = 1:6$ class solutions


``` r
model_results <- data.frame()

for (i in 1:length(output_lca10faq)) {
  
  temp <- output_lca10faq[[i]]$parameters$probability.scale %>%                                       
    mutate(model = paste(i,"-Class Model"))                                                  
  
  model_results <- rbind(model_results, temp)
}

rm(temp)

compare_plot <-
  model_results %>%
  filter(category == 2) %>%
  dplyr::select(est, model, LatentClass, param) %>%
  mutate(param = as.factor(str_to_lower(param))) 

compare_plot$param <- fct_inorder(compare_plot$param)

ggplot(
  compare_plot,
  aes(
    x = param,
    y = est,
    color = LatentClass,
    shape = LatentClass,
    group = LatentClass,
    lty = LatentClass
  )
) +
  geom_point() + 
  geom_line() +
  scale_colour_viridis_d() +
  facet_wrap( ~ model, ncol = 2) +
  labs(title = "Bullying Items",
       x = " ", y = "Probability") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
                          axis.text.x = element_text(angle = -45, hjust = -.1))        
```

<img src="19-lca-faq_files/figure-html/unnamed-chunk-18-1.png" width="672" />


``` r
source(here("functions", "plot_lca.R"))

plot_lca(model_name = output_lca10faq$c4_lca10faq.out)
```

<img src="19-lca-faq_files/figure-html/unnamed-chunk-19-1.png" width="672" />

*Model Selection*: 

Fit indices did not converge on a single solution, and this is generally the rule rather than the exception in applied practice. The ICs and the cmP suggested a three-class solution, whereas the likelihood tests supported a four-class solution. Evaluating Figure 2, the most prominent “elbow” was at the two-class model, whereas the lowest point for the ICs was at the three-class model. The BF suggested that the three-, four-, and five-class solutions were plausible. Given that the BLRT specifically has been shown to be robust across a diversity of modeling conditions (Nylund, Asparouhov, & Muthén, 2007), we tentatively selected the four-class solution.

*Model Interpretation*: 

The above figure illustrates the conditional item probabilities for the selected 4-class solution, where “conditional” again refers to the likelihood of endorsing each item as a function of class membership (e.g., the probabilities are “conditioned” on class). The model indicators are labeled on the x axis whereas the y axis presents the metric of the item probabilities (0 to 1). The four classes are defined by the crisscrossing lines, and their preliminary labels are listed at the bottom of Figure 3 with class prevalence (e.g., relative class sizes) in parentheses. We referred to the indicators in Table 2 in evaluating the substantive meaning of the joint patterns of item responses that emerged within each class. The first and largest class is characterized by high response probabilities for all indicators and were thus labeled the Holistic–Collaborative class. Youth in this class are likely to value social contribution overall and pursue pertinent activities in cooperation with others. The second class was labeled Altruistic–Low Selfefficacy and is likely to be comprised of youth who highly value social contribution yet do not believe in their own effectiveness for impactful engagement with their community. The third class was labeled Low Engagement given the characteristically low or ambivalent response probabilities for all model indicators. The fourth class was labeled Holistic–Independent given its similarity to the Holistic–Collaborative class apart from two indicators measuring interpersonal and community engagement. Youth in this class are likely to value social contribution but may prefer pursuing relevant activities in a more independent or introverted manner. Examining Figure 3, it is apparent that these two classes are quite homogenous (and thus not well-separated) in their item responses except on items 1 and 3, which may diminish the model classification statistics.

------------------------------------------------------------------------

## Including Auxilary Variables

------------------------------------------------------------------------

### Step 1 - Class Enumeration w/ Auxiliary Specification

------------------------------------------------------------------------

This step is done after class enumeration (or after you have selected the best latent class model). In this example, the four class model was the best. Now, I am re-estimating the four-class model using `optseed` for efficiency. The difference here is the `SAVEDATA` command, where I can save the posterior probabilities and the modal class assignment for steps two and three. 


``` r
step1  <- mplusObject(
  TITLE = "Step 1 - Three-Step", 
  VARIABLE = 
  "categorical = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab; 
   usevar = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab;
  MISSING ARE ALL (9999);
   classes = c(4); 
    
   auxiliary =   ! list all potential covariates and distals here
   hispanic        ! covariate
   LifeSatA;      ! distal ",
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 0;
    optseed = 371246;",
  
  SAVEDATA = 
   "File=3step_savedata.dat;
    Save=cprob;",
  
  OUTPUT = "residual tech11 tech14 svalues(2 4 3 1);",
  
  PLOT = 
    "type = plot3; 
    series = PYDI1Ab PYDI2Ab PYDI3Ab PYDI4Ab PYDI5Ab PYDI6Ab PYDI7Ab(*);",
  
  usevariables = colnames(df_qa),
  rdata = df_qa)

step1_fit <- mplusModeler(step1,
                            dataout=here("10faq", "manual_3step", "Step1.dat"),
                            modelout=here("10faq", "manual_3step", "one.inp") ,
                            check=TRUE, run = TRUE, hashfilename = FALSE)
```


``` r
source(here("functions", "plot_lca.R"))
output_lsay <- readModels(here("10faq", "manual_3step","one.out"))

plot_lca(model_name = output_lsay)
```

<img src="19-lca-faq_files/figure-html/unnamed-chunk-21-1.png" width="672" />

Class 1:  Holistic-Collaborative (65%)
Class 2:  Altruistic-Low Self-efficacy (8%)
Class 3:  Low Engagement (5%)
Class 4:  Holistic-Independent (22%)

------------------------------------------------------------------------

### Step 2 - Determine Measurement Error

------------------------------------------------------------------------

Extract logits for the classification probabilities for the most likely latent class

``` r
logit_cprobs <- as.data.frame(output_lsay[["class_counts"]]
                                       [["logitProbs.mostLikely"]])
```

Extract saved dataset which is part of the mplusObject "step1_fit"


``` r
savedata <- as.data.frame(output_lsay[["savedata"]])
```

Rename the column in savedata named "C" and change to "N"


``` r
colnames(savedata)[colnames(savedata)=="C"] <- "N"
```

------------------------------------------------------------------------

### Step 3 - Add Auxiliary Variables

------------------------------------------------------------------------

Model with 1 covariate and 1 distal outcome


``` r
step3  <- mplusObject(
  TITLE = "Step3 - 3step LSAY", 
  
  VARIABLE = 
 "nominal=N;
  usevar = n;
  
  classes = c(4);
  
  usevar = hispanic LifeSatA;" ,
  
  ANALYSIS = 
 "estimator = mlr; 
  type = mixture; 
  starts = 0;",
 
  DEFINE = 
   "center hispanic (grandmean);",
  
  MODEL =
  glue(
 " %OVERALL%
 
  LifeSatA on hispanic; ! covariate as a predictor of the distal outcome
  C on hispanic;        ! covariate as predictor of C

     %C#1%
  [n#1@{logit_cprobs[1,1]}]; ! MUST EDIT if you do not have a 4-class model. 
  [n#2@{logit_cprobs[1,2]}];
  [n#3@{logit_cprobs[1,3]}];
  
  [LifeSatA](m1);    ! conditional distal mean 
  LifeSatA;          ! conditional distal variance (freely estimated)

  %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  
  [LifeSatA](m2);
  LifeSatA;
  
  %C#3%
  [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  
  [LifeSatA](m3);
  LifeSatA;

  %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  
  [LifeSatA](m4);
  LifeSatA; "),
  
  MODELCONSTRAINT = 
   "New (diff12 diff13 diff23 
    diff14 diff24 diff34);
  
    diff12 = m1-m2;  ! test pairwise distal mean differences
    diff13 = m1-m3;
    diff23 = m2-m3;
    diff14 = m1-m4;
    diff24 = m2-m4;
    diff34 = m3-m4;",
  
  MODELTEST = "     ! omnibus test of distal means 
    m1=m2;
    m2=m3;
    m3=m4;",
 
  usevariables = colnames(savedata), 
  rdata = savedata)

step3_fit <- mplusModeler(step3,
               dataout=here("10faq", "manual_3step", "Step3.dat"), 
               modelout=here("10faq", "manual_3step", "three_starts.inp"), 
               check=TRUE, run = TRUE, hashfilename = FALSE)
```

------------------------------------------------------------------------

#### Wald Test Table


``` r
modelParams <- readModels(here("10faq", "manual_3step", "three_starts.out"))

# Extract information as data frame
wald <- as.data.frame(modelParams[["summaries"]]) %>%
  dplyr::select(WaldChiSq_Value:WaldChiSq_PValue) %>% 
  mutate(WaldChiSq_DF = paste0("(", WaldChiSq_DF, ")")) %>% 
  unite(wald_test, WaldChiSq_Value, WaldChiSq_DF, sep = " ") %>% 
  rename(pval = WaldChiSq_PValue) %>% 
  mutate(pval = ifelse(pval<0.001, paste0("<.001*"),
                       ifelse(pval<0.05, paste0(scales::number(pval, accuracy = .001), "*"),
                              scales::number(pval, accuracy = .001))))

# Create table

wald_table <- wald %>% 
  gt() %>%
    tab_header(
    title = "Wald Test of Paramter Constraints (Math)") %>%
    cols_label(
      wald_test = md("Wald Test (*df*)"),
      pval = md("*p*-value")) %>% 
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "serif")

wald_table
```


```{=html}
<div id="hvtgtauhwf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hvtgtauhwf table {
  font-family: serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hvtgtauhwf thead, #hvtgtauhwf tbody, #hvtgtauhwf tfoot, #hvtgtauhwf tr, #hvtgtauhwf td, #hvtgtauhwf th {
  border-style: none;
}

#hvtgtauhwf p {
  margin: 0;
  padding: 0;
}

#hvtgtauhwf .gt_table {
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

#hvtgtauhwf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hvtgtauhwf .gt_title {
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

#hvtgtauhwf .gt_subtitle {
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

#hvtgtauhwf .gt_heading {
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

#hvtgtauhwf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hvtgtauhwf .gt_col_headings {
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

#hvtgtauhwf .gt_col_heading {
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

#hvtgtauhwf .gt_column_spanner_outer {
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

#hvtgtauhwf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hvtgtauhwf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hvtgtauhwf .gt_column_spanner {
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

#hvtgtauhwf .gt_spanner_row {
  border-bottom-style: hidden;
}

#hvtgtauhwf .gt_group_heading {
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

#hvtgtauhwf .gt_empty_group_heading {
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

#hvtgtauhwf .gt_from_md > :first-child {
  margin-top: 0;
}

#hvtgtauhwf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hvtgtauhwf .gt_row {
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

#hvtgtauhwf .gt_stub {
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

#hvtgtauhwf .gt_stub_row_group {
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

#hvtgtauhwf .gt_row_group_first td {
  border-top-width: 2px;
}

#hvtgtauhwf .gt_row_group_first th {
  border-top-width: 2px;
}

#hvtgtauhwf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvtgtauhwf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hvtgtauhwf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hvtgtauhwf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hvtgtauhwf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvtgtauhwf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hvtgtauhwf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hvtgtauhwf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hvtgtauhwf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hvtgtauhwf .gt_footnotes {
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

#hvtgtauhwf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvtgtauhwf .gt_sourcenotes {
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

#hvtgtauhwf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvtgtauhwf .gt_left {
  text-align: left;
}

#hvtgtauhwf .gt_center {
  text-align: center;
}

#hvtgtauhwf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hvtgtauhwf .gt_font_normal {
  font-weight: normal;
}

#hvtgtauhwf .gt_font_bold {
  font-weight: bold;
}

#hvtgtauhwf .gt_font_italic {
  font-style: italic;
}

#hvtgtauhwf .gt_super {
  font-size: 65%;
}

#hvtgtauhwf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hvtgtauhwf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hvtgtauhwf .gt_indent_1 {
  text-indent: 5px;
}

#hvtgtauhwf .gt_indent_2 {
  text-indent: 10px;
}

#hvtgtauhwf .gt_indent_3 {
  text-indent: 15px;
}

#hvtgtauhwf .gt_indent_4 {
  text-indent: 20px;
}

#hvtgtauhwf .gt_indent_5 {
  text-indent: 25px;
}

#hvtgtauhwf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hvtgtauhwf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Wald Test of Paramter Constraints (Math)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="wald_test"><span class='gt_from_md'>Wald Test (<em>df</em>)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="pval"><span class='gt_from_md'><em>p</em>-value</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="wald_test" class="gt_row gt_center">442.089 (3)</td>
<td headers="pval" class="gt_row gt_center">&lt;.001*</td></tr>
  </tbody>
  
  
</table>
</div>
```


------------------------------------------------------------------------

#### Table of Distal Outcome Differences


``` r
# Extract information as data frame
diff <- as.data.frame(modelParams[["parameters"]][["unstandardized"]]) %>%
  filter(grepl("DIFF", param)) %>% 
  dplyr::select(param:pval) %>% 
  mutate(se = paste0("(", format(round(se,2), nsmall =2), ")")) %>% 
  unite(estimate, est, se, sep = " ") %>% 
  mutate(param = str_remove(param, "DIFF"),
         param = as.numeric(param)) %>% 
  separate(param, into = paste0("Group", 1:2), sep = 1) %>% 
  mutate(class = paste0("Class ", Group1, " vs ", Group2)) %>% 
  select(class, estimate, pval) %>% 
  mutate(pval = ifelse(pval<0.001, paste0("<.001*"),
                       ifelse(pval<0.05, paste0(scales::number(pval, accuracy = .001), "*"),
                              scales::number(pval, accuracy = .001))))

# Create table

diff %>% 
  gt() %>%
    tab_header(
    title = "Distal Outcome Differences") %>%
    cols_label(
      class = "Class",
      estimate = md("Mean (*se*)"),
      pval = md("*p*-value")) %>% 
    sub_missing(1:3,
              missing_text = "") %>%
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "serif")
```


```{=html}
<div id="okfecnfcdu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#okfecnfcdu table {
  font-family: serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#okfecnfcdu thead, #okfecnfcdu tbody, #okfecnfcdu tfoot, #okfecnfcdu tr, #okfecnfcdu td, #okfecnfcdu th {
  border-style: none;
}

#okfecnfcdu p {
  margin: 0;
  padding: 0;
}

#okfecnfcdu .gt_table {
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

#okfecnfcdu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#okfecnfcdu .gt_title {
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

#okfecnfcdu .gt_subtitle {
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

#okfecnfcdu .gt_heading {
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

#okfecnfcdu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okfecnfcdu .gt_col_headings {
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

#okfecnfcdu .gt_col_heading {
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

#okfecnfcdu .gt_column_spanner_outer {
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

#okfecnfcdu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#okfecnfcdu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#okfecnfcdu .gt_column_spanner {
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

#okfecnfcdu .gt_spanner_row {
  border-bottom-style: hidden;
}

#okfecnfcdu .gt_group_heading {
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

#okfecnfcdu .gt_empty_group_heading {
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

#okfecnfcdu .gt_from_md > :first-child {
  margin-top: 0;
}

#okfecnfcdu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#okfecnfcdu .gt_row {
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

#okfecnfcdu .gt_stub {
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

#okfecnfcdu .gt_stub_row_group {
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

#okfecnfcdu .gt_row_group_first td {
  border-top-width: 2px;
}

#okfecnfcdu .gt_row_group_first th {
  border-top-width: 2px;
}

#okfecnfcdu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#okfecnfcdu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#okfecnfcdu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#okfecnfcdu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okfecnfcdu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#okfecnfcdu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#okfecnfcdu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#okfecnfcdu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#okfecnfcdu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okfecnfcdu .gt_footnotes {
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

#okfecnfcdu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#okfecnfcdu .gt_sourcenotes {
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

#okfecnfcdu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#okfecnfcdu .gt_left {
  text-align: left;
}

#okfecnfcdu .gt_center {
  text-align: center;
}

#okfecnfcdu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#okfecnfcdu .gt_font_normal {
  font-weight: normal;
}

#okfecnfcdu .gt_font_bold {
  font-weight: bold;
}

#okfecnfcdu .gt_font_italic {
  font-style: italic;
}

#okfecnfcdu .gt_super {
  font-size: 65%;
}

#okfecnfcdu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#okfecnfcdu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#okfecnfcdu .gt_indent_1 {
  text-indent: 5px;
}

#okfecnfcdu .gt_indent_2 {
  text-indent: 10px;
}

#okfecnfcdu .gt_indent_3 {
  text-indent: 15px;
}

#okfecnfcdu .gt_indent_4 {
  text-indent: 20px;
}

#okfecnfcdu .gt_indent_5 {
  text-indent: 25px;
}

#okfecnfcdu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#okfecnfcdu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Distal Outcome Differences</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="class">Class</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'>Mean (<em>se</em>)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="pval"><span class='gt_from_md'><em>p</em>-value</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="class" class="gt_row gt_center">Class 1 vs 2</td>
<td headers="estimate" class="gt_row gt_center">20.613 (2.37)</td>
<td headers="pval" class="gt_row gt_center">&lt;.001*</td></tr>
    <tr><td headers="class" class="gt_row gt_center">Class 1 vs 3</td>
<td headers="estimate" class="gt_row gt_center">23.645 (3.69)</td>
<td headers="pval" class="gt_row gt_center">&lt;.001*</td></tr>
    <tr><td headers="class" class="gt_row gt_center">Class 2 vs 3</td>
<td headers="estimate" class="gt_row gt_center">3.032 (4.89)</td>
<td headers="pval" class="gt_row gt_center">0.536</td></tr>
    <tr><td headers="class" class="gt_row gt_center">Class 1 vs 4</td>
<td headers="estimate" class="gt_row gt_center">22.07 (1.32)</td>
<td headers="pval" class="gt_row gt_center">&lt;.001*</td></tr>
    <tr><td headers="class" class="gt_row gt_center">Class 2 vs 4</td>
<td headers="estimate" class="gt_row gt_center">1.457 (2.70)</td>
<td headers="pval" class="gt_row gt_center">0.590</td></tr>
    <tr><td headers="class" class="gt_row gt_center">Class 3 vs 4</td>
<td headers="estimate" class="gt_row gt_center">-1.575 (3.95)</td>
<td headers="pval" class="gt_row gt_center">0.690</td></tr>
  </tbody>
  
  
</table>
</div>
```


------------------------------------------------------------------------

#### Covariate Relations


``` r
# Extract information as data frame
cov <- as.data.frame(modelParams[["parameters"]][["unstandardized"]]) %>%
   filter(str_detect(paramHeader, "^C#\\d+\\.ON$")) %>% 
  mutate(param = str_replace(param, "HISPANIC", "Hispanic")) %>%
  mutate(est = format(round(est, 3), nsmall = 3),
         se = round(se, 2),
         pval = round(pval, 3)) %>% 
  mutate(latent_class = str_replace(paramHeader, "^C#(\\d+)\\.ON$", "Class \\1")) %>% 
  dplyr::select(param, est, se, pval, latent_class) %>% 
  mutate(se = paste0("(", format(round(se,2), nsmall =2), ")")) %>% 
  unite(logit, est, se, sep = " ") %>% 
  dplyr::select(param, logit, pval, latent_class) %>% 
  mutate(pval = ifelse(pval<0.001, paste0("<.001*"),
                       ifelse(pval<0.05, paste0(scales::number(pval, accuracy = .001), "*"),
                              scales::number(pval, accuracy = .001)))) 

or <- as.data.frame(modelParams[["parameters"]][["odds"]]) %>%
  filter(str_detect(paramHeader, "^C#\\d+\\.ON$")) %>% 
  mutate(param = str_replace(param, "HISPANIC", "Hispanic")) %>%
  mutate(est = format(round(est, 3), nsmall = 3)) %>% 
  mutate(latent_class = str_replace(paramHeader, "^C#(\\d+)\\.ON$", "Class \\1")) %>% 
  mutate(CI = paste0("[", format(round(lower_2.5ci, 3), nsmall = 3), ", ", format(round(upper_2.5ci, 3), nsmall = 3), "]")) %>% 
  dplyr::select(param, est, CI, latent_class) %>% 
  rename(or = est)
  
combined <- or %>% 
  full_join(cov) %>% 
  dplyr::select(param, latent_class, logit, pval, or, CI)


# Create table

combined %>% 
  gt(groupname_col = "latent_class", rowname_col = "param") %>%
  tab_header(
    title = "Predictors of Class Membership") %>%
  cols_label(
    logit = md("Logit (*se*)"),
    or = md("Odds Ratio"),
    CI = md("95% CI"),
    pval = md("*p*-value")) %>% 
  sub_missing(1:3,
              missing_text = "") %>%
  sub_values(values = c("999.000"), replacement = "-") %>% 
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "serif") %>%   
  tab_footnote(
    footnote = "Reference Class: 4",
    locations = cells_title(groups = "title")
  )
```


```{=html}
<div id="xtwauxnfqe" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xtwauxnfqe table {
  font-family: serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xtwauxnfqe thead, #xtwauxnfqe tbody, #xtwauxnfqe tfoot, #xtwauxnfqe tr, #xtwauxnfqe td, #xtwauxnfqe th {
  border-style: none;
}

#xtwauxnfqe p {
  margin: 0;
  padding: 0;
}

#xtwauxnfqe .gt_table {
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

#xtwauxnfqe .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xtwauxnfqe .gt_title {
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

#xtwauxnfqe .gt_subtitle {
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

#xtwauxnfqe .gt_heading {
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

#xtwauxnfqe .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xtwauxnfqe .gt_col_headings {
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

#xtwauxnfqe .gt_col_heading {
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

#xtwauxnfqe .gt_column_spanner_outer {
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

#xtwauxnfqe .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xtwauxnfqe .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xtwauxnfqe .gt_column_spanner {
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

#xtwauxnfqe .gt_spanner_row {
  border-bottom-style: hidden;
}

#xtwauxnfqe .gt_group_heading {
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

#xtwauxnfqe .gt_empty_group_heading {
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

#xtwauxnfqe .gt_from_md > :first-child {
  margin-top: 0;
}

#xtwauxnfqe .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xtwauxnfqe .gt_row {
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

#xtwauxnfqe .gt_stub {
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

#xtwauxnfqe .gt_stub_row_group {
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

#xtwauxnfqe .gt_row_group_first td {
  border-top-width: 2px;
}

#xtwauxnfqe .gt_row_group_first th {
  border-top-width: 2px;
}

#xtwauxnfqe .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xtwauxnfqe .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xtwauxnfqe .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xtwauxnfqe .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xtwauxnfqe .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xtwauxnfqe .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xtwauxnfqe .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xtwauxnfqe .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xtwauxnfqe .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xtwauxnfqe .gt_footnotes {
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

#xtwauxnfqe .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xtwauxnfqe .gt_sourcenotes {
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

#xtwauxnfqe .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xtwauxnfqe .gt_left {
  text-align: left;
}

#xtwauxnfqe .gt_center {
  text-align: center;
}

#xtwauxnfqe .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xtwauxnfqe .gt_font_normal {
  font-weight: normal;
}

#xtwauxnfqe .gt_font_bold {
  font-weight: bold;
}

#xtwauxnfqe .gt_font_italic {
  font-style: italic;
}

#xtwauxnfqe .gt_super {
  font-size: 65%;
}

#xtwauxnfqe .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xtwauxnfqe .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xtwauxnfqe .gt_indent_1 {
  text-indent: 5px;
}

#xtwauxnfqe .gt_indent_2 {
  text-indent: 10px;
}

#xtwauxnfqe .gt_indent_3 {
  text-indent: 15px;
}

#xtwauxnfqe .gt_indent_4 {
  text-indent: 20px;
}

#xtwauxnfqe .gt_indent_5 {
  text-indent: 25px;
}

#xtwauxnfqe .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xtwauxnfqe div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Predictors of Class Membership<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="logit"><span class='gt_from_md'>Logit (<em>se</em>)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="pval"><span class='gt_from_md'><em>p</em>-value</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="or"><span class='gt_from_md'>Odds Ratio</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CI"><span class='gt_from_md'>95% CI</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" scope="colgroup" id="Class 1">Class 1</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_center gt_stub">Hispanic</th>
<td headers="Class 1 stub_1_1 logit" class="gt_row gt_center"> 0.485 (0.18)</td>
<td headers="Class 1 stub_1_1 pval" class="gt_row gt_center">0.009*</td>
<td headers="Class 1 stub_1_1 or" class="gt_row gt_center">1.624</td>
<td headers="Class 1 stub_1_1 CI" class="gt_row gt_center">[1.130, 2.332]</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" scope="colgroup" id="Class 2">Class 2</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_2" scope="row" class="gt_row gt_center gt_stub">Hispanic</th>
<td headers="Class 2 stub_1_2 logit" class="gt_row gt_center">-0.513 (0.33)</td>
<td headers="Class 2 stub_1_2 pval" class="gt_row gt_center">0.120</td>
<td headers="Class 2 stub_1_2 or" class="gt_row gt_center">0.599</td>
<td headers="Class 2 stub_1_2 CI" class="gt_row gt_center">[0.314, 1.144]</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" scope="colgroup" id="Class 3">Class 3</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_3" scope="row" class="gt_row gt_center gt_stub">Hispanic</th>
<td headers="Class 3 stub_1_3 logit" class="gt_row gt_center">-0.104 (0.36)</td>
<td headers="Class 3 stub_1_3 pval" class="gt_row gt_center">0.770</td>
<td headers="Class 3 stub_1_3 or" class="gt_row gt_center">0.901</td>
<td headers="Class 3 stub_1_3 CI" class="gt_row gt_center">[0.447, 1.815]</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> Reference Class: 4</td>
    </tr>
  </tfoot>
</table>
</div>
```


------------------------------------------------------------------------

#### Distal Outcome Regressed on Covariate


``` r
donx <- as.data.frame(modelParams[["parameters"]][["unstandardized"]]) %>%
  filter(param %in% c("HISPANIC")) %>% 
  mutate(param = str_replace(param, "HISPANIC", "Hispanic")) %>% 
  mutate(LatentClass = sub("^","Class ", LatentClass)) %>%  
  dplyr::select(!paramHeader) %>% 
  mutate(se = paste0("(", format(round(se,2), nsmall =2), ")")) %>% 
    unite(estimate, est, se, sep = " ") %>% 
  dplyr::select(param, estimate, pval) %>% 
  distinct(param, .keep_all=TRUE) %>% 
  mutate(pval = ifelse(pval<0.001, paste0("<.001*"),
                       ifelse(pval<0.05, paste0(scales::number(pval, accuracy = .001), "*"),
                              scales::number(pval, accuracy = .001))))


# Create table

donx %>% 
  gt(groupname_col = "LatentClass", rowname_col = "param") %>%
  tab_header(
    title = "Race (Hispanic) Predicting Life Satisfaction") %>%
  cols_label(
    estimate = md("Estimate (*se*)"),
    pval = md("*p*-value")) %>% 
  sub_missing(1:3,
              missing_text = "") %>%
  sub_values(values = c("999.000"), replacement = "-") %>% 
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "serif")
```


```{=html}
<div id="syqsjbjykt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#syqsjbjykt table {
  font-family: serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#syqsjbjykt thead, #syqsjbjykt tbody, #syqsjbjykt tfoot, #syqsjbjykt tr, #syqsjbjykt td, #syqsjbjykt th {
  border-style: none;
}

#syqsjbjykt p {
  margin: 0;
  padding: 0;
}

#syqsjbjykt .gt_table {
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

#syqsjbjykt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#syqsjbjykt .gt_title {
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

#syqsjbjykt .gt_subtitle {
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

#syqsjbjykt .gt_heading {
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

#syqsjbjykt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#syqsjbjykt .gt_col_headings {
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

#syqsjbjykt .gt_col_heading {
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

#syqsjbjykt .gt_column_spanner_outer {
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

#syqsjbjykt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#syqsjbjykt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#syqsjbjykt .gt_column_spanner {
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

#syqsjbjykt .gt_spanner_row {
  border-bottom-style: hidden;
}

#syqsjbjykt .gt_group_heading {
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

#syqsjbjykt .gt_empty_group_heading {
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

#syqsjbjykt .gt_from_md > :first-child {
  margin-top: 0;
}

#syqsjbjykt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#syqsjbjykt .gt_row {
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

#syqsjbjykt .gt_stub {
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

#syqsjbjykt .gt_stub_row_group {
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

#syqsjbjykt .gt_row_group_first td {
  border-top-width: 2px;
}

#syqsjbjykt .gt_row_group_first th {
  border-top-width: 2px;
}

#syqsjbjykt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#syqsjbjykt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#syqsjbjykt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#syqsjbjykt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#syqsjbjykt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#syqsjbjykt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#syqsjbjykt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#syqsjbjykt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#syqsjbjykt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#syqsjbjykt .gt_footnotes {
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

#syqsjbjykt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#syqsjbjykt .gt_sourcenotes {
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

#syqsjbjykt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#syqsjbjykt .gt_left {
  text-align: left;
}

#syqsjbjykt .gt_center {
  text-align: center;
}

#syqsjbjykt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#syqsjbjykt .gt_font_normal {
  font-weight: normal;
}

#syqsjbjykt .gt_font_bold {
  font-weight: bold;
}

#syqsjbjykt .gt_font_italic {
  font-style: italic;
}

#syqsjbjykt .gt_super {
  font-size: 65%;
}

#syqsjbjykt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#syqsjbjykt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#syqsjbjykt .gt_indent_1 {
  text-indent: 5px;
}

#syqsjbjykt .gt_indent_2 {
  text-indent: 10px;
}

#syqsjbjykt .gt_indent_3 {
  text-indent: 15px;
}

#syqsjbjykt .gt_indent_4 {
  text-indent: 20px;
}

#syqsjbjykt .gt_indent_5 {
  text-indent: 25px;
}

#syqsjbjykt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#syqsjbjykt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Race (Hispanic) Predicting Life Satisfaction</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'>Estimate (<em>se</em>)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="pval"><span class='gt_from_md'><em>p</em>-value</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_center gt_stub">Hispanic</th>
<td headers="stub_1_1 estimate" class="gt_row gt_center">0.365 (0.72)</td>
<td headers="stub_1_1 pval" class="gt_row gt_center">0.614</td></tr>
  </tbody>
  
  
</table>
</div>
```



Following the three-step procedure, the logit values of the classification probabilities for the four-class solution for the first step were copied to be used in the third step. We used these values in a second set of analyses to fix the measurement parameters of the latent classes, and the auxiliary variables relations were estimated thereafter (see Appendix B). We used multinomial logistic regression to evaluate whether the relative proportions of Hispanic and non-Hispanic youth were equal across the four classes, and the results are reported in Table 5. Notably, Hispanic youth (versus nonHispanic) were more likely to be in the Holistic–Collaborative class compared with both the Altruistic–Low Self-efficacy class (OR 2.71, p .001) and the Holistic Independent class (OR 1.62, p .009). No other covariate-class relations were statistically significant. Simultaneously, we estimated classspecific means of life satisfaction for each of the four classes. For the interpretation of distal outcomes, we centered the covariate such that the distal outcome mean differences across the latent classes accounted for the relative proportion of Hispanic versus non-Hispanic youth in the entire sample. Pairwise Wald tests revealed that life satisfaction in the Holistic-Collaborative class was significantly higher compared to each of the other three classes. No other distal mean comparisons were statistically significant.

------------------------------------------------------------------------

## References

Arnold, M. E., Nott, B. D., & Meinhold, J. L. (2012). The Positive Youth Development Inventory full version. Corvallis: Oregon State University.

Lerner, R. M., Lerner, J. V., Almerigi, J. B., Theokas, C., Phelps, E., Gestsdottir, S., . . . von Eye, A. (2005). Positive youth development, participation in community youth development programs, and community contributions of fifth-grade adolescents: Findings from the first wave of the 4-H Study of Positive Youth Development. The Journal of Early Adolescence, 25, 17–71. http://dx.doi .org/10.1177/0272431604272461

Nylund, K. L., Asparouhov, T., & Muthén, B. O. (2007). Deciding on the number of classes in latent class analysis and growth mixture modeling: A Monte Carlo simulation study. Structural Equation Modeling, 14, 535–569. http://dx.doi.org/10.1080/ 10705510701575396

------------------------------------------------------------------------
