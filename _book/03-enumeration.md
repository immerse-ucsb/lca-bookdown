



# LCA Enumeration

------------------------------------------------------------------------

Example: Bullying in Schools

------------------------------------------------------------------------

To demonstrate mixture modeling in the training program and online resource components of the IES grant we utilize the *Civil Rights Data Collection (CRDC)*[@usdoe2014] data repository.
The CRDC is a federally mandated school-level data collection effort that occurs every other year.
This public data is currently available for selected latent class indicators across 4 years (2011, 2013, 2015, 2017) and all US states.
In this example, we use the Arizona state sample.
We utilize six focal indicators which constitute the latent class model in our example; three variables which report on harassment/bullying in schools based on disability, race, or sex, and three variables on full-time equivalent school staff hires (counselor, psychologist, law enforcement).
This data source also includes covariates on a variety of subjects and distal outcomes reported in 2018 such as math/reading assessments and graduation rates.

------------------------------------------------------------------------

Load packages


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
```

## Variable Description


```{=html}
<div id="yawffewizm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#yawffewizm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#yawffewizm thead, #yawffewizm tbody, #yawffewizm tfoot, #yawffewizm tr, #yawffewizm td, #yawffewizm th {
  border-style: none;
}

#yawffewizm p {
  margin: 0;
  padding: 0;
}

#yawffewizm .gt_table {
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
  width: 75%;
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

#yawffewizm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#yawffewizm .gt_title {
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

#yawffewizm .gt_subtitle {
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

#yawffewizm .gt_heading {
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

#yawffewizm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yawffewizm .gt_col_headings {
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

#yawffewizm .gt_col_heading {
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

#yawffewizm .gt_column_spanner_outer {
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

#yawffewizm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yawffewizm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yawffewizm .gt_column_spanner {
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

#yawffewizm .gt_spanner_row {
  border-bottom-style: hidden;
}

#yawffewizm .gt_group_heading {
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

#yawffewizm .gt_empty_group_heading {
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

#yawffewizm .gt_from_md > :first-child {
  margin-top: 0;
}

#yawffewizm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yawffewizm .gt_row {
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

#yawffewizm .gt_stub {
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

#yawffewizm .gt_stub_row_group {
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

#yawffewizm .gt_row_group_first td {
  border-top-width: 2px;
}

#yawffewizm .gt_row_group_first th {
  border-top-width: 2px;
}

#yawffewizm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yawffewizm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#yawffewizm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#yawffewizm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yawffewizm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yawffewizm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yawffewizm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#yawffewizm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yawffewizm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yawffewizm .gt_footnotes {
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

#yawffewizm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yawffewizm .gt_sourcenotes {
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

#yawffewizm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yawffewizm .gt_left {
  text-align: left;
}

#yawffewizm .gt_center {
  text-align: center;
}

#yawffewizm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yawffewizm .gt_font_normal {
  font-weight: normal;
}

#yawffewizm .gt_font_bold {
  font-weight: bold;
}

#yawffewizm .gt_font_italic {
  font-style: italic;
}

#yawffewizm .gt_super {
  font-size: 65%;
}

#yawffewizm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#yawffewizm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#yawffewizm .gt_indent_1 {
  text-indent: 5px;
}

#yawffewizm .gt_indent_2 {
  text-indent: 10px;
}

#yawffewizm .gt_indent_3 {
  text-indent: 15px;
}

#yawffewizm .gt_indent_4 {
  text-indent: 20px;
}

#yawffewizm .gt_indent_5 {
  text-indent: 25px;
}

#yawffewizm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#yawffewizm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>LCA indicators<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Name">Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Label">Label</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Values">Values</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Name" class="gt_row gt_left">leaid</td>
<td headers="Label" class="gt_row gt_left">District Identification Code</td>
<td headers="Values" class="gt_row gt_left"></td></tr>
    <tr><td headers="Name" class="gt_row gt_left">ncessch</td>
<td headers="Label" class="gt_row gt_left">School Identification Code</td>
<td headers="Values" class="gt_row gt_left"></td></tr>
    <tr><td headers="Name" class="gt_row gt_left">report_dis</td>
<td headers="Label" class="gt_row gt_left">Number of students harassed or bullied on the basis of disability</td>
<td headers="Values" class="gt_row gt_left">0 = No reported incidents, 1 = At least one reported incident</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">report_race</td>
<td headers="Label" class="gt_row gt_left">Number of students harassed or bullied on the basis of race, color, or national origin</td>
<td headers="Values" class="gt_row gt_left">0 = No reported incidents, 1 = At least one reported incident</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">report_sex</td>
<td headers="Label" class="gt_row gt_left">Number of students harassed or bullied on the basis of sex</td>
<td headers="Values" class="gt_row gt_left">0 = No reported incidents, 1 = At least one reported incident</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">counselors_fte</td>
<td headers="Label" class="gt_row gt_left">Number of full time equivalent counselors hired as school staff</td>
<td headers="Values" class="gt_row gt_left">0 = No staff present, 1 = At least one staff present</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">psych_fte</td>
<td headers="Label" class="gt_row gt_left">Number of full time equivalent psychologists hired as school staff</td>
<td headers="Values" class="gt_row gt_left">0 = No staff present, 1 = At least one staff present</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">law_fte</td>
<td headers="Label" class="gt_row gt_left">Number of full time equivalent law enforcement officers hired as school staff</td>
<td headers="Values" class="gt_row gt_left">0 = No staff present, 1 = At least one staff present</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> Civil Rights Data Collection (CRDC)</td>
    </tr>
  </tfoot>
</table>
</div>
```



------------------------------------------------------------------------

**Variables have been transformed to be dichotomous indicators using the following coding strategy**

Harassment and bullying count variables are recoded `1` if the school reported at least one incident of harassment (`0` indicates no reported incidents).
On the original scale reported by the CDRC staff variables for full time equivalent employees (FTE) are represented as `1` and part time employees are represented by values between `1` and `0`.
Schools with greater than one staff of the designated type are represented by values greater than 1.
All values greater than zero were recorded as `1s` (e.g., `.5`, `1`,`3`) indicating that the school has a staff present on campus at least part time.
Schools with no staff of the designated type are indicated as `0` for the dichotomous variable.

------------------------------------------------------------------------


```{=html}
<div class="grViz html-widget html-fill-item" id="htmlwidget-bf1e360c156ae29c9f5c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-bf1e360c156ae29c9f5c">{"x":{"diagram":" digraph cfa_model {\n\n# The `graph` statement - No editing needed\n\n    graph [layout = dot, overlap = true]\n \n# Two `node` statements\n \n# One for measured variables (box) \n\n    node [shape=box]\n    report_dis report_race report_sex counselors_fte psych_fte law_fte;\n \n# One for latent variables (circle) \n \n    node [shape=circle]\n    bully [label=<Bullying <br/>C<sub>k<\/sub>>];\n    \n# `edge` statements\n \n    edge [minlen = 2]\n    bully -> {report_dis report_race report_sex counselors_fte psych_fte law_fte}\n \n }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```


------------------------------------------------------------------------

## Prepare Data


``` r
df_bully <- read_csv(here("data", "crdc_lca_data.csv")) %>% 
  clean_names() %>% 
  dplyr::select(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte) 
```

------------------------------------------------------------------------

## Descriptive Statistics


``` r
# Set up data to find proportions of binary indicators
ds <- df_bully %>% 
  pivot_longer(c(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte), names_to = "variable") 


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
<div id="wmvkczemty" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wmvkczemty table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wmvkczemty thead, #wmvkczemty tbody, #wmvkczemty tfoot, #wmvkczemty tr, #wmvkczemty td, #wmvkczemty th {
  border-style: none;
}

#wmvkczemty p {
  margin: 0;
  padding: 0;
}

#wmvkczemty .gt_table {
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

#wmvkczemty .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wmvkczemty .gt_title {
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

#wmvkczemty .gt_subtitle {
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

#wmvkczemty .gt_heading {
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

#wmvkczemty .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmvkczemty .gt_col_headings {
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

#wmvkczemty .gt_col_heading {
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

#wmvkczemty .gt_column_spanner_outer {
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

#wmvkczemty .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wmvkczemty .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wmvkczemty .gt_column_spanner {
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

#wmvkczemty .gt_spanner_row {
  border-bottom-style: hidden;
}

#wmvkczemty .gt_group_heading {
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

#wmvkczemty .gt_empty_group_heading {
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

#wmvkczemty .gt_from_md > :first-child {
  margin-top: 0;
}

#wmvkczemty .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wmvkczemty .gt_row {
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

#wmvkczemty .gt_stub {
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

#wmvkczemty .gt_stub_row_group {
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

#wmvkczemty .gt_row_group_first td {
  border-top-width: 2px;
}

#wmvkczemty .gt_row_group_first th {
  border-top-width: 2px;
}

#wmvkczemty .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmvkczemty .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wmvkczemty .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wmvkczemty .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmvkczemty .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmvkczemty .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wmvkczemty .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wmvkczemty .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wmvkczemty .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmvkczemty .gt_footnotes {
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

#wmvkczemty .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmvkczemty .gt_sourcenotes {
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

#wmvkczemty .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmvkczemty .gt_left {
  text-align: left;
}

#wmvkczemty .gt_center {
  text-align: center;
}

#wmvkczemty .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wmvkczemty .gt_font_normal {
  font-weight: normal;
}

#wmvkczemty .gt_font_bold {
  font-weight: bold;
}

#wmvkczemty .gt_font_italic {
  font-style: italic;
}

#wmvkczemty .gt_super {
  font-size: 65%;
}

#wmvkczemty .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wmvkczemty .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wmvkczemty .gt_indent_1 {
  text-indent: 5px;
}

#wmvkczemty .gt_indent_2 {
  text-indent: 10px;
}

#wmvkczemty .gt_indent_3 {
  text-indent: 15px;
}

#wmvkczemty .gt_indent_4 {
  text-indent: 20px;
}

#wmvkczemty .gt_indent_5 {
  text-indent: 25px;
}

#wmvkczemty .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#wmvkczemty div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="counselors_fte">counselors_fte</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="counselors_fte stub_1_1 n" class="gt_row gt_right">1081</td>
<td headers="counselors_fte stub_1_1 prop" class="gt_row gt_right">0.533</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="counselors_fte stub_1_2 n" class="gt_row gt_right">919</td>
<td headers="counselors_fte stub_1_2 prop" class="gt_row gt_right">0.453</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="counselors_fte stub_1_3 n" class="gt_row gt_right">27</td>
<td headers="counselors_fte stub_1_3 prop" class="gt_row gt_right">0.013</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="law_fte">law_fte</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_4" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="law_fte stub_1_4 n" class="gt_row gt_right">1749</td>
<td headers="law_fte stub_1_4 prop" class="gt_row gt_right">0.863</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="law_fte stub_1_5 n" class="gt_row gt_right">251</td>
<td headers="law_fte stub_1_5 prop" class="gt_row gt_right">0.124</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="law_fte stub_1_6 n" class="gt_row gt_right">27</td>
<td headers="law_fte stub_1_6 prop" class="gt_row gt_right">0.013</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="psych_fte">psych_fte</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_7" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="psych_fte stub_1_7 n" class="gt_row gt_right">1050</td>
<td headers="psych_fte stub_1_7 prop" class="gt_row gt_right">0.518</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="psych_fte stub_1_8 n" class="gt_row gt_right">947</td>
<td headers="psych_fte stub_1_8 prop" class="gt_row gt_right">0.467</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="psych_fte stub_1_9 n" class="gt_row gt_right">30</td>
<td headers="psych_fte stub_1_9 prop" class="gt_row gt_right">0.015</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="report_dis">report_dis</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_10" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="report_dis stub_1_10 n" class="gt_row gt_right">1915</td>
<td headers="report_dis stub_1_10 prop" class="gt_row gt_right">0.945</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="report_dis stub_1_11 n" class="gt_row gt_right">85</td>
<td headers="report_dis stub_1_11 prop" class="gt_row gt_right">0.042</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="report_dis stub_1_12 n" class="gt_row gt_right">27</td>
<td headers="report_dis stub_1_12 prop" class="gt_row gt_right">0.013</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="report_race">report_race</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_13" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="report_race stub_1_13 n" class="gt_row gt_right">1794</td>
<td headers="report_race stub_1_13 prop" class="gt_row gt_right">0.885</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="report_race stub_1_14 n" class="gt_row gt_right">206</td>
<td headers="report_race stub_1_14 prop" class="gt_row gt_right">0.102</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="report_race stub_1_15 n" class="gt_row gt_right">27</td>
<td headers="report_race stub_1_15 prop" class="gt_row gt_right">0.013</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="report_sex">report_sex</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_16" scope="row" class="gt_row gt_right gt_stub">0</th>
<td headers="report_sex stub_1_16 n" class="gt_row gt_right">1660</td>
<td headers="report_sex stub_1_16 prop" class="gt_row gt_right">0.819</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_right gt_stub">1</th>
<td headers="report_sex stub_1_17 n" class="gt_row gt_right">340</td>
<td headers="report_sex stub_1_17 prop" class="gt_row gt_right">0.168</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_right gt_stub">NA</th>
<td headers="report_sex stub_1_18 n" class="gt_row gt_right">27</td>
<td headers="report_sex stub_1_18 prop" class="gt_row gt_right">0.013</td></tr>
  </tbody>
  
  
</table>
</div>
```


Save as image


``` r
gtsave(prop_table, here("figures", "prop_table.png"))
```

------------------------------------------------------------------------

## Enumeration

This code uses the `mplusObject` function in the `MplusAutomation` package and saves all model runs in the `enum` folder.


``` r

lca_6  <- lapply(1:6, function(k) {
  lca_enum  <- mplusObject(
      
    TITLE = glue("{k}-Class"), 
  
    VARIABLE = glue(
    "categorical = report_dis-law_fte; 
     usevar = report_dis-law_fte;
     classes = c({k}); "),
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 200 100; 
    processors = 10;",
  
  OUTPUT = "sampstat residual tech11 tech14;",
  
  PLOT = 
    "type = plot3; 
    series = report_dis-law_fte(*);",
  
  usevariables = colnames(df_bully),
  rdata = df_bully)

lca_enum_fit <- mplusModeler(lca_enum, 
                            dataout=glue(here("enum", "bully.dat")),
                            modelout=glue(here("enum", "c{k}_bully.inp")) ,
                            check=TRUE, run = TRUE, hashfilename = FALSE)
})

```

**IMPORTANT**: Before moving forward, make sure to open each output document to ensure models were estimated normally. 

------------------------------------------------------------------------

## Table of Fit

First, extract data:


``` r
# 
output_bully <- readModels(here("enum"), filefilter = "bully", quiet = TRUE)

enum_extract <- LatexSummaryTable(
  output_bully,
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

Then, create table:


``` r
fit_table1 <- allFit %>%
  gt() %>%
  tab_header(title = md("**Model Fit Summary Table**")) %>%
  cols_label(
    Title = "Classes",
    Parameters = md("Par"),
    LL = md("*LL*"),
    T11_VLMR_PValue = "VLMR",
    BLRT_PValue = "BLRT",
    BF = md("BF"),
    cmPk = md("*cmPk*")
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
  fmt_number(c(3:7),
             decimals = 2) %>%
  sub_missing(1:11,
              missing_text = "--") %>%
  fmt(
    c(8:9, 11),
    fns = function(x)
      ifelse(x < 0.001, "<.001",
             scales::number(x, accuracy = .01))
  ) %>%
  fmt(
    10,
    fns = function (x)
      ifelse(x > 100, ">100",
             scales::number(x, accuracy = .01))
  ) %>%  
  tab_style(
    style = list(
      cell_text(weight = "bold")
      ),
    locations = list(cells_body(
     columns = BIC,
     row = BIC == min(BIC[c(1:6)]) # Change this to the number of classes you are evaluating
    ),
    cells_body(
     columns = aBIC,
     row = aBIC == min(aBIC[1:6])
    ),
    cells_body(
     columns = CAIC,
     row = CAIC == min(CAIC[1:6])
    ),
    cells_body(
     columns = AWE,
     row = AWE == min(AWE[1:6])
    ),
    cells_body(
     columns = cmPk,
     row =  cmPk == max(cmPk[1:6])
     ),    
    cells_body(
     columns = BF,
     row =  BF > 10),
    cells_body( 
     columns =  T11_VLMR_PValue,
     row =  ifelse(T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue) > .05, T11_VLMR_PValue < .05, NA)),
    cells_body(
     columns =  BLRT_PValue,
     row =  ifelse(BLRT_PValue < .05 & lead(BLRT_PValue) > .05, BLRT_PValue < .05, NA))
  )
)

fit_table1
```


```{=html}
<div id="opfpcwkfjq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#opfpcwkfjq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#opfpcwkfjq thead, #opfpcwkfjq tbody, #opfpcwkfjq tfoot, #opfpcwkfjq tr, #opfpcwkfjq td, #opfpcwkfjq th {
  border-style: none;
}

#opfpcwkfjq p {
  margin: 0;
  padding: 0;
}

#opfpcwkfjq .gt_table {
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

#opfpcwkfjq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#opfpcwkfjq .gt_title {
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

#opfpcwkfjq .gt_subtitle {
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

#opfpcwkfjq .gt_heading {
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

#opfpcwkfjq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opfpcwkfjq .gt_col_headings {
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

#opfpcwkfjq .gt_col_heading {
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

#opfpcwkfjq .gt_column_spanner_outer {
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

#opfpcwkfjq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#opfpcwkfjq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#opfpcwkfjq .gt_column_spanner {
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

#opfpcwkfjq .gt_spanner_row {
  border-bottom-style: hidden;
}

#opfpcwkfjq .gt_group_heading {
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

#opfpcwkfjq .gt_empty_group_heading {
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

#opfpcwkfjq .gt_from_md > :first-child {
  margin-top: 0;
}

#opfpcwkfjq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#opfpcwkfjq .gt_row {
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

#opfpcwkfjq .gt_stub {
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

#opfpcwkfjq .gt_stub_row_group {
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

#opfpcwkfjq .gt_row_group_first td {
  border-top-width: 2px;
}

#opfpcwkfjq .gt_row_group_first th {
  border-top-width: 2px;
}

#opfpcwkfjq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfpcwkfjq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#opfpcwkfjq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#opfpcwkfjq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opfpcwkfjq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfpcwkfjq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#opfpcwkfjq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#opfpcwkfjq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#opfpcwkfjq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#opfpcwkfjq .gt_footnotes {
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

#opfpcwkfjq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfpcwkfjq .gt_sourcenotes {
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

#opfpcwkfjq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#opfpcwkfjq .gt_left {
  text-align: left;
}

#opfpcwkfjq .gt_center {
  text-align: center;
}

#opfpcwkfjq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#opfpcwkfjq .gt_font_normal {
  font-weight: normal;
}

#opfpcwkfjq .gt_font_bold {
  font-weight: bold;
}

#opfpcwkfjq .gt_font_italic {
  font-style: italic;
}

#opfpcwkfjq .gt_super {
  font-size: 65%;
}

#opfpcwkfjq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#opfpcwkfjq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#opfpcwkfjq .gt_indent_1 {
  text-indent: 5px;
}

#opfpcwkfjq .gt_indent_2 {
  text-indent: 10px;
}

#opfpcwkfjq .gt_indent_3 {
  text-indent: 15px;
}

#opfpcwkfjq .gt_indent_4 {
  text-indent: 20px;
}

#opfpcwkfjq .gt_indent_5 {
  text-indent: 25px;
}

#opfpcwkfjq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#opfpcwkfjq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Fit Summary Table</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Title">Classes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>Par</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="LL"><span class='gt_from_md'><em>LL</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="BIC">BIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aBIC">aBIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="CAIC">CAIC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="AWE">AWE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="BLRT_PValue">BLRT</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="T11_VLMR_PValue">VLMR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="BF"><span class='gt_from_md'>BF</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="cmPk"><span class='gt_from_md'><em>cmPk</em></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Title" class="gt_row gt_left">1-Class</td>
<td headers="Parameters" class="gt_row gt_right">6</td>
<td headers="LL" class="gt_row gt_right">−5,443.41</td>
<td headers="BIC" class="gt_row gt_right">10,932.50</td>
<td headers="aBIC" class="gt_row gt_right">10,913.44</td>
<td headers="CAIC" class="gt_row gt_right">10,938.50</td>
<td headers="AWE" class="gt_row gt_right">10,996.19</td>
<td headers="BLRT_PValue" class="gt_row gt_right">–</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right">–</td>
<td headers="BF" class="gt_row gt_right">0.00</td>
<td headers="cmPk" class="gt_row gt_right"><.001</td></tr>
    <tr><td headers="Title" class="gt_row gt_left">2-Class</td>
<td headers="Parameters" class="gt_row gt_right">13</td>
<td headers="LL" class="gt_row gt_right">−5,194.14</td>
<td headers="BIC" class="gt_row gt_right">10,487.26</td>
<td headers="aBIC" class="gt_row gt_right">10,445.96</td>
<td headers="CAIC" class="gt_row gt_right">10,500.26</td>
<td headers="AWE" class="gt_row gt_right">10,625.24</td>
<td headers="BLRT_PValue" class="gt_row gt_right"><.001</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right"><.001</td>
<td headers="BF" class="gt_row gt_right">0.00</td>
<td headers="cmPk" class="gt_row gt_right"><.001</td></tr>
    <tr><td headers="Title" class="gt_row gt_left">3-Class</td>
<td headers="Parameters" class="gt_row gt_right">20</td>
<td headers="LL" class="gt_row gt_right">−5,122.48</td>
<td headers="BIC" class="gt_row gt_right" style="font-weight: bold;">10,397.24</td>
<td headers="aBIC" class="gt_row gt_right" style="font-weight: bold;">10,333.70</td>
<td headers="CAIC" class="gt_row gt_right" style="font-weight: bold;">10,417.24</td>
<td headers="AWE" class="gt_row gt_right" style="font-weight: bold;">10,609.53</td>
<td headers="BLRT_PValue" class="gt_row gt_right"><.001</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right"><.001</td>
<td headers="BF" class="gt_row gt_right" style="font-weight: bold;">>100</td>
<td headers="cmPk" class="gt_row gt_right" style="font-weight: bold;">1.00</td></tr>
    <tr><td headers="Title" class="gt_row gt_left">4-Class</td>
<td headers="Parameters" class="gt_row gt_right">27</td>
<td headers="LL" class="gt_row gt_right">−5,111.76</td>
<td headers="BIC" class="gt_row gt_right">10,429.10</td>
<td headers="aBIC" class="gt_row gt_right">10,343.32</td>
<td headers="CAIC" class="gt_row gt_right">10,456.10</td>
<td headers="AWE" class="gt_row gt_right">10,715.69</td>
<td headers="BLRT_PValue" class="gt_row gt_right" style="font-weight: bold;"><.001</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right" style="font-weight: bold;">0.01</td>
<td headers="BF" class="gt_row gt_right" style="font-weight: bold;">>100</td>
<td headers="cmPk" class="gt_row gt_right"><.001</td></tr>
    <tr><td headers="Title" class="gt_row gt_left">5-Class</td>
<td headers="Parameters" class="gt_row gt_right">34</td>
<td headers="LL" class="gt_row gt_right">−5,105.59</td>
<td headers="BIC" class="gt_row gt_right">10,470.07</td>
<td headers="aBIC" class="gt_row gt_right">10,362.04</td>
<td headers="CAIC" class="gt_row gt_right">10,504.06</td>
<td headers="AWE" class="gt_row gt_right">10,830.95</td>
<td headers="BLRT_PValue" class="gt_row gt_right">0.29</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right">0.18</td>
<td headers="BF" class="gt_row gt_right" style="font-weight: bold;">>100</td>
<td headers="cmPk" class="gt_row gt_right"><.001</td></tr>
    <tr><td headers="Title" class="gt_row gt_left">6-Class</td>
<td headers="Parameters" class="gt_row gt_right">41</td>
<td headers="LL" class="gt_row gt_right">−5,099.88</td>
<td headers="BIC" class="gt_row gt_right">10,511.95</td>
<td headers="aBIC" class="gt_row gt_right">10,381.69</td>
<td headers="CAIC" class="gt_row gt_right">10,552.95</td>
<td headers="AWE" class="gt_row gt_right">10,947.14</td>
<td headers="BLRT_PValue" class="gt_row gt_right">0.38</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_right">0.18</td>
<td headers="BF" class="gt_row gt_right">–</td>
<td headers="cmPk" class="gt_row gt_right"><.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="11"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> Par = Parameters; <em>LL</em> = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
<em>cmPk</em> = approximate correct model probability.</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


------------------------------------------------------------------------

Save table


``` r
gtsave(fit_table1, here("figures", "fit_table.png"))
```

------------------------------------------------------------------------

## Information Criteria Plot


``` r
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
  geom_point(size = 2.0) + geom_line(linewidth = .8) +
  scale_x_continuous(breaks = 1:nrow(allFit)) +
  scale_colour_grey(end = .5) +
  theme_cowplot() +
  labs(x = "Number of Classes", y = "Information Criteria Value", title = "Information Criteria") +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.text = element_text(family="serif", size=12),
    legend.key.width = unit(3, "line"),
    legend.title = element_blank(),
    legend.position = "top"  
  )
```

<img src="03-enumeration_files/figure-html/unnamed-chunk-11-1.png" width="672" />

------------------------------------------------------------------------

Save figure


``` r
ggsave(here("figures", "info_criteria.png"), dpi=300, height=5, width=7, units="in")
```

------------------------------------------------------------------------

## Compare Class Solutions

Compare probability plots for $K = 1:6$ class solutions


``` r
model_results <- data.frame()

for (i in 1:length(output_bully)) {
  
  temp <- output_bully[[i]]$parameters$probability.scale %>%                                       
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

<img src="03-enumeration_files/figure-html/unnamed-chunk-13-1.png" width="672" />

------------------------------------------------------------------------

Save figure:


``` r
ggsave(here("figures", "compare_kclass_plot.png"), dpi=300, height=5, width=7, units="in")
```

------------------------------------------------------------------------

## 3-Class Probability Plot

Use the `plot_lca` function provided in the folder to plot the item probability plot. This function requires one argument:
- `model_name`: The name of the Mplus `readModels` object (e.g., `output_bully$c3_bully.out`)


``` r
source("plot_lca.txt")

plot_lca(model_name = output_bully$c3_bully.out)
```

<img src="03-enumeration_files/figure-html/unnamed-chunk-15-1.png" width="672" />

------------------------------------------------------------------------

Save figure:


``` r
ggsave(here("figures", "C3_bully_LCA_Plot.png"), dpi="retina", height=5, width=7, units="in")
```

------------------------------------------------------------------------

## Observed Response Patterns

Save response frequencies for the 3-class model from the previous lab with `response is _____.dat` under `SAVEDATA.`


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
<div id="nayjplddhn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nayjplddhn table {
  font-family: 'Times New Roman';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nayjplddhn thead, #nayjplddhn tbody, #nayjplddhn tfoot, #nayjplddhn tr, #nayjplddhn td, #nayjplddhn th {
  border-style: none;
}

#nayjplddhn p {
  margin: 0;
  padding: 0;
}

#nayjplddhn .gt_table {
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

#nayjplddhn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nayjplddhn .gt_title {
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

#nayjplddhn .gt_subtitle {
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

#nayjplddhn .gt_heading {
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

#nayjplddhn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nayjplddhn .gt_col_headings {
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

#nayjplddhn .gt_col_heading {
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

#nayjplddhn .gt_column_spanner_outer {
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

#nayjplddhn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nayjplddhn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nayjplddhn .gt_column_spanner {
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

#nayjplddhn .gt_spanner_row {
  border-bottom-style: hidden;
}

#nayjplddhn .gt_group_heading {
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

#nayjplddhn .gt_empty_group_heading {
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

#nayjplddhn .gt_from_md > :first-child {
  margin-top: 0;
}

#nayjplddhn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nayjplddhn .gt_row {
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

#nayjplddhn .gt_stub {
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

#nayjplddhn .gt_stub_row_group {
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

#nayjplddhn .gt_row_group_first td {
  border-top-width: 2px;
}

#nayjplddhn .gt_row_group_first th {
  border-top-width: 2px;
}

#nayjplddhn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nayjplddhn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nayjplddhn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nayjplddhn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nayjplddhn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nayjplddhn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nayjplddhn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#nayjplddhn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nayjplddhn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nayjplddhn .gt_footnotes {
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

#nayjplddhn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nayjplddhn .gt_sourcenotes {
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

#nayjplddhn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nayjplddhn .gt_left {
  text-align: left;
}

#nayjplddhn .gt_center {
  text-align: center;
}

#nayjplddhn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nayjplddhn .gt_font_normal {
  font-weight: normal;
}

#nayjplddhn .gt_font_bold {
  font-weight: bold;
}

#nayjplddhn .gt_font_italic {
  font-style: italic;
}

#nayjplddhn .gt_super {
  font-size: 65%;
}

#nayjplddhn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#nayjplddhn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nayjplddhn .gt_indent_1 {
  text-indent: 5px;
}

#nayjplddhn .gt_indent_2 {
  text-indent: 10px;
}

#nayjplddhn .gt_indent_3 {
  text-indent: 15px;
}

#nayjplddhn .gt_indent_4 {
  text-indent: 20px;
}

#nayjplddhn .gt_indent_5 {
  text-indent: 25px;
}

#nayjplddhn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#nayjplddhn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

------------------------------------------------------------------------

## Classification Diagnostics

Use Mplus to calculate k-class confidence intervals (Note: Change the synax to make your chosen *k*-class model):


``` r
classification  <- mplusObject(
  
  TITLE = "C3 LCA - Calculated k-Class 95% CI",
  
  VARIABLE =
    "categorical = report_dis-law_fte;
   usevar =  report_dis-law_fte;
   classes = c(3);", 
  
  ANALYSIS =
    "estimator = ml;
    type = mixture;
    starts = 0; 
    processors = 10;
    optseed = 802779;
    bootstrap = 1000;",
  
  MODEL =
    "
  !CHANGE THIS SECTION TO YOUR CHOSEN k-CLASS MODEL
    
  %OVERALL%
  [C#1](c1);
  
  [C#2](C2);

  Model Constraint:
  New(p1 p2 p3);
  
  p1 = exp(c1)/(1+exp(c1)+exp(c2));
  p2 = exp(c2)/(1+exp(c1)+exp(c2));
  p3 = 1/(1+exp(c1)+exp(c2));",

  
  OUTPUT = "cinterval(bcbootstrap)",
  
  usevariables = colnames(df_bully),
  rdata = df_bully)

classification_fit <- mplusModeler(classification,
                dataout=here("mplus", "bully.dat"),
                modelout=here("mplus", "class.inp") ,
                check=TRUE, run = TRUE, hashfilename = FALSE)
```

*Note*: Ensure that the classes did not shift during this step (i.g., Class 1 in the enumeration run is now Class 4). Evaluate output and compare the class counts and proportions for the latent classes. Using the OPTSEED function ensures replication of the best loglikelihood value run.

------------------------------------------------------------------------

Read in the 3-class model:


``` r
# Read in the 3-class model and extract information needed
output_bully <- readModels(here("mplus", "class.out"))

# Entropy
entropy <- c(output_bully$summaries$Entropy, rep(NA, output_bully$summaries$NLatentClasses-1))

# 95% k-Class and k-class 95% Confidence Intervals
k_ci <- output_bully$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters") %>% 
  unite(CI, c(low2.5,up2.5), sep=", ", remove = TRUE) %>% 
  mutate(CI = paste0("[", CI, "]")) %>% 
  rename(kclass=est) %>% 
  dplyr::select(kclass, CI)

# AvePPk = Average Latent Class Probabilities for Most Likely Latent Class Membership (Row) by Latent Class (Column)
avePPk <- tibble(avePPk = diag(output_bully$class_counts$avgProbs.mostLikely))

# mcaPk = modal class assignment proportion 
mcaPk <- round(output_bully$class_counts$mostLikely,3) %>% 
  mutate(model = paste0("Class ", class)) %>% 
  add_column(avePPk, k_ci) %>% 
  rename(mcaPk = proportion) %>% 
  dplyr::select(model, kclass, CI, mcaPk, avePPk)

# OCCk = odds of correct classification
OCCk <- mcaPk %>% 
  mutate(OCCk = round((avePPk/(1-avePPk))/(kclass/(1-kclass)),3))

# Put everything together
class_table <- data.frame(OCCk, entropy)
```

Now, use `{gt}` to make a nicely formatted table


``` r
class_table <- class_table %>% 
  gt() %>%
    tab_header(
    title = "Model Classification Diagnostics for the 3-Class Solution") %>%
    cols_label(
      model = md("*k*-Class"),
      kclass = md("*k*-Class Proportions"),
      CI = "95% CI",
      mcaPk = html("McaP<sub>k</sub>"),
      avePPk = md("AvePP<sub>k</sub>"),
      OCCk = md("OCC<sub>k</sub>"),
      entropy = "Entropy") %>% 
    sub_missing(7,
              missing_text = "") %>%
    tab_footnote(
    footnote = html(
      "<i>Note.</i> McaP<sub>k</sub> = Modal class assignment proportion; AvePP<sub>k</sub> = Average posterior class probabilities; OCC<sub>k</sub> = Odds of correct classification; "
    )
  ) %>% 
  cols_align(align = "center") %>% 
  opt_align_table_header(align = "left") %>% 
  gt::tab_options(table.font.names = "Times New Roman")

class_table
```


```{=html}
<div id="rjraimluja" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rjraimluja table {
  font-family: 'Times New Roman';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rjraimluja thead, #rjraimluja tbody, #rjraimluja tfoot, #rjraimluja tr, #rjraimluja td, #rjraimluja th {
  border-style: none;
}

#rjraimluja p {
  margin: 0;
  padding: 0;
}

#rjraimluja .gt_table {
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

#rjraimluja .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rjraimluja .gt_title {
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

#rjraimluja .gt_subtitle {
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

#rjraimluja .gt_heading {
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

#rjraimluja .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjraimluja .gt_col_headings {
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

#rjraimluja .gt_col_heading {
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

#rjraimluja .gt_column_spanner_outer {
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

#rjraimluja .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rjraimluja .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rjraimluja .gt_column_spanner {
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

#rjraimluja .gt_spanner_row {
  border-bottom-style: hidden;
}

#rjraimluja .gt_group_heading {
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

#rjraimluja .gt_empty_group_heading {
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

#rjraimluja .gt_from_md > :first-child {
  margin-top: 0;
}

#rjraimluja .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rjraimluja .gt_row {
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

#rjraimluja .gt_stub {
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

#rjraimluja .gt_stub_row_group {
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

#rjraimluja .gt_row_group_first td {
  border-top-width: 2px;
}

#rjraimluja .gt_row_group_first th {
  border-top-width: 2px;
}

#rjraimluja .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjraimluja .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rjraimluja .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rjraimluja .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjraimluja .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjraimluja .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rjraimluja .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rjraimluja .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rjraimluja .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rjraimluja .gt_footnotes {
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

#rjraimluja .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjraimluja .gt_sourcenotes {
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

#rjraimluja .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rjraimluja .gt_left {
  text-align: left;
}

#rjraimluja .gt_center {
  text-align: center;
}

#rjraimluja .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rjraimluja .gt_font_normal {
  font-weight: normal;
}

#rjraimluja .gt_font_bold {
  font-weight: bold;
}

#rjraimluja .gt_font_italic {
  font-style: italic;
}

#rjraimluja .gt_super {
  font-size: 65%;
}

#rjraimluja .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rjraimluja .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rjraimluja .gt_indent_1 {
  text-indent: 5px;
}

#rjraimluja .gt_indent_2 {
  text-indent: 10px;
}

#rjraimluja .gt_indent_3 {
  text-indent: 15px;
}

#rjraimluja .gt_indent_4 {
  text-indent: 20px;
}

#rjraimluja .gt_indent_5 {
  text-indent: 25px;
}

#rjraimluja .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rjraimluja div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Model Classification Diagnostics for the 3-Class Solution</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="model"><span class='gt_from_md'><em>k</em>-Class</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="kclass"><span class='gt_from_md'><em>k</em>-Class Proportions</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="mcaPk">McaP<sub>k</sub></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="avePPk"><span class='gt_from_md'>AvePP<sub>k</sub></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="OCCk"><span class='gt_from_md'>OCC<sub>k</sub></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="entropy">Entropy</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="model" class="gt_row gt_center">Class 1</td>
<td headers="kclass" class="gt_row gt_center">0.249</td>
<td headers="CI" class="gt_row gt_center">[0.166, 0.329]</td>
<td headers="mcaPk" class="gt_row gt_center">0.282</td>
<td headers="avePPk" class="gt_row gt_center">0.675</td>
<td headers="OCCk" class="gt_row gt_center">6.264</td>
<td headers="entropy" class="gt_row gt_center">0.635</td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 2</td>
<td headers="kclass" class="gt_row gt_center">0.106</td>
<td headers="CI" class="gt_row gt_center">[0.083, 0.136]</td>
<td headers="mcaPk" class="gt_row gt_center">0.095</td>
<td headers="avePPk" class="gt_row gt_center">0.904</td>
<td headers="OCCk" class="gt_row gt_center">79.420</td>
<td headers="entropy" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 3</td>
<td headers="kclass" class="gt_row gt_center">0.644</td>
<td headers="CI" class="gt_row gt_center">[0.561, 0.731]</td>
<td headers="mcaPk" class="gt_row gt_center">0.623</td>
<td headers="avePPk" class="gt_row gt_center">0.893</td>
<td headers="OCCk" class="gt_row gt_center">4.614</td>
<td headers="entropy" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"> <i>Note.</i> McaP<sub>k</sub> = Modal class assignment proportion; AvePP<sub>k</sub> = Average posterior class probabilities; OCC<sub>k</sub> = Odds of correct classification; </td>
    </tr>
  </tfoot>
</table>
</div>
```


<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
