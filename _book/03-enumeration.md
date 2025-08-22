


# (PART) Latent Class Analysis {-}

# Enumeration

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
<div id="ifyaeqjsoy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ifyaeqjsoy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ifyaeqjsoy thead, #ifyaeqjsoy tbody, #ifyaeqjsoy tfoot, #ifyaeqjsoy tr, #ifyaeqjsoy td, #ifyaeqjsoy th {
  border-style: none;
}

#ifyaeqjsoy p {
  margin: 0;
  padding: 0;
}

#ifyaeqjsoy .gt_table {
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

#ifyaeqjsoy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ifyaeqjsoy .gt_title {
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

#ifyaeqjsoy .gt_subtitle {
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

#ifyaeqjsoy .gt_heading {
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

#ifyaeqjsoy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifyaeqjsoy .gt_col_headings {
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

#ifyaeqjsoy .gt_col_heading {
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

#ifyaeqjsoy .gt_column_spanner_outer {
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

#ifyaeqjsoy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ifyaeqjsoy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ifyaeqjsoy .gt_column_spanner {
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

#ifyaeqjsoy .gt_spanner_row {
  border-bottom-style: hidden;
}

#ifyaeqjsoy .gt_group_heading {
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

#ifyaeqjsoy .gt_empty_group_heading {
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

#ifyaeqjsoy .gt_from_md > :first-child {
  margin-top: 0;
}

#ifyaeqjsoy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ifyaeqjsoy .gt_row {
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

#ifyaeqjsoy .gt_stub {
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

#ifyaeqjsoy .gt_stub_row_group {
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

#ifyaeqjsoy .gt_row_group_first td {
  border-top-width: 2px;
}

#ifyaeqjsoy .gt_row_group_first th {
  border-top-width: 2px;
}

#ifyaeqjsoy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifyaeqjsoy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ifyaeqjsoy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ifyaeqjsoy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifyaeqjsoy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifyaeqjsoy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ifyaeqjsoy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ifyaeqjsoy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ifyaeqjsoy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ifyaeqjsoy .gt_footnotes {
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

#ifyaeqjsoy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifyaeqjsoy .gt_sourcenotes {
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

#ifyaeqjsoy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ifyaeqjsoy .gt_left {
  text-align: left;
}

#ifyaeqjsoy .gt_center {
  text-align: center;
}

#ifyaeqjsoy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ifyaeqjsoy .gt_font_normal {
  font-weight: normal;
}

#ifyaeqjsoy .gt_font_bold {
  font-weight: bold;
}

#ifyaeqjsoy .gt_font_italic {
  font-style: italic;
}

#ifyaeqjsoy .gt_super {
  font-size: 65%;
}

#ifyaeqjsoy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ifyaeqjsoy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ifyaeqjsoy .gt_indent_1 {
  text-indent: 5px;
}

#ifyaeqjsoy .gt_indent_2 {
  text-indent: 10px;
}

#ifyaeqjsoy .gt_indent_3 {
  text-indent: 15px;
}

#ifyaeqjsoy .gt_indent_4 {
  text-indent: 20px;
}

#ifyaeqjsoy .gt_indent_5 {
  text-indent: 25px;
}

#ifyaeqjsoy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ifyaeqjsoy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<div class="grViz html-widget html-fill-item" id="htmlwidget-316b5923f965509c1700" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-316b5923f965509c1700">{"x":{"diagram":" digraph cfa_model {\n\n# The `graph` statement - No editing needed\n\n    graph [layout = dot, overlap = true]\n \n# Two `node` statements\n \n# One for measured variables (box) \n\n    node [shape=box]\n    report_dis report_race report_sex counselors_fte psych_fte law_fte;\n \n# One for latent variables (circle) \n \n    node [shape=circle]\n    bully [label=<Bullying <br/>C<sub>k<\/sub>>];\n    \n# `edge` statements\n \n    edge [minlen = 2]\n    bully -> {report_dis report_race report_sex counselors_fte psych_fte law_fte}\n \n }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
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
dframe <- df_bully %>%
  pivot_longer(
    c(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte),
    names_to = "Variable"
  ) %>%
  group_by(Variable) %>%
  summarise(
    Count = sum(value == 1, na.rm = TRUE),
    Total = n(),
    .groups = "drop"
  ) %>%
  mutate(`Proportion Endorsed` = round(Count / Total, 3)) %>%
  select(Variable, `Proportion Endorsed`, Count)

gt(dframe) %>%
  tab_header(
    title = md("**LCA Indicator Endorsement**"),
    subtitle = md("&nbsp;")
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold"
  )
```


```{=html}
<div id="rlzdzfqxau" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rlzdzfqxau table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rlzdzfqxau thead, #rlzdzfqxau tbody, #rlzdzfqxau tfoot, #rlzdzfqxau tr, #rlzdzfqxau td, #rlzdzfqxau th {
  border-style: none;
}

#rlzdzfqxau p {
  margin: 0;
  padding: 0;
}

#rlzdzfqxau .gt_table {
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

#rlzdzfqxau .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rlzdzfqxau .gt_title {
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

#rlzdzfqxau .gt_subtitle {
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

#rlzdzfqxau .gt_heading {
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

#rlzdzfqxau .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlzdzfqxau .gt_col_headings {
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

#rlzdzfqxau .gt_col_heading {
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

#rlzdzfqxau .gt_column_spanner_outer {
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

#rlzdzfqxau .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rlzdzfqxau .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rlzdzfqxau .gt_column_spanner {
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

#rlzdzfqxau .gt_spanner_row {
  border-bottom-style: hidden;
}

#rlzdzfqxau .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
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

#rlzdzfqxau .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rlzdzfqxau .gt_from_md > :first-child {
  margin-top: 0;
}

#rlzdzfqxau .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rlzdzfqxau .gt_row {
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

#rlzdzfqxau .gt_stub {
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

#rlzdzfqxau .gt_stub_row_group {
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

#rlzdzfqxau .gt_row_group_first td {
  border-top-width: 2px;
}

#rlzdzfqxau .gt_row_group_first th {
  border-top-width: 2px;
}

#rlzdzfqxau .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlzdzfqxau .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rlzdzfqxau .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rlzdzfqxau .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlzdzfqxau .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlzdzfqxau .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rlzdzfqxau .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rlzdzfqxau .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rlzdzfqxau .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlzdzfqxau .gt_footnotes {
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

#rlzdzfqxau .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlzdzfqxau .gt_sourcenotes {
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

#rlzdzfqxau .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlzdzfqxau .gt_left {
  text-align: left;
}

#rlzdzfqxau .gt_center {
  text-align: center;
}

#rlzdzfqxau .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rlzdzfqxau .gt_font_normal {
  font-weight: normal;
}

#rlzdzfqxau .gt_font_bold {
  font-weight: bold;
}

#rlzdzfqxau .gt_font_italic {
  font-style: italic;
}

#rlzdzfqxau .gt_super {
  font-size: 65%;
}

#rlzdzfqxau .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rlzdzfqxau .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rlzdzfqxau .gt_indent_1 {
  text-indent: 5px;
}

#rlzdzfqxau .gt_indent_2 {
  text-indent: 10px;
}

#rlzdzfqxau .gt_indent_3 {
  text-indent: 15px;
}

#rlzdzfqxau .gt_indent_4 {
  text-indent: 20px;
}

#rlzdzfqxau .gt_indent_5 {
  text-indent: 25px;
}

#rlzdzfqxau .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rlzdzfqxau div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>LCA Indicator Endorsement</strong></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'> </span></td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Variable">Variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Proportion-Endorsed">Proportion Endorsed</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Count">Count</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Variable" class="gt_row gt_left">counselors_fte</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.453</td>
<td headers="Count" class="gt_row gt_right">919</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">law_fte</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.124</td>
<td headers="Count" class="gt_row gt_right">251</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">psych_fte</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.467</td>
<td headers="Count" class="gt_row gt_right">947</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">report_dis</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.042</td>
<td headers="Count" class="gt_row gt_right">85</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">report_race</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.102</td>
<td headers="Count" class="gt_row gt_right">206</td></tr>
    <tr><td headers="Variable" class="gt_row gt_left">report_sex</td>
<td headers="Proportion Endorsed" class="gt_row gt_right">0.168</td>
<td headers="Count" class="gt_row gt_right">340</td></tr>
  </tbody>
  
  
</table>
</div>
```


Save as image


``` r
gtsave(prop_table, here("figures", "prop_table.png"))
```

Frequency Plot


``` r
data_long <- df_bully %>%
  pivot_longer(c(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte), names_to = "variable")

# Bar plot for 0/1 indicators
ggplot(data_long, aes(x = factor(value))) +
  geom_bar(fill = "#69b3a2", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ variable) +
  labs(
    title = "Binary Indicator Distributions",
    x = "Value (0 = No, 1 = Yes)",
    y = "Count"
  ) +
  theme_cowplot()
```

<img src="03-enumeration_files/figure-html/unnamed-chunk-7-1.png" width="90%" />

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

## Examine and extract Mplus files

Code by Delwin Carter (2025)

Check all Models for:

1. Warnings  
2. Errors  
3. Convergence and Loglikelihood Replication Information



``` r
source(here("functions", "extract_mplus_info.R"))

# Define the directory where all of the .out files are located.
output_dir <- here("enum")

# Get all .out files
output_files <- list.files(output_dir, pattern = "\\.out$", full.names = TRUE)

# Process all .out files into one dataframe
final_data <- map_dfr(output_files, extract_mplus_info_extended)

# Extract Sample_Size from final_data
sample_size <- unique(final_data$Sample_Size)

```

### Examine Mplus Warnings


``` r
source(here("functions", "extract_warnings.R"))

warnings_table <- extract_warnings(final_data)
warnings_table
```


```{=html}
<div id="irixiwtbxl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#irixiwtbxl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#irixiwtbxl thead, #irixiwtbxl tbody, #irixiwtbxl tfoot, #irixiwtbxl tr, #irixiwtbxl td, #irixiwtbxl th {
  border-style: none;
}

#irixiwtbxl p {
  margin: 0;
  padding: 0;
}

#irixiwtbxl .gt_table {
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

#irixiwtbxl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#irixiwtbxl .gt_title {
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

#irixiwtbxl .gt_subtitle {
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

#irixiwtbxl .gt_heading {
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

#irixiwtbxl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#irixiwtbxl .gt_col_headings {
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

#irixiwtbxl .gt_col_heading {
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

#irixiwtbxl .gt_column_spanner_outer {
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

#irixiwtbxl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#irixiwtbxl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#irixiwtbxl .gt_column_spanner {
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

#irixiwtbxl .gt_spanner_row {
  border-bottom-style: hidden;
}

#irixiwtbxl .gt_group_heading {
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

#irixiwtbxl .gt_empty_group_heading {
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

#irixiwtbxl .gt_from_md > :first-child {
  margin-top: 0;
}

#irixiwtbxl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#irixiwtbxl .gt_row {
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

#irixiwtbxl .gt_stub {
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

#irixiwtbxl .gt_stub_row_group {
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

#irixiwtbxl .gt_row_group_first td {
  border-top-width: 2px;
}

#irixiwtbxl .gt_row_group_first th {
  border-top-width: 2px;
}

#irixiwtbxl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#irixiwtbxl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#irixiwtbxl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#irixiwtbxl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#irixiwtbxl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#irixiwtbxl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#irixiwtbxl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#irixiwtbxl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#irixiwtbxl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#irixiwtbxl .gt_footnotes {
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

#irixiwtbxl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#irixiwtbxl .gt_sourcenotes {
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

#irixiwtbxl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#irixiwtbxl .gt_left {
  text-align: left;
}

#irixiwtbxl .gt_center {
  text-align: center;
}

#irixiwtbxl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#irixiwtbxl .gt_font_normal {
  font-weight: normal;
}

#irixiwtbxl .gt_font_bold {
  font-weight: bold;
}

#irixiwtbxl .gt_font_italic {
  font-style: italic;
}

#irixiwtbxl .gt_super {
  font-size: 65%;
}

#irixiwtbxl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#irixiwtbxl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#irixiwtbxl .gt_indent_1 {
  text-indent: 5px;
}

#irixiwtbxl .gt_indent_2 {
  text-indent: 10px;
}

#irixiwtbxl .gt_indent_3 {
  text-indent: 15px;
}

#irixiwtbxl .gt_indent_4 {
  text-indent: 20px;
}

#irixiwtbxl .gt_indent_5 {
  text-indent: 25px;
}

#irixiwtbxl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#irixiwtbxl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="File_Name" class="gt_row gt_left">c1_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 5 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   TECH11 option is not available for TYPE=MIXTURE with only one class.   Request for TECH11 is ignored.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   TECH14 option is not available for TYPE=MIXTURE with only one class.   Request for TECH14 is ignored.    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c2_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 3 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 3 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 3 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 3 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.    </div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_bully.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 3 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in VARIABLE command   Note that only the first 8 characters of variable names are used in the output.   Shorten variable names to avoid any confusion.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in PLOT command   Note that only the first 8 characters of variable names are used in plots.   If variable names are not unique within the first 8 characters, problems   may occur.</div></td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left"><div style='white-space: normal; word-wrap: break-word;'>*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.    </div></td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r

# Save the warnings table
#gtsave(warnings_table, here("figures", "warnings_table.png"))
```

### Examine Mplus Errors


``` r
source(here("functions", "error_visualization.R"))

# Process errors
error_table_data <- process_error_data(final_data)
error_table_data
```


```{=html}
<div id="zpxevxmouw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zpxevxmouw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zpxevxmouw thead, #zpxevxmouw tbody, #zpxevxmouw tfoot, #zpxevxmouw tr, #zpxevxmouw td, #zpxevxmouw th {
  border-style: none;
}

#zpxevxmouw p {
  margin: 0;
  padding: 0;
}

#zpxevxmouw .gt_table {
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

#zpxevxmouw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zpxevxmouw .gt_title {
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

#zpxevxmouw .gt_subtitle {
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

#zpxevxmouw .gt_heading {
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

#zpxevxmouw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zpxevxmouw .gt_col_headings {
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

#zpxevxmouw .gt_col_heading {
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

#zpxevxmouw .gt_column_spanner_outer {
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

#zpxevxmouw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zpxevxmouw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zpxevxmouw .gt_column_spanner {
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

#zpxevxmouw .gt_spanner_row {
  border-bottom-style: hidden;
}

#zpxevxmouw .gt_group_heading {
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

#zpxevxmouw .gt_empty_group_heading {
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

#zpxevxmouw .gt_from_md > :first-child {
  margin-top: 0;
}

#zpxevxmouw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zpxevxmouw .gt_row {
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

#zpxevxmouw .gt_stub {
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

#zpxevxmouw .gt_stub_row_group {
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

#zpxevxmouw .gt_row_group_first td {
  border-top-width: 2px;
}

#zpxevxmouw .gt_row_group_first th {
  border-top-width: 2px;
}

#zpxevxmouw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpxevxmouw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zpxevxmouw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zpxevxmouw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zpxevxmouw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpxevxmouw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zpxevxmouw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zpxevxmouw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zpxevxmouw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zpxevxmouw .gt_footnotes {
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

#zpxevxmouw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpxevxmouw .gt_sourcenotes {
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

#zpxevxmouw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zpxevxmouw .gt_left {
  text-align: left;
}

#zpxevxmouw .gt_center {
  text-align: center;
}

#zpxevxmouw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zpxevxmouw .gt_font_normal {
  font-weight: normal;
}

#zpxevxmouw .gt_font_bold {
  font-weight: bold;
}

#zpxevxmouw .gt_font_italic {
  font-style: italic;
}

#zpxevxmouw .gt_super {
  font-size: 65%;
}

#zpxevxmouw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zpxevxmouw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zpxevxmouw .gt_indent_1 {
  text-indent: 5px;
}

#zpxevxmouw .gt_indent_2 {
  text-indent: 10px;
}

#zpxevxmouw .gt_indent_3 {
  text-indent: 15px;
}

#zpxevxmouw .gt_indent_4 {
  text-indent: 20px;
}

#zpxevxmouw .gt_indent_5 {
  text-indent: 25px;
}

#zpxevxmouw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zpxevxmouw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="File_Name" class="gt_row gt_left">c2_bully.out</td>
<td headers="Class_Model" class="gt_row gt_left">2-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.  </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_bully.out</td>
<td headers="Class_Model" class="gt_row gt_left">3-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_bully.out</td>
<td headers="Class_Model" class="gt_row gt_left">4-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_bully.out</td>
<td headers="Class_Model" class="gt_row gt_left">5-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_bully.out</td>
<td headers="Class_Model" class="gt_row gt_left">6-Class</td>
<td headers="Error_Message" class="gt_row gt_left">THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.       IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES      OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE      VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE      PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.    </td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r

# Save the errors table
#gtsave(error_table, here("figures", "error_table.png"))
```

### Examine Convergence and Loglikelihood Replications


``` r
source(here("functions", "summary_table.R"))

# Print Table with Superheader & Heatmap
summary_table <- create_flextable(final_data, sample_size)
summary_table
```


```{=html}
<div class="tabwid"><style>.cl-4fdbd01a{}.cl-4fd47bbc{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4fd47bc6{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4fd47bc7{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-4fd76c0a{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4fd76c0b{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4fd76c14{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4fd78dc0{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dca{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dcb{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dcc{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dd4{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dd5{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dd6{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dde{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78ddf{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78de0{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78de8{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78de9{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dea{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78df2{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78df3{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78df4{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78df5{width:0.5in;background-color:rgba(237, 134, 107, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dfc{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dfd{width:0.5in;background-color:rgba(252, 219, 210, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78dfe{width:0.5in;background-color:rgba(250, 210, 198, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e06{width:0.5in;background-color:rgba(234, 112, 80, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e07{width:0.5in;background-color:rgba(237, 131, 103, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e08{width:0.5in;background-color:rgba(229, 88, 51, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e09{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e0a{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e0b{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e10{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4fd78e11{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-4fdbd01a'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-4fd78dc0"><p class="cl-4fd76c0a"><span class="cl-4fd47bbc">N</span><span class="cl-4fd47bc6"> = </span><span class="cl-4fd47bc6">2027</span></p></th><th  colspan="2"class="cl-4fd78dcc"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Random Starts</span></p></th><th  colspan="2"class="cl-4fd78dcc"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Final starting value sets converging</span></p></th><th  colspan="2"class="cl-4fd78dcc"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">LL Replication</span></p></th><th  colspan="2"class="cl-4fd78dcc"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-4fd78dd4"><p class="cl-4fd76c0a"><span class="cl-4fd47bc6">Model</span></p></th><th class="cl-4fd78dd5"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Best LL</span></p></th><th class="cl-4fd78dd6"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">npar</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Initial</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">Final</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bbc">f</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">%</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bbc">f</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">%</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bbc">f</span></p></th><th class="cl-4fd78dde"><p class="cl-4fd76c0b"><span class="cl-4fd47bc6">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-4fd78ddf"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">1-Class</span></p></td><td class="cl-4fd78de0"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,443.409</span></p></td><td class="cl-4fd78de8"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">6</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100.0%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">2,027</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4fd78dea"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">2-Class</span></p></td><td class="cl-4fd78df2"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,194.136</span></p></td><td class="cl-4fd78df3"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">13</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">57</span></p></td><td class="cl-4fd78df5"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">57%</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">57</span></p></td><td class="cl-4fd78dfc"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100.0%</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">444</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">21.9%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4fd78ddf"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">3-Class</span></p></td><td class="cl-4fd78de0"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,122.478</span></p></td><td class="cl-4fd78de8"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">20</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">93</span></p></td><td class="cl-4fd78dfd"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">93%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">80</span></p></td><td class="cl-4fd78dfe"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">86.0%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">216</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">10.6%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4fd78dea"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">4-Class</span></p></td><td class="cl-4fd78df2"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,111.757</span></p></td><td class="cl-4fd78df3"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">27</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">47</span></p></td><td class="cl-4fd78e06"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">47%</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">20</span></p></td><td class="cl-4fd78e07"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">42.6%</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">212</span></p></td><td class="cl-4fd78df4"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">10.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4fd78ddf"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">5-Class</span></p></td><td class="cl-4fd78de0"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,105.589</span></p></td><td class="cl-4fd78de8"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">34</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">37</span></p></td><td class="cl-4fd78e08"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">37%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">7</span></p></td><td class="cl-4fd78e08"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">18.9%</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">43</span></p></td><td class="cl-4fd78de9"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">2.1%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4fd78e09"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">6-Class</span></p></td><td class="cl-4fd78e0a"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">-5,099.881</span></p></td><td class="cl-4fd78e0b"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">41</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">200</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">100</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">32</span></p></td><td class="cl-4fd78e11"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">32%</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">4</span></p></td><td class="cl-4fd78e11"><p class="cl-4fd76c14"><span class="cl-4fd47bc7">12.5%</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">36</span></p></td><td class="cl-4fd78e10"><p class="cl-4fd76c14"><span class="cl-4fd47bc6">1.8%</span></p></td></tr></tbody></table></div>
```


``` r

# Save the flextable as a PNG image
#invisible(save_as_image(summary_table, path = here("figures", "housekeeping.png")))
```


### Check for Loglikelihood Replication

Visualize and examine loglikelihood replication values for each ouput file individually


``` r
# Load the function for separate plots
source(here("functions", "ll_replication_plots.R"))

# Generate individual log-likelihood replication tables
ll_replication_tables <- generate_ll_replication_plots(final_data)
ll_replication_tables
```

$c1_bully.out
<div id="ovmokliwyj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#ovmokliwyj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ovmokliwyj thead, #ovmokliwyj tbody, #ovmokliwyj tfoot, #ovmokliwyj tr, #ovmokliwyj td, #ovmokliwyj th {
  border-style: none;
}

#ovmokliwyj p {
  margin: 0;
  padding: 0;
}

#ovmokliwyj .gt_table {
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

#ovmokliwyj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ovmokliwyj .gt_title {
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

#ovmokliwyj .gt_subtitle {
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

#ovmokliwyj .gt_heading {
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

#ovmokliwyj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ovmokliwyj .gt_col_headings {
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

#ovmokliwyj .gt_col_heading {
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

#ovmokliwyj .gt_column_spanner_outer {
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

#ovmokliwyj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ovmokliwyj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ovmokliwyj .gt_column_spanner {
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

#ovmokliwyj .gt_spanner_row {
  border-bottom-style: hidden;
}

#ovmokliwyj .gt_group_heading {
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

#ovmokliwyj .gt_empty_group_heading {
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

#ovmokliwyj .gt_from_md > :first-child {
  margin-top: 0;
}

#ovmokliwyj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ovmokliwyj .gt_row {
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

#ovmokliwyj .gt_stub {
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

#ovmokliwyj .gt_stub_row_group {
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

#ovmokliwyj .gt_row_group_first td {
  border-top-width: 2px;
}

#ovmokliwyj .gt_row_group_first th {
  border-top-width: 2px;
}

#ovmokliwyj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ovmokliwyj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ovmokliwyj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ovmokliwyj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ovmokliwyj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ovmokliwyj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ovmokliwyj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ovmokliwyj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ovmokliwyj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ovmokliwyj .gt_footnotes {
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

#ovmokliwyj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ovmokliwyj .gt_sourcenotes {
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

#ovmokliwyj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ovmokliwyj .gt_left {
  text-align: left;
}

#ovmokliwyj .gt_center {
  text-align: center;
}

#ovmokliwyj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ovmokliwyj .gt_font_normal {
  font-weight: normal;
}

#ovmokliwyj .gt_font_bold {
  font-weight: bold;
}

#ovmokliwyj .gt_font_italic {
  font-style: italic;
}

#ovmokliwyj .gt_super {
  font-size: 65%;
}

#ovmokliwyj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ovmokliwyj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ovmokliwyj .gt_indent_1 {
  text-indent: 5px;
}

#ovmokliwyj .gt_indent_2 {
  text-indent: 10px;
}

#ovmokliwyj .gt_indent_3 {
  text-indent: 15px;
}

#ovmokliwyj .gt_indent_4 {
  text-indent: 20px;
}

#ovmokliwyj .gt_indent_5 {
  text-indent: 25px;
}

#ovmokliwyj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ovmokliwyj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 1-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c1_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,443.409</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c2_bully.out
<div id="eehquawhyy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#eehquawhyy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#eehquawhyy thead, #eehquawhyy tbody, #eehquawhyy tfoot, #eehquawhyy tr, #eehquawhyy td, #eehquawhyy th {
  border-style: none;
}

#eehquawhyy p {
  margin: 0;
  padding: 0;
}

#eehquawhyy .gt_table {
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

#eehquawhyy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#eehquawhyy .gt_title {
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

#eehquawhyy .gt_subtitle {
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

#eehquawhyy .gt_heading {
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

#eehquawhyy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eehquawhyy .gt_col_headings {
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

#eehquawhyy .gt_col_heading {
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

#eehquawhyy .gt_column_spanner_outer {
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

#eehquawhyy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eehquawhyy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eehquawhyy .gt_column_spanner {
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

#eehquawhyy .gt_spanner_row {
  border-bottom-style: hidden;
}

#eehquawhyy .gt_group_heading {
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

#eehquawhyy .gt_empty_group_heading {
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

#eehquawhyy .gt_from_md > :first-child {
  margin-top: 0;
}

#eehquawhyy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eehquawhyy .gt_row {
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

#eehquawhyy .gt_stub {
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

#eehquawhyy .gt_stub_row_group {
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

#eehquawhyy .gt_row_group_first td {
  border-top-width: 2px;
}

#eehquawhyy .gt_row_group_first th {
  border-top-width: 2px;
}

#eehquawhyy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eehquawhyy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#eehquawhyy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#eehquawhyy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eehquawhyy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eehquawhyy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eehquawhyy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#eehquawhyy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eehquawhyy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eehquawhyy .gt_footnotes {
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

#eehquawhyy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eehquawhyy .gt_sourcenotes {
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

#eehquawhyy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eehquawhyy .gt_left {
  text-align: left;
}

#eehquawhyy .gt_center {
  text-align: center;
}

#eehquawhyy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eehquawhyy .gt_font_normal {
  font-weight: normal;
}

#eehquawhyy .gt_font_bold {
  font-weight: bold;
}

#eehquawhyy .gt_font_italic {
  font-style: italic;
}

#eehquawhyy .gt_super {
  font-size: 65%;
}

#eehquawhyy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#eehquawhyy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#eehquawhyy .gt_indent_1 {
  text-indent: 5px;
}

#eehquawhyy .gt_indent_2 {
  text-indent: 10px;
}

#eehquawhyy .gt_indent_3 {
  text-indent: 15px;
}

#eehquawhyy .gt_indent_4 {
  text-indent: 20px;
}

#eehquawhyy .gt_indent_5 {
  text-indent: 25px;
}

#eehquawhyy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#eehquawhyy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 2-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c2_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,194.136</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">57.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c3_bully.out
<div id="mtphwyeukf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#mtphwyeukf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mtphwyeukf thead, #mtphwyeukf tbody, #mtphwyeukf tfoot, #mtphwyeukf tr, #mtphwyeukf td, #mtphwyeukf th {
  border-style: none;
}

#mtphwyeukf p {
  margin: 0;
  padding: 0;
}

#mtphwyeukf .gt_table {
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

#mtphwyeukf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mtphwyeukf .gt_title {
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

#mtphwyeukf .gt_subtitle {
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

#mtphwyeukf .gt_heading {
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

#mtphwyeukf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtphwyeukf .gt_col_headings {
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

#mtphwyeukf .gt_col_heading {
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

#mtphwyeukf .gt_column_spanner_outer {
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

#mtphwyeukf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mtphwyeukf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mtphwyeukf .gt_column_spanner {
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

#mtphwyeukf .gt_spanner_row {
  border-bottom-style: hidden;
}

#mtphwyeukf .gt_group_heading {
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

#mtphwyeukf .gt_empty_group_heading {
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

#mtphwyeukf .gt_from_md > :first-child {
  margin-top: 0;
}

#mtphwyeukf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mtphwyeukf .gt_row {
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

#mtphwyeukf .gt_stub {
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

#mtphwyeukf .gt_stub_row_group {
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

#mtphwyeukf .gt_row_group_first td {
  border-top-width: 2px;
}

#mtphwyeukf .gt_row_group_first th {
  border-top-width: 2px;
}

#mtphwyeukf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtphwyeukf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mtphwyeukf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mtphwyeukf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtphwyeukf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtphwyeukf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mtphwyeukf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mtphwyeukf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mtphwyeukf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtphwyeukf .gt_footnotes {
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

#mtphwyeukf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtphwyeukf .gt_sourcenotes {
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

#mtphwyeukf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtphwyeukf .gt_left {
  text-align: left;
}

#mtphwyeukf .gt_center {
  text-align: center;
}

#mtphwyeukf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mtphwyeukf .gt_font_normal {
  font-weight: normal;
}

#mtphwyeukf .gt_font_bold {
  font-weight: bold;
}

#mtphwyeukf .gt_font_italic {
  font-style: italic;
}

#mtphwyeukf .gt_super {
  font-size: 65%;
}

#mtphwyeukf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mtphwyeukf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mtphwyeukf .gt_indent_1 {
  text-indent: 5px;
}

#mtphwyeukf .gt_indent_2 {
  text-indent: 10px;
}

#mtphwyeukf .gt_indent_3 {
  text-indent: 15px;
}

#mtphwyeukf .gt_indent_4 {
  text-indent: 20px;
}

#mtphwyeukf .gt_indent_5 {
  text-indent: 25px;
}

#mtphwyeukf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mtphwyeukf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 3-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c3_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,122.478</td>
<td headers="Count" class="gt_row gt_center">80.000</td>
<td headers="Percentage" class="gt_row gt_center">86.02%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,123.945</td>
<td headers="Count" class="gt_row gt_center">10.000</td>
<td headers="Percentage" class="gt_row gt_center">10.75%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,123.979</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">3.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">3.23%</td></tr>
  </tbody>
  
  
</table>
</div>

$c4_bully.out
<div id="wewxuoqglm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#wewxuoqglm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wewxuoqglm thead, #wewxuoqglm tbody, #wewxuoqglm tfoot, #wewxuoqglm tr, #wewxuoqglm td, #wewxuoqglm th {
  border-style: none;
}

#wewxuoqglm p {
  margin: 0;
  padding: 0;
}

#wewxuoqglm .gt_table {
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

#wewxuoqglm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wewxuoqglm .gt_title {
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

#wewxuoqglm .gt_subtitle {
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

#wewxuoqglm .gt_heading {
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

#wewxuoqglm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wewxuoqglm .gt_col_headings {
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

#wewxuoqglm .gt_col_heading {
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

#wewxuoqglm .gt_column_spanner_outer {
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

#wewxuoqglm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wewxuoqglm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wewxuoqglm .gt_column_spanner {
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

#wewxuoqglm .gt_spanner_row {
  border-bottom-style: hidden;
}

#wewxuoqglm .gt_group_heading {
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

#wewxuoqglm .gt_empty_group_heading {
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

#wewxuoqglm .gt_from_md > :first-child {
  margin-top: 0;
}

#wewxuoqglm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wewxuoqglm .gt_row {
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

#wewxuoqglm .gt_stub {
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

#wewxuoqglm .gt_stub_row_group {
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

#wewxuoqglm .gt_row_group_first td {
  border-top-width: 2px;
}

#wewxuoqglm .gt_row_group_first th {
  border-top-width: 2px;
}

#wewxuoqglm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wewxuoqglm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wewxuoqglm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wewxuoqglm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wewxuoqglm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wewxuoqglm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wewxuoqglm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wewxuoqglm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wewxuoqglm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wewxuoqglm .gt_footnotes {
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

#wewxuoqglm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wewxuoqglm .gt_sourcenotes {
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

#wewxuoqglm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wewxuoqglm .gt_left {
  text-align: left;
}

#wewxuoqglm .gt_center {
  text-align: center;
}

#wewxuoqglm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wewxuoqglm .gt_font_normal {
  font-weight: normal;
}

#wewxuoqglm .gt_font_bold {
  font-weight: bold;
}

#wewxuoqglm .gt_font_italic {
  font-style: italic;
}

#wewxuoqglm .gt_super {
  font-size: 65%;
}

#wewxuoqglm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wewxuoqglm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wewxuoqglm .gt_indent_1 {
  text-indent: 5px;
}

#wewxuoqglm .gt_indent_2 {
  text-indent: 10px;
}

#wewxuoqglm .gt_indent_3 {
  text-indent: 15px;
}

#wewxuoqglm .gt_indent_4 {
  text-indent: 20px;
}

#wewxuoqglm .gt_indent_5 {
  text-indent: 25px;
}

#wewxuoqglm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#wewxuoqglm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 4-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c4_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,111.757</td>
<td headers="Count" class="gt_row gt_center">20.000</td>
<td headers="Percentage" class="gt_row gt_center">42.55%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,111.759</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">6.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,112.253</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">8.51%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,112.955</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.13%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,115.532</td>
<td headers="Count" class="gt_row gt_center">11.000</td>
<td headers="Percentage" class="gt_row gt_center">23.40%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,115.538</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.13%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,115.884</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.13%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,116.981</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">6.38%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,117.829</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">3.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">6.38%</td></tr>
  </tbody>
  
  
</table>
</div>

$c5_bully.out
<div id="zhpmayclii" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#zhpmayclii table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zhpmayclii thead, #zhpmayclii tbody, #zhpmayclii tfoot, #zhpmayclii tr, #zhpmayclii td, #zhpmayclii th {
  border-style: none;
}

#zhpmayclii p {
  margin: 0;
  padding: 0;
}

#zhpmayclii .gt_table {
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

#zhpmayclii .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zhpmayclii .gt_title {
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

#zhpmayclii .gt_subtitle {
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

#zhpmayclii .gt_heading {
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

#zhpmayclii .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zhpmayclii .gt_col_headings {
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

#zhpmayclii .gt_col_heading {
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

#zhpmayclii .gt_column_spanner_outer {
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

#zhpmayclii .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zhpmayclii .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zhpmayclii .gt_column_spanner {
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

#zhpmayclii .gt_spanner_row {
  border-bottom-style: hidden;
}

#zhpmayclii .gt_group_heading {
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

#zhpmayclii .gt_empty_group_heading {
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

#zhpmayclii .gt_from_md > :first-child {
  margin-top: 0;
}

#zhpmayclii .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zhpmayclii .gt_row {
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

#zhpmayclii .gt_stub {
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

#zhpmayclii .gt_stub_row_group {
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

#zhpmayclii .gt_row_group_first td {
  border-top-width: 2px;
}

#zhpmayclii .gt_row_group_first th {
  border-top-width: 2px;
}

#zhpmayclii .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zhpmayclii .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zhpmayclii .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zhpmayclii .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zhpmayclii .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zhpmayclii .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zhpmayclii .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zhpmayclii .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zhpmayclii .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zhpmayclii .gt_footnotes {
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

#zhpmayclii .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zhpmayclii .gt_sourcenotes {
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

#zhpmayclii .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zhpmayclii .gt_left {
  text-align: left;
}

#zhpmayclii .gt_center {
  text-align: center;
}

#zhpmayclii .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zhpmayclii .gt_font_normal {
  font-weight: normal;
}

#zhpmayclii .gt_font_bold {
  font-weight: bold;
}

#zhpmayclii .gt_font_italic {
  font-style: italic;
}

#zhpmayclii .gt_super {
  font-size: 65%;
}

#zhpmayclii .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zhpmayclii .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zhpmayclii .gt_indent_1 {
  text-indent: 5px;
}

#zhpmayclii .gt_indent_2 {
  text-indent: 10px;
}

#zhpmayclii .gt_indent_3 {
  text-indent: 15px;
}

#zhpmayclii .gt_indent_4 {
  text-indent: 20px;
}

#zhpmayclii .gt_indent_5 {
  text-indent: 25px;
}

#zhpmayclii .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zhpmayclii div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 5-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c5_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.589</td>
<td headers="Count" class="gt_row gt_center">7.000</td>
<td headers="Percentage" class="gt_row gt_center">18.92%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.661</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.791</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">8.11%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.799</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">10.81%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.748</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.41%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.983</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.169</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.41%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.172</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">8.11%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.449</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.450</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.458</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.728</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.958</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.058</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.084</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.096</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.860</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">10.81%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,109.002</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,110.474</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.70%</td></tr>
  </tbody>
  
  
</table>
</div>

$c6_bully.out
<div id="zwfhfqwppo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#zwfhfqwppo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zwfhfqwppo thead, #zwfhfqwppo tbody, #zwfhfqwppo tfoot, #zwfhfqwppo tr, #zwfhfqwppo td, #zwfhfqwppo th {
  border-style: none;
}

#zwfhfqwppo p {
  margin: 0;
  padding: 0;
}

#zwfhfqwppo .gt_table {
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

#zwfhfqwppo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zwfhfqwppo .gt_title {
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

#zwfhfqwppo .gt_subtitle {
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

#zwfhfqwppo .gt_heading {
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

#zwfhfqwppo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwfhfqwppo .gt_col_headings {
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

#zwfhfqwppo .gt_col_heading {
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

#zwfhfqwppo .gt_column_spanner_outer {
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

#zwfhfqwppo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zwfhfqwppo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zwfhfqwppo .gt_column_spanner {
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

#zwfhfqwppo .gt_spanner_row {
  border-bottom-style: hidden;
}

#zwfhfqwppo .gt_group_heading {
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

#zwfhfqwppo .gt_empty_group_heading {
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

#zwfhfqwppo .gt_from_md > :first-child {
  margin-top: 0;
}

#zwfhfqwppo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zwfhfqwppo .gt_row {
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

#zwfhfqwppo .gt_stub {
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

#zwfhfqwppo .gt_stub_row_group {
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

#zwfhfqwppo .gt_row_group_first td {
  border-top-width: 2px;
}

#zwfhfqwppo .gt_row_group_first th {
  border-top-width: 2px;
}

#zwfhfqwppo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwfhfqwppo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zwfhfqwppo .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zwfhfqwppo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwfhfqwppo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwfhfqwppo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zwfhfqwppo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zwfhfqwppo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zwfhfqwppo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zwfhfqwppo .gt_footnotes {
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

#zwfhfqwppo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwfhfqwppo .gt_sourcenotes {
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

#zwfhfqwppo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zwfhfqwppo .gt_left {
  text-align: left;
}

#zwfhfqwppo .gt_center {
  text-align: center;
}

#zwfhfqwppo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zwfhfqwppo .gt_font_normal {
  font-weight: normal;
}

#zwfhfqwppo .gt_font_bold {
  font-weight: bold;
}

#zwfhfqwppo .gt_font_italic {
  font-style: italic;
}

#zwfhfqwppo .gt_super {
  font-size: 65%;
}

#zwfhfqwppo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zwfhfqwppo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zwfhfqwppo .gt_indent_1 {
  text-indent: 5px;
}

#zwfhfqwppo .gt_indent_2 {
  text-indent: 10px;
}

#zwfhfqwppo .gt_indent_3 {
  text-indent: 15px;
}

#zwfhfqwppo .gt_indent_4 {
  text-indent: 20px;
}

#zwfhfqwppo .gt_indent_5 {
  text-indent: 25px;
}

#zwfhfqwppo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zwfhfqwppo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal" style>Log Likelihood Replications: 6-Class</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Source File: c6_bully.out</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="LogLikelihood">Log Likelihood</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Count">Replication Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Percentage">% of Valid Replications</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,099.881</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">12.50%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.272</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.842</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">6.25%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.874</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.928</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">6.25%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.017</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.071</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">6.25%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.089</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.117</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.316</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.332</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.452</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.494</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.512</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.592</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.593</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.913</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.075</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.613</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.616</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.662</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,104.167</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,104.462</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.309</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.302</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">3.12%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,107.624</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">3.12%</td></tr>
  </tbody>
  
  
</table>
</div>


Visualize and examine loglikelihood replication for each output file together


``` r
ll_replication_table_all <- source(here("functions", "ll_replication_processing.R"), local = TRUE)$value
ll_replication_table_all
```


```{=html}
<div class="tabwid"><style>.cl-503eab72{}.cl-5035bab2{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-5035babc{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-5035babd{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-5038b7b2{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-5038b7bc{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-5038d44a{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d454{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d455{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d45e{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d45f{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d460{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d468{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5038d472{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-503eab72'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">1-Class</span></p></th><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">2-Class</span></p></th><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">3-Class</span></p></th><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">4-Class</span></p></th><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">5-Class</span></p></th><th  colspan="3"class="cl-5038d44a"><p class="cl-5038b7b2"><span class="cl-5035bab2">6-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th><th class="cl-5038d455"><p class="cl-5038b7b2"><span class="cl-5035babc">LL</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">N</span></p></th><th class="cl-5038d45e"><p class="cl-5038b7b2"><span class="cl-5035babc">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5443.409</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">100</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">100</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5194.136</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">57</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">100</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5122.478</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">80</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">86</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5111.757</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">20</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">42.6</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5105.589</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">7</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">18.9</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,099.881</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">4</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">12.5</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5123.945</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">10</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">10.8</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5111.759</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5105.661</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,100.272</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5123.979</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.2</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5112.253</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">4</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">8.5</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5105.791</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">8.1</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,100.842</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5112.955</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.1</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5105.799</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">4</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">10.8</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,100.874</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5115.532</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">11</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">23.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5106.748</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">5.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,100.928</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5115.538</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.1</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5106.983</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.017</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5115.884</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.1</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.169</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">5.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.071</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5116.981</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.172</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">8.1</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.089</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5117.829</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">6.4</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.449</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.117</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.45</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.316</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.458</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.332</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.728</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.452</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5107.958</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.494</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5108.058</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.512</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5108.084</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.592</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5108.096</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.593</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5108.86</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">4</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">10.8</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,101.913</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5109.002</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,102.075</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5110.474</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">2.7</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,102.613</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,102.616</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,102.662</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,104.167</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,104.462</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,105.309</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d45f"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,107.302</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d460"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">—</span></p></td><td class="cl-5038d468"><p class="cl-5038b7bc"><span class="cl-5035babd">-5,107.624</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">1</span></p></td><td class="cl-5038d472"><p class="cl-5038b7bc"><span class="cl-5035babd">3.1</span></p></td></tr></tbody></table></div>
```




![](figures/stophere.png){style=" width="500"}

------------------------------------------------------------------------

## Table of Fit

First, extract data:


``` r
output_enum <- readModels(here("enum"), filefilter = "bully", quiet = TRUE)

# Extract fit indices
enum_extract <- LatexSummaryTable(
  output_enum,
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

# Calculate additional fit indices
allFit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  mutate(SIC = -.5 * BIC) %>%
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  dplyr::select(Title, Parameters, LL, BIC, aBIC, CAIC, AWE, BLRT_PValue, T11_VLMR_PValue, BF, cmPk) %>%
  arrange(Parameters)

# Merge columns with LL replications and class size from `final_data`
merged_table <- allFit %>%
  mutate(Title = str_trim(Title)) %>%
  left_join(
    final_data %>%
      select(
        Class_Model,
        Perc_Convergence,
        Replicated_LL_Perc,
        Smallest_Class,
        Smallest_Class_Perc
      ),
    by = c("Title" = "Class_Model")
  ) %>%
  mutate(Smallest_Class = coalesce(Smallest_Class, final_data$Smallest_Class[match(Title, final_data$Class_Model)])) %>%
  relocate(Perc_Convergence, Replicated_LL_Perc, .after = LL) %>%
  mutate(Smallest_Class_Combined = paste0(Smallest_Class, "\u00A0(", Smallest_Class_Perc, "%)")) %>%
  select(
    Title,
    Parameters,
    LL,
    Perc_Convergence,
    Replicated_LL_Perc,
    BIC,
    aBIC,
    CAIC,
    AWE,
    T11_VLMR_PValue,
    BLRT_PValue,
    Smallest_Class_Combined,
    BF,
    cmPk
  )
```

Then, create table:


``` r
fit_table1 <- merged_table %>%
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
  sub_missing(1:11,
              missing_text = "--") %>%
  fmt(
    c(T11_VLMR_PValue, BLRT_PValue),
    fns = function(x)
      ifelse(x < 0.001, "<.001",
             scales::number(x, accuracy = .01))
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

fit_table1
```


```{=html}
<div id="ftdcswmhel" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ftdcswmhel table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ftdcswmhel thead, #ftdcswmhel tbody, #ftdcswmhel tfoot, #ftdcswmhel tr, #ftdcswmhel td, #ftdcswmhel th {
  border-style: none;
}

#ftdcswmhel p {
  margin: 0;
  padding: 0;
}

#ftdcswmhel .gt_table {
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

#ftdcswmhel .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ftdcswmhel .gt_title {
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

#ftdcswmhel .gt_subtitle {
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

#ftdcswmhel .gt_heading {
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

#ftdcswmhel .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ftdcswmhel .gt_col_headings {
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

#ftdcswmhel .gt_col_heading {
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

#ftdcswmhel .gt_column_spanner_outer {
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

#ftdcswmhel .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ftdcswmhel .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ftdcswmhel .gt_column_spanner {
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

#ftdcswmhel .gt_spanner_row {
  border-bottom-style: hidden;
}

#ftdcswmhel .gt_group_heading {
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

#ftdcswmhel .gt_empty_group_heading {
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

#ftdcswmhel .gt_from_md > :first-child {
  margin-top: 0;
}

#ftdcswmhel .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ftdcswmhel .gt_row {
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

#ftdcswmhel .gt_stub {
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

#ftdcswmhel .gt_stub_row_group {
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

#ftdcswmhel .gt_row_group_first td {
  border-top-width: 2px;
}

#ftdcswmhel .gt_row_group_first th {
  border-top-width: 2px;
}

#ftdcswmhel .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ftdcswmhel .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ftdcswmhel .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ftdcswmhel .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ftdcswmhel .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ftdcswmhel .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ftdcswmhel .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ftdcswmhel .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ftdcswmhel .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ftdcswmhel .gt_footnotes {
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

#ftdcswmhel .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ftdcswmhel .gt_sourcenotes {
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

#ftdcswmhel .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ftdcswmhel .gt_left {
  text-align: left;
}

#ftdcswmhel .gt_center {
  text-align: center;
}

#ftdcswmhel .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ftdcswmhel .gt_font_normal {
  font-weight: normal;
}

#ftdcswmhel .gt_font_bold {
  font-weight: bold;
}

#ftdcswmhel .gt_font_italic {
  font-style: italic;
}

#ftdcswmhel .gt_super {
  font-size: 65%;
}

#ftdcswmhel .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ftdcswmhel .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ftdcswmhel .gt_indent_1 {
  text-indent: 5px;
}

#ftdcswmhel .gt_indent_2 {
  text-indent: 10px;
}

#ftdcswmhel .gt_indent_3 {
  text-indent: 15px;
}

#ftdcswmhel .gt_indent_4 {
  text-indent: 20px;
}

#ftdcswmhel .gt_indent_5 {
  text-indent: 25px;
}

#ftdcswmhel .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ftdcswmhel div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>Par</span></th>
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
<td headers="Parameters" class="gt_row gt_center">6</td>
<td headers="LL" class="gt_row gt_center">−5,443.41</td>
<td headers="Perc_Convergence" class="gt_row gt_center">100%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
<td headers="BIC" class="gt_row gt_center">10,932.50</td>
<td headers="aBIC" class="gt_row gt_center">10,913.44</td>
<td headers="CAIC" class="gt_row gt_center">10,938.50</td>
<td headers="AWE" class="gt_row gt_center">10,996.19</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">–</td>
<td headers="BLRT_PValue" class="gt_row gt_center">–</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">2027 (100%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">2-Class</td>
<td headers="Parameters" class="gt_row gt_center">13</td>
<td headers="LL" class="gt_row gt_center">−5,194.14</td>
<td headers="Perc_Convergence" class="gt_row gt_center">57%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
<td headers="BIC" class="gt_row gt_center">10,487.26</td>
<td headers="aBIC" class="gt_row gt_center">10,445.96</td>
<td headers="CAIC" class="gt_row gt_center">10,500.26</td>
<td headers="AWE" class="gt_row gt_center">10,625.24</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">444 (21.9%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">3-Class</td>
<td headers="Parameters" class="gt_row gt_center">20</td>
<td headers="LL" class="gt_row gt_center">−5,122.48</td>
<td headers="Perc_Convergence" class="gt_row gt_center">93%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">86%</td>
<td headers="BIC" class="gt_row gt_center" style="font-weight: bold;">10,397.24</td>
<td headers="aBIC" class="gt_row gt_center" style="font-weight: bold;">10,333.70</td>
<td headers="CAIC" class="gt_row gt_center" style="font-weight: bold;">10,417.24</td>
<td headers="AWE" class="gt_row gt_center" style="font-weight: bold;">10,609.53</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">216 (10.6%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">4-Class</td>
<td headers="Parameters" class="gt_row gt_center">27</td>
<td headers="LL" class="gt_row gt_center">−5,111.76</td>
<td headers="Perc_Convergence" class="gt_row gt_center">47%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">43%</td>
<td headers="BIC" class="gt_row gt_center">10,429.10</td>
<td headers="aBIC" class="gt_row gt_center">10,343.32</td>
<td headers="CAIC" class="gt_row gt_center">10,456.10</td>
<td headers="AWE" class="gt_row gt_center">10,715.69</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.01</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;"><.001</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">212 (10.5%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">5-Class</td>
<td headers="Parameters" class="gt_row gt_center">34</td>
<td headers="LL" class="gt_row gt_center">−5,105.59</td>
<td headers="Perc_Convergence" class="gt_row gt_center">37%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">19%</td>
<td headers="BIC" class="gt_row gt_center">10,470.07</td>
<td headers="aBIC" class="gt_row gt_center">10,362.04</td>
<td headers="CAIC" class="gt_row gt_center">10,504.06</td>
<td headers="AWE" class="gt_row gt_center">10,830.95</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.18</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.29</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">43 (2.1%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">6-Class</td>
<td headers="Parameters" class="gt_row gt_center">41</td>
<td headers="LL" class="gt_row gt_center">−5,099.88</td>
<td headers="Perc_Convergence" class="gt_row gt_center">32%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">12%</td>
<td headers="BIC" class="gt_row gt_center">10,511.95</td>
<td headers="aBIC" class="gt_row gt_center">10,381.69</td>
<td headers="CAIC" class="gt_row gt_center">10,552.95</td>
<td headers="AWE" class="gt_row gt_center">10,947.14</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.18</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.38</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">36 (1.8%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="12"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> Par = Parameters; <em>LL</em> = model log likelihood;
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

<img src="03-enumeration_files/figure-html/unnamed-chunk-19-1.png" width="672" />

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

for (i in 1:length(output_enum)) {
  
  temp <- output_enum[[i]]$parameters$probability.scale %>%                                       
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

<img src="03-enumeration_files/figure-html/unnamed-chunk-21-1.png" width="672" />

------------------------------------------------------------------------

Save figure:


``` r
ggsave(here("figures", "compare_kclass_plot.png"), dpi=300, height=5, width=7, units="in")
```


<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
