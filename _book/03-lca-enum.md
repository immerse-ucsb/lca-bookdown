


# (PART) Latent Class Analysis {-}

# Enumeration {#lca-enum}

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
<div id="mybefapnid" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mybefapnid table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mybefapnid thead, #mybefapnid tbody, #mybefapnid tfoot, #mybefapnid tr, #mybefapnid td, #mybefapnid th {
  border-style: none;
}

#mybefapnid p {
  margin: 0;
  padding: 0;
}

#mybefapnid .gt_table {
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

#mybefapnid .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mybefapnid .gt_title {
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

#mybefapnid .gt_subtitle {
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

#mybefapnid .gt_heading {
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

#mybefapnid .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mybefapnid .gt_col_headings {
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

#mybefapnid .gt_col_heading {
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

#mybefapnid .gt_column_spanner_outer {
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

#mybefapnid .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mybefapnid .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mybefapnid .gt_column_spanner {
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

#mybefapnid .gt_spanner_row {
  border-bottom-style: hidden;
}

#mybefapnid .gt_group_heading {
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

#mybefapnid .gt_empty_group_heading {
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

#mybefapnid .gt_from_md > :first-child {
  margin-top: 0;
}

#mybefapnid .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mybefapnid .gt_row {
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

#mybefapnid .gt_stub {
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

#mybefapnid .gt_stub_row_group {
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

#mybefapnid .gt_row_group_first td {
  border-top-width: 2px;
}

#mybefapnid .gt_row_group_first th {
  border-top-width: 2px;
}

#mybefapnid .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mybefapnid .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mybefapnid .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mybefapnid .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mybefapnid .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mybefapnid .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mybefapnid .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mybefapnid .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mybefapnid .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mybefapnid .gt_footnotes {
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

#mybefapnid .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mybefapnid .gt_sourcenotes {
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

#mybefapnid .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mybefapnid .gt_left {
  text-align: left;
}

#mybefapnid .gt_center {
  text-align: center;
}

#mybefapnid .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mybefapnid .gt_font_normal {
  font-weight: normal;
}

#mybefapnid .gt_font_bold {
  font-weight: bold;
}

#mybefapnid .gt_font_italic {
  font-style: italic;
}

#mybefapnid .gt_super {
  font-size: 65%;
}

#mybefapnid .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mybefapnid .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mybefapnid .gt_indent_1 {
  text-indent: 5px;
}

#mybefapnid .gt_indent_2 {
  text-indent: 10px;
}

#mybefapnid .gt_indent_3 {
  text-indent: 15px;
}

#mybefapnid .gt_indent_4 {
  text-indent: 20px;
}

#mybefapnid .gt_indent_5 {
  text-indent: 25px;
}

#mybefapnid .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mybefapnid div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<div class="grViz html-widget html-fill-item" id="htmlwidget-fd209f7597903a49569b" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-fd209f7597903a49569b">{"x":{"diagram":" digraph cfa_model {\n\n# The `graph` statement - No editing needed\n\n    graph [layout = dot, overlap = true]\n \n# Two `node` statements\n \n# One for measured variables (box) \n\n    node [shape=box]\n    report_dis report_race report_sex counselors_fte psych_fte law_fte;\n \n# One for latent variables (circle) \n \n    node [shape=circle]\n    bully [label=<Bullying <br/>C<sub>k<\/sub>>];\n    \n# `edge` statements\n \n    edge [minlen = 2]\n    bully -> {report_dis report_race report_sex counselors_fte psych_fte law_fte}\n \n }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
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
<div id="avxqjwieju" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#avxqjwieju table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#avxqjwieju thead, #avxqjwieju tbody, #avxqjwieju tfoot, #avxqjwieju tr, #avxqjwieju td, #avxqjwieju th {
  border-style: none;
}

#avxqjwieju p {
  margin: 0;
  padding: 0;
}

#avxqjwieju .gt_table {
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

#avxqjwieju .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#avxqjwieju .gt_title {
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

#avxqjwieju .gt_subtitle {
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

#avxqjwieju .gt_heading {
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

#avxqjwieju .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avxqjwieju .gt_col_headings {
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

#avxqjwieju .gt_col_heading {
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

#avxqjwieju .gt_column_spanner_outer {
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

#avxqjwieju .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#avxqjwieju .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#avxqjwieju .gt_column_spanner {
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

#avxqjwieju .gt_spanner_row {
  border-bottom-style: hidden;
}

#avxqjwieju .gt_group_heading {
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

#avxqjwieju .gt_empty_group_heading {
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

#avxqjwieju .gt_from_md > :first-child {
  margin-top: 0;
}

#avxqjwieju .gt_from_md > :last-child {
  margin-bottom: 0;
}

#avxqjwieju .gt_row {
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

#avxqjwieju .gt_stub {
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

#avxqjwieju .gt_stub_row_group {
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

#avxqjwieju .gt_row_group_first td {
  border-top-width: 2px;
}

#avxqjwieju .gt_row_group_first th {
  border-top-width: 2px;
}

#avxqjwieju .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#avxqjwieju .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#avxqjwieju .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#avxqjwieju .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avxqjwieju .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#avxqjwieju .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#avxqjwieju .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#avxqjwieju .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#avxqjwieju .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#avxqjwieju .gt_footnotes {
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

#avxqjwieju .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#avxqjwieju .gt_sourcenotes {
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

#avxqjwieju .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#avxqjwieju .gt_left {
  text-align: left;
}

#avxqjwieju .gt_center {
  text-align: center;
}

#avxqjwieju .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#avxqjwieju .gt_font_normal {
  font-weight: normal;
}

#avxqjwieju .gt_font_bold {
  font-weight: bold;
}

#avxqjwieju .gt_font_italic {
  font-style: italic;
}

#avxqjwieju .gt_super {
  font-size: 65%;
}

#avxqjwieju .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#avxqjwieju .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#avxqjwieju .gt_indent_1 {
  text-indent: 5px;
}

#avxqjwieju .gt_indent_2 {
  text-indent: 10px;
}

#avxqjwieju .gt_indent_3 {
  text-indent: 15px;
}

#avxqjwieju .gt_indent_4 {
  text-indent: 20px;
}

#avxqjwieju .gt_indent_5 {
  text-indent: 25px;
}

#avxqjwieju .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#avxqjwieju div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

<img src="03-lca-enum_files/figure-html/unnamed-chunk-7-1.png" width="90%" />

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
    starts = 500 100; 
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
<div id="ivwvhstegg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ivwvhstegg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ivwvhstegg thead, #ivwvhstegg tbody, #ivwvhstegg tfoot, #ivwvhstegg tr, #ivwvhstegg td, #ivwvhstegg th {
  border-style: none;
}

#ivwvhstegg p {
  margin: 0;
  padding: 0;
}

#ivwvhstegg .gt_table {
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

#ivwvhstegg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ivwvhstegg .gt_title {
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

#ivwvhstegg .gt_subtitle {
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

#ivwvhstegg .gt_heading {
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

#ivwvhstegg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivwvhstegg .gt_col_headings {
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

#ivwvhstegg .gt_col_heading {
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

#ivwvhstegg .gt_column_spanner_outer {
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

#ivwvhstegg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ivwvhstegg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ivwvhstegg .gt_column_spanner {
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

#ivwvhstegg .gt_spanner_row {
  border-bottom-style: hidden;
}

#ivwvhstegg .gt_group_heading {
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

#ivwvhstegg .gt_empty_group_heading {
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

#ivwvhstegg .gt_from_md > :first-child {
  margin-top: 0;
}

#ivwvhstegg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ivwvhstegg .gt_row {
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

#ivwvhstegg .gt_stub {
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

#ivwvhstegg .gt_stub_row_group {
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

#ivwvhstegg .gt_row_group_first td {
  border-top-width: 2px;
}

#ivwvhstegg .gt_row_group_first th {
  border-top-width: 2px;
}

#ivwvhstegg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivwvhstegg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ivwvhstegg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ivwvhstegg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivwvhstegg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivwvhstegg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ivwvhstegg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ivwvhstegg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ivwvhstegg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivwvhstegg .gt_footnotes {
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

#ivwvhstegg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivwvhstegg .gt_sourcenotes {
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

#ivwvhstegg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivwvhstegg .gt_left {
  text-align: left;
}

#ivwvhstegg .gt_center {
  text-align: center;
}

#ivwvhstegg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ivwvhstegg .gt_font_normal {
  font-weight: normal;
}

#ivwvhstegg .gt_font_bold {
  font-weight: bold;
}

#ivwvhstegg .gt_font_italic {
  font-style: italic;
}

#ivwvhstegg .gt_super {
  font-size: 65%;
}

#ivwvhstegg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ivwvhstegg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ivwvhstegg .gt_indent_1 {
  text-indent: 5px;
}

#ivwvhstegg .gt_indent_2 {
  text-indent: 10px;
}

#ivwvhstegg .gt_indent_3 {
  text-indent: 15px;
}

#ivwvhstegg .gt_indent_4 {
  text-indent: 20px;
}

#ivwvhstegg .gt_indent_5 {
  text-indent: 25px;
}

#ivwvhstegg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ivwvhstegg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<div id="lsjfsycwfx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lsjfsycwfx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lsjfsycwfx thead, #lsjfsycwfx tbody, #lsjfsycwfx tfoot, #lsjfsycwfx tr, #lsjfsycwfx td, #lsjfsycwfx th {
  border-style: none;
}

#lsjfsycwfx p {
  margin: 0;
  padding: 0;
}

#lsjfsycwfx .gt_table {
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

#lsjfsycwfx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lsjfsycwfx .gt_title {
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

#lsjfsycwfx .gt_subtitle {
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

#lsjfsycwfx .gt_heading {
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

#lsjfsycwfx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lsjfsycwfx .gt_col_headings {
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

#lsjfsycwfx .gt_col_heading {
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

#lsjfsycwfx .gt_column_spanner_outer {
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

#lsjfsycwfx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lsjfsycwfx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lsjfsycwfx .gt_column_spanner {
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

#lsjfsycwfx .gt_spanner_row {
  border-bottom-style: hidden;
}

#lsjfsycwfx .gt_group_heading {
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

#lsjfsycwfx .gt_empty_group_heading {
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

#lsjfsycwfx .gt_from_md > :first-child {
  margin-top: 0;
}

#lsjfsycwfx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lsjfsycwfx .gt_row {
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

#lsjfsycwfx .gt_stub {
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

#lsjfsycwfx .gt_stub_row_group {
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

#lsjfsycwfx .gt_row_group_first td {
  border-top-width: 2px;
}

#lsjfsycwfx .gt_row_group_first th {
  border-top-width: 2px;
}

#lsjfsycwfx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lsjfsycwfx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lsjfsycwfx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lsjfsycwfx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lsjfsycwfx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lsjfsycwfx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lsjfsycwfx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lsjfsycwfx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lsjfsycwfx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lsjfsycwfx .gt_footnotes {
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

#lsjfsycwfx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lsjfsycwfx .gt_sourcenotes {
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

#lsjfsycwfx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lsjfsycwfx .gt_left {
  text-align: left;
}

#lsjfsycwfx .gt_center {
  text-align: center;
}

#lsjfsycwfx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lsjfsycwfx .gt_font_normal {
  font-weight: normal;
}

#lsjfsycwfx .gt_font_bold {
  font-weight: bold;
}

#lsjfsycwfx .gt_font_italic {
  font-style: italic;
}

#lsjfsycwfx .gt_super {
  font-size: 65%;
}

#lsjfsycwfx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lsjfsycwfx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lsjfsycwfx .gt_indent_1 {
  text-indent: 5px;
}

#lsjfsycwfx .gt_indent_2 {
  text-indent: 10px;
}

#lsjfsycwfx .gt_indent_3 {
  text-indent: 15px;
}

#lsjfsycwfx .gt_indent_4 {
  text-indent: 20px;
}

#lsjfsycwfx .gt_indent_5 {
  text-indent: 25px;
}

#lsjfsycwfx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lsjfsycwfx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<div class="tabwid"><style>.cl-55ea914e{}.cl-55e23a30{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-55e23a3a{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-55e23a3b{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-55e579f2{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-55e579fc{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-55e57a06{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-55e5ad82{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ad8c{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ad96{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ad97{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ada0{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ada1{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ada2{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adaa{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adab{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adb4{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adb5{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adb6{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adb7{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adbe{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adbf{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adc0{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adc8{width:0.5in;background-color:rgba(249, 202, 189, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adc9{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adca{width:0.5in;background-color:rgba(252, 221, 212, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5add2{width:0.5in;background-color:rgba(235, 121, 92, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5add3{width:0.5in;background-color:rgba(234, 113, 82, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5add4{width:0.5in;background-color:rgba(228, 81, 43, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5add5{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5addc{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5addd{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5adde{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ade6{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ade7{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e5ade8{width:0.5in;background-color:rgba(228, 78, 39, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-55ea914e'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-55e5ad82"><p class="cl-55e579f2"><span class="cl-55e23a30">N</span><span class="cl-55e23a3a"> = </span><span class="cl-55e23a3a">2027</span></p></th><th  colspan="2"class="cl-55e5ad97"><p class="cl-55e579fc"><span class="cl-55e23a3a">Random Starts</span></p></th><th  colspan="2"class="cl-55e5ad97"><p class="cl-55e579fc"><span class="cl-55e23a3a">Final starting value sets converging</span></p></th><th  colspan="2"class="cl-55e5ad97"><p class="cl-55e579fc"><span class="cl-55e23a3a">LL Replication</span></p></th><th  colspan="2"class="cl-55e5ad97"><p class="cl-55e579fc"><span class="cl-55e23a3a">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-55e5ada0"><p class="cl-55e579f2"><span class="cl-55e23a3a">Model</span></p></th><th class="cl-55e5ada1"><p class="cl-55e579fc"><span class="cl-55e23a3a">Best LL</span></p></th><th class="cl-55e5ada2"><p class="cl-55e579fc"><span class="cl-55e23a3a">npar</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a3a">Initial</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a3a">Final</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a30">f</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a3a">%</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a30">f</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a3a">%</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a30">f</span></p></th><th class="cl-55e5adaa"><p class="cl-55e579fc"><span class="cl-55e23a3a">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-55e5adab"><p class="cl-55e57a06"><span class="cl-55e23a3a">1-Class</span></p></td><td class="cl-55e5adb4"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,443.409</span></p></td><td class="cl-55e5adb5"><p class="cl-55e57a06"><span class="cl-55e23a3a">6</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100.0%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">2,027</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-55e5adb7"><p class="cl-55e57a06"><span class="cl-55e23a3a">2-Class</span></p></td><td class="cl-55e5adbe"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,194.136</span></p></td><td class="cl-55e5adbf"><p class="cl-55e57a06"><span class="cl-55e23a3a">13</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">86</span></p></td><td class="cl-55e5adc8"><p class="cl-55e57a06"><span class="cl-55e23a3a">86%</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">86</span></p></td><td class="cl-55e5adc9"><p class="cl-55e57a06"><span class="cl-55e23a3a">100.0%</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">444</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">21.9%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-55e5adab"><p class="cl-55e57a06"><span class="cl-55e23a3a">3-Class</span></p></td><td class="cl-55e5adb4"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,122.478</span></p></td><td class="cl-55e5adb5"><p class="cl-55e57a06"><span class="cl-55e23a3a">20</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">94</span></p></td><td class="cl-55e5adca"><p class="cl-55e57a06"><span class="cl-55e23a3a">94%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">94</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100.0%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">216</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">10.6%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-55e5adb7"><p class="cl-55e57a06"><span class="cl-55e23a3a">4-Class</span></p></td><td class="cl-55e5adbe"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,111.757</span></p></td><td class="cl-55e5adbf"><p class="cl-55e57a06"><span class="cl-55e23a3a">27</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">54</span></p></td><td class="cl-55e5add2"><p class="cl-55e57a06"><span class="cl-55e23a3b">54%</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">19</span></p></td><td class="cl-55e5add3"><p class="cl-55e57a06"><span class="cl-55e23a3a">35.2%</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">212</span></p></td><td class="cl-55e5adc0"><p class="cl-55e57a06"><span class="cl-55e23a3a">10.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-55e5adab"><p class="cl-55e57a06"><span class="cl-55e23a3a">5-Class</span></p></td><td class="cl-55e5adb4"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,105.589</span></p></td><td class="cl-55e5adb5"><p class="cl-55e57a06"><span class="cl-55e23a3a">34</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">38</span></p></td><td class="cl-55e5add4"><p class="cl-55e57a06"><span class="cl-55e23a3b">38%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">6</span></p></td><td class="cl-55e5add5"><p class="cl-55e57a06"><span class="cl-55e23a3b">15.8%</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">43</span></p></td><td class="cl-55e5adb6"><p class="cl-55e57a06"><span class="cl-55e23a3a">2.1%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-55e5addc"><p class="cl-55e57a06"><span class="cl-55e23a3a">6-Class</span></p></td><td class="cl-55e5addd"><p class="cl-55e57a06"><span class="cl-55e23a3a">-5,099.881</span></p></td><td class="cl-55e5adde"><p class="cl-55e57a06"><span class="cl-55e23a3a">41</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">500</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">100</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">36</span></p></td><td class="cl-55e5ade7"><p class="cl-55e57a06"><span class="cl-55e23a3b">36%</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">6</span></p></td><td class="cl-55e5ade8"><p class="cl-55e57a06"><span class="cl-55e23a3b">16.7%</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">36</span></p></td><td class="cl-55e5ade6"><p class="cl-55e57a06"><span class="cl-55e23a3a">1.8%</span></p></td></tr></tbody></table></div>
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
<div id="ybnqxlxddk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#ybnqxlxddk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ybnqxlxddk thead, #ybnqxlxddk tbody, #ybnqxlxddk tfoot, #ybnqxlxddk tr, #ybnqxlxddk td, #ybnqxlxddk th {
  border-style: none;
}

#ybnqxlxddk p {
  margin: 0;
  padding: 0;
}

#ybnqxlxddk .gt_table {
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

#ybnqxlxddk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ybnqxlxddk .gt_title {
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

#ybnqxlxddk .gt_subtitle {
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

#ybnqxlxddk .gt_heading {
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

#ybnqxlxddk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybnqxlxddk .gt_col_headings {
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

#ybnqxlxddk .gt_col_heading {
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

#ybnqxlxddk .gt_column_spanner_outer {
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

#ybnqxlxddk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ybnqxlxddk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ybnqxlxddk .gt_column_spanner {
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

#ybnqxlxddk .gt_spanner_row {
  border-bottom-style: hidden;
}

#ybnqxlxddk .gt_group_heading {
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

#ybnqxlxddk .gt_empty_group_heading {
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

#ybnqxlxddk .gt_from_md > :first-child {
  margin-top: 0;
}

#ybnqxlxddk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ybnqxlxddk .gt_row {
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

#ybnqxlxddk .gt_stub {
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

#ybnqxlxddk .gt_stub_row_group {
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

#ybnqxlxddk .gt_row_group_first td {
  border-top-width: 2px;
}

#ybnqxlxddk .gt_row_group_first th {
  border-top-width: 2px;
}

#ybnqxlxddk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybnqxlxddk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ybnqxlxddk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ybnqxlxddk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybnqxlxddk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybnqxlxddk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ybnqxlxddk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ybnqxlxddk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ybnqxlxddk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybnqxlxddk .gt_footnotes {
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

#ybnqxlxddk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybnqxlxddk .gt_sourcenotes {
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

#ybnqxlxddk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybnqxlxddk .gt_left {
  text-align: left;
}

#ybnqxlxddk .gt_center {
  text-align: center;
}

#ybnqxlxddk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ybnqxlxddk .gt_font_normal {
  font-weight: normal;
}

#ybnqxlxddk .gt_font_bold {
  font-weight: bold;
}

#ybnqxlxddk .gt_font_italic {
  font-style: italic;
}

#ybnqxlxddk .gt_super {
  font-size: 65%;
}

#ybnqxlxddk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ybnqxlxddk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ybnqxlxddk .gt_indent_1 {
  text-indent: 5px;
}

#ybnqxlxddk .gt_indent_2 {
  text-indent: 10px;
}

#ybnqxlxddk .gt_indent_3 {
  text-indent: 15px;
}

#ybnqxlxddk .gt_indent_4 {
  text-indent: 20px;
}

#ybnqxlxddk .gt_indent_5 {
  text-indent: 25px;
}

#ybnqxlxddk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ybnqxlxddk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<div id="hrgymzkfop" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#hrgymzkfop table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hrgymzkfop thead, #hrgymzkfop tbody, #hrgymzkfop tfoot, #hrgymzkfop tr, #hrgymzkfop td, #hrgymzkfop th {
  border-style: none;
}

#hrgymzkfop p {
  margin: 0;
  padding: 0;
}

#hrgymzkfop .gt_table {
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

#hrgymzkfop .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hrgymzkfop .gt_title {
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

#hrgymzkfop .gt_subtitle {
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

#hrgymzkfop .gt_heading {
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

#hrgymzkfop .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hrgymzkfop .gt_col_headings {
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

#hrgymzkfop .gt_col_heading {
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

#hrgymzkfop .gt_column_spanner_outer {
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

#hrgymzkfop .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hrgymzkfop .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hrgymzkfop .gt_column_spanner {
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

#hrgymzkfop .gt_spanner_row {
  border-bottom-style: hidden;
}

#hrgymzkfop .gt_group_heading {
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

#hrgymzkfop .gt_empty_group_heading {
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

#hrgymzkfop .gt_from_md > :first-child {
  margin-top: 0;
}

#hrgymzkfop .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hrgymzkfop .gt_row {
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

#hrgymzkfop .gt_stub {
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

#hrgymzkfop .gt_stub_row_group {
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

#hrgymzkfop .gt_row_group_first td {
  border-top-width: 2px;
}

#hrgymzkfop .gt_row_group_first th {
  border-top-width: 2px;
}

#hrgymzkfop .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hrgymzkfop .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hrgymzkfop .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hrgymzkfop .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hrgymzkfop .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hrgymzkfop .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hrgymzkfop .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hrgymzkfop .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hrgymzkfop .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hrgymzkfop .gt_footnotes {
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

#hrgymzkfop .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hrgymzkfop .gt_sourcenotes {
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

#hrgymzkfop .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hrgymzkfop .gt_left {
  text-align: left;
}

#hrgymzkfop .gt_center {
  text-align: center;
}

#hrgymzkfop .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hrgymzkfop .gt_font_normal {
  font-weight: normal;
}

#hrgymzkfop .gt_font_bold {
  font-weight: bold;
}

#hrgymzkfop .gt_font_italic {
  font-style: italic;
}

#hrgymzkfop .gt_super {
  font-size: 65%;
}

#hrgymzkfop .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hrgymzkfop .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hrgymzkfop .gt_indent_1 {
  text-indent: 5px;
}

#hrgymzkfop .gt_indent_2 {
  text-indent: 10px;
}

#hrgymzkfop .gt_indent_3 {
  text-indent: 15px;
}

#hrgymzkfop .gt_indent_4 {
  text-indent: 20px;
}

#hrgymzkfop .gt_indent_5 {
  text-indent: 25px;
}

#hrgymzkfop .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hrgymzkfop div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">86.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c3_bully.out
<div id="bmhyuevicr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#bmhyuevicr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bmhyuevicr thead, #bmhyuevicr tbody, #bmhyuevicr tfoot, #bmhyuevicr tr, #bmhyuevicr td, #bmhyuevicr th {
  border-style: none;
}

#bmhyuevicr p {
  margin: 0;
  padding: 0;
}

#bmhyuevicr .gt_table {
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

#bmhyuevicr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bmhyuevicr .gt_title {
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

#bmhyuevicr .gt_subtitle {
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

#bmhyuevicr .gt_heading {
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

#bmhyuevicr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bmhyuevicr .gt_col_headings {
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

#bmhyuevicr .gt_col_heading {
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

#bmhyuevicr .gt_column_spanner_outer {
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

#bmhyuevicr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bmhyuevicr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bmhyuevicr .gt_column_spanner {
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

#bmhyuevicr .gt_spanner_row {
  border-bottom-style: hidden;
}

#bmhyuevicr .gt_group_heading {
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

#bmhyuevicr .gt_empty_group_heading {
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

#bmhyuevicr .gt_from_md > :first-child {
  margin-top: 0;
}

#bmhyuevicr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bmhyuevicr .gt_row {
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

#bmhyuevicr .gt_stub {
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

#bmhyuevicr .gt_stub_row_group {
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

#bmhyuevicr .gt_row_group_first td {
  border-top-width: 2px;
}

#bmhyuevicr .gt_row_group_first th {
  border-top-width: 2px;
}

#bmhyuevicr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bmhyuevicr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bmhyuevicr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bmhyuevicr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bmhyuevicr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bmhyuevicr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bmhyuevicr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bmhyuevicr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bmhyuevicr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bmhyuevicr .gt_footnotes {
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

#bmhyuevicr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bmhyuevicr .gt_sourcenotes {
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

#bmhyuevicr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bmhyuevicr .gt_left {
  text-align: left;
}

#bmhyuevicr .gt_center {
  text-align: center;
}

#bmhyuevicr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bmhyuevicr .gt_font_normal {
  font-weight: normal;
}

#bmhyuevicr .gt_font_bold {
  font-weight: bold;
}

#bmhyuevicr .gt_font_italic {
  font-style: italic;
}

#bmhyuevicr .gt_super {
  font-size: 65%;
}

#bmhyuevicr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bmhyuevicr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bmhyuevicr .gt_indent_1 {
  text-indent: 5px;
}

#bmhyuevicr .gt_indent_2 {
  text-indent: 10px;
}

#bmhyuevicr .gt_indent_3 {
  text-indent: 15px;
}

#bmhyuevicr .gt_indent_4 {
  text-indent: 20px;
}

#bmhyuevicr .gt_indent_5 {
  text-indent: 25px;
}

#bmhyuevicr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bmhyuevicr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,122.478</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">94.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">100.00%</td></tr>
  </tbody>
  
  
</table>
</div>

$c4_bully.out
<div id="ivfrfgjvaj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#ivfrfgjvaj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ivfrfgjvaj thead, #ivfrfgjvaj tbody, #ivfrfgjvaj tfoot, #ivfrfgjvaj tr, #ivfrfgjvaj td, #ivfrfgjvaj th {
  border-style: none;
}

#ivfrfgjvaj p {
  margin: 0;
  padding: 0;
}

#ivfrfgjvaj .gt_table {
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

#ivfrfgjvaj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ivfrfgjvaj .gt_title {
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

#ivfrfgjvaj .gt_subtitle {
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

#ivfrfgjvaj .gt_heading {
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

#ivfrfgjvaj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivfrfgjvaj .gt_col_headings {
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

#ivfrfgjvaj .gt_col_heading {
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

#ivfrfgjvaj .gt_column_spanner_outer {
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

#ivfrfgjvaj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ivfrfgjvaj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ivfrfgjvaj .gt_column_spanner {
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

#ivfrfgjvaj .gt_spanner_row {
  border-bottom-style: hidden;
}

#ivfrfgjvaj .gt_group_heading {
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

#ivfrfgjvaj .gt_empty_group_heading {
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

#ivfrfgjvaj .gt_from_md > :first-child {
  margin-top: 0;
}

#ivfrfgjvaj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ivfrfgjvaj .gt_row {
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

#ivfrfgjvaj .gt_stub {
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

#ivfrfgjvaj .gt_stub_row_group {
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

#ivfrfgjvaj .gt_row_group_first td {
  border-top-width: 2px;
}

#ivfrfgjvaj .gt_row_group_first th {
  border-top-width: 2px;
}

#ivfrfgjvaj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivfrfgjvaj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ivfrfgjvaj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ivfrfgjvaj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivfrfgjvaj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivfrfgjvaj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ivfrfgjvaj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ivfrfgjvaj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ivfrfgjvaj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ivfrfgjvaj .gt_footnotes {
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

#ivfrfgjvaj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivfrfgjvaj .gt_sourcenotes {
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

#ivfrfgjvaj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ivfrfgjvaj .gt_left {
  text-align: left;
}

#ivfrfgjvaj .gt_center {
  text-align: center;
}

#ivfrfgjvaj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ivfrfgjvaj .gt_font_normal {
  font-weight: normal;
}

#ivfrfgjvaj .gt_font_bold {
  font-weight: bold;
}

#ivfrfgjvaj .gt_font_italic {
  font-style: italic;
}

#ivfrfgjvaj .gt_super {
  font-size: 65%;
}

#ivfrfgjvaj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ivfrfgjvaj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ivfrfgjvaj .gt_indent_1 {
  text-indent: 5px;
}

#ivfrfgjvaj .gt_indent_2 {
  text-indent: 10px;
}

#ivfrfgjvaj .gt_indent_3 {
  text-indent: 15px;
}

#ivfrfgjvaj .gt_indent_4 {
  text-indent: 20px;
}

#ivfrfgjvaj .gt_indent_5 {
  text-indent: 25px;
}

#ivfrfgjvaj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ivfrfgjvaj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Count" class="gt_row gt_center">19.000</td>
<td headers="Percentage" class="gt_row gt_center">35.19%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,111.759</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">7.41%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,112.253</td>
<td headers="Count" class="gt_row gt_center">5.000</td>
<td headers="Percentage" class="gt_row gt_center">9.26%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,113.910</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">1.85%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,115.532</td>
<td headers="Count" class="gt_row gt_center">17.000</td>
<td headers="Percentage" class="gt_row gt_center">31.48%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,115.538</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">1.85%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,116.981</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">5.56%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,117.829</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">3.70%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,117.837</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">3.70%</td></tr>
  </tbody>
  
  
</table>
</div>

$c5_bully.out
<div id="mmhigqsndf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#mmhigqsndf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mmhigqsndf thead, #mmhigqsndf tbody, #mmhigqsndf tfoot, #mmhigqsndf tr, #mmhigqsndf td, #mmhigqsndf th {
  border-style: none;
}

#mmhigqsndf p {
  margin: 0;
  padding: 0;
}

#mmhigqsndf .gt_table {
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

#mmhigqsndf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mmhigqsndf .gt_title {
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

#mmhigqsndf .gt_subtitle {
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

#mmhigqsndf .gt_heading {
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

#mmhigqsndf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mmhigqsndf .gt_col_headings {
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

#mmhigqsndf .gt_col_heading {
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

#mmhigqsndf .gt_column_spanner_outer {
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

#mmhigqsndf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mmhigqsndf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mmhigqsndf .gt_column_spanner {
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

#mmhigqsndf .gt_spanner_row {
  border-bottom-style: hidden;
}

#mmhigqsndf .gt_group_heading {
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

#mmhigqsndf .gt_empty_group_heading {
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

#mmhigqsndf .gt_from_md > :first-child {
  margin-top: 0;
}

#mmhigqsndf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mmhigqsndf .gt_row {
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

#mmhigqsndf .gt_stub {
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

#mmhigqsndf .gt_stub_row_group {
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

#mmhigqsndf .gt_row_group_first td {
  border-top-width: 2px;
}

#mmhigqsndf .gt_row_group_first th {
  border-top-width: 2px;
}

#mmhigqsndf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mmhigqsndf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mmhigqsndf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mmhigqsndf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mmhigqsndf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mmhigqsndf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mmhigqsndf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mmhigqsndf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mmhigqsndf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mmhigqsndf .gt_footnotes {
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

#mmhigqsndf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mmhigqsndf .gt_sourcenotes {
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

#mmhigqsndf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mmhigqsndf .gt_left {
  text-align: left;
}

#mmhigqsndf .gt_center {
  text-align: center;
}

#mmhigqsndf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mmhigqsndf .gt_font_normal {
  font-weight: normal;
}

#mmhigqsndf .gt_font_bold {
  font-weight: bold;
}

#mmhigqsndf .gt_font_italic {
  font-style: italic;
}

#mmhigqsndf .gt_super {
  font-size: 65%;
}

#mmhigqsndf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mmhigqsndf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mmhigqsndf .gt_indent_1 {
  text-indent: 5px;
}

#mmhigqsndf .gt_indent_2 {
  text-indent: 10px;
}

#mmhigqsndf .gt_indent_3 {
  text-indent: 15px;
}

#mmhigqsndf .gt_indent_4 {
  text-indent: 20px;
}

#mmhigqsndf .gt_indent_5 {
  text-indent: 25px;
}

#mmhigqsndf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mmhigqsndf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Count" class="gt_row gt_center">6.000</td>
<td headers="Percentage" class="gt_row gt_center">15.79%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.661</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.791</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">7.89%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,105.799</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">7.89%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.628</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.748</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.864</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.975</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.983</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.172</td>
<td headers="Count" class="gt_row gt_center">4.000</td>
<td headers="Percentage" class="gt_row gt_center">10.53%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.449</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.450</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.26%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.458</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.728</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,107.958</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.003</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.058</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,108.860</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">7.89%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,109.002</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,110.373</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,110.474</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,111.532</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.63%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,112.695</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.63%</td></tr>
  </tbody>
  
  
</table>
</div>

$c6_bully.out
<div id="bukiwwfsbq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#bukiwwfsbq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bukiwwfsbq thead, #bukiwwfsbq tbody, #bukiwwfsbq tfoot, #bukiwwfsbq tr, #bukiwwfsbq td, #bukiwwfsbq th {
  border-style: none;
}

#bukiwwfsbq p {
  margin: 0;
  padding: 0;
}

#bukiwwfsbq .gt_table {
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

#bukiwwfsbq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bukiwwfsbq .gt_title {
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

#bukiwwfsbq .gt_subtitle {
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

#bukiwwfsbq .gt_heading {
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

#bukiwwfsbq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bukiwwfsbq .gt_col_headings {
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

#bukiwwfsbq .gt_col_heading {
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

#bukiwwfsbq .gt_column_spanner_outer {
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

#bukiwwfsbq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bukiwwfsbq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bukiwwfsbq .gt_column_spanner {
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

#bukiwwfsbq .gt_spanner_row {
  border-bottom-style: hidden;
}

#bukiwwfsbq .gt_group_heading {
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

#bukiwwfsbq .gt_empty_group_heading {
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

#bukiwwfsbq .gt_from_md > :first-child {
  margin-top: 0;
}

#bukiwwfsbq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bukiwwfsbq .gt_row {
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

#bukiwwfsbq .gt_stub {
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

#bukiwwfsbq .gt_stub_row_group {
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

#bukiwwfsbq .gt_row_group_first td {
  border-top-width: 2px;
}

#bukiwwfsbq .gt_row_group_first th {
  border-top-width: 2px;
}

#bukiwwfsbq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bukiwwfsbq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bukiwwfsbq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bukiwwfsbq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bukiwwfsbq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bukiwwfsbq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bukiwwfsbq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bukiwwfsbq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bukiwwfsbq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bukiwwfsbq .gt_footnotes {
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

#bukiwwfsbq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bukiwwfsbq .gt_sourcenotes {
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

#bukiwwfsbq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bukiwwfsbq .gt_left {
  text-align: left;
}

#bukiwwfsbq .gt_center {
  text-align: center;
}

#bukiwwfsbq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bukiwwfsbq .gt_font_normal {
  font-weight: normal;
}

#bukiwwfsbq .gt_font_bold {
  font-weight: bold;
}

#bukiwwfsbq .gt_font_italic {
  font-style: italic;
}

#bukiwwfsbq .gt_super {
  font-size: 65%;
}

#bukiwwfsbq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bukiwwfsbq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bukiwwfsbq .gt_indent_1 {
  text-indent: 5px;
}

#bukiwwfsbq .gt_indent_2 {
  text-indent: 10px;
}

#bukiwwfsbq .gt_indent_3 {
  text-indent: 15px;
}

#bukiwwfsbq .gt_indent_4 {
  text-indent: 20px;
}

#bukiwwfsbq .gt_indent_5 {
  text-indent: 25px;
}

#bukiwwfsbq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bukiwwfsbq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Count" class="gt_row gt_center">6.000</td>
<td headers="Percentage" class="gt_row gt_center">16.67%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.272</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.780</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.874</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,100.928</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.56%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.017</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.071</td>
<td headers="Count" class="gt_row gt_center">3.000</td>
<td headers="Percentage" class="gt_row gt_center">8.33%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.089</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.332</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.448</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.494</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.502</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.56%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.512</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.579</td>
<td headers="Count" class="gt_row gt_center">2.000</td>
<td headers="Percentage" class="gt_row gt_center">5.56%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.859</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.923</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,101.964</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.075</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.275</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.613</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,102.616</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,103.084</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,104.611</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.123</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center">−5,106.486</td>
<td headers="Count" class="gt_row gt_center">1.000</td>
<td headers="Percentage" class="gt_row gt_center">2.78%</td></tr>
    <tr><td headers="LogLikelihood" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">−5,107.624</td>
<td headers="Count" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">1.000</td>
<td headers="Percentage" class="gt_row gt_center" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000;">2.78%</td></tr>
  </tbody>
  
  
</table>
</div>


Visualize and examine loglikelihood replication for each output file together


``` r
ll_replication_table_all <- source(here("functions", "ll_replication_processing.R"), local = TRUE)$value
ll_replication_table_all
```


```{=html}
<div class="tabwid"><style>.cl-56525d56{}.cl-5648197c{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-56481986{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-56481987{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-564ba83a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-564ba844{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-564bdfb2{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfbc{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfc6{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfc7{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfc8{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfd0{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfd1{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-564bdfd2{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-56525d56'><thead><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">1-Class</span></p></th><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">2-Class</span></p></th><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">3-Class</span></p></th><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">4-Class</span></p></th><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">5-Class</span></p></th><th  colspan="3"class="cl-564bdfb2"><p class="cl-564ba83a"><span class="cl-5648197c">6-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th><th class="cl-564bdfc6"><p class="cl-564ba83a"><span class="cl-56481986">LL</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">N</span></p></th><th class="cl-564bdfc7"><p class="cl-564ba83a"><span class="cl-56481986">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5443.409</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">100</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">100</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5194.136</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">86</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">100</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5122.478</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">94</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">100</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5111.757</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">19</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">35.2</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5105.589</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">6</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">15.8</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,099.881</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">6</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">16.7</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5111.759</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">4</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">7.4</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5105.661</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,100.272</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5112.253</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">9.3</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5105.791</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">7.9</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,100.780</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5113.91</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1.9</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5105.799</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">7.9</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,100.874</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5115.532</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">17</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">31.5</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5106.628</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,100.928</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5115.538</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1.9</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5106.748</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.017</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5116.981</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5106.864</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.071</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">8.3</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5117.829</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3.7</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5106.975</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.089</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5117.837</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3.7</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5106.983</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.332</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.172</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">4</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">10.5</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.448</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.449</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.494</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.45</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5.3</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.502</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.458</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.512</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.728</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.579</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">5.6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5107.958</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.859</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5108.003</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.923</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5108.058</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,101.964</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5108.86</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">3</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">7.9</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,102.075</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5109.002</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,102.275</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5110.373</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,102.613</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5110.474</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,102.616</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5111.532</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,103.084</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5112.695</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.6</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,104.611</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,106.123</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfc8"><p class="cl-564ba844"><span class="cl-56481987">-5,106.486</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd0"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">—</span></p></td><td class="cl-564bdfd1"><p class="cl-564ba844"><span class="cl-56481987">-5,107.624</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">1</span></p></td><td class="cl-564bdfd2"><p class="cl-564ba844"><span class="cl-56481987">2.8</span></p></td></tr></tbody></table></div>
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
<div id="tubpjppvmq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tubpjppvmq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tubpjppvmq thead, #tubpjppvmq tbody, #tubpjppvmq tfoot, #tubpjppvmq tr, #tubpjppvmq td, #tubpjppvmq th {
  border-style: none;
}

#tubpjppvmq p {
  margin: 0;
  padding: 0;
}

#tubpjppvmq .gt_table {
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

#tubpjppvmq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tubpjppvmq .gt_title {
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

#tubpjppvmq .gt_subtitle {
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

#tubpjppvmq .gt_heading {
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

#tubpjppvmq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tubpjppvmq .gt_col_headings {
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

#tubpjppvmq .gt_col_heading {
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

#tubpjppvmq .gt_column_spanner_outer {
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

#tubpjppvmq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tubpjppvmq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tubpjppvmq .gt_column_spanner {
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

#tubpjppvmq .gt_spanner_row {
  border-bottom-style: hidden;
}

#tubpjppvmq .gt_group_heading {
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

#tubpjppvmq .gt_empty_group_heading {
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

#tubpjppvmq .gt_from_md > :first-child {
  margin-top: 0;
}

#tubpjppvmq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tubpjppvmq .gt_row {
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

#tubpjppvmq .gt_stub {
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

#tubpjppvmq .gt_stub_row_group {
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

#tubpjppvmq .gt_row_group_first td {
  border-top-width: 2px;
}

#tubpjppvmq .gt_row_group_first th {
  border-top-width: 2px;
}

#tubpjppvmq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tubpjppvmq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tubpjppvmq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tubpjppvmq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tubpjppvmq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tubpjppvmq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tubpjppvmq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tubpjppvmq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tubpjppvmq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tubpjppvmq .gt_footnotes {
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

#tubpjppvmq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tubpjppvmq .gt_sourcenotes {
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

#tubpjppvmq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tubpjppvmq .gt_left {
  text-align: left;
}

#tubpjppvmq .gt_center {
  text-align: center;
}

#tubpjppvmq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tubpjppvmq .gt_font_normal {
  font-weight: normal;
}

#tubpjppvmq .gt_font_bold {
  font-weight: bold;
}

#tubpjppvmq .gt_font_italic {
  font-style: italic;
}

#tubpjppvmq .gt_super {
  font-size: 65%;
}

#tubpjppvmq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tubpjppvmq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tubpjppvmq .gt_indent_1 {
  text-indent: 5px;
}

#tubpjppvmq .gt_indent_2 {
  text-indent: 10px;
}

#tubpjppvmq .gt_indent_3 {
  text-indent: 15px;
}

#tubpjppvmq .gt_indent_4 {
  text-indent: 20px;
}

#tubpjppvmq .gt_indent_5 {
  text-indent: 25px;
}

#tubpjppvmq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#tubpjppvmq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Perc_Convergence" class="gt_row gt_center">86%</td>
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
<td headers="Perc_Convergence" class="gt_row gt_center">94%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">100%</td>
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
<td headers="Perc_Convergence" class="gt_row gt_center">54%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">35%</td>
<td headers="BIC" class="gt_row gt_center">10,429.10</td>
<td headers="aBIC" class="gt_row gt_center">10,343.32</td>
<td headers="CAIC" class="gt_row gt_center">10,456.10</td>
<td headers="AWE" class="gt_row gt_center">10,715.69</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.01</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;">0.03</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">212 (10.5%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">5-Class</td>
<td headers="Parameters" class="gt_row gt_center">34</td>
<td headers="LL" class="gt_row gt_center">−5,105.59</td>
<td headers="Perc_Convergence" class="gt_row gt_center">38%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">16%</td>
<td headers="BIC" class="gt_row gt_center">10,470.07</td>
<td headers="aBIC" class="gt_row gt_center">10,362.04</td>
<td headers="CAIC" class="gt_row gt_center">10,504.06</td>
<td headers="AWE" class="gt_row gt_center">10,830.95</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.18</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.38</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">43 (2.1%)</td></tr>
    <tr><td headers="Title" class="gt_row gt_center">6-Class</td>
<td headers="Parameters" class="gt_row gt_center">41</td>
<td headers="LL" class="gt_row gt_center">−5,099.88</td>
<td headers="Perc_Convergence" class="gt_row gt_center">36%</td>
<td headers="Replicated_LL_Perc" class="gt_row gt_center">17%</td>
<td headers="BIC" class="gt_row gt_center">10,511.95</td>
<td headers="aBIC" class="gt_row gt_center">10,381.69</td>
<td headers="CAIC" class="gt_row gt_center">10,552.95</td>
<td headers="AWE" class="gt_row gt_center">10,947.14</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.18</td>
<td headers="BLRT_PValue" class="gt_row gt_center">0.21</td>
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

<img src="03-lca-enum_files/figure-html/unnamed-chunk-19-1.png" width="672" />

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
    mutate(model = paste(i, "-Class Model")) 
  
  model_results <- rbind(model_results, temp)
} 

rm(temp)

compare_plot <- model_results %>% 
  filter(category == 2) %>% 
  dplyr::select(est, model, LatentClass, param) %>% 
  mutate(param = as.factor(str_to_lower(param)),
         est = as.numeric(est))

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
  ylim(0,1) +
  facet_wrap( ~ model, ncol = 2) +
  labs(title = "Bullying Items", x = NULL, y = "Probability") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = -.1))                            
```

<img src="03-lca-enum_files/figure-html/unnamed-chunk-21-1.png" width="672" />

------------------------------------------------------------------------

Save figure:


``` r
ggsave(here("figures", "compare_kclass_plot.png"), dpi=300, height=5, width=7, units="in")
```


<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
