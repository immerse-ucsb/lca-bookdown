



# The Importance of Early Attitudes Toward Mathematics and Science (Ing & Nylund-Gibson, 2017)

------------------------------------------------------------------------

This document presents a replication of the latent transition analysis (LTA) conducted by Ing and Nylund-Gibson, (2017), which investigated how students’ attitudes toward mathematics and science evolve over time and how those attitudinal trajectories relate to later academic outcomes. Using nationally representative longitudinal data from the LSAY study, we follow students from Grade 7 to Grade 12, classifying them into latent attitudinal profiles at each wave and modeling transitions between those profiles across time. This replication reproduces the authors’ manual 3-step LTA approach, including: (1) estimating an invariant unconditional model to identify latent profiles across grades; (2) using fixed logits to classify students into profiles at each wave; and (3) examining how class membership and transitions relate to distal outcomes and demographic covariates. Where applicable, we extend the original analysis by visualizing transition patterns, computing subgroup differences using z-tests, and formatting outputs for clear interpretability.


Citation: [Ing,M., & Nylund-Gibson, K. (2017). The importance of early attitudes toward mathematics and science. *Teachers College Record: The Voice of Scholarship in Education*, *119*(5), 1-32.](https://doi.org/10.1177/016146811711900507)

------------------------------------------------------------------------

## Load Packages


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
library(flextable)
library(officer)
library(dplyr)
library(tidyr)
library(haven)
library(psych)
library(ggrepel)
library(PNWColors)
library(multcompView)
```

------------------------------------------------------------------------

## Prepare Data


``` r
lsay_data <- read_sav(here("tc_lta", "data", "Dataset_Jul9_Mplus.sav"))

# Filter to follow-up sample (target ~1824 rows)
lsay_data <- lsay_data %>% filter(RSTEMM %in% c(0, 1))

# Define survey questions
all_questions <- c(
  "AB39A", "AB39H", "AB39I", "AB39K", "AB39L", "AB39M", "AB39T", "AB39U", "AB39W", "AB39X", # 7th grade
  "GA32A", "GA32H", "GA32I", "GA32K", "GA32L", "GA33A", "GA33H", "GA33I", "GA33K", "GA33L", # 10th grade
  "KA46A", "KA46H", "KA46I", "KA46K", "KA46L", "KA47A", "KA47H", "KA47I", "KA47K", "KA47L"  # 12th grade
)

# Rename variables >8 characters
lsay_data <- lsay_data %>%
  rename(
    SCIG8 = ScienceG8,
    SCIG11 = ScienceG11
  )

# Recode 9999 to NA (missing)
lsay_data <- lsay_data %>%
  mutate(across(all_of(all_questions), ~if_else(. == 9999, NA_real_, .)))

names(lsay_data) <- toupper(names(lsay_data))
```

------------------------------------------------------------------------

## Descriptive Statistics

### Create Table 1

This table provides the overall mean and standard deviation for each attitudinal item at Grades 7, 10, and 12. These values summarize general trends in the full sample and serve as a foundation for the latent class models that follow.


``` r
# Function to compute stats (count, mean, SD)
compute_stats <- function(data, question, grade, question_name) {
  data %>%
    summarise(
      Grade = grade,
      Count = sum(!is.na(.data[[question]])),
      Mean = mean(.data[[question]], na.rm = TRUE),
      SD = sd(.data[[question]], na.rm = TRUE)
    ) %>%
    mutate(Question = question_name)
}

# Define question names and mappings
table_setup <- tibble(
  question_code = all_questions,
  grade = rep(c(7, 10, 12), each = 10),
  question_name = rep(
    c(
      "I enjoy math",
      "Math is useful in everyday problems",
      "Math helps a person think logically",
      "It is important to know math to get a good job",
      "I will use math in many ways as an adult",
      "I enjoy science",
      "Science is useful in everyday problems",
      "Science helps a person think logically",
      "It is important to know science to get a good job",
      "I will use science in many ways as an adult"
    ),
    times = 3
  )
)

# Compute stats for all questions
table1_data <- pmap_dfr(
  list(table_setup$question_code, table_setup$grade, table_setup$question_name),
  ~compute_stats(lsay_data, ..1, ..2, ..3)
) %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  ) %>%
  arrange(match(Question, table_setup$question_name), Grade) %>%
  select(Question, Grade, Count, Mean, SD)

# Build table
table1_gt <- table1_data %>%
  gt(groupname_col = "Question") %>%
  tab_header(
    title = "Table 1. Descriptive Statistics for Mathematics and Science Attitudinal Survey Items Included In Analyses"
  ) %>%
  cols_label(
    Grade = "Grade",
    Count = "N",
    Mean = "M",
    SD = "SD"
  ) %>%
  fmt_number(
    columns = c(Mean, SD),
    decimals = 2
  )

# Show table
table1_gt
```


```{=html}
<div id="lanvefgqwk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lanvefgqwk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lanvefgqwk thead, #lanvefgqwk tbody, #lanvefgqwk tfoot, #lanvefgqwk tr, #lanvefgqwk td, #lanvefgqwk th {
  border-style: none;
}

#lanvefgqwk p {
  margin: 0;
  padding: 0;
}

#lanvefgqwk .gt_table {
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

#lanvefgqwk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lanvefgqwk .gt_title {
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

#lanvefgqwk .gt_subtitle {
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

#lanvefgqwk .gt_heading {
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

#lanvefgqwk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lanvefgqwk .gt_col_headings {
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

#lanvefgqwk .gt_col_heading {
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

#lanvefgqwk .gt_column_spanner_outer {
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

#lanvefgqwk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lanvefgqwk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lanvefgqwk .gt_column_spanner {
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

#lanvefgqwk .gt_spanner_row {
  border-bottom-style: hidden;
}

#lanvefgqwk .gt_group_heading {
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

#lanvefgqwk .gt_empty_group_heading {
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

#lanvefgqwk .gt_from_md > :first-child {
  margin-top: 0;
}

#lanvefgqwk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lanvefgqwk .gt_row {
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

#lanvefgqwk .gt_stub {
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

#lanvefgqwk .gt_stub_row_group {
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

#lanvefgqwk .gt_row_group_first td {
  border-top-width: 2px;
}

#lanvefgqwk .gt_row_group_first th {
  border-top-width: 2px;
}

#lanvefgqwk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lanvefgqwk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lanvefgqwk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lanvefgqwk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lanvefgqwk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lanvefgqwk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lanvefgqwk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lanvefgqwk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lanvefgqwk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lanvefgqwk .gt_footnotes {
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

#lanvefgqwk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lanvefgqwk .gt_sourcenotes {
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

#lanvefgqwk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lanvefgqwk .gt_left {
  text-align: left;
}

#lanvefgqwk .gt_center {
  text-align: center;
}

#lanvefgqwk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lanvefgqwk .gt_font_normal {
  font-weight: normal;
}

#lanvefgqwk .gt_font_bold {
  font-weight: bold;
}

#lanvefgqwk .gt_font_italic {
  font-style: italic;
}

#lanvefgqwk .gt_super {
  font-size: 65%;
}

#lanvefgqwk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lanvefgqwk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lanvefgqwk .gt_indent_1 {
  text-indent: 5px;
}

#lanvefgqwk .gt_indent_2 {
  text-indent: 10px;
}

#lanvefgqwk .gt_indent_3 {
  text-indent: 15px;
}

#lanvefgqwk .gt_indent_4 {
  text-indent: 20px;
}

#lanvefgqwk .gt_indent_5 {
  text-indent: 25px;
}

#lanvefgqwk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lanvefgqwk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Table 1. Descriptive Statistics for Mathematics and Science Attitudinal Survey Items Included In Analyses</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Grade">Grade</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Count">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean">M</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD">SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="I enjoy math">I enjoy math</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="I enjoy math  Grade" class="gt_row gt_right">7</td>
<td headers="I enjoy math  Count" class="gt_row gt_right">1882</td>
<td headers="I enjoy math  Mean" class="gt_row gt_right">0.69</td>
<td headers="I enjoy math  SD" class="gt_row gt_right">0.46</td></tr>
    <tr><td headers="I enjoy math  Grade" class="gt_row gt_right">10</td>
<td headers="I enjoy math  Count" class="gt_row gt_right">1532</td>
<td headers="I enjoy math  Mean" class="gt_row gt_right">0.63</td>
<td headers="I enjoy math  SD" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="I enjoy math  Grade" class="gt_row gt_right">12</td>
<td headers="I enjoy math  Count" class="gt_row gt_right">1120</td>
<td headers="I enjoy math  Mean" class="gt_row gt_right">0.57</td>
<td headers="I enjoy math  SD" class="gt_row gt_right">0.50</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Math is useful in everyday problems">Math is useful in everyday problems</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Math is useful in everyday problems  Grade" class="gt_row gt_right">7</td>
<td headers="Math is useful in everyday problems  Count" class="gt_row gt_right">1851</td>
<td headers="Math is useful in everyday problems  Mean" class="gt_row gt_right">0.72</td>
<td headers="Math is useful in everyday problems  SD" class="gt_row gt_right">0.45</td></tr>
    <tr><td headers="Math is useful in everyday problems  Grade" class="gt_row gt_right">10</td>
<td headers="Math is useful in everyday problems  Count" class="gt_row gt_right">1520</td>
<td headers="Math is useful in everyday problems  Mean" class="gt_row gt_right">0.65</td>
<td headers="Math is useful in everyday problems  SD" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="Math is useful in everyday problems  Grade" class="gt_row gt_right">12</td>
<td headers="Math is useful in everyday problems  Count" class="gt_row gt_right">1111</td>
<td headers="Math is useful in everyday problems  Mean" class="gt_row gt_right">0.67</td>
<td headers="Math is useful in everyday problems  SD" class="gt_row gt_right">0.47</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Math helps a person think logically">Math helps a person think logically</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Math helps a person think logically  Grade" class="gt_row gt_right">7</td>
<td headers="Math helps a person think logically  Count" class="gt_row gt_right">1847</td>
<td headers="Math helps a person think logically  Mean" class="gt_row gt_right">0.65</td>
<td headers="Math helps a person think logically  SD" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="Math helps a person think logically  Grade" class="gt_row gt_right">10</td>
<td headers="Math helps a person think logically  Count" class="gt_row gt_right">1517</td>
<td headers="Math helps a person think logically  Mean" class="gt_row gt_right">0.69</td>
<td headers="Math helps a person think logically  SD" class="gt_row gt_right">0.46</td></tr>
    <tr><td headers="Math helps a person think logically  Grade" class="gt_row gt_right">12</td>
<td headers="Math helps a person think logically  Count" class="gt_row gt_right">1108</td>
<td headers="Math helps a person think logically  Mean" class="gt_row gt_right">0.71</td>
<td headers="Math helps a person think logically  SD" class="gt_row gt_right">0.45</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="It is important to know math to get a good job">It is important to know math to get a good job</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="It is important to know math to get a good job  Grade" class="gt_row gt_right">7</td>
<td headers="It is important to know math to get a good job  Count" class="gt_row gt_right">1854</td>
<td headers="It is important to know math to get a good job  Mean" class="gt_row gt_right">0.77</td>
<td headers="It is important to know math to get a good job  SD" class="gt_row gt_right">0.42</td></tr>
    <tr><td headers="It is important to know math to get a good job  Grade" class="gt_row gt_right">10</td>
<td headers="It is important to know math to get a good job  Count" class="gt_row gt_right">1517</td>
<td headers="It is important to know math to get a good job  Mean" class="gt_row gt_right">0.68</td>
<td headers="It is important to know math to get a good job  SD" class="gt_row gt_right">0.47</td></tr>
    <tr><td headers="It is important to know math to get a good job  Grade" class="gt_row gt_right">12</td>
<td headers="It is important to know math to get a good job  Count" class="gt_row gt_right">1105</td>
<td headers="It is important to know math to get a good job  Mean" class="gt_row gt_right">0.61</td>
<td headers="It is important to know math to get a good job  SD" class="gt_row gt_right">0.49</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="I will use math in many ways as an adult">I will use math in many ways as an adult</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="I will use math in many ways as an adult  Grade" class="gt_row gt_right">7</td>
<td headers="I will use math in many ways as an adult  Count" class="gt_row gt_right">1857</td>
<td headers="I will use math in many ways as an adult  Mean" class="gt_row gt_right">0.75</td>
<td headers="I will use math in many ways as an adult  SD" class="gt_row gt_right">0.43</td></tr>
    <tr><td headers="I will use math in many ways as an adult  Grade" class="gt_row gt_right">10</td>
<td headers="I will use math in many ways as an adult  Count" class="gt_row gt_right">1525</td>
<td headers="I will use math in many ways as an adult  Mean" class="gt_row gt_right">0.65</td>
<td headers="I will use math in many ways as an adult  SD" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="I will use math in many ways as an adult  Grade" class="gt_row gt_right">12</td>
<td headers="I will use math in many ways as an adult  Count" class="gt_row gt_right">1104</td>
<td headers="I will use math in many ways as an adult  Mean" class="gt_row gt_right">0.65</td>
<td headers="I will use math in many ways as an adult  SD" class="gt_row gt_right">0.48</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="I enjoy science">I enjoy science</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="I enjoy science  Grade" class="gt_row gt_right">7</td>
<td headers="I enjoy science  Count" class="gt_row gt_right">1873</td>
<td headers="I enjoy science  Mean" class="gt_row gt_right">0.62</td>
<td headers="I enjoy science  SD" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="I enjoy science  Grade" class="gt_row gt_right">10</td>
<td headers="I enjoy science  Count" class="gt_row gt_right">1526</td>
<td headers="I enjoy science  Mean" class="gt_row gt_right">0.59</td>
<td headers="I enjoy science  SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="I enjoy science  Grade" class="gt_row gt_right">12</td>
<td headers="I enjoy science  Count" class="gt_row gt_right">1105</td>
<td headers="I enjoy science  Mean" class="gt_row gt_right">0.55</td>
<td headers="I enjoy science  SD" class="gt_row gt_right">0.50</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Science is useful in everyday problems">Science is useful in everyday problems</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Science is useful in everyday problems  Grade" class="gt_row gt_right">7</td>
<td headers="Science is useful in everyday problems  Count" class="gt_row gt_right">1840</td>
<td headers="Science is useful in everyday problems  Mean" class="gt_row gt_right">0.40</td>
<td headers="Science is useful in everyday problems  SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="Science is useful in everyday problems  Grade" class="gt_row gt_right">10</td>
<td headers="Science is useful in everyday problems  Count" class="gt_row gt_right">1516</td>
<td headers="Science is useful in everyday problems  Mean" class="gt_row gt_right">0.43</td>
<td headers="Science is useful in everyday problems  SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="Science is useful in everyday problems  Grade" class="gt_row gt_right">12</td>
<td headers="Science is useful in everyday problems  Count" class="gt_row gt_right">1099</td>
<td headers="Science is useful in everyday problems  Mean" class="gt_row gt_right">0.48</td>
<td headers="Science is useful in everyday problems  SD" class="gt_row gt_right">0.50</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="Science helps a person think logically">Science helps a person think logically</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Science helps a person think logically  Grade" class="gt_row gt_right">7</td>
<td headers="Science helps a person think logically  Count" class="gt_row gt_right">1850</td>
<td headers="Science helps a person think logically  Mean" class="gt_row gt_right">0.49</td>
<td headers="Science helps a person think logically  SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="Science helps a person think logically  Grade" class="gt_row gt_right">10</td>
<td headers="Science helps a person think logically  Count" class="gt_row gt_right">1516</td>
<td headers="Science helps a person think logically  Mean" class="gt_row gt_right">0.53</td>
<td headers="Science helps a person think logically  SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="Science helps a person think logically  Grade" class="gt_row gt_right">12</td>
<td headers="Science helps a person think logically  Count" class="gt_row gt_right">1100</td>
<td headers="Science helps a person think logically  Mean" class="gt_row gt_right">0.56</td>
<td headers="Science helps a person think logically  SD" class="gt_row gt_right">0.50</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="It is important to know science to get a good job">It is important to know science to get a good job</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="It is important to know science to get a good job  Grade" class="gt_row gt_right">7</td>
<td headers="It is important to know science to get a good job  Count" class="gt_row gt_right">1857</td>
<td headers="It is important to know science to get a good job  Mean" class="gt_row gt_right">0.40</td>
<td headers="It is important to know science to get a good job  SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="It is important to know science to get a good job  Grade" class="gt_row gt_right">10</td>
<td headers="It is important to know science to get a good job  Count" class="gt_row gt_right">1518</td>
<td headers="It is important to know science to get a good job  Mean" class="gt_row gt_right">0.43</td>
<td headers="It is important to know science to get a good job  SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="It is important to know science to get a good job  Grade" class="gt_row gt_right">12</td>
<td headers="It is important to know science to get a good job  Count" class="gt_row gt_right">1099</td>
<td headers="It is important to know science to get a good job  Mean" class="gt_row gt_right">0.38</td>
<td headers="It is important to know science to get a good job  SD" class="gt_row gt_right">0.49</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="4" class="gt_group_heading" scope="colgroup" id="I will use science in many ways as an adult">I will use science in many ways as an adult</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="I will use science in many ways as an adult  Grade" class="gt_row gt_right">7</td>
<td headers="I will use science in many ways as an adult  Count" class="gt_row gt_right">1873</td>
<td headers="I will use science in many ways as an adult  Mean" class="gt_row gt_right">0.46</td>
<td headers="I will use science in many ways as an adult  SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="I will use science in many ways as an adult  Grade" class="gt_row gt_right">10</td>
<td headers="I will use science in many ways as an adult  Count" class="gt_row gt_right">1524</td>
<td headers="I will use science in many ways as an adult  Mean" class="gt_row gt_right">0.42</td>
<td headers="I will use science in many ways as an adult  SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="I will use science in many ways as an adult  Grade" class="gt_row gt_right">12</td>
<td headers="I will use science in many ways as an adult  Count" class="gt_row gt_right">1103</td>
<td headers="I will use science in many ways as an adult  Mean" class="gt_row gt_right">0.44</td>
<td headers="I will use science in many ways as an adult  SD" class="gt_row gt_right">0.50</td></tr>
  </tbody>
  
  
</table>
</div>
```


Save Table 1


``` r
# Save table as PNG
gtsave(table1_gt, here("figures", "table1.png"))
```

------------------------------------------------------------------------

### Create Table 2

This table presents model fit statistics for latent profile enumeration at Grades 7, 10, and 12. For each grade, models with 1 through 6 profiles were estimated and evaluated using the commonly used information criteria (AIC, BIC, SSA-BIC), entropy, and likelihood ratio tests (LMR, BLRT). These values help determine the most appropriate number of latent profiles to retain at each grade.


``` r
# Coerce to numeric just in case
lsay_data <- lsay_data %>%
  mutate(
    MATHG12 = as.numeric(MATHG12),
    SCIG11  = as.numeric(SCIG11),
    STEMSUP = as.numeric(STEMSUP)
  )

# Build the summary table
table2_data <- tibble::tibble(
  `Outcome Variable` = c(
    "12th Grade Mathematics Achievement",
    "11th Grade Science Achievement",
    "STEM Career Attainmentᵃ"
  ),
  N = c(
    sum(lsay_data$MATHG12 != 9999 & !is.na(lsay_data$MATHG12)),
    sum(lsay_data$SCIG11  != 9999 & !is.na(lsay_data$SCIG11)),
    sum(lsay_data$STEMSUP != 9999 & !is.na(lsay_data$STEMSUP))
  ),
  M = c(
    round(mean(lsay_data$MATHG12[lsay_data$MATHG12 != 9999 & !is.na(lsay_data$MATHG12)]), 2),
    round(mean(lsay_data$SCIG11[lsay_data$SCIG11 != 9999 & !is.na(lsay_data$SCIG11)]), 2),
    round(mean(lsay_data$STEMSUP[lsay_data$STEMSUP != 9999 & !is.na(lsay_data$STEMSUP)]), 2)
  ),
  SD = c(
    round(sd(lsay_data$MATHG12[lsay_data$MATHG12 != 9999 & !is.na(lsay_data$MATHG12)]), 2),
    round(sd(lsay_data$SCIG11[lsay_data$SCIG11 != 9999 & !is.na(lsay_data$SCIG11)]), 2),
    NA
  )
)

# Render the gt table
table2_gt <- table2_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Descriptive Statistics for Distal Outcome Variables**")
  ) %>%
  cols_label(
    `Outcome Variable` = "Outcome Variable",
    N = "N",
    M = "M",
    SD = "SD"
  ) %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  tab_footnote(
    footnote = "Binary indicator coded 1 = STEM occupation in mid-30s follow-up.",
    locations = cells_body(rows = 3, columns = "Outcome Variable")
  )

table2_gt
```


```{=html}
<div id="conbtwqgka" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#conbtwqgka table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#conbtwqgka thead, #conbtwqgka tbody, #conbtwqgka tfoot, #conbtwqgka tr, #conbtwqgka td, #conbtwqgka th {
  border-style: none;
}

#conbtwqgka p {
  margin: 0;
  padding: 0;
}

#conbtwqgka .gt_table {
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

#conbtwqgka .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#conbtwqgka .gt_title {
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

#conbtwqgka .gt_subtitle {
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

#conbtwqgka .gt_heading {
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

#conbtwqgka .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#conbtwqgka .gt_col_headings {
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

#conbtwqgka .gt_col_heading {
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

#conbtwqgka .gt_column_spanner_outer {
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

#conbtwqgka .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#conbtwqgka .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#conbtwqgka .gt_column_spanner {
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

#conbtwqgka .gt_spanner_row {
  border-bottom-style: hidden;
}

#conbtwqgka .gt_group_heading {
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

#conbtwqgka .gt_empty_group_heading {
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

#conbtwqgka .gt_from_md > :first-child {
  margin-top: 0;
}

#conbtwqgka .gt_from_md > :last-child {
  margin-bottom: 0;
}

#conbtwqgka .gt_row {
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

#conbtwqgka .gt_stub {
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

#conbtwqgka .gt_stub_row_group {
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

#conbtwqgka .gt_row_group_first td {
  border-top-width: 2px;
}

#conbtwqgka .gt_row_group_first th {
  border-top-width: 2px;
}

#conbtwqgka .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#conbtwqgka .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#conbtwqgka .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#conbtwqgka .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#conbtwqgka .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#conbtwqgka .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#conbtwqgka .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#conbtwqgka .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#conbtwqgka .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#conbtwqgka .gt_footnotes {
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

#conbtwqgka .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#conbtwqgka .gt_sourcenotes {
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

#conbtwqgka .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#conbtwqgka .gt_left {
  text-align: left;
}

#conbtwqgka .gt_center {
  text-align: center;
}

#conbtwqgka .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#conbtwqgka .gt_font_normal {
  font-weight: normal;
}

#conbtwqgka .gt_font_bold {
  font-weight: bold;
}

#conbtwqgka .gt_font_italic {
  font-style: italic;
}

#conbtwqgka .gt_super {
  font-size: 65%;
}

#conbtwqgka .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#conbtwqgka .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#conbtwqgka .gt_indent_1 {
  text-indent: 5px;
}

#conbtwqgka .gt_indent_2 {
  text-indent: 10px;
}

#conbtwqgka .gt_indent_3 {
  text-indent: 15px;
}

#conbtwqgka .gt_indent_4 {
  text-indent: 20px;
}

#conbtwqgka .gt_indent_5 {
  text-indent: 25px;
}

#conbtwqgka .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#conbtwqgka div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Table 2. Descriptive Statistics for Distal Outcome Variables</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Outcome-Variable">Outcome Variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="M">M</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD">SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Outcome Variable" class="gt_row gt_left">12th Grade Mathematics Achievement</td>
<td headers="N" class="gt_row gt_right">830</td>
<td headers="M" class="gt_row gt_right">71.20</td>
<td headers="SD" class="gt_row gt_right">14.55</td></tr>
    <tr><td headers="Outcome Variable" class="gt_row gt_left">11th Grade Science Achievement</td>
<td headers="N" class="gt_row gt_right">1106</td>
<td headers="M" class="gt_row gt_right">66.67</td>
<td headers="SD" class="gt_row gt_right">11.30</td></tr>
    <tr><td headers="Outcome Variable" class="gt_row gt_left">STEM Career Attainmentᵃ<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
<td headers="N" class="gt_row gt_right">1912</td>
<td headers="M" class="gt_row gt_right">0.15</td>
<td headers="SD" class="gt_row gt_right"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> Binary indicator coded 1 = STEM occupation in mid-30s follow-up.</td>
    </tr>
  </tfoot>
</table>
</div>
```


Save Table 2


``` r
gtsave(
  data = table2_gt,
  filename = here("tc_lta","figures", "table2_distal_outcomes.png")
)

```

------------------------------------------------------------------------

## Run Independent LCAs for Each Timepoint

### Prepare Data for MPlusAutomation

To determine the appropriate number and structure of attitudinal profiles at each timepoint, we first estimate separate latent class models for Grades 7, 10, and 12. This step involves preparing the data in wide format and running independent LCAs using `MplusAutomation`. The goal is to identify the optimal number of latent profiles per grade based on model fit before moving to the longitudinal transition model.

### Run LCA on 7th Grade Items


``` r

lca_belonging <- lapply(1:8, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("{k}-Class LCA for LSAY 7th Grade"),

    VARIABLE = glue(
      "categorical = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X;
       usevar = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X;
       missing = all(9999);
       classes = c({k});"
    ),

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 500 10;
      processors = 10;",

    OUTPUT = "sampstat; residual; tech11; tech14;",

    PLOT = "
      type = plot3;
      series = AB39A AB39H AB39I AB39K AB39L AB39M 
               AB39T AB39U AB39W AB39X(*);",

    rdata = lsay_data
  )

  lca_enum_fit <- mplusModeler(
    lca_enum,
    dataout = glue(here("tc_lta","g7_enum", "lsay_g7.dat")),
    modelout = glue(here("tc_lta","g7_enum", "c{k}_g7.inp")),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
})

```

### Run LCA on 10th Grade


``` r

lca_belonging <- lapply(1:8, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("{k}-Class LCA for LCA 10th Grade"),

    VARIABLE = glue(
      "categorical = GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L;
       usevar = GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L;
       missing = all(9999);
       classes = c({k});"
    ),

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 500 10;
      processors = 10;",

    OUTPUT = "sampstat; residual; tech11; tech14;",

    PLOT = "
      type = plot3;
      series = GA32A GA32H GA32I GA32K GA32L GA33A 
               GA33H GA33I GA33K GA33L (*);",

    rdata = lsay_data
  )

  lca_enum_fit <- mplusModeler(
    lca_enum,
    dataout = glue(here("tc_lta","g10_enum", "lsay_g10.dat")),
    modelout = glue(here("tc_lta","g10_enum", "c{k}_g10.inp")),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
})

```

### Run LCA for 12th Grade


``` r

lca_belonging <- lapply(1:8, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("{k}-Class LCA for LCA 12th Grade"),

    VARIABLE = glue(
      "categorical = KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L;
       usevar = KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L;
       missing = all(9999);
       classes = c({k});"
    ),

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 500 10;
      processors = 10;",

    OUTPUT = "sampstat; residual; tech11; tech14;",

    PLOT = "
      type = plot3;
      series = KA46A KA46H KA46I KA46K KA46L KA47A 
               KA47H KA47I KA47K KA47L (*);",

    rdata = lsay_data
  )

  lca_enum_fit <- mplusModeler(
    lca_enum,
    dataout = glue(here("tc_lta","g12_enum", "lsay_g12.dat")),
    modelout = glue(here("tc_lta","g12_enum", "c{k}_g12.inp")),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
})

```

Extract Mplus Information


``` r
# LCA Extraction
source(here("tc_lta","functions", "extract_mplus_info.R"))
output_dir_lca <- here("tc_lta","enum")
output_files_lca <- list.files(output_dir_lca, pattern = "\\.out$", full.names = TRUE)
final_data_lca <- map_dfr(output_files_lca, extract_mplus_info_extended) %>% 
  mutate(Model_Type = "LCA")

```

------------------------------------------------------------------------

## Screen Output for Warnings, Errors, and Loglikelihood Replication

After estimating each LCA model, we examine the Mplus output files for warnings, estimation errors, and loglikelihood replication issues. This quality check helps ensure that solutions are trustworthy and that selected models are not based on local maxima or convergence failures. In this step, we flag any estimation concerns and verify that the best loglikelihood value is replicated consistently across random starts.

------------------------------------------------------------------------

Extract Warnings from Output Files


``` r
source(here("tc_lta","functions", "extract_mplus_info.R"))

# Extract 7th grade LCAs
output_dir_g7 <- here("tc_lta","g7_enum")
output_files_g7 <- list.files(output_dir_g7, pattern = "\\.out$", full.names = TRUE)

final_data_g7 <- map_dfr(output_files_g7, extract_mplus_info_extended) %>%
  mutate(Model_Type = "LCA", Grade = "7th")


# Extract 10th grade LCAs
output_dir_g10 <- here("tc_lta","g10_enum")
output_files_g10 <- list.files(output_dir_g10, pattern = "\\.out$", full.names = TRUE)

final_data_g10 <- map_dfr(output_files_g10, extract_mplus_info_extended) %>%
  mutate(Model_Type = "LCA", Grade = "10th")


# Extract 12th grade LCAs
output_dir_g12 <- here("tc_lta","g12_enum")
output_files_g12 <- list.files(output_dir_g12, pattern = "\\.out$", full.names = TRUE)

final_data_g12 <- map_dfr(output_files_g12, extract_mplus_info_extended) %>%
  mutate(Model_Type = "LCA", Grade = "12th")

```

### Examine Output Warnings


``` r
source(here("tc_lta","functions", "extract_warnings.R"))

# ---- 7th Grade ----
warnings_g7 <- extract_warnings(final_data_g7) %>%
  left_join(select(final_data_g7, File_Name), by = "File_Name")

warnings_table_g7 <- warnings_g7 %>%
  gt() %>%
  tab_header(title = md("**Model Warnings — 7th Grade LCA**")) %>%
  cols_label(
    File_Name = "Output File",
    Warning_Summary = "# of Warnings",
    Warnings = "Warning Message(s)"
  ) %>%
  cols_align(align = "left", columns = everything()) %>%
  cols_width(
    File_Name ~ px(150),
    Warning_Summary ~ px(150),
    Warnings ~ px(400)
  ) %>%
  tab_options(table.width = pct(100))

# ---- 10th Grade ----
warnings_g10 <- extract_warnings(final_data_g10) %>%
  left_join(select(final_data_g10, File_Name), by = "File_Name")

warnings_table_g10 <- warnings_g10 %>%
  gt() %>%
  tab_header(title = md("**Model Warnings — 10th Grade LCA**")) %>%
  cols_label(
    File_Name = "Output File",
    Warning_Summary = "# of Warnings",
    Warnings = "Warning Message(s)"
  ) %>%
  cols_align(align = "left", columns = everything()) %>%
  cols_width(
    File_Name ~ px(150),
    Warning_Summary ~ px(150),
    Warnings ~ px(400)
  ) %>%
  tab_options(table.width = pct(100))

# ---- 12th Grade ----
warnings_g12 <- extract_warnings(final_data_g12) %>%
  left_join(select(final_data_g12, File_Name), by = "File_Name")

warnings_table_g12 <- warnings_g12 %>%
  gt() %>%
  tab_header(title = md("**Model Warnings — 12th Grade LCA**")) %>%
  cols_label(
    File_Name = "Output File",
    Warning_Summary = "# of Warnings",
    Warnings = "Warning Message(s)"
  ) %>%
  cols_align(align = "left", columns = everything()) %>%
  cols_width(
    File_Name ~ px(150),
    Warning_Summary ~ px(150),
    Warnings ~ px(400)
  ) %>%
  tab_options(table.width = pct(100))

# Print all three
warnings_table_g7
```


```{=html}
<div id="iycypiircu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#iycypiircu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#iycypiircu thead, #iycypiircu tbody, #iycypiircu tfoot, #iycypiircu tr, #iycypiircu td, #iycypiircu th {
  border-style: none;
}

#iycypiircu p {
  margin: 0;
  padding: 0;
}

#iycypiircu .gt_table {
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

#iycypiircu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#iycypiircu .gt_title {
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

#iycypiircu .gt_subtitle {
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

#iycypiircu .gt_heading {
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

#iycypiircu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iycypiircu .gt_col_headings {
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

#iycypiircu .gt_col_heading {
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

#iycypiircu .gt_column_spanner_outer {
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

#iycypiircu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#iycypiircu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#iycypiircu .gt_column_spanner {
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

#iycypiircu .gt_spanner_row {
  border-bottom-style: hidden;
}

#iycypiircu .gt_group_heading {
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

#iycypiircu .gt_empty_group_heading {
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

#iycypiircu .gt_from_md > :first-child {
  margin-top: 0;
}

#iycypiircu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#iycypiircu .gt_row {
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

#iycypiircu .gt_stub {
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

#iycypiircu .gt_stub_row_group {
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

#iycypiircu .gt_row_group_first td {
  border-top-width: 2px;
}

#iycypiircu .gt_row_group_first th {
  border-top-width: 2px;
}

#iycypiircu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iycypiircu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#iycypiircu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#iycypiircu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iycypiircu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iycypiircu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#iycypiircu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#iycypiircu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#iycypiircu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iycypiircu .gt_footnotes {
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

#iycypiircu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iycypiircu .gt_sourcenotes {
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

#iycypiircu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iycypiircu .gt_left {
  text-align: left;
}

#iycypiircu .gt_center {
  text-align: center;
}

#iycypiircu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#iycypiircu .gt_font_normal {
  font-weight: normal;
}

#iycypiircu .gt_font_bold {
  font-weight: bold;
}

#iycypiircu .gt_font_italic {
  font-style: italic;
}

#iycypiircu .gt_super {
  font-size: 65%;
}

#iycypiircu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#iycypiircu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#iycypiircu .gt_indent_1 {
  text-indent: 5px;
}

#iycypiircu .gt_indent_2 {
  text-indent: 10px;
}

#iycypiircu .gt_indent_3 {
  text-indent: 15px;
}

#iycypiircu .gt_indent_4 {
  text-indent: 20px;
}

#iycypiircu .gt_indent_5 {
  text-indent: 25px;
}

#iycypiircu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#iycypiircu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Warnings — 7th Grade LCA</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warning_Summary"># of Warnings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warnings">Warning Message(s)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c1_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 4 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH11 option is not available for TYPE=MIXTURE with only one class.   Request for TECH11 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH14 option is not available for TYPE=MIXTURE with only one class.   Request for TECH14 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g7.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  26    </td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r
warnings_table_g10
```


```{=html}
<div id="cxkyoduimw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cxkyoduimw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cxkyoduimw thead, #cxkyoduimw tbody, #cxkyoduimw tfoot, #cxkyoduimw tr, #cxkyoduimw td, #cxkyoduimw th {
  border-style: none;
}

#cxkyoduimw p {
  margin: 0;
  padding: 0;
}

#cxkyoduimw .gt_table {
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

#cxkyoduimw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cxkyoduimw .gt_title {
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

#cxkyoduimw .gt_subtitle {
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

#cxkyoduimw .gt_heading {
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

#cxkyoduimw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cxkyoduimw .gt_col_headings {
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

#cxkyoduimw .gt_col_heading {
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

#cxkyoduimw .gt_column_spanner_outer {
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

#cxkyoduimw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cxkyoduimw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cxkyoduimw .gt_column_spanner {
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

#cxkyoduimw .gt_spanner_row {
  border-bottom-style: hidden;
}

#cxkyoduimw .gt_group_heading {
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

#cxkyoduimw .gt_empty_group_heading {
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

#cxkyoduimw .gt_from_md > :first-child {
  margin-top: 0;
}

#cxkyoduimw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cxkyoduimw .gt_row {
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

#cxkyoduimw .gt_stub {
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

#cxkyoduimw .gt_stub_row_group {
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

#cxkyoduimw .gt_row_group_first td {
  border-top-width: 2px;
}

#cxkyoduimw .gt_row_group_first th {
  border-top-width: 2px;
}

#cxkyoduimw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cxkyoduimw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cxkyoduimw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cxkyoduimw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cxkyoduimw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cxkyoduimw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cxkyoduimw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cxkyoduimw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cxkyoduimw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cxkyoduimw .gt_footnotes {
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

#cxkyoduimw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cxkyoduimw .gt_sourcenotes {
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

#cxkyoduimw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cxkyoduimw .gt_left {
  text-align: left;
}

#cxkyoduimw .gt_center {
  text-align: center;
}

#cxkyoduimw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cxkyoduimw .gt_font_normal {
  font-weight: normal;
}

#cxkyoduimw .gt_font_bold {
  font-weight: bold;
}

#cxkyoduimw .gt_font_italic {
  font-style: italic;
}

#cxkyoduimw .gt_super {
  font-size: 65%;
}

#cxkyoduimw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cxkyoduimw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cxkyoduimw .gt_indent_1 {
  text-indent: 5px;
}

#cxkyoduimw .gt_indent_2 {
  text-indent: 10px;
}

#cxkyoduimw .gt_indent_3 {
  text-indent: 15px;
}

#cxkyoduimw .gt_indent_4 {
  text-indent: 20px;
}

#cxkyoduimw .gt_indent_5 {
  text-indent: 25px;
}

#cxkyoduimw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#cxkyoduimw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Warnings — 10th Grade LCA</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warning_Summary"># of Warnings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warnings">Warning Message(s)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c1_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 4 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH11 option is not available for TYPE=MIXTURE with only one class.   Request for TECH11 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH14 option is not available for TYPE=MIXTURE with only one class.   Request for TECH14 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g10.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  378    </td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r
warnings_table_g12
```


```{=html}
<div id="jwvfxoymms" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jwvfxoymms table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jwvfxoymms thead, #jwvfxoymms tbody, #jwvfxoymms tfoot, #jwvfxoymms tr, #jwvfxoymms td, #jwvfxoymms th {
  border-style: none;
}

#jwvfxoymms p {
  margin: 0;
  padding: 0;
}

#jwvfxoymms .gt_table {
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

#jwvfxoymms .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jwvfxoymms .gt_title {
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

#jwvfxoymms .gt_subtitle {
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

#jwvfxoymms .gt_heading {
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

#jwvfxoymms .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jwvfxoymms .gt_col_headings {
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

#jwvfxoymms .gt_col_heading {
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

#jwvfxoymms .gt_column_spanner_outer {
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

#jwvfxoymms .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jwvfxoymms .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jwvfxoymms .gt_column_spanner {
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

#jwvfxoymms .gt_spanner_row {
  border-bottom-style: hidden;
}

#jwvfxoymms .gt_group_heading {
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

#jwvfxoymms .gt_empty_group_heading {
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

#jwvfxoymms .gt_from_md > :first-child {
  margin-top: 0;
}

#jwvfxoymms .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jwvfxoymms .gt_row {
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

#jwvfxoymms .gt_stub {
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

#jwvfxoymms .gt_stub_row_group {
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

#jwvfxoymms .gt_row_group_first td {
  border-top-width: 2px;
}

#jwvfxoymms .gt_row_group_first th {
  border-top-width: 2px;
}

#jwvfxoymms .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jwvfxoymms .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jwvfxoymms .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jwvfxoymms .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jwvfxoymms .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jwvfxoymms .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jwvfxoymms .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jwvfxoymms .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jwvfxoymms .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jwvfxoymms .gt_footnotes {
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

#jwvfxoymms .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jwvfxoymms .gt_sourcenotes {
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

#jwvfxoymms .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jwvfxoymms .gt_left {
  text-align: left;
}

#jwvfxoymms .gt_center {
  text-align: center;
}

#jwvfxoymms .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jwvfxoymms .gt_font_normal {
  font-weight: normal;
}

#jwvfxoymms .gt_font_bold {
  font-weight: bold;
}

#jwvfxoymms .gt_font_italic {
  font-style: italic;
}

#jwvfxoymms .gt_super {
  font-size: 65%;
}

#jwvfxoymms .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jwvfxoymms .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jwvfxoymms .gt_indent_1 {
  text-indent: 5px;
}

#jwvfxoymms .gt_indent_2 {
  text-indent: 10px;
}

#jwvfxoymms .gt_indent_3 {
  text-indent: 15px;
}

#jwvfxoymms .gt_indent_4 {
  text-indent: 20px;
}

#jwvfxoymms .gt_indent_5 {
  text-indent: 25px;
}

#jwvfxoymms .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jwvfxoymms div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Warnings — 12th Grade LCA</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warning_Summary"># of Warnings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Warnings">Warning Message(s)</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c1_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 4 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH11 option is not available for TYPE=MIXTURE with only one class.   Request for TECH11 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   TECH14 option is not available for TYPE=MIXTURE with only one class.   Request for TECH14 is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g12.out</td>
<td headers="Warning_Summary" class="gt_row gt_left">There are 2 warnings in the output file.</td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING in OUTPUT command   SAMPSTAT option is not available when all outcomes are censored, ordered   categorical, unordered categorical (nominal), count or continuous-time   survival variables.  Request for SAMPSTAT is ignored.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left"></td>
<td headers="Warning_Summary" class="gt_row gt_left"></td>
<td headers="Warnings" class="gt_row gt_left">*** WARNING   Data set contains cases with missing on all variables.   These cases were not included in the analysis.   Number of cases with missing on all variables:  790    </td></tr>
  </tbody>
  
  
</table>
</div>
```


Save Warning Tables


``` r
gtsave(warnings_table_g7, filename = here("tc_lta","figures", "warnings_g7_lca.png"))
gtsave(warnings_table_g10, filename = here("tc_lta","figures", "warnings_g10_lca.png"))
gtsave(warnings_table_g12, filename = here("tc_lta","figures", "warnings_g12_lca.png"))

```

Extract Errors from Output Files


``` r
source(here("tc_lta","functions", "error_visualization.R"))

# ---- 7th Grade Errors ----
error_table_g7 <- process_error_data(final_data_g7)

# ---- 10th Grade Errors ----
error_table_g10 <- process_error_data(final_data_g10)

# ---- 12th Grade Errors ----
error_table_g12 <- process_error_data(final_data_g12)

```

### Examine Errors


``` r

# Helper to conditionally render or notify
render_error_table <- function(error_df, grade_label) {
  if (nrow(error_df) > 0) {
    error_df %>%
      gt() %>%
      tab_header(title = md(glue("**Model Estimation Errors — {grade_label} Grade**"))) %>%
      cols_label(
        File_Name     = "Output File",
        Class_Model   = "Model Type",
        Error_Message = "Error Message"
      ) %>%
      cols_align(align = "left", columns = everything()) %>%
      cols_width(
        File_Name     ~ px(150),
        Class_Model   ~ px(100),
        Error_Message ~ px(400)
      ) %>%
      tab_options(table.width = px(600)) %>%
      fmt(
        columns = "Error_Message",
        fns = function(x) gsub("\n", "<br>", x)
      )
  } else {
    cat(glue("✅ No errors detected for {grade_label} Grade.\n"))
  }
}

# Print or notify for each grade
render_error_table(error_table_g7, "7th")
```


```{=html}
<div id="ytzywrtveg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ytzywrtveg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ytzywrtveg thead, #ytzywrtveg tbody, #ytzywrtveg tfoot, #ytzywrtveg tr, #ytzywrtveg td, #ytzywrtveg th {
  border-style: none;
}

#ytzywrtveg p {
  margin: 0;
  padding: 0;
}

#ytzywrtveg .gt_table {
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

#ytzywrtveg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ytzywrtveg .gt_title {
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

#ytzywrtveg .gt_subtitle {
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

#ytzywrtveg .gt_heading {
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

#ytzywrtveg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ytzywrtveg .gt_col_headings {
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

#ytzywrtveg .gt_col_heading {
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

#ytzywrtveg .gt_column_spanner_outer {
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

#ytzywrtveg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ytzywrtveg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ytzywrtveg .gt_column_spanner {
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

#ytzywrtveg .gt_spanner_row {
  border-bottom-style: hidden;
}

#ytzywrtveg .gt_group_heading {
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

#ytzywrtveg .gt_empty_group_heading {
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

#ytzywrtveg .gt_from_md > :first-child {
  margin-top: 0;
}

#ytzywrtveg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ytzywrtveg .gt_row {
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

#ytzywrtveg .gt_stub {
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

#ytzywrtveg .gt_stub_row_group {
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

#ytzywrtveg .gt_row_group_first td {
  border-top-width: 2px;
}

#ytzywrtveg .gt_row_group_first th {
  border-top-width: 2px;
}

#ytzywrtveg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ytzywrtveg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ytzywrtveg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ytzywrtveg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ytzywrtveg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ytzywrtveg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ytzywrtveg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ytzywrtveg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ytzywrtveg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ytzywrtveg .gt_footnotes {
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

#ytzywrtveg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ytzywrtveg .gt_sourcenotes {
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

#ytzywrtveg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ytzywrtveg .gt_left {
  text-align: left;
}

#ytzywrtveg .gt_center {
  text-align: center;
}

#ytzywrtveg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ytzywrtveg .gt_font_normal {
  font-weight: normal;
}

#ytzywrtveg .gt_font_bold {
  font-weight: bold;
}

#ytzywrtveg .gt_font_italic {
  font-style: italic;
}

#ytzywrtveg .gt_super {
  font-size: 65%;
}

#ytzywrtveg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ytzywrtveg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ytzywrtveg .gt_indent_1 {
  text-indent: 5px;
}

#ytzywrtveg .gt_indent_2 {
  text-indent: 10px;
}

#ytzywrtveg .gt_indent_3 {
  text-indent: 15px;
}

#ytzywrtveg .gt_indent_4 {
  text-indent: 20px;
}

#ytzywrtveg .gt_indent_5 {
  text-indent: 25px;
}

#ytzywrtveg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ytzywrtveg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Estimation Errors — 7th Grade</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Class_Model">Model Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Error_Message">Error Message</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">2-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">3-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">4-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">5-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">6-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">7-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g7.out</td>
<td headers="Class_Model" class="gt_row gt_left">8-Class</td>
<td headers="Error_Message" class="gt_row gt_left">SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA.  INCREASE THE NUMBER OF RANDOM STARTS. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r
render_error_table(error_table_g10, "10th")
```


```{=html}
<div id="zxqipdpruu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zxqipdpruu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zxqipdpruu thead, #zxqipdpruu tbody, #zxqipdpruu tfoot, #zxqipdpruu tr, #zxqipdpruu td, #zxqipdpruu th {
  border-style: none;
}

#zxqipdpruu p {
  margin: 0;
  padding: 0;
}

#zxqipdpruu .gt_table {
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

#zxqipdpruu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zxqipdpruu .gt_title {
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

#zxqipdpruu .gt_subtitle {
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

#zxqipdpruu .gt_heading {
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

#zxqipdpruu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zxqipdpruu .gt_col_headings {
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

#zxqipdpruu .gt_col_heading {
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

#zxqipdpruu .gt_column_spanner_outer {
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

#zxqipdpruu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zxqipdpruu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zxqipdpruu .gt_column_spanner {
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

#zxqipdpruu .gt_spanner_row {
  border-bottom-style: hidden;
}

#zxqipdpruu .gt_group_heading {
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

#zxqipdpruu .gt_empty_group_heading {
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

#zxqipdpruu .gt_from_md > :first-child {
  margin-top: 0;
}

#zxqipdpruu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zxqipdpruu .gt_row {
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

#zxqipdpruu .gt_stub {
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

#zxqipdpruu .gt_stub_row_group {
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

#zxqipdpruu .gt_row_group_first td {
  border-top-width: 2px;
}

#zxqipdpruu .gt_row_group_first th {
  border-top-width: 2px;
}

#zxqipdpruu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zxqipdpruu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zxqipdpruu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zxqipdpruu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zxqipdpruu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zxqipdpruu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zxqipdpruu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zxqipdpruu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zxqipdpruu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zxqipdpruu .gt_footnotes {
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

#zxqipdpruu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zxqipdpruu .gt_sourcenotes {
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

#zxqipdpruu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zxqipdpruu .gt_left {
  text-align: left;
}

#zxqipdpruu .gt_center {
  text-align: center;
}

#zxqipdpruu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zxqipdpruu .gt_font_normal {
  font-weight: normal;
}

#zxqipdpruu .gt_font_bold {
  font-weight: bold;
}

#zxqipdpruu .gt_font_italic {
  font-style: italic;
}

#zxqipdpruu .gt_super {
  font-size: 65%;
}

#zxqipdpruu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zxqipdpruu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zxqipdpruu .gt_indent_1 {
  text-indent: 5px;
}

#zxqipdpruu .gt_indent_2 {
  text-indent: 10px;
}

#zxqipdpruu .gt_indent_3 {
  text-indent: 15px;
}

#zxqipdpruu .gt_indent_4 {
  text-indent: 20px;
}

#zxqipdpruu .gt_indent_5 {
  text-indent: 25px;
}

#zxqipdpruu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#zxqipdpruu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Estimation Errors — 10th Grade</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Class_Model">Model Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Error_Message">Error Message</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">2-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">3-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">4-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">5-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">6-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">7-Class</td>
<td headers="Error_Message" class="gt_row gt_left">SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA.  INCREASE THE NUMBER OF RANDOM STARTS. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g10.out</td>
<td headers="Class_Model" class="gt_row gt_left">8-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS. THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE TRUSTWORTHY FOR SOME PARAMETERS DUE TO A NON-POSITIVE DEFINITE FIRST-ORDER DERIVATIVE PRODUCT MATRIX.  THIS MAY BE DUE TO THE STARTING VALUES BUT MAY ALSO BE AN INDICATION OF MODEL NONIDENTIFICATION.  THE CONDITION NUMBER IS       0.526D-11.  PROBLEM INVOLVING THE FOLLOWING PARAMETER: Parameter 28, %C#3%: [ GA33I$1 ]</td></tr>
  </tbody>
  
  
</table>
</div>
```


``` r
render_error_table(error_table_g12, "12th")
```


```{=html}
<div id="mnpccyvqlj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mnpccyvqlj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#mnpccyvqlj thead, #mnpccyvqlj tbody, #mnpccyvqlj tfoot, #mnpccyvqlj tr, #mnpccyvqlj td, #mnpccyvqlj th {
  border-style: none;
}

#mnpccyvqlj p {
  margin: 0;
  padding: 0;
}

#mnpccyvqlj .gt_table {
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

#mnpccyvqlj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#mnpccyvqlj .gt_title {
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

#mnpccyvqlj .gt_subtitle {
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

#mnpccyvqlj .gt_heading {
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

#mnpccyvqlj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mnpccyvqlj .gt_col_headings {
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

#mnpccyvqlj .gt_col_heading {
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

#mnpccyvqlj .gt_column_spanner_outer {
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

#mnpccyvqlj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mnpccyvqlj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mnpccyvqlj .gt_column_spanner {
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

#mnpccyvqlj .gt_spanner_row {
  border-bottom-style: hidden;
}

#mnpccyvqlj .gt_group_heading {
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

#mnpccyvqlj .gt_empty_group_heading {
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

#mnpccyvqlj .gt_from_md > :first-child {
  margin-top: 0;
}

#mnpccyvqlj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mnpccyvqlj .gt_row {
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

#mnpccyvqlj .gt_stub {
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

#mnpccyvqlj .gt_stub_row_group {
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

#mnpccyvqlj .gt_row_group_first td {
  border-top-width: 2px;
}

#mnpccyvqlj .gt_row_group_first th {
  border-top-width: 2px;
}

#mnpccyvqlj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mnpccyvqlj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mnpccyvqlj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mnpccyvqlj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mnpccyvqlj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mnpccyvqlj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mnpccyvqlj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#mnpccyvqlj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mnpccyvqlj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mnpccyvqlj .gt_footnotes {
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

#mnpccyvqlj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mnpccyvqlj .gt_sourcenotes {
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

#mnpccyvqlj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mnpccyvqlj .gt_left {
  text-align: left;
}

#mnpccyvqlj .gt_center {
  text-align: center;
}

#mnpccyvqlj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mnpccyvqlj .gt_font_normal {
  font-weight: normal;
}

#mnpccyvqlj .gt_font_bold {
  font-weight: bold;
}

#mnpccyvqlj .gt_font_italic {
  font-style: italic;
}

#mnpccyvqlj .gt_super {
  font-size: 65%;
}

#mnpccyvqlj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#mnpccyvqlj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mnpccyvqlj .gt_indent_1 {
  text-indent: 5px;
}

#mnpccyvqlj .gt_indent_2 {
  text-indent: 10px;
}

#mnpccyvqlj .gt_indent_3 {
  text-indent: 15px;
}

#mnpccyvqlj .gt_indent_4 {
  text-indent: 20px;
}

#mnpccyvqlj .gt_indent_5 {
  text-indent: 25px;
}

#mnpccyvqlj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#mnpccyvqlj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Estimation Errors — 12th Grade</strong></span></td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="File_Name">Output File</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Class_Model">Model Type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Error_Message">Error Message</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="File_Name" class="gt_row gt_left">c2_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">2-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c3_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">3-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c4_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">4-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c5_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">5-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c6_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">6-Class</td>
<td headers="Error_Message" class="gt_row gt_left">RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS. THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE TRUSTWORTHY FOR SOME PARAMETERS DUE TO A NON-POSITIVE DEFINITE FIRST-ORDER DERIVATIVE PRODUCT MATRIX.  THIS MAY BE DUE TO THE STARTING VALUES BUT MAY ALSO BE AN INDICATION OF MODEL NONIDENTIFICATION.  THE CONDITION NUMBER IS       0.212D-11.  PROBLEM INVOLVING THE FOLLOWING PARAMETER: Parameter 39, %C#4%: [ KA47K$1 ]</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c7_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">7-Class</td>
<td headers="Error_Message" class="gt_row gt_left">SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA.  INCREASE THE NUMBER OF RANDOM STARTS. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS. THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE TRUSTWORTHY FOR SOME PARAMETERS DUE TO A NON-POSITIVE DEFINITE FIRST-ORDER DERIVATIVE PRODUCT MATRIX.  THIS MAY BE DUE TO THE STARTING VALUES BUT MAY ALSO BE AN INDICATION OF MODEL NONIDENTIFICATION.  THE CONDITION NUMBER IS       0.120D-13.  PROBLEM INVOLVING THE FOLLOWING PARAMETER: Parameter 16, %C#2%: [ KA47A$1 ]</td></tr>
    <tr><td headers="File_Name" class="gt_row gt_left">c8_g12.out</td>
<td headers="Class_Model" class="gt_row gt_left">8-Class</td>
<td headers="Error_Message" class="gt_row gt_left">SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA.  INCREASE THE NUMBER OF RANDOM STARTS. IN THE OPTIMIZATION, ONE OR MORE LOGIT THRESHOLDS APPROACHED EXTREME VALUES OF -15.000 AND 15.000 AND WERE FIXED TO STABILIZE MODEL ESTIMATION.  THESE VALUES IMPLY PROBABILITIES OF 0 AND 1.  IN THE MODEL RESULTS SECTION, THESE PARAMETERS HAVE 0 STANDARD ERRORS AND 999 IN THE Z-SCORE AND P-VALUE COLUMNS.</td></tr>
  </tbody>
  
  
</table>
</div>
```


Save Error Tables


``` r
if (exists("error_table_g7") && nrow(error_table_g7) > 0) {
  gtsave(render_error_table(error_table_g7, "7th"), here("tc_lta","figures", "errors_g7_lca.png"))
}

if (exists("error_table_g10") && nrow(error_table_g10) > 0) {
  gtsave(render_error_table(error_table_g10, "10th"), here("tc_lta","figures", "errors_g10_lca.png"))
}

if (exists("error_table_g12") && nrow(error_table_g12) > 0) {
  gtsave(render_error_table(error_table_g12, "12th"), here("tc_lta","figures", "errors_g12_lca.png"))
}

```

### Examine Convergence Information


``` r
# Load function
source(here("tc_lta","functions", "summary_table.R"))

# Helper: clean + prep for each dataset
prepare_convergence_table <- function(data_object, grade_label) {
  sample_size <- data_object$Sample_Size[1]

  data_flat <- data_object %>%
    select(-LogLikelihoods, -Errors, -Warnings) %>%
    mutate(across(
      c(
        Best_LogLikelihood,
        Perc_Convergence,
        Replicated_LL_Perc,
        Smallest_Class_Perc,
        Condition_Number
      ),
      ~ as.numeric(gsub(",", "", .))
    ))

  tbl <- create_flextable(data_flat, sample_size)

  # Get actual number of columns in the flextable object
  n_cols <- length(tbl$body$col_keys)

  # Title string
  title_text <- glue("LCA Convergence Table — {grade_label} Grade (N = {sample_size})")

  # This is the correct call: title as one string, colwidth = full span
  tbl <- add_header_row(
    tbl,
    values = title_text,
    colwidths = n_cols
  ) %>%
    align(i = 1, align = "center", part = "header") %>%
    fontsize(i = 1, size = 12, part = "header") %>%
    bg(i = 1, bg = "#ffffff", part = "header")

  return(tbl)
}

# Create tables
summary_table_g7  <- prepare_convergence_table(final_data_g7, "7th")
summary_table_g10 <- prepare_convergence_table(final_data_g10, "10th")
summary_table_g12 <- prepare_convergence_table(final_data_g12, "12th")

summary_table_g7 
```


```{=html}
<div class="tabwid"><style>.cl-aa4be7e0{}.cl-aa44d50e{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa44d522{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa44d523{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-aa47b71a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa47b71b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa47b724{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa47d1be{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1bf{width:0.8in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1c0{width:0.4in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1c1{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1c8{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1c9{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1ca{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1cb{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1d2{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1d3{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1d4{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1dc{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1dd{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1de{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1df{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1e0{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1e6{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1e7{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1e8{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1e9{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1f0{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1f1{width:0.5in;background-color:rgba(243, 165, 144, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1f2{width:0.5in;background-color:rgba(249, 200, 187, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1fa{width:0.5in;background-color:rgba(240, 147, 123, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1fb{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1fc{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d1fd{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d204{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d205{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa47d206{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-aa4be7e0'><thead><tr style="overflow-wrap:break-word;"><th  colspan="11"class="cl-aa47d1be"><p class="cl-aa47b71a"><span class="cl-aa44d50e">LCA Convergence Table — 7th Grade (N = 1886)</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-aa47d1c8"><p class="cl-aa47b71b"><span class="cl-aa44d522">N = 1886</span></p></th><th  colspan="2"class="cl-aa47d1cb"><p class="cl-aa47b71a"><span class="cl-aa44d522">Random Starts</span></p></th><th  colspan="2"class="cl-aa47d1cb"><p class="cl-aa47b71a"><span class="cl-aa44d522">Final starts converging</span></p></th><th  colspan="2"class="cl-aa47d1cb"><p class="cl-aa47b71a"><span class="cl-aa44d522">LL Replication</span></p></th><th  colspan="2"class="cl-aa47d1cb"><p class="cl-aa47b71a"><span class="cl-aa44d522">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-aa47d1d2"><p class="cl-aa47b71b"><span class="cl-aa44d522">Model</span></p></th><th class="cl-aa47d1d3"><p class="cl-aa47b71a"><span class="cl-aa44d522">Best LL</span></p></th><th class="cl-aa47d1d4"><p class="cl-aa47b71a"><span class="cl-aa44d522">npar</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">Initial</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">Final</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">𝒇</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">%</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">𝒇</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">%</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">𝒇</span></p></th><th class="cl-aa47d1dc"><p class="cl-aa47b71a"><span class="cl-aa44d522">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1dd"><p class="cl-aa47b724"><span class="cl-aa44d522">1-Class</span></p></td><td class="cl-aa47d1de"><p class="cl-aa47b724"><span class="cl-aa44d522">-11,803.429</span></p></td><td class="cl-aa47d1df"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">1,886</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1e6"><p class="cl-aa47b724"><span class="cl-aa44d522">2-Class</span></p></td><td class="cl-aa47d1e7"><p class="cl-aa47b724"><span class="cl-aa44d522">-10,418.761</span></p></td><td class="cl-aa47d1e8"><p class="cl-aa47b724"><span class="cl-aa44d522">21</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1f0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1f0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">782</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">41.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1dd"><p class="cl-aa47b724"><span class="cl-aa44d522">3-Class</span></p></td><td class="cl-aa47d1de"><p class="cl-aa47b724"><span class="cl-aa44d522">-10,165.874</span></p></td><td class="cl-aa47d1df"><p class="cl-aa47b724"><span class="cl-aa44d522">32</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">384</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">20.4%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1e6"><p class="cl-aa47b724"><span class="cl-aa44d522">4-Class</span></p></td><td class="cl-aa47d1e7"><p class="cl-aa47b724"><span class="cl-aa44d522">-10,042.970</span></p></td><td class="cl-aa47d1e8"><p class="cl-aa47b724"><span class="cl-aa44d522">43</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1f0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1f0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">390</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">20.7%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1dd"><p class="cl-aa47b724"><span class="cl-aa44d522">5-Class</span></p></td><td class="cl-aa47d1de"><p class="cl-aa47b724"><span class="cl-aa44d522">-9,969.239</span></p></td><td class="cl-aa47d1df"><p class="cl-aa47b724"><span class="cl-aa44d522">54</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">6</span></p></td><td class="cl-aa47d1f1"><p class="cl-aa47b724"><span class="cl-aa44d522">60%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">175</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">9.3%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1e6"><p class="cl-aa47b724"><span class="cl-aa44d522">6-Class</span></p></td><td class="cl-aa47d1e7"><p class="cl-aa47b724"><span class="cl-aa44d522">-9,915.148</span></p></td><td class="cl-aa47d1e8"><p class="cl-aa47b724"><span class="cl-aa44d522">65</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1f0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">8</span></p></td><td class="cl-aa47d1f2"><p class="cl-aa47b724"><span class="cl-aa44d522">80%</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">180</span></p></td><td class="cl-aa47d1e9"><p class="cl-aa47b724"><span class="cl-aa44d522">9.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1dd"><p class="cl-aa47b724"><span class="cl-aa44d522">7-Class</span></p></td><td class="cl-aa47d1de"><p class="cl-aa47b724"><span class="cl-aa44d522">-9,891.204</span></p></td><td class="cl-aa47d1df"><p class="cl-aa47b724"><span class="cl-aa44d522">76</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">5</span></p></td><td class="cl-aa47d1fa"><p class="cl-aa47b724"><span class="cl-aa44d522">50%</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">47</span></p></td><td class="cl-aa47d1e0"><p class="cl-aa47b724"><span class="cl-aa44d522">2.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa47d1fb"><p class="cl-aa47b724"><span class="cl-aa44d522">8-Class</span></p></td><td class="cl-aa47d1fc"><p class="cl-aa47b724"><span class="cl-aa44d522">-9,871.411</span></p></td><td class="cl-aa47d1fd"><p class="cl-aa47b724"><span class="cl-aa44d522">87</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">500</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">10</span></p></td><td class="cl-aa47d205"><p class="cl-aa47b724"><span class="cl-aa44d522">100%</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">1</span></p></td><td class="cl-aa47d206"><p class="cl-aa47b724"><span class="cl-aa44d523">10%</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">55</span></p></td><td class="cl-aa47d204"><p class="cl-aa47b724"><span class="cl-aa44d522">2.9%</span></p></td></tr></tbody></table></div>
```


``` r
summary_table_g10
```


```{=html}
<div class="tabwid"><style>.cl-aa56c160{}.cl-aa504df8{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa504e02{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa504e03{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-aa52bdfe{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa52be08{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa52be09{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa52d6b8{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6b9{width:0.8in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6c2{width:0.4in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6c3{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6cc{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6cd{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6ce{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6cf{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6d6{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6d7{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6d8{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6d9{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6e0{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6e1{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6ea{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6eb{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6f4{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6f5{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6f6{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6fe{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d6ff{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d700{width:0.5in;background-color:rgba(234, 112, 80, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d701{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d708{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d709{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d70a{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d712{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d713{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa52d71c{width:0.5in;background-color:rgba(231, 94, 59, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-aa56c160'><thead><tr style="overflow-wrap:break-word;"><th  colspan="11"class="cl-aa52d6b8"><p class="cl-aa52bdfe"><span class="cl-aa504df8">LCA Convergence Table — 10th Grade (N = 1534)</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-aa52d6cc"><p class="cl-aa52be08"><span class="cl-aa504e02">N = 1534</span></p></th><th  colspan="2"class="cl-aa52d6cf"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Random Starts</span></p></th><th  colspan="2"class="cl-aa52d6cf"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Final starts converging</span></p></th><th  colspan="2"class="cl-aa52d6cf"><p class="cl-aa52bdfe"><span class="cl-aa504e02">LL Replication</span></p></th><th  colspan="2"class="cl-aa52d6cf"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-aa52d6d6"><p class="cl-aa52be08"><span class="cl-aa504e02">Model</span></p></th><th class="cl-aa52d6d7"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Best LL</span></p></th><th class="cl-aa52d6d8"><p class="cl-aa52bdfe"><span class="cl-aa504e02">npar</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Initial</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">Final</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">𝒇</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">%</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">𝒇</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">%</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">𝒇</span></p></th><th class="cl-aa52d6d9"><p class="cl-aa52bdfe"><span class="cl-aa504e02">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6e0"><p class="cl-aa52be09"><span class="cl-aa504e02">1-Class</span></p></td><td class="cl-aa52d6e1"><p class="cl-aa52be09"><span class="cl-aa504e02">-10,072.926</span></p></td><td class="cl-aa52d6ea"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">1,534</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6f4"><p class="cl-aa52be09"><span class="cl-aa504e02">2-Class</span></p></td><td class="cl-aa52d6f5"><p class="cl-aa52be09"><span class="cl-aa504e02">-8,428.384</span></p></td><td class="cl-aa52d6f6"><p class="cl-aa52be09"><span class="cl-aa504e02">21</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6ff"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6ff"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">658</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">42.9%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6e0"><p class="cl-aa52be09"><span class="cl-aa504e02">3-Class</span></p></td><td class="cl-aa52d6e1"><p class="cl-aa52be09"><span class="cl-aa504e02">-8,067.612</span></p></td><td class="cl-aa52d6ea"><p class="cl-aa52be09"><span class="cl-aa504e02">32</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">297</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">19.4%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6f4"><p class="cl-aa52be09"><span class="cl-aa504e02">4-Class</span></p></td><td class="cl-aa52d6f5"><p class="cl-aa52be09"><span class="cl-aa504e02">-7,905.535</span></p></td><td class="cl-aa52d6f6"><p class="cl-aa52be09"><span class="cl-aa504e02">43</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6ff"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6ff"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">290</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">18.9%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6e0"><p class="cl-aa52be09"><span class="cl-aa504e02">5-Class</span></p></td><td class="cl-aa52d6e1"><p class="cl-aa52be09"><span class="cl-aa504e02">-7,845.441</span></p></td><td class="cl-aa52d6ea"><p class="cl-aa52be09"><span class="cl-aa504e02">54</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">220</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">14.3%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6f4"><p class="cl-aa52be09"><span class="cl-aa504e02">6-Class</span></p></td><td class="cl-aa52d6f5"><p class="cl-aa52be09"><span class="cl-aa504e02">-7,806.987</span></p></td><td class="cl-aa52d6f6"><p class="cl-aa52be09"><span class="cl-aa504e02">65</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6ff"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">3</span></p></td><td class="cl-aa52d700"><p class="cl-aa52be09"><span class="cl-aa504e03">30%</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">130</span></p></td><td class="cl-aa52d6fe"><p class="cl-aa52be09"><span class="cl-aa504e02">8.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d6e0"><p class="cl-aa52be09"><span class="cl-aa504e02">7-Class</span></p></td><td class="cl-aa52d6e1"><p class="cl-aa52be09"><span class="cl-aa504e02">-7,779.821</span></p></td><td class="cl-aa52d6ea"><p class="cl-aa52be09"><span class="cl-aa504e02">76</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">1</span></p></td><td class="cl-aa52d701"><p class="cl-aa52be09"><span class="cl-aa504e03">10%</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">62</span></p></td><td class="cl-aa52d6eb"><p class="cl-aa52be09"><span class="cl-aa504e02">4.1%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa52d708"><p class="cl-aa52be09"><span class="cl-aa504e02">8-Class</span></p></td><td class="cl-aa52d709"><p class="cl-aa52be09"><span class="cl-aa504e02">-7,754.190</span></p></td><td class="cl-aa52d70a"><p class="cl-aa52be09"><span class="cl-aa504e02">87</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">500</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">10</span></p></td><td class="cl-aa52d713"><p class="cl-aa52be09"><span class="cl-aa504e02">100%</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">2</span></p></td><td class="cl-aa52d71c"><p class="cl-aa52be09"><span class="cl-aa504e03">20%</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">84</span></p></td><td class="cl-aa52d712"><p class="cl-aa52be09"><span class="cl-aa504e02">5.5%</span></p></td></tr></tbody></table></div>
```


``` r
summary_table_g12
```


```{=html}
<div class="tabwid"><style>.cl-aa6220c8{}.cl-aa5ae984{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa5ae98e{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-aa5ae98f{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(255, 255, 255, 1.00);background-color:transparent;}.cl-aa5d6ccc{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa5d6cd6{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa5d6ce0{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-aa5d8464{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d846e{width:0.8in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d846f{width:0.4in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8470{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8478{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8479{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8482{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8483{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: bottom;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8484{width:0.7in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d848c{width:0.8in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d848d{width:0.4in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8496{width:0.5in;background-color:transparent;vertical-align: bottom;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8497{width:0.7in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d8498{width:0.8in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84a0{width:0.4in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84a1{width:0.5in;background-color:rgba(240, 240, 240, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84aa{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84ab{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84ac{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84b4{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84b5{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84b6{width:0.5in;background-color:rgba(252, 218, 208, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84be{width:0.5in;background-color:rgba(240, 147, 123, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84bf{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84c0{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84c8{width:0.8in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84c9{width:0.4in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84d2{width:0.5in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84d3{width:0.5in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-aa5d84d4{width:0.5in;background-color:rgba(228, 77, 38, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-aa6220c8'><thead><tr style="overflow-wrap:break-word;"><th  colspan="11"class="cl-aa5d8464"><p class="cl-aa5d6ccc"><span class="cl-aa5ae984">LCA Convergence Table — 12th Grade (N = 1122)</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-aa5d8478"><p class="cl-aa5d6cd6"><span class="cl-aa5ae98e">N = 1122</span></p></th><th  colspan="2"class="cl-aa5d8483"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Random Starts</span></p></th><th  colspan="2"class="cl-aa5d8483"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Final starts converging</span></p></th><th  colspan="2"class="cl-aa5d8483"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">LL Replication</span></p></th><th  colspan="2"class="cl-aa5d8483"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Smallest Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-aa5d8484"><p class="cl-aa5d6cd6"><span class="cl-aa5ae98e">Model</span></p></th><th class="cl-aa5d848c"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Best LL</span></p></th><th class="cl-aa5d848d"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">npar</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Initial</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">Final</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">𝒇</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">%</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">𝒇</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">%</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">𝒇</span></p></th><th class="cl-aa5d8496"><p class="cl-aa5d6ccc"><span class="cl-aa5ae98e">%</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-aa5d8497"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">1-Class</span></p></td><td class="cl-aa5d8498"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-7,349.129</span></p></td><td class="cl-aa5d84a0"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">1,122</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100.0%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d84aa"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">2-Class</span></p></td><td class="cl-aa5d84ab"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,976.598</span></p></td><td class="cl-aa5d84ac"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">21</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b5"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b5"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">534</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">47.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d8497"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">3-Class</span></p></td><td class="cl-aa5d8498"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,670.595</span></p></td><td class="cl-aa5d84a0"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">32</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">203</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">18.1%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d84aa"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">4-Class</span></p></td><td class="cl-aa5d84ab"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,543.616</span></p></td><td class="cl-aa5d84ac"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">43</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b5"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b5"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">219</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">19.5%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d8497"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">5-Class</span></p></td><td class="cl-aa5d8498"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,483.669</span></p></td><td class="cl-aa5d84a0"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">54</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">9</span></p></td><td class="cl-aa5d84b6"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">90%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">131</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">11.7%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d84aa"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">6-Class</span></p></td><td class="cl-aa5d84ab"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,444.055</span></p></td><td class="cl-aa5d84ac"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">65</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84b5"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">5</span></p></td><td class="cl-aa5d84be"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">50%</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">81</span></p></td><td class="cl-aa5d84b4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">7.2%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d8497"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">7-Class</span></p></td><td class="cl-aa5d8498"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,420.551</span></p></td><td class="cl-aa5d84a0"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">76</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">1</span></p></td><td class="cl-aa5d84bf"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98f">10%</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">44</span></p></td><td class="cl-aa5d84a1"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">3.9%</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-aa5d84c0"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">8-Class</span></p></td><td class="cl-aa5d84c8"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">-5,395.532</span></p></td><td class="cl-aa5d84c9"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">87</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">500</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">10</span></p></td><td class="cl-aa5d84d3"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">100%</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">1</span></p></td><td class="cl-aa5d84d4"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98f">10%</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">61</span></p></td><td class="cl-aa5d84d2"><p class="cl-aa5d6ce0"><span class="cl-aa5ae98e">5.4%</span></p></td></tr></tbody></table></div>
```


Save Convergence Tables


``` r
# Save convergence tables as PNGs
invisible(save_as_image(summary_table_g7,  path = here("tc_lta","figures", "convergence_g7_lca.png")))
invisible(save_as_image(summary_table_g10, path = here("tc_lta","figures", "convergence_g10_lca.png")))
invisible(save_as_image(summary_table_g12, path = here("tc_lta","figures", "convergence_g12_lca.png")))
```

Scrape Replication Data


``` r
# Load the function file containing generate_ll_replication_plots and create_ll_replication_table_all
source(here("tc_lta","functions", "ll_replication_plots.R"))
# Load the second function from ll_replication_processing.R
source(here("tc_lta","functions", "ll_replication_processing.R"))
```

### Examine Loglikelihood Replication Information


``` r
# Generate replication plots (invisible, for diagnostic use)
ll_replication_tables_g7  <- generate_ll_replication_plots(final_data_g7)
ll_replication_tables_g10 <- generate_ll_replication_plots(final_data_g10)
ll_replication_tables_g12 <- generate_ll_replication_plots(final_data_g12)

# Create replication tables
ll_replication_table_g7 <- create_ll_replication_table_all(final_data_g7)
ll_replication_table_g10 <- create_ll_replication_table_all(final_data_g10)
ll_replication_table_g12 <- create_ll_replication_table_all(final_data_g12)

# Add visible title row to each table
ll_replication_table_g7 <- add_header_row(
  ll_replication_table_g7,
  values = "Log-Likelihood Replication Table — 7th Grade LCA",
  colwidths = ncol(ll_replication_table_g7$body$dataset)
) %>%
  align(i = 1, align = "center", part = "header") %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  bg(i = 1, bg = "#ffffff", part = "header")

ll_replication_table_g10 <- add_header_row(
  ll_replication_table_g10,
  values = "Log-Likelihood Replication Table — 10th Grade LCA",
  colwidths = ncol(ll_replication_table_g10$body$dataset)
) %>%
  align(i = 1, align = "center", part = "header") %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  bg(i = 1, bg = "#ffffff", part = "header")

ll_replication_table_g12 <- add_header_row(
  ll_replication_table_g12,
  values = "Log-Likelihood Replication Table — 12th Grade LCA",
  colwidths = ncol(ll_replication_table_g12$body$dataset)
) %>%
  align(i = 1, align = "center", part = "header") %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  bg(i = 1, bg = "#ffffff", part = "header")

# Display the three titled tables
ll_replication_table_g7
```


```{=html}
<div class="tabwid"><style>.cl-ab23beae{}.cl-ab1c4f20{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab1c4f2a{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab1c4f2b{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab1f07f6{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab1f0800{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab1f2178{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f2182{width:0.3in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f2183{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f2184{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f218c{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f218d{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f218e{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f218f{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f2196{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab1f2197{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ab23beae'><thead><tr style="overflow-wrap:break-word;"><th  colspan="24"class="cl-ab1f2178"><p class="cl-ab1f07f6"><span class="cl-ab1c4f20">Log-Likelihood Replication Table — 7th Grade LCA</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">1-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">2-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">3-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">4-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">5-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">6-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">7-Class</span></p></th><th  colspan="3"class="cl-ab1f2183"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">8-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c1</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c1</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c1</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c2</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c2</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c2</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c3</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c3</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c3</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c4</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c4</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c4</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c5</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c5</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c5</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c6</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c6</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c6</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c7</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c7</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c7</span></p></th><th class="cl-ab1f218c"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">LL_c8</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">n_c8</span></p></th><th class="cl-ab1f218d"><p class="cl-ab1f07f6"><span class="cl-ab1c4f2a">perc_c8</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-11803.429</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">100</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-10418.761</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">100</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-10165.874</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">100</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-10042.97</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">100</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9969.239</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">6</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">60</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9915.148</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">8</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">80</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9891.204</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">5</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">50</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,871.411</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9969.697</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">4</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">40</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9915.778</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9893.864</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,872.124</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9916.262</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9894.265</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,874.200</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9898.309</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,876.173</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9904.214</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,876.914</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9904.532</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,877.583</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,877.741</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,877.806</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f218e"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,879.960</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f218f"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">—</span></p></td><td class="cl-ab1f2196"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">-9,888.932</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">1</span></p></td><td class="cl-ab1f2197"><p class="cl-ab1f0800"><span class="cl-ab1c4f2b">10</span></p></td></tr></tbody></table></div>
```


``` r
ll_replication_table_g10
```


```{=html}
<div class="tabwid"><style>.cl-ab322516{}.cl-ab2af02a{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab2af034{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab2af035{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab2d9b04{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab2d9b0e{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab2db47c{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db486{width:0.3in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db487{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db488{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db490{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db491{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db492{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db493{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db494{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab2db49a{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ab322516'><thead><tr style="overflow-wrap:break-word;"><th  colspan="24"class="cl-ab2db47c"><p class="cl-ab2d9b04"><span class="cl-ab2af02a">Log-Likelihood Replication Table — 10th Grade LCA</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">1-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">2-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">3-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">4-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">5-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">6-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">7-Class</span></p></th><th  colspan="3"class="cl-ab2db487"><p class="cl-ab2d9b04"><span class="cl-ab2af034">8-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c1</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c1</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c1</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c2</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c2</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c2</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c3</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c3</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c3</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c4</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c4</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c4</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c5</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c5</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c5</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c6</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c6</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c6</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c7</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c7</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c7</span></p></th><th class="cl-ab2db490"><p class="cl-ab2d9b04"><span class="cl-ab2af034">LL_c8</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">n_c8</span></p></th><th class="cl-ab2db491"><p class="cl-ab2d9b04"><span class="cl-ab2af034">perc_c8</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-10072.926</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">100</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-8428.384</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">100</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-8067.612</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">100</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7905.535</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">100</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7845.441</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">100</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7806.987</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">3</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">30</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7779.821</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,754.190</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">2</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">20</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7807.464</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">4</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">40</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7780.664</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">5</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">50</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,754.206</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7807.485</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7781.893</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,757.915</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7808.751</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7783.866</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,758.288</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7820.453</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7784.195</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,759.634</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7784.933</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,759.730</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,760.773</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db492"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,762.451</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db493"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">—</span></p></td><td class="cl-ab2db494"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">-7,762.777</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">1</span></p></td><td class="cl-ab2db49a"><p class="cl-ab2d9b0e"><span class="cl-ab2af035">10</span></p></td></tr></tbody></table></div>
```


``` r
ll_replication_table_g12
```


```{=html}
<div class="tabwid"><style>.cl-ab40d0c0{}.cl-ab38af6c{font-family:'Avenir Next';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab38af76{font-family:'Avenir Next';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab38af80{font-family:'Avenir Next';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ab3b60a4{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab3b60ae{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ab3b7be8{width:0.7in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7bf2{width:0.3in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7bf3{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7bfc{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(255, 255, 255, 0.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7bfd{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7bfe{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7c06{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7c07{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7c10{width:0.7in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ab3b7c11{width:0.3in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ab40d0c0'><thead><tr style="overflow-wrap:break-word;"><th  colspan="24"class="cl-ab3b7be8"><p class="cl-ab3b60a4"><span class="cl-ab38af6c">Log-Likelihood Replication Table — 12th Grade LCA</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">1-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">2-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">3-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">4-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">5-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">6-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">7-Class</span></p></th><th  colspan="3"class="cl-ab3b7bf3"><p class="cl-ab3b60a4"><span class="cl-ab38af76">8-Class</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c1</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c1</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c1</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c2</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c2</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c2</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c3</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c3</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c3</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c4</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c4</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c4</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c5</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c5</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c5</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c6</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c6</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c6</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c7</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c7</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c7</span></p></th><th class="cl-ab3b7bfd"><p class="cl-ab3b60a4"><span class="cl-ab38af76">LL_c8</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">n_c8</span></p></th><th class="cl-ab3b7bfe"><p class="cl-ab3b60a4"><span class="cl-ab38af76">perc_c8</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-7349.129</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">100</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5976.598</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">100</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5670.595</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">100</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5543.616</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">100</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5483.669</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">9</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">90</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5444.055</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">5</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">50</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5420.551</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,395.532</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5484.101</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5446.399</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">4</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">40</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5421.469</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,397.310</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5446.424</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5421.554</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">2</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">20</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,397.311</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5421.845</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,398.708</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5422.176</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,399.930</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5423.003</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,400.458</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5424.777</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,400.463</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5426.79</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,401.156</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5427.065</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td><td class="cl-ab3b7c06"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,403.971</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c07"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">—</span></p></td><td class="cl-ab3b7c10"><p class="cl-ab3b60ae"><span class="cl-ab38af80">-5,405.328</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">1</span></p></td><td class="cl-ab3b7c11"><p class="cl-ab3b60ae"><span class="cl-ab38af80">10</span></p></td></tr></tbody></table></div>
```


Save LL Replication Tables


``` r

invisible({
  flextable::save_as_image(ll_replication_table_g7, path = here::here("tc_lta","figures", "ll_replication_table_g7.png"))
  flextable::save_as_image(ll_replication_table_g10, path = here::here("tc_lta","figures", "ll_replication_table_g10.png"))
  flextable::save_as_image(ll_replication_table_g12, path = here::here("tc_lta","figures", "ll_replication_table_g12.png"))
})

```

------------------------------------------------------------------------

## Examine Model Fit for Optimal Solution

We evaluate model fit for each grade-level LCA to identify the optimal number of latent profiles. Fit statistics such as BIC, entropy, and likelihood ratio tests (LMR and BLRT) are compared across models. The goal is to select the most parsimonious solution that provides clear separation between classes and replicates reliably. These selections will form the basis for the longitudinal model in the next stage.

------------------------------------------------------------------------

Extract fit statistics


``` r

# Define grade-specific folders and data
grades <- c("g7", "g10", "g12")
folder_paths <- c(
  g7 = here("tc_lta","g7_enum"),
  g10 = here("tc_lta","g10_enum"),
  g12 = here("tc_lta","g12_enum")
)
final_data_list <- list(
  g7 = final_data_g7,
  g10 = final_data_g10,
  g12 = final_data_g12
)

# Initialize storage
output_models_all <- list()
allFit_list <- list()

# Process each grade
for (grade in grades) {
  # Get all .out files (1-8 classes)
  out_files <- list.files(folder_paths[grade], pattern = "^c[1-8]_g\\d+\\.out$", full.names = TRUE)
  
  # Read Mplus output files
  output_models <- list()
  for (file in out_files) {
    model <- readModels(file, quiet = TRUE)
    if (!is.null(model) && length(model) > 0) {
      output_models[[basename(file)]] <- model
    }
  }
  
  # Store models
  output_models_all[[grade]] <- output_models
  
  # Extract summary table
  model_extract <- LatexSummaryTable(
    output_models,
    keepCols = c("Title", "Parameters", "LL", "BIC", "aBIC", "T11_VLMR_PValue", "BLRT_PValue", "Observations", "Entropy"),
    sortBy = "Title"
  )
  
  # Compute additional fit indices
  allFit <- model_extract %>%
    mutate(
      Title = str_trim(Title),
      Grade = toupper(grade),
      Classes = as.integer(str_extract(Title, "\\d+")), # Extract class number
      File_Name = names(output_models),
      CAIC = -2 * LL + Parameters * (log(Observations) + 1),
      AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5),
      SIC = -0.5 * BIC,
      expSIC = exp(SIC - max(SIC, na.rm = TRUE)),
      BF = if_else(is.na(lead(SIC)), NA_real_, exp(SIC - lead(SIC))),
      cmPk = expSIC / sum(expSIC, na.rm = TRUE)
    ) %>%
    arrange(Classes)
  
  # Clean Class_Model for joining
  final_data_clean <- final_data_list[[grade]] %>%
    mutate(
      Classes = as.integer(str_extract(Class_Model, "\\d+"))
    ) %>%
    select(Class_Model, Classes, Perc_Convergence, Replicated_LL_Perc, Smallest_Class, Smallest_Class_Perc)
  
  # Merge with final_data
  allFit <- allFit %>%
    left_join(
      final_data_clean,
      by = "Classes"
    ) %>%
    mutate(
      Smallest_Class_Combined = paste0(Smallest_Class, "\u00A0(", Smallest_Class_Perc, "%)")
    ) %>%
    relocate(Grade, Classes, Parameters, LL, Perc_Convergence, Replicated_LL_Perc, .before = BIC) %>%
    select(
      Grade, Classes, Parameters, LL, Perc_Convergence, Replicated_LL_Perc,
      BIC, aBIC, CAIC, AWE, T11_VLMR_PValue, BLRT_PValue, Entropy, Smallest_Class_Combined, BF, cmPk
    )
  
  # Store fit data
  allFit_list[[grade]] <- allFit
}

# Combine fit data
allFit_combined <- bind_rows(allFit_list) %>%
  arrange(Grade, Classes)
```

### Examine Fit Statistics


``` r

# Initialize list to store tables
fit_tables <- list()

# Create and render table for each grade
for (grade in grades) {
  # Filter data for current grade
  allFit_grade <- allFit_combined %>%
    filter(Grade == toupper(grade)) %>%
    select(-Perc_Convergence, -Replicated_LL_Perc) # Exclude convergence columns
  
  # Create table
  fit_table <- allFit_grade %>%
    gt() %>%
    tab_header(title = md(sprintf("**Model Fit Summary Table for %s Grade**", toupper(grade)))) %>%
    tab_spanner(label = "Model Fit Indices", columns = c(BIC, aBIC, CAIC, AWE)) %>%
    tab_spanner(label = "LRTs", columns = c(T11_VLMR_PValue, BLRT_PValue)) %>%
    tab_spanner(label = md("Smallest\u00A0Class"), columns = c(Smallest_Class_Combined)) %>%
    cols_label(
      Grade = "Grade",
      Classes = "Classes",
      Parameters = md("npar"),
      LL = md("*LL*"),
      # Perc_Convergence = "% Converged", # Commented out
      # Replicated_LL_Perc = "% Replicated", # Commented out
      BIC = "BIC",
      aBIC = "aBIC",
      CAIC = "CAIC",
      AWE = "AWE",
      T11_VLMR_PValue = "VLMR",
      BLRT_PValue = "BLRT",
      Entropy = "Entropy",
      Smallest_Class_Combined = "n (%)",
      BF = "BF",
      cmPk = "cmPk"
    ) %>%
    tab_footnote(
      footnote = md(
        "*Note.* npar = Parameters; *LL* = model log likelihood;
        BIC = Bayesian information criterion;
        aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
        AWE = approximate weight of evidence criterion;
        BLRT = bootstrapped likelihood ratio test p-value;
        VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
        Smallest n (%) = Number of cases in the smallest class."
      ),
      locations = cells_title()
    ) %>%
    tab_options(column_labels.font.weight = "bold") %>%
    fmt_number(
      columns = c(LL, BIC, aBIC, CAIC, AWE, Entropy, BF, cmPk),
      decimals = 2
    ) %>%
    fmt(
      columns = c(T11_VLMR_PValue, BLRT_PValue),
      fns = function(x) ifelse(is.na(x), "—", ifelse(x < 0.001, "<.001", scales::number(x, accuracy = .01)))
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = list(
        cells_body(columns = BIC, rows = BIC == min(BIC)),
        cells_body(columns = aBIC, rows = aBIC == min(aBIC)),
        cells_body(columns = CAIC, rows = CAIC == min(CAIC)),
        cells_body(columns = AWE, rows = AWE == min(AWE)),
        cells_body(
          columns = T11_VLMR_PValue,
          rows = T11_VLMR_PValue < .05 & lead(T11_VLMR_PValue, default = 1) > .05
        ),
        cells_body(
          columns = BLRT_PValue,
          rows = BLRT_PValue < .05 & lead(BLRT_PValue, default = 1) > .05
        )
      )
    )
  
  # Store table
  fit_tables[[grade]] <- fit_table
  
  # Render table without markdown header
  # cat(sprintf("\n### LCA Fit Table for %s Grade\n", toupper(grade))) # Commented out to avoid markdown headers
  print(fit_table)
}
```

<div id="elhozkrhey" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#elhozkrhey table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#elhozkrhey thead, #elhozkrhey tbody, #elhozkrhey tfoot, #elhozkrhey tr, #elhozkrhey td, #elhozkrhey th {
  border-style: none;
}

#elhozkrhey p {
  margin: 0;
  padding: 0;
}

#elhozkrhey .gt_table {
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

#elhozkrhey .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#elhozkrhey .gt_title {
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

#elhozkrhey .gt_subtitle {
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

#elhozkrhey .gt_heading {
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

#elhozkrhey .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#elhozkrhey .gt_col_headings {
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

#elhozkrhey .gt_col_heading {
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

#elhozkrhey .gt_column_spanner_outer {
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

#elhozkrhey .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#elhozkrhey .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#elhozkrhey .gt_column_spanner {
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

#elhozkrhey .gt_spanner_row {
  border-bottom-style: hidden;
}

#elhozkrhey .gt_group_heading {
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

#elhozkrhey .gt_empty_group_heading {
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

#elhozkrhey .gt_from_md > :first-child {
  margin-top: 0;
}

#elhozkrhey .gt_from_md > :last-child {
  margin-bottom: 0;
}

#elhozkrhey .gt_row {
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

#elhozkrhey .gt_stub {
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

#elhozkrhey .gt_stub_row_group {
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

#elhozkrhey .gt_row_group_first td {
  border-top-width: 2px;
}

#elhozkrhey .gt_row_group_first th {
  border-top-width: 2px;
}

#elhozkrhey .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#elhozkrhey .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#elhozkrhey .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#elhozkrhey .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#elhozkrhey .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#elhozkrhey .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#elhozkrhey .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#elhozkrhey .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#elhozkrhey .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#elhozkrhey .gt_footnotes {
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

#elhozkrhey .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#elhozkrhey .gt_sourcenotes {
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

#elhozkrhey .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#elhozkrhey .gt_left {
  text-align: left;
}

#elhozkrhey .gt_center {
  text-align: center;
}

#elhozkrhey .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#elhozkrhey .gt_font_normal {
  font-weight: normal;
}

#elhozkrhey .gt_font_bold {
  font-weight: bold;
}

#elhozkrhey .gt_font_italic {
  font-style: italic;
}

#elhozkrhey .gt_super {
  font-size: 65%;
}

#elhozkrhey .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#elhozkrhey .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#elhozkrhey .gt_indent_1 {
  text-indent: 5px;
}

#elhozkrhey .gt_indent_2 {
  text-indent: 10px;
}

#elhozkrhey .gt_indent_3 {
  text-indent: 15px;
}

#elhozkrhey .gt_indent_4 {
  text-indent: 20px;
}

#elhozkrhey .gt_indent_5 {
  text-indent: 25px;
}

#elhozkrhey .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#elhozkrhey div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="14" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Fit Summary Table for G7 Grade</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Grade">Grade</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Classes">Classes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>npar</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="LL"><span class='gt_from_md'><em>LL</em></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Model Fit Indices">
        <div class="gt_column_spanner">Model Fit Indices</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="LRTs">
        <div class="gt_column_spanner">LRTs</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Entropy">Entropy</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="Smallest Class">
        <div class="gt_column_spanner"><span class='gt_from_md'>Smallest Class</span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="BF">BF</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="cmPk">cmPk</th>
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
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">1</td>
<td headers="Parameters" class="gt_row gt_center">10</td>
<td headers="LL" class="gt_row gt_center">−11,803.43</td>
<td headers="BIC" class="gt_row gt_center">23,682.28</td>
<td headers="aBIC" class="gt_row gt_center">23,650.51</td>
<td headers="CAIC" class="gt_row gt_center">23,692.28</td>
<td headers="AWE" class="gt_row gt_center">23,787.70</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">—</td>
<td headers="BLRT_PValue" class="gt_row gt_center">—</td>
<td headers="Entropy" class="gt_row gt_center">NA</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">1886 (100%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">2</td>
<td headers="Parameters" class="gt_row gt_center">21</td>
<td headers="LL" class="gt_row gt_center">−10,418.76</td>
<td headers="BIC" class="gt_row gt_center">20,995.91</td>
<td headers="aBIC" class="gt_row gt_center">20,929.19</td>
<td headers="CAIC" class="gt_row gt_center">21,016.91</td>
<td headers="AWE" class="gt_row gt_center">21,217.29</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.81</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">782 (41.5%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">3</td>
<td headers="Parameters" class="gt_row gt_center">32</td>
<td headers="LL" class="gt_row gt_center">−10,165.87</td>
<td headers="BIC" class="gt_row gt_center">20,573.10</td>
<td headers="aBIC" class="gt_row gt_center">20,471.44</td>
<td headers="CAIC" class="gt_row gt_center">20,605.10</td>
<td headers="AWE" class="gt_row gt_center">20,910.45</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.00</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.75</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">384 (20.4%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">4</td>
<td headers="Parameters" class="gt_row gt_center">43</td>
<td headers="LL" class="gt_row gt_center">−10,042.97</td>
<td headers="BIC" class="gt_row gt_center">20,410.26</td>
<td headers="aBIC" class="gt_row gt_center">20,273.64</td>
<td headers="CAIC" class="gt_row gt_center">20,453.26</td>
<td headers="AWE" class="gt_row gt_center" style="font-weight: bold;">20,863.57</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.00</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.70</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">390 (20.7%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">5</td>
<td headers="Parameters" class="gt_row gt_center">54</td>
<td headers="LL" class="gt_row gt_center">−9,969.24</td>
<td headers="BIC" class="gt_row gt_center">20,345.76</td>
<td headers="aBIC" class="gt_row gt_center">20,174.20</td>
<td headers="CAIC" class="gt_row gt_center">20,399.76</td>
<td headers="AWE" class="gt_row gt_center">20,915.04</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.04</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.76</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">175 (9.3%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">6</td>
<td headers="Parameters" class="gt_row gt_center">65</td>
<td headers="LL" class="gt_row gt_center">−9,915.15</td>
<td headers="BIC" class="gt_row gt_center" style="font-weight: bold;">20,320.54</td>
<td headers="aBIC" class="gt_row gt_center" style="font-weight: bold;">20,114.04</td>
<td headers="CAIC" class="gt_row gt_center" style="font-weight: bold;">20,385.54</td>
<td headers="AWE" class="gt_row gt_center">21,005.78</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.01</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.77</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">180 (9.5%)</td>
<td headers="BF" class="gt_row gt_center">41,346,568.94</td>
<td headers="cmPk" class="gt_row gt_center">1.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">7</td>
<td headers="Parameters" class="gt_row gt_center">76</td>
<td headers="LL" class="gt_row gt_center">−9,891.20</td>
<td headers="BIC" class="gt_row gt_center">20,355.62</td>
<td headers="aBIC" class="gt_row gt_center">20,114.16</td>
<td headers="CAIC" class="gt_row gt_center">20,431.62</td>
<td headers="AWE" class="gt_row gt_center">21,156.82</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.08</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.77</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">47 (2.5%)</td>
<td headers="BF" class="gt_row gt_center">2,628,029,072.23</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G7</td>
<td headers="Classes" class="gt_row gt_center">8</td>
<td headers="Parameters" class="gt_row gt_center">87</td>
<td headers="LL" class="gt_row gt_center">−9,871.41</td>
<td headers="BIC" class="gt_row gt_center">20,398.99</td>
<td headers="aBIC" class="gt_row gt_center">20,122.60</td>
<td headers="CAIC" class="gt_row gt_center">20,485.99</td>
<td headers="AWE" class="gt_row gt_center">21,316.17</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.36</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.75</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">55 (2.9%)</td>
<td headers="BF" class="gt_row gt_center">NA</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="14"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> npar = Parameters; <em>LL</em> = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
Smallest n (%) = Number of cases in the smallest class.</span></td>
    </tr>
  </tfoot>
</table>
</div>
<div id="rbamqfxrhm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#rbamqfxrhm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rbamqfxrhm thead, #rbamqfxrhm tbody, #rbamqfxrhm tfoot, #rbamqfxrhm tr, #rbamqfxrhm td, #rbamqfxrhm th {
  border-style: none;
}

#rbamqfxrhm p {
  margin: 0;
  padding: 0;
}

#rbamqfxrhm .gt_table {
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

#rbamqfxrhm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rbamqfxrhm .gt_title {
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

#rbamqfxrhm .gt_subtitle {
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

#rbamqfxrhm .gt_heading {
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

#rbamqfxrhm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rbamqfxrhm .gt_col_headings {
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

#rbamqfxrhm .gt_col_heading {
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

#rbamqfxrhm .gt_column_spanner_outer {
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

#rbamqfxrhm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rbamqfxrhm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rbamqfxrhm .gt_column_spanner {
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

#rbamqfxrhm .gt_spanner_row {
  border-bottom-style: hidden;
}

#rbamqfxrhm .gt_group_heading {
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

#rbamqfxrhm .gt_empty_group_heading {
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

#rbamqfxrhm .gt_from_md > :first-child {
  margin-top: 0;
}

#rbamqfxrhm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rbamqfxrhm .gt_row {
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

#rbamqfxrhm .gt_stub {
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

#rbamqfxrhm .gt_stub_row_group {
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

#rbamqfxrhm .gt_row_group_first td {
  border-top-width: 2px;
}

#rbamqfxrhm .gt_row_group_first th {
  border-top-width: 2px;
}

#rbamqfxrhm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rbamqfxrhm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rbamqfxrhm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rbamqfxrhm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rbamqfxrhm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rbamqfxrhm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rbamqfxrhm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rbamqfxrhm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rbamqfxrhm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rbamqfxrhm .gt_footnotes {
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

#rbamqfxrhm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rbamqfxrhm .gt_sourcenotes {
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

#rbamqfxrhm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rbamqfxrhm .gt_left {
  text-align: left;
}

#rbamqfxrhm .gt_center {
  text-align: center;
}

#rbamqfxrhm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rbamqfxrhm .gt_font_normal {
  font-weight: normal;
}

#rbamqfxrhm .gt_font_bold {
  font-weight: bold;
}

#rbamqfxrhm .gt_font_italic {
  font-style: italic;
}

#rbamqfxrhm .gt_super {
  font-size: 65%;
}

#rbamqfxrhm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rbamqfxrhm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rbamqfxrhm .gt_indent_1 {
  text-indent: 5px;
}

#rbamqfxrhm .gt_indent_2 {
  text-indent: 10px;
}

#rbamqfxrhm .gt_indent_3 {
  text-indent: 15px;
}

#rbamqfxrhm .gt_indent_4 {
  text-indent: 20px;
}

#rbamqfxrhm .gt_indent_5 {
  text-indent: 25px;
}

#rbamqfxrhm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rbamqfxrhm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="14" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Fit Summary Table for G10 Grade</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Grade">Grade</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Classes">Classes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>npar</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="LL"><span class='gt_from_md'><em>LL</em></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Model Fit Indices">
        <div class="gt_column_spanner">Model Fit Indices</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="LRTs">
        <div class="gt_column_spanner">LRTs</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Entropy">Entropy</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="Smallest Class">
        <div class="gt_column_spanner"><span class='gt_from_md'>Smallest Class</span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="BF">BF</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="cmPk">cmPk</th>
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
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">1</td>
<td headers="Parameters" class="gt_row gt_center">10</td>
<td headers="LL" class="gt_row gt_center">−10,072.93</td>
<td headers="BIC" class="gt_row gt_center">20,219.21</td>
<td headers="aBIC" class="gt_row gt_center">20,187.44</td>
<td headers="CAIC" class="gt_row gt_center">20,229.21</td>
<td headers="AWE" class="gt_row gt_center">20,322.56</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">—</td>
<td headers="BLRT_PValue" class="gt_row gt_center">—</td>
<td headers="Entropy" class="gt_row gt_center">NA</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">1534 (100%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">2</td>
<td headers="Parameters" class="gt_row gt_center">21</td>
<td headers="LL" class="gt_row gt_center">−8,428.38</td>
<td headers="BIC" class="gt_row gt_center">17,010.82</td>
<td headers="aBIC" class="gt_row gt_center">16,944.10</td>
<td headers="CAIC" class="gt_row gt_center">17,031.82</td>
<td headers="AWE" class="gt_row gt_center">17,227.86</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.86</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">658 (42.9%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">3</td>
<td headers="Parameters" class="gt_row gt_center">32</td>
<td headers="LL" class="gt_row gt_center">−8,067.61</td>
<td headers="BIC" class="gt_row gt_center">16,369.96</td>
<td headers="aBIC" class="gt_row gt_center">16,268.31</td>
<td headers="CAIC" class="gt_row gt_center">16,401.96</td>
<td headers="AWE" class="gt_row gt_center">16,700.70</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.83</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">297 (19.4%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">4</td>
<td headers="Parameters" class="gt_row gt_center">43</td>
<td headers="LL" class="gt_row gt_center">−7,905.53</td>
<td headers="BIC" class="gt_row gt_center">16,126.50</td>
<td headers="aBIC" class="gt_row gt_center">15,989.90</td>
<td headers="CAIC" class="gt_row gt_center">16,169.50</td>
<td headers="AWE" class="gt_row gt_center" style="font-weight: bold;">16,570.93</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.79</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">290 (18.9%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">5</td>
<td headers="Parameters" class="gt_row gt_center">54</td>
<td headers="LL" class="gt_row gt_center">−7,845.44</td>
<td headers="BIC" class="gt_row gt_center" style="font-weight: bold;">16,087.01</td>
<td headers="aBIC" class="gt_row gt_center">15,915.46</td>
<td headers="CAIC" class="gt_row gt_center" style="font-weight: bold;">16,141.01</td>
<td headers="AWE" class="gt_row gt_center">16,645.13</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.01</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.78</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">220 (14.3%)</td>
<td headers="BF" class="gt_row gt_center">6.63</td>
<td headers="cmPk" class="gt_row gt_center">0.87</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">6</td>
<td headers="Parameters" class="gt_row gt_center">65</td>
<td headers="LL" class="gt_row gt_center">−7,806.99</td>
<td headers="BIC" class="gt_row gt_center">16,090.79</td>
<td headers="aBIC" class="gt_row gt_center">15,884.30</td>
<td headers="CAIC" class="gt_row gt_center">16,155.79</td>
<td headers="AWE" class="gt_row gt_center">16,762.61</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.02</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.80</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">130 (8.5%)</td>
<td headers="BF" class="gt_row gt_center">529,929.89</td>
<td headers="cmPk" class="gt_row gt_center">0.13</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">7</td>
<td headers="Parameters" class="gt_row gt_center">76</td>
<td headers="LL" class="gt_row gt_center">−7,779.82</td>
<td headers="BIC" class="gt_row gt_center">16,117.15</td>
<td headers="aBIC" class="gt_row gt_center">15,875.72</td>
<td headers="CAIC" class="gt_row gt_center">16,193.15</td>
<td headers="AWE" class="gt_row gt_center">16,902.66</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.53</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.85</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">62 (4.1%)</td>
<td headers="BF" class="gt_row gt_center">2,455,890.54</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G10</td>
<td headers="Classes" class="gt_row gt_center">8</td>
<td headers="Parameters" class="gt_row gt_center">87</td>
<td headers="LL" class="gt_row gt_center">−7,754.19</td>
<td headers="BIC" class="gt_row gt_center">16,146.58</td>
<td headers="aBIC" class="gt_row gt_center" style="font-weight: bold;">15,870.20</td>
<td headers="CAIC" class="gt_row gt_center">16,233.58</td>
<td headers="AWE" class="gt_row gt_center">17,045.78</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.32</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.80</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">84 (5.5%)</td>
<td headers="BF" class="gt_row gt_center">NA</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="14"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> npar = Parameters; <em>LL</em> = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
Smallest n (%) = Number of cases in the smallest class.</span></td>
    </tr>
  </tfoot>
</table>
</div>
<div id="gdenheyxcu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>#gdenheyxcu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gdenheyxcu thead, #gdenheyxcu tbody, #gdenheyxcu tfoot, #gdenheyxcu tr, #gdenheyxcu td, #gdenheyxcu th {
  border-style: none;
}

#gdenheyxcu p {
  margin: 0;
  padding: 0;
}

#gdenheyxcu .gt_table {
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

#gdenheyxcu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gdenheyxcu .gt_title {
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

#gdenheyxcu .gt_subtitle {
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

#gdenheyxcu .gt_heading {
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

#gdenheyxcu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gdenheyxcu .gt_col_headings {
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

#gdenheyxcu .gt_col_heading {
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

#gdenheyxcu .gt_column_spanner_outer {
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

#gdenheyxcu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gdenheyxcu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gdenheyxcu .gt_column_spanner {
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

#gdenheyxcu .gt_spanner_row {
  border-bottom-style: hidden;
}

#gdenheyxcu .gt_group_heading {
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

#gdenheyxcu .gt_empty_group_heading {
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

#gdenheyxcu .gt_from_md > :first-child {
  margin-top: 0;
}

#gdenheyxcu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gdenheyxcu .gt_row {
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

#gdenheyxcu .gt_stub {
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

#gdenheyxcu .gt_stub_row_group {
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

#gdenheyxcu .gt_row_group_first td {
  border-top-width: 2px;
}

#gdenheyxcu .gt_row_group_first th {
  border-top-width: 2px;
}

#gdenheyxcu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdenheyxcu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gdenheyxcu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gdenheyxcu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gdenheyxcu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdenheyxcu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gdenheyxcu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gdenheyxcu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gdenheyxcu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gdenheyxcu .gt_footnotes {
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

#gdenheyxcu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdenheyxcu .gt_sourcenotes {
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

#gdenheyxcu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdenheyxcu .gt_left {
  text-align: left;
}

#gdenheyxcu .gt_center {
  text-align: center;
}

#gdenheyxcu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gdenheyxcu .gt_font_normal {
  font-weight: normal;
}

#gdenheyxcu .gt_font_bold {
  font-weight: bold;
}

#gdenheyxcu .gt_font_italic {
  font-style: italic;
}

#gdenheyxcu .gt_super {
  font-size: 65%;
}

#gdenheyxcu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gdenheyxcu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gdenheyxcu .gt_indent_1 {
  text-indent: 5px;
}

#gdenheyxcu .gt_indent_2 {
  text-indent: 10px;
}

#gdenheyxcu .gt_indent_3 {
  text-indent: 15px;
}

#gdenheyxcu .gt_indent_4 {
  text-indent: 20px;
}

#gdenheyxcu .gt_indent_5 {
  text-indent: 25px;
}

#gdenheyxcu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#gdenheyxcu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="14" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'><strong>Model Fit Summary Table for G12 Grade</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Grade">Grade</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Classes">Classes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Parameters"><span class='gt_from_md'>npar</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="LL"><span class='gt_from_md'><em>LL</em></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Model Fit Indices">
        <div class="gt_column_spanner">Model Fit Indices</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="LRTs">
        <div class="gt_column_spanner">LRTs</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="Entropy">Entropy</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" scope="col" id="Smallest Class">
        <div class="gt_column_spanner"><span class='gt_from_md'>Smallest Class</span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="BF">BF</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="cmPk">cmPk</th>
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
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">1</td>
<td headers="Parameters" class="gt_row gt_center">10</td>
<td headers="LL" class="gt_row gt_center">−7,349.13</td>
<td headers="BIC" class="gt_row gt_center">14,768.49</td>
<td headers="aBIC" class="gt_row gt_center">14,736.72</td>
<td headers="CAIC" class="gt_row gt_center">14,778.49</td>
<td headers="AWE" class="gt_row gt_center">14,868.72</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">—</td>
<td headers="BLRT_PValue" class="gt_row gt_center">—</td>
<td headers="Entropy" class="gt_row gt_center">NA</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">1122 (100%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">2</td>
<td headers="Parameters" class="gt_row gt_center">21</td>
<td headers="LL" class="gt_row gt_center">−5,976.60</td>
<td headers="BIC" class="gt_row gt_center">12,100.68</td>
<td headers="aBIC" class="gt_row gt_center">12,033.98</td>
<td headers="CAIC" class="gt_row gt_center">12,121.68</td>
<td headers="AWE" class="gt_row gt_center">12,311.16</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.86</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">534 (47.5%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">3</td>
<td headers="Parameters" class="gt_row gt_center">32</td>
<td headers="LL" class="gt_row gt_center">−5,670.60</td>
<td headers="BIC" class="gt_row gt_center">11,565.92</td>
<td headers="aBIC" class="gt_row gt_center">11,464.28</td>
<td headers="CAIC" class="gt_row gt_center">11,597.92</td>
<td headers="AWE" class="gt_row gt_center">11,886.65</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center"><.001</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.85</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">203 (18.1%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">4</td>
<td headers="Parameters" class="gt_row gt_center">43</td>
<td headers="LL" class="gt_row gt_center">−5,543.62</td>
<td headers="BIC" class="gt_row gt_center">11,389.22</td>
<td headers="aBIC" class="gt_row gt_center">11,252.64</td>
<td headers="CAIC" class="gt_row gt_center">11,432.22</td>
<td headers="AWE" class="gt_row gt_center" style="font-weight: bold;">11,820.20</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.00</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.80</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">219 (19.5%)</td>
<td headers="BF" class="gt_row gt_center">0.00</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">5</td>
<td headers="Parameters" class="gt_row gt_center">54</td>
<td headers="LL" class="gt_row gt_center">−5,483.67</td>
<td headers="BIC" class="gt_row gt_center">11,346.57</td>
<td headers="aBIC" class="gt_row gt_center">11,175.06</td>
<td headers="CAIC" class="gt_row gt_center" style="font-weight: bold;">11,400.57</td>
<td headers="AWE" class="gt_row gt_center">11,887.81</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.00</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.84</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">131 (11.7%)</td>
<td headers="BF" class="gt_row gt_center">0.37</td>
<td headers="cmPk" class="gt_row gt_center">0.27</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">6</td>
<td headers="Parameters" class="gt_row gt_center">65</td>
<td headers="LL" class="gt_row gt_center">−5,444.06</td>
<td headers="BIC" class="gt_row gt_center" style="font-weight: bold;">11,344.60</td>
<td headers="aBIC" class="gt_row gt_center">11,138.14</td>
<td headers="CAIC" class="gt_row gt_center">11,409.60</td>
<td headers="AWE" class="gt_row gt_center">11,996.08</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.09</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.84</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">81 (7.2%)</td>
<td headers="BF" class="gt_row gt_center">3,693,185.79</td>
<td headers="cmPk" class="gt_row gt_center">0.73</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">7</td>
<td headers="Parameters" class="gt_row gt_center">76</td>
<td headers="LL" class="gt_row gt_center">−5,420.55</td>
<td headers="BIC" class="gt_row gt_center">11,374.84</td>
<td headers="aBIC" class="gt_row gt_center">11,133.44</td>
<td headers="CAIC" class="gt_row gt_center">11,450.84</td>
<td headers="AWE" class="gt_row gt_center">12,136.58</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center" style="font-weight: bold;">0.03</td>
<td headers="BLRT_PValue" class="gt_row gt_center"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.85</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">44 (3.9%)</td>
<td headers="BF" class="gt_row gt_center">810,981.08</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
    <tr><td headers="Grade" class="gt_row gt_center">G12</td>
<td headers="Classes" class="gt_row gt_center">8</td>
<td headers="Parameters" class="gt_row gt_center">87</td>
<td headers="LL" class="gt_row gt_center">−5,395.53</td>
<td headers="BIC" class="gt_row gt_center">11,402.05</td>
<td headers="aBIC" class="gt_row gt_center" style="font-weight: bold;">11,125.72</td>
<td headers="CAIC" class="gt_row gt_center">11,489.05</td>
<td headers="AWE" class="gt_row gt_center">12,274.04</td>
<td headers="T11_VLMR_PValue" class="gt_row gt_center">0.10</td>
<td headers="BLRT_PValue" class="gt_row gt_center" style="font-weight: bold;"><.001</td>
<td headers="Entropy" class="gt_row gt_center">0.84</td>
<td headers="Smallest_Class_Combined" class="gt_row gt_center">61 (5.4%)</td>
<td headers="BF" class="gt_row gt_center">NA</td>
<td headers="cmPk" class="gt_row gt_center">0.00</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="14"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> npar = Parameters; <em>LL</em> = model log likelihood;
BIC = Bayesian information criterion;
aBIC = sample size adjusted BIC; CAIC = consistent Akaike information criterion;
AWE = approximate weight of evidence criterion;
BLRT = bootstrapped likelihood ratio test p-value;
VLMR = Vuong-Lo-Mendell-Rubin adjusted likelihood ratio test p-value;
Smallest n (%) = Number of cases in the smallest class.</span></td>
    </tr>
  </tfoot>
</table>
</div>

Save Fit Tables


``` r
# Save tables as PNG files
gtsave(fit_tables[["g7"]], filename = here("tc_lta","figures", "fit_table_lca_g7.png"))
gtsave(fit_tables[["g10"]], filename = here("tc_lta","figures", "fit_table_lca_g10.png"))
gtsave(fit_tables[["g12"]], filename = here("tc_lta","figures", "fit_table_lca_g12.png"))
```

------------------------------------------------------------------------

## Create and Examine Probability Plots for Four Class Solution

To aid in interpretation, we visualize the conditional response probabilities for the selected four-class solution at each grade. These plots show the probability of endorsing each response category within each latent profile and help clarify how the classes


``` r
# Source plot_lca function
source(here("tc_lta","functions", "plot_lca.txt"))

# Define grades and folders
grades <- c("g7", "g10", "g12")
folder_paths <- c(
  g7 = here("tc_lta","g7_enum"),
  g10 = here("tc_lta","g10_enum"),
  g12 = here("tc_lta","g12_enum")
)

# Generate plot for each grade
for (grade in grades) {
  # Read 4-class model
  model_file <- file.path(folder_paths[grade], paste0("c4_", grade, ".out"))
  model <- readModels(model_file, quiet = TRUE)
  
  # Base plot
  base_plot <- plot_lca(model_name = model)
  
  # Get class sizes and round to 1 decimal
  c_size <- round(model$class_counts$modelEstimated$proportion * 100, 1)
  
  # Customize plot with generic class labels
  final_plot <- base_plot +
    scale_colour_discrete(labels = c(
      glue("Class 1 ({c_size[1]}%)"),
      glue("Class 2 ({c_size[2]}%)"),
      glue("Class 3 ({c_size[3]}%)"),
      glue("Class 4 ({c_size[4]}%)")
    )) +
    scale_shape_discrete(labels = c(
      glue("Class 1 ({c_size[1]}%)"),
      glue("Class 2 ({c_size[2]}%)"),
      glue("Class 3 ({c_size[3]}%)"),
      glue("Class 4 ({c_size[4]}%)")
    )) +
    scale_linetype_discrete(labels = c(
      glue("Class 1 ({c_size[1]}%)"),
      glue("Class 2 ({c_size[2]}%)"),
      glue("Class 3 ({c_size[3]}%)"),
      glue("Class 4 ({c_size[4]}%)")
    )) +
    labs(title = glue("4-Class LCA Item Probability Plot for Grade {toupper(grade)}"))
  
  # Display plot
  print(final_plot)
}
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-25-1.png" width="95%" /><img src="28-tc-lta_files/figure-html/unnamed-chunk-25-2.png" width="95%" /><img src="28-tc-lta_files/figure-html/unnamed-chunk-25-3.png" width="95%" />

------------------------------------------------------------------------

## Conduct step 1: Invariant Latent Transition Analysis

In Step 1 of the latent transition analysis (LTA), we estimate an unconditional longitudinal model with measurement invariance across timepoints. This means that item-response thresholds are constrained to be equal across Grades 7, 10, and 12, allowing us to interpret latent class transitions over time on a consistent measurement scale. This step provides the foundation for the 3-step approach by establishing a stable class structure across waves.


``` r

# Define LTA model
lta_model <- mplusObject(
  TITLE = "4-Class LTA for G7, G10, G12",
  
  VARIABLE = glue(
    "categorical = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X
                   GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L
                   KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L;
     usevar = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X
              GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L
              KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L
               FEMALE MINORITY STEM STEMSUP ENGINEER MATHG8 SCIG8 MATHG11 SCIG11
               MATHG7 MATHG10 MATHG12;
     auxiliary = FEMALE MINORITY STEM STEMSUP ENGINEER MATHG8 SCIG8 MATHG11 SCIG11
     MATHG7 MATHG10 MATHG12;
     missing = all(9999);
     classes = c1(4) c2(4) c3(4);
    auxiliary = stem;"
  ),
  
  ANALYSIS = "
    estimator = mlr;
    type = mixture;
    starts = 500 10;
    processors = 4;",
  
  MODEL = glue(
    "%overall%
     c2 on c1;
     c3 on c2;
     
     MODEL c1:
     %c1#1%
     [AB39A$1-AB39X$1] (1-10);
     %c1#2%
     [AB39A$1-AB39X$1] (11-20);
     %c1#3%
     [AB39A$1-AB39X$1] (21-30);
     %c1#4%
     [AB39A$1-AB39X$1] (31-40);
     
     MODEL c2:
     %c2#1%
     [GA32A$1-GA33L$1] (1-10);
     %c2#2%
     [GA32A$1-GA33L$1] (11-20);
     %c2#3%
     [GA32A$1-GA33L$1] (21-30);
     %c2#4%
     [GA32A$1-GA33L$1] (31-40);
     
     MODEL c3:
     %c3#1%
     [KA46A$1-KA47L$1] (1-10);
     %c3#2%
     [KA46A$1-KA47L$1] (11-20);
     %c3#3%
     [KA46A$1-KA47L$1] (21-30);
     %c3#4%
     [KA46A$1-KA47L$1] (31-40);"
  ),
  
  OUTPUT = "svalues; sampstat; tech11; tech14;",
  
    SAVEDATA = "
    file = lta_4class_cprobs.dat;
    save = cprobabilities;
    missflag = 9999;",
  
  PLOT = "
    type = plot3;
    series = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X
             GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L
             KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L (*);",
  
  rdata = lsay_data
)

# Run LTA model
lta_fit <- mplusModeler(
  lta_model,
  dataout = here("tc_lta","lta_enum", "lsay_lta.dat"),
  modelout = here("tc_lta","lta_enum", "lta_4class.inp"),
  check = TRUE,
  run = TRUE,
  hashfilename = FALSE
)

```

------------------------------------------------------------------------

### Plot Invariant Probability Plot

After fitting the invariant LTA model, we examine the conditional response probabilities for each latent class. These plots illustrate how students in each profile tend to respond to the attitudinal items, averaged across timepoints. Because the model assumes measurement invariance, differences between classes can be interpreted consistently across Grades 7, 10, and 12.


``` r
# Load LTA model
lta_model <- readModels(here("tc_lta","lta_enum", "lta_4class.out"), quiet = TRUE)

# Define G7 items
g7_items <- c("AB39A", "AB39H", "AB39I", "AB39K", "AB39L", "AB39M", "AB39T", "AB39U", "AB39W", "AB39X")

# Define descriptive item labels (from Table 1 in the paper)
item_labels <- c(
  "I enjoy math",
  "Math is useful in everyday problems",
  "Math helps a person think logically",
  "It is important to know math to get a good job",
  "I will use math in many ways as an adult",
  "I enjoy science",
  "Science is useful in everyday problems",
  "Science helps a person think logically",
  "It is important to know science to get a good job",
  "I will use science in many ways as an adult"
)

# Define class names from the paper
class_names <- c(
  "Very Positive",
  "Qualified Positive",
  "Neutral",
  "Less Positive"
)

# Inline plot_lta function (with updated title to match Figure 1)
plot_lta <- function(model_name, time_point = "c1", items = NULL) {
  probs <- data.frame(model_name$parameters$probability.scale)
  
  pp_plots <- probs %>%
    mutate(LatentClass = sub("^", "Class ", LatentClass)) %>%
    filter(grepl(paste0("^Class ", toupper(time_point), "#"), LatentClass)) %>%
    filter(category == 2) %>%
    dplyr::select(est, LatentClass, param)
  
  if (!is.null(items)) {
    pp_plots <- pp_plots %>%
      filter(param %in% items)
  } else {
    pp_plots <- pp_plots %>%
      filter(param %in% unique(param)[1:10])
  }
  
  # Apply class names from the paper
  pp_plots <- pp_plots %>%
    mutate(LatentClass = case_when(
      LatentClass == "Class C1#1" ~ class_names[1],
      LatentClass == "Class C1#2" ~ class_names[2],
      LatentClass == "Class C1#3" ~ class_names[3],
      LatentClass == "Class C1#4" ~ class_names[4],
      TRUE ~ LatentClass
    ))
  
  pp_plots <- pp_plots %>%
    pivot_wider(names_from = LatentClass, values_from = est) %>%
    relocate(param, .after = last_col())
  
  # Extract class proportions (using Grade 7 proportions from Table 4)
  c_size <- data.frame(cs = c(34, 33, 18, 14))
  
  colnames(pp_plots)[1:4] <- paste0(colnames(pp_plots)[1:4], glue(" ({c_size[1:4,]}%)"))
  
  plot_data <- pp_plots %>%
    rename("param" = ncol(pp_plots)) %>%
    reshape2::melt(id.vars = "param") %>%
    mutate(param = fct_inorder(param))
  
  # Apply descriptive item labels
  levels(plot_data$param) <- item_labels
  
  # Create the plot with the exact title from Figure 1
  p <- plot_data %>%
    ggplot(
      aes(
        x = param,
        y = value,
        shape = variable,
        linetype = variable,
        group = variable
      )
    ) +
    geom_point(size = 3) +
    geom_line() +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_color_grey(start = 0.1, end = 0.6) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    ylim(0, 1) +
    labs(
      title = "Proportion of seventh graders endorsing each item by attitudinal profile",  # Exact title from Figure 1
      x = "",
      y = "Probability of Endorsement",
      caption = "Note: The four-class structure is invariant across grades 7, 10, and 12, but probabilities and proportions shown are for Grade 7."
    ) +
    theme_classic() +
    theme(
      text = element_text(family = "serif", size = 12),
      legend.text = element_text(family = "serif", size = 12),
      legend.key.width = unit(1, "cm"),
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(hjust = 0.5),
      plot.margin = margin(b = 40),
      plot.caption = element_text(hjust = 0, size = 10, family = "serif"),
      plot.title = element_text(size = 12, family = "serif")  # Ensure title font matches
    )
  
  return(p)
}

# Generate plot using plot_lta
final_plot <- plot_lta(
  model_name = lta_model,
  time_point = "c1",
  items = g7_items
)

# Display plot
print(final_plot)
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-27-1.png" width="672" />

Save Invariant Probability Plot


``` r

# Save the final_plot from Chunk 1
ggsave(
  filename = here("tc_lta","figures", "LTA_4class_invariant_plot.png"),
  plot = final_plot,
  dpi = 300,
  height = 5,
  width = 7,
  units = "in"
)
```

------------------------------------------------------------------------

## Conduct Step 2 LTA

In Step 2 of the 3-step LTA procedure, we re-estimate the latent class models separately for Grades 7, 10, and 12, this time fixing the item-response thresholds to the logits obtained from the invariant Step 1 model. This ensures that the class measurement remains consistent across time. We also include auxiliary variables to save the modal class assignments for each student at each grade level. These class assignments will be used in Step 3 to estimate covariate and distal outcome effects while accounting for classification uncertainty.

### Scrape logits for each grade for the invariant LTA


``` r
lta_fit <- readModels(here("tc_lta","lta_enum", "lta_4class.out"))


# Extract item thresholds (logits) for all items across all latent classes
item_logits_full <- lta_fit$parameters$unstandardized %>%
  filter(paramHeader == "Thresholds") %>%
  filter(str_detect(param, "\\$1")) %>%
  select(class = LatentClass, item = param, logit = est)

```

### Rerun LCA for Grade 7, 10, and 12 with invariant item probabilities

Run LCA for G7 with fixed item probabilities


``` r

lca_belonging_g7 <- {
  g7_model <- mplusObject(
    TITLE = "4-Class G7 LCA with Fixed Thresholds from Invariant LTA",

  VARIABLE = "
  categorical = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X;
  usevar = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X;
  missing = all(9999);
  classes = c(4);
  auxiliary = GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L
            KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L
            STEM STEMSup ENGINEER MathG8 SciG8 MathG11 SciG11 Female
            Minority MathG7 MathG10 MathG12 CASENUM;",

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 0;
      processors = 4;",

    MODEL = glue("
      %OVERALL%
        [c#1@0];

      %c#1%
        [AB39A$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39A$1']}]
        [AB39H$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39H$1']}]
        [AB39I$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39I$1']}]
        [AB39K$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39K$1']}]
        [AB39L$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39L$1']}]
        [AB39M$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39M$1']}]
        [AB39T$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39T$1']}]
        [AB39U$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39U$1']}]
        [AB39W$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39W$1']}]
        [AB39X$1@{item_logits_full$logit[item_logits_full$class == 'C1#1' & item_logits_full$item == 'AB39X$1']}]

      %c#2%
        [AB39A$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39A$1']}]
        [AB39H$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39H$1']}]
        [AB39I$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39I$1']}]
        [AB39K$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39K$1']}]
        [AB39L$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39L$1']}]
        [AB39M$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39M$1']}]
        [AB39T$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39T$1']}]
        [AB39U$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39U$1']}]
        [AB39W$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39W$1']}]
        [AB39X$1@{item_logits_full$logit[item_logits_full$class == 'C1#2' & item_logits_full$item == 'AB39X$1']}]

      %c#3%
        [AB39A$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39A$1']}]
        [AB39H$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39H$1']}]
        [AB39I$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39I$1']}]
        [AB39K$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39K$1']}]
        [AB39L$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39L$1']}]
        [AB39M$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39M$1']}]
        [AB39T$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39T$1']}]
        [AB39U$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39U$1']}]
        [AB39W$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39W$1']}]
        [AB39X$1@{item_logits_full$logit[item_logits_full$class == 'C1#3' & item_logits_full$item == 'AB39X$1']}]

      %c#4%
        [AB39A$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39A$1']}]
        [AB39H$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39H$1']}]
        [AB39I$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39I$1']}]
        [AB39K$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39K$1']}]
        [AB39L$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39L$1']}]
        [AB39M$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39M$1']}]
        [AB39T$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39T$1']}]
        [AB39U$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39U$1']}]
        [AB39W$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39W$1']}]
        [AB39X$1@{item_logits_full$logit[item_logits_full$class == 'C1#4' & item_logits_full$item == 'AB39X$1']}]
    "),

    OUTPUT = "
      sampstat;
      tech1;
      tech11;
      tech14;
      svalues;",

    SAVEDATA = "
      file = g7.dat;
      save = cprobabilities;
      missflag = 9999;",
  
    rdata = lsay_data
  )

  mplusModeler(
    g7_model,
    dataout = here("tc_lta","lca_enum2", "lca_g7.dat"),
    modelout = here("tc_lta","lca_enum2", "lca_g7.inp"),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
}
```

Run LCA for G10 with fixed item probabilities


``` r

lca_belonging_g10 <- {
  g10_model <- mplusObject(
    TITLE = "4-Class G10 LCA with Fixed Thresholds from Invariant LTA",

    VARIABLE = "
  categorical = GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L;
  usevar = GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L;
  auxiliary = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X
              KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L
              STEM STEMSup ENGINEER MathG8 SciG8 MathG11 SciG11 Female 
              Minority MathG7 MathG10 MathG12 CASENUM;
  missing = all(9999);
  classes = c(4);",

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 0;
      processors = 4;",

    MODEL = glue("
      %OVERALL%
        [c#1@0];

      %c#1%
        [GA32A$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA32A$1']}]
        [GA32H$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA32H$1']}]
        [GA32I$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA32I$1']}]
        [GA32K$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA32K$1']}]
        [GA32L$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA32L$1']}]
        [GA33A$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA33A$1']}]
        [GA33H$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA33H$1']}]
        [GA33I$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA33I$1']}]
        [GA33K$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA33K$1']}]
        [GA33L$1@{item_logits_full$logit[item_logits_full$class == 'C2#1' & item_logits_full$item == 'GA33L$1']}]

      %c#2%
        [GA32A$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA32A$1']}]
        [GA32H$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA32H$1']}]
        [GA32I$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA32I$1']}]
        [GA32K$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA32K$1']}]
        [GA32L$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA32L$1']}]
        [GA33A$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA33A$1']}]
        [GA33H$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA33H$1']}]
        [GA33I$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA33I$1']}]
        [GA33K$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA33K$1']}]
        [GA33L$1@{item_logits_full$logit[item_logits_full$class == 'C2#2' & item_logits_full$item == 'GA33L$1']}]

      %c#3%
        [GA32A$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA32A$1']}]
        [GA32H$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA32H$1']}]
        [GA32I$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA32I$1']}]
        [GA32K$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA32K$1']}]
        [GA32L$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA32L$1']}]
        [GA33A$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA33A$1']}]
        [GA33H$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA33H$1']}]
        [GA33I$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA33I$1']}]
        [GA33K$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA33K$1']}]
        [GA33L$1@{item_logits_full$logit[item_logits_full$class == 'C2#3' & item_logits_full$item == 'GA33L$1']}]

      %c#4%
        [GA32A$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA32A$1']}]
        [GA32H$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA32H$1']}]
        [GA32I$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA32I$1']}]
        [GA32K$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA32K$1']}]
        [GA32L$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA32L$1']}]
        [GA33A$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA33A$1']}]
        [GA33H$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA33H$1']}]
        [GA33I$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA33I$1']}]
        [GA33K$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA33K$1']}]
        [GA33L$1@{item_logits_full$logit[item_logits_full$class == 'C2#4' & item_logits_full$item == 'GA33L$1']}]
    "),

    OUTPUT = "
      sampstat;
      tech1;
      tech11;
      tech14;
      svalues;",

    SAVEDATA = "
      file = g10.dat;
      save = cprob;
      missflag = 9999;",

    rdata = lsay_data
  )

  mplusModeler(
    g10_model,
    dataout = here("tc_lta","lca_enum2", "lca_g10.dat"),
    modelout = here("tc_lta","lca_enum2", "lca_g10.inp"),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
}

```

Run LCA for G12 with fixed item probabilities


``` r
lca_belonging_g12 <- {
  g12_model <- mplusObject(
    TITLE = "4-Class G12 LCA with Fixed Thresholds from Invariant LTA",

    VARIABLE = "
  categorical = KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L;
  usevar = KA46A KA46H KA46I KA46K KA46L KA47A KA47H KA47I KA47K KA47L;
  auxiliary = AB39A AB39H AB39I AB39K AB39L AB39M AB39T AB39U AB39W AB39X
              GA32A GA32H GA32I GA32K GA32L GA33A GA33H GA33I GA33K GA33L
              STEM STEMSup ENGINEER MathG8 SciG8 MathG11 SciG11 Female 
              Minority MathG7 MathG10 MathG12 CASENUM;
  missing = all(9999);
  classes = c(4);",

    ANALYSIS = "
      estimator = mlr;
      type = mixture;
      starts = 0;
      processors = 4;",

    MODEL = glue("
      %OVERALL%
        [c#1@0];

      %c#1%
        [KA46A$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA46A$1']}]
        [KA46H$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA46H$1']}]
        [KA46I$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA46I$1']}]
        [KA46K$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA46K$1']}]
        [KA46L$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA46L$1']}]
        [KA47A$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA47A$1']}]
        [KA47H$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA47H$1']}]
        [KA47I$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA47I$1']}]
        [KA47K$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA47K$1']}]
        [KA47L$1@{item_logits_full$logit[item_logits_full$class == 'C3#1' & item_logits_full$item == 'KA47L$1']}]

      %c#2%
        [KA46A$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA46A$1']}]
        [KA46H$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA46H$1']}]
        [KA46I$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA46I$1']}]
        [KA46K$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA46K$1']}]
        [KA46L$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA46L$1']}]
        [KA47A$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA47A$1']}]
        [KA47H$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA47H$1']}]
        [KA47I$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA47I$1']}]
        [KA47K$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA47K$1']}]
        [KA47L$1@{item_logits_full$logit[item_logits_full$class == 'C3#2' & item_logits_full$item == 'KA47L$1']}]

      %c#3%
        [KA46A$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA46A$1']}]
        [KA46H$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA46H$1']}]
        [KA46I$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA46I$1']}]
        [KA46K$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA46K$1']}]
        [KA46L$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA46L$1']}]
        [KA47A$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA47A$1']}]
        [KA47H$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA47H$1']}]
        [KA47I$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA47I$1']}]
        [KA47K$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA47K$1']}]
        [KA47L$1@{item_logits_full$logit[item_logits_full$class == 'C3#3' & item_logits_full$item == 'KA47L$1']}]

      %c#4%
        [KA46A$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA46A$1']}]
        [KA46H$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA46H$1']}]
        [KA46I$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA46I$1']}]
        [KA46K$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA46K$1']}]
        [KA46L$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA46L$1']}]
        [KA47A$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA47A$1']}]
        [KA47H$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA47H$1']}]
        [KA47I$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA47I$1']}]
        [KA47K$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA47K$1']}]
        [KA47L$1@{item_logits_full$logit[item_logits_full$class == 'C3#4' & item_logits_full$item == 'KA47L$1']}]
    "),

    OUTPUT = "
      sampstat;
      tech1;
      tech11;
      tech14;
      svalues;",

    SAVEDATA = "
      file = g12.dat;
      save = cprob;
      missflag = 9999;",

    rdata = lsay_data
  )

  mplusModeler(
    g12_model,
    dataout = here("tc_lta","lca_enum2", "lca_g12.dat"),
    modelout = here("tc_lta","lca_enum2", "lca_g12.inp"),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
}

```

### Prepare data for Step 2 LTA


``` r
# Helper: extract variable names from a .out file
scrape_saved_vars <- function(out_path) {
  lines <- readLines(out_path)
  start <- grep("^SAVEDATA INFORMATION$", lines)
  section <- lines[start:length(lines)]
  var_start <- grep("^\\s*Order and format of variables\\s*$", section)
  var_lines <- section[(var_start + 1):length(section)]
  end_idx <- which(grepl("^\\s*Save file format\\s*$", var_lines))[1]
  if (!is.na(end_idx)) {
    var_lines <- var_lines[1:(end_idx - 1)]
  }
  var_lines <- var_lines[grepl("^\\s*[A-Za-z0-9_]+\\s+F10\\.3\\s*$", var_lines)]
  trimws(gsub("\\s+F10\\.3\\s*$", "", var_lines))
}

# Paths
g7_path <- here("tc_lta","lca_enum2", "g7.dat")
g10_path <- here("tc_lta","lca_enum2", "g10.dat")
g12_path <- here("tc_lta","lca_enum2", "g12.dat")

# Output files
g7_out <- here("tc_lta","lca_enum2", "lca_g7.out")
g10_out <- here("tc_lta","lca_enum2", "lca_g10.out")
g12_out <- here("tc_lta","lca_enum2", "lca_g12.out")

# Scrape column names
g7_vars <- scrape_saved_vars(g7_out)
g10_vars <- scrape_saved_vars(g10_out)
g12_vars <- scrape_saved_vars(g12_out)

# Read and rename C column
g7 <- read.table(g7_path, col.names = g7_vars)
g10 <- read.table(g10_path, col.names = g10_vars)
g12 <- read.table(g12_path, col.names = g12_vars)

# Rename C columns
g7 <- g7 %>% rename(N1 = C)
g10 <- g10 %>% rename(N2 = C)
g12 <- g12 %>% rename(N3 = C)

# Write to CSV for inspection
write_csv(g7, here("tc_lta","lca_enum2", "g7_named.csv"))
write_csv(g10, here("tc_lta","lca_enum2", "g10_named.csv"))
write_csv(g12, here("tc_lta","lca_enum2", "g12_named.csv"))

```

## Merge Files


``` r

# Load base file
g7 <- read_csv(here("tc_lta","lca_enum2", "g7_named.csv"), col_types = cols())

# Only grab CASENUM + N2 / N3 from others
g10 <- read_csv(here("tc_lta","lca_enum2", "g10_named.csv"), col_types = cols()) %>% select(CASENUM, N2)
g12 <- read_csv(here("tc_lta","lca_enum2", "g12_named.csv"), col_types = cols()) %>% select(CASENUM, N3)
# After loading all three CSVs:
g7 <- g7 %>% mutate(across(everything(), ~replace_na(.x, 9999)))
g10 <- g10 %>% mutate(across(everything(), ~replace_na(.x, 9999)))
g12 <- g12 %>% mutate(across(everything(), ~replace_na(.x, 9999)))

# Merge cleanly by CASENUM
merged <- g7 %>%
  left_join(g10, by = "CASENUM") %>%
  left_join(g12, by = "CASENUM") %>%
  mutate(
    N1 = replace_na(N1, 9999),
    N2 = replace_na(N2, 9999),
    N3 = replace_na(N3, 9999)
  )

# Write final output
write_csv(merged, here("tc_lta","lca_enum2", "merged.csv"))

```

### Scrape Logits for LTA


``` r
g7_model  <- readModels(here("tc_lta","lca_enum2", "lca_g7.out"))
g10_model <- readModels(here("tc_lta","lca_enum2", "lca_g10.out"))
g12_model <- readModels(here("tc_lta","lca_enum2", "lca_g12.out"))
```

### Extract Logits


``` r
# Model list using correct source: class_counts$logitProbs.mostLikely
model_list <- list(
  G7  = g7_model$class_counts$logitProbs.mostLikely,
  G10 = g10_model$class_counts$logitProbs.mostLikely,
  G12 = g12_model$class_counts$logitProbs.mostLikely
)

# Reshape each 4x4 matrix into tidy long format
step2_logits <- map2_dfr(
  model_list,
  names(model_list),
  function(mat, grade) {
    # Assign column names explicitly
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))

    as.data.frame(mat) %>%
      mutate(source_class = row_number()) %>%
      pivot_longer(
        cols = starts_with("V"),
        names_to = "assigned_class",
        names_prefix = "V",
        values_to = "logit"
      ) %>%
      mutate(
        assigned_class = as.integer(assigned_class),
        grade = grade
      ) %>%
      select(grade, source_class, assigned_class, logit)
  }
)

```

------------------------------------------------------------------------

### Conduct Step 2 LTA with fixed classes

After rerunning the grade-specific LCAs with fixed logits, we merge the saved modal class assignment files into a single dataset. This merged file includes class membership at each grade along with relevant auxiliary variables. Using this file, we then run the Step 2 LTA model by fixing the class assignment logits (classification probabilities) from the grade-specific models. This step allows us to model transitions across latent profiles over time while incorporating the uncertainty in class membership derived from the classification error rates.

As a quality check, we cross-reference the estimated class sizes from Step 2 against those from the invariant Step 1 model. Matching class sizes confirms that the Step 2 specification correctly replicates the measurement structure and preserves consistency in profile estimation.


``` r

# Extract logits for each timepoint into matrices
logits_n1 <- matrix(step2_logits$logit[step2_logits$grade == "G7"],  nrow = 4, byrow = TRUE)
logits_n2 <- matrix(step2_logits$logit[step2_logits$grade == "G10"], nrow = 4, byrow = TRUE)
logits_n3 <- matrix(step2_logits$logit[step2_logits$grade == "G12"], nrow = 4, byrow = TRUE)

# Build Mplus object
step2_lta_model <- mplusObject(
  TITLE = "Step 2 LTA with Fixed Logits — No Covariates",

  VARIABLE = "
  USEVAR = n1 n2 n3;
  NOMINAL = n1 n2 n3;
  MISSING = all(9999);
  CLASSES = c1(4) c2(4) c3(4);",

  ANALYSIS = "
  TYPE = mixture;
  STARTS = 0;",

  MODEL = glue("
  MODEL c1:
    %c1#1%
      [n1#1@{logits_n1[1,1]}];
      [n1#2@{logits_n1[1,2]}];
      [n1#3@{logits_n1[1,3]}];
    %c1#2%
      [n1#1@{logits_n1[2,1]}];
      [n1#2@{logits_n1[2,2]}];
      [n1#3@{logits_n1[2,3]}];
    %c1#3%
      [n1#1@{logits_n1[3,1]}];
      [n1#2@{logits_n1[3,2]}];
      [n1#3@{logits_n1[3,3]}];
    %c1#4%
      [n1#1@{logits_n1[4,1]}];
      [n1#2@{logits_n1[4,2]}];
      [n1#3@{logits_n1[4,3]}];

  MODEL c2:
    %c2#1%
      [n2#1@{logits_n2[1,1]}];
      [n2#2@{logits_n2[1,2]}];
      [n2#3@{logits_n2[1,3]}];
    %c2#2%
      [n2#1@{logits_n2[2,1]}];
      [n2#2@{logits_n2[2,2]}];
      [n2#3@{logits_n2[2,3]}];
    %c2#3%
      [n2#1@{logits_n2[3,1]}];
      [n2#2@{logits_n2[3,2]}];
      [n2#3@{logits_n2[3,3]}];
    %c2#4%
      [n2#1@{logits_n2[4,1]}];
      [n2#2@{logits_n2[4,2]}];
      [n2#3@{logits_n2[4,3]}];

  MODEL c3:
    %c3#1%
      [n3#1@{logits_n3[1,1]}];
      [n3#2@{logits_n3[1,2]}];
      [n3#3@{logits_n3[1,3]}];
    %c3#2%
      [n3#1@{logits_n3[2,1]}];
      [n3#2@{logits_n3[2,2]}];
      [n3#3@{logits_n3[2,3]}];
    %c3#3%
      [n3#1@{logits_n3[3,1]}];
      [n3#2@{logits_n3[3,2]}];
      [n3#3@{logits_n3[3,3]}];
    %c3#4%
      [n3#1@{logits_n3[4,1]}];
      [n3#2@{logits_n3[4,2]}];
      [n3#3@{logits_n3[4,3]}];
  "),

  OUTPUT = "svalues; tech11; tech14;",

  rdata = merged
)

# Run model
step2_lta_fit <- mplusModeler(
  step2_lta_model,
  modelout = here("tc_lta","lta_enum", "step2_lta.inp"),
  dataout  = here("tc_lta","lta_enum", "step2_lta.dat"),
  check = TRUE,
  run = TRUE,
  hashfilename = FALSE
)

```

------------------------------------------------------------------------

## Run Step 3 LTA with Covariates

In Step 3, we estimate the full LTA model with covariates using the merged dataset from Step 2. This step regresses latent class membership and class transitions on external variables (e.g., gender, race/ethnicity, or prior achievement) to examine how background characteristics predict profile membership and change over time. Because classification error was accounted for in Step 2, the resulting covariate estimates are unbiased and reflect the true relationships between predictors and latent trajectories.


``` r

# Build Mplus object
step3_lta_model <- mplusObject(
  TITLE = "Step 3 LTA with Fixed Logits and Covariates",

  VARIABLE = "
  USEVAR = n1 n2 n3 Female Minority MathG7 MathG10 MathG12;
  NOMINAL = n1 n2 n3;
  MISSING = all(9999);
  CLASSES = c1(4) c2(4) c3(4);",

  ANALYSIS = "
  TYPE = mixture;
  STARTS = 0;",

  MODEL = glue("
  %OVERALL%
  c1 ON Minority Female (c11f c12f c13f) MathG7;
  c2 ON c1 Minority Female MathG10;
  c3 ON c2 Minority Female MathG12;
  
  MODEL c1:
    %c1#1%
      [n1#1@{logits_n1[1,1]}];
      [n1#2@{logits_n1[1,2]}];
      [n1#3@{logits_n1[1,3]}];
    %c1#2%
      [n1#1@{logits_n1[2,1]}];
      [n1#2@{logits_n1[2,2]}];
      [n1#3@{logits_n1[2,3]}];
    %c1#3%
      [n1#1@{logits_n1[3,1]}];
      [n1#2@{logits_n1[3,2]}];
      [n1#3@{logits_n1[3,3]}];
    %c1#4%
      [n1#1@{logits_n1[4,1]}];
      [n1#2@{logits_n1[4,2]}];
      [n1#3@{logits_n1[4,3]}];

  MODEL c2:
    %c2#1%
      [n2#1@{logits_n2[1,1]}];
      [n2#2@{logits_n2[1,2]}];
      [n2#3@{logits_n2[1,3]}];
    %c2#2%
      [n2#1@{logits_n2[2,1]}];
      [n2#2@{logits_n2[2,2]}];
      [n2#3@{logits_n2[2,3]}];
    %c2#3%
      [n2#1@{logits_n2[3,1]}];
      [n2#2@{logits_n2[3,2]}];
      [n2#3@{logits_n2[3,3]}];
    %c2#4%
      [n2#1@{logits_n2[4,1]}];
      [n2#2@{logits_n2[4,2]}];
      [n2#3@{logits_n2[4,3]}];

  MODEL c3:
    %c3#1%
      [n3#1@{logits_n3[1,1]}];
      [n3#2@{logits_n3[1,2]}];
      [n3#3@{logits_n3[1,3]}];
    %c3#2%
      [n3#1@{logits_n3[2,1]}];
      [n3#2@{logits_n3[2,2]}];
      [n3#3@{logits_n3[2,3]}];
    %c3#3%
      [n3#1@{logits_n3[3,1]}];
      [n3#2@{logits_n3[3,2]}];
      [n3#3@{logits_n3[3,3]}];
    %c3#4%
      [n3#1@{logits_n3[4,1]}];
      [n3#2@{logits_n3[4,2]}];
      [n3#3@{logits_n3[4,3]}];
  "),

  OUTPUT = "svalues; tech11; tech14;",

  rdata = merged
)

# Run model
step3_lta_fit <- mplusModeler(
  step3_lta_model,
  modelout = here("tc_lta","lta_enum", "step3_lta.inp"),
  dataout  = here("tc_lta","lta_enum", "step3_lta.dat"),
  check = TRUE,
  run = TRUE,
  hashfilename = FALSE
)

```

------------------------------------------------------------------------

## Create Transition Probability Plots

To explore how students' attitudinal profiles evolve over time, we generated transition probability plots using Sankey diagrams. We began with the full sample, then created subgroup-specific plots to highlight potential differences by gender and racial/ethnic identity. Specifically, we examined transitions for female students, male students, female students from minoritized backgrounds, and female students who are not part of a racial/ethnic minority. These visualizations help illustrate which profiles are most stable and where notable shifts occur across grades.

------------------------------------------------------------------------

Re order classes


``` r

merged <- merged %>%
  mutate(
    # Recode N1 (Grade 7)
    N1_label = case_when(
      N1 == 1 ~ "Very Positive",
      N1 == 4 ~ "Qualified Positive",
      N1 == 3 ~ "Neutral",
      N1 == 2 ~ "Less Positive"
    ),
    # Recode N2 (Grade 10)
    N2_label = case_when(
      N2 == 1 ~ "Very Positive",
      N2 == 4 ~ "Qualified Positive",
      N2 == 3 ~ "Neutral",
      N2 == 2 ~ "Less Positive"
    ),
    # Recode N3 (Grade 12)
    N3_label = case_when(
      N3 == 1 ~ "Very Positive",
      N3 == 4 ~ "Qualified Positive",
      N3 == 3 ~ "Neutral",
      N3 == 2 ~ "Less Positive"
    ),
    
    # Factorize with correct top-down plotting order
    N1_label = factor(N1_label, levels = c(
      "Very Positive", "Qualified Positive", "Neutral", "Less Positive"
    )),
    N2_label = factor(N2_label, levels = c(
      "Very Positive", "Qualified Positive", "Neutral", "Less Positive"
    )),
    N3_label = factor(N3_label, levels = c(
      "Very Positive", "Qualified Positive", "Neutral", "Less Positive"
    ))
  )

```

### Examine Transition Probability Plot for the Full Sample


``` r
#### 1. Reload transition function and read model ####
source(here("tc_lta", "functions", "plot_transitions_function.R"))

step3_full <- readModels(here::here("tc_lta","lta_enum", "step3_lta.out"))

#### 2. Define color palette (new correct labels) ####
class_colors <- c(
  "Very Positive"       = "#BA68C8",
  "Qualified Positive"  = "#00BCD4",
  "Neutral"             = "#8BC34A",
  "Less Positive"       = "#E74C3C"
)

#### 3. Set correct class names per quadrant (TL → TR → BL → BR) ####
plot_full <- plot_transitions_function(
  model_name       = step3_full,
  color_palette    = class_colors,
  facet_labels     = c(
    `1` = "Very Positive",
    `2` = "Qualified Positive",
    `3` = "Neutral",
    `4` = "Less Positive"
  ),
  timepoint_labels = c(
    `1` = "Grade 7",
    `2` = "Grade 10",
    `3` = "Grade 12"
  ),
  class_labels     = names(class_colors)
)

#### 4. Add single plot title and theme settings ####
plot_full <- plot_full +
  ggtitle("Attitudinal Profile Transitions: Grade 7 to 10 and Grade 10 to 12") +
  theme(
    strip.text = element_text(
      family = "Avenir Next",
      size = 11,
      face = "plain"
    ),
    plot.title = element_text(
      family = "Avenir Next",
      hjust = 0.5,
      size = 14,
      margin = margin(b = 10)
    ),
    text = element_text(family = "Avenir Next")
  )

#### 5. Render plot ####
plot_full
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-40-1.png" width="672" />


``` r
ggsave(
  filename = here("tc_lta","figures", "transitions_full_sample.png"),
  plot     = plot_full,
  width    = 8,
  height   = 8,
  dpi      = 300
)

```

### Examine Transition Probability Plot for Females


``` r
source(here("tc_lta", "functions", "make_transition_object.R"))

merged_female <- merged %>%
  filter(FEMALE == 1)

#### 2. Build Mplus-style transition object from modal assignments ####
female_obj <- make_transition_object(merged_female, "N1", "N2", "N3")

#### 3. Create plot with updated facet labels and font ####
plot_female <- plot_transitions_function(
  model_name       = female_obj,
  color_palette    = class_colors,
  facet_labels     = c(
    `1` = "Very Positive",
    `2` = "Qualified Positive",
    `3` = "Neutral",
    `4` = "Less Positive"
  ),
  timepoint_labels = c(
    `1` = "Grade 7",
    `2` = "Grade 10",
    `3` = "Grade 12"
  ),
  class_labels     = names(class_colors)
) +
  ggtitle("Attitudinal Profile Transitions\nFemale Students: Grade 7 to 10 and Grade 10 to 12") +
  theme(
    strip.text = element_text(
      family = "Avenir Next",
      size = 11,
      face = "plain"
    ),
    plot.title = element_text(
      family = "Avenir Next",
      hjust = 0.5,
      size = 14,
      margin = margin(b = 10)
    ),
    text = element_text(family = "Avenir Next")
  )

#### 4. Display plot ####
plot_female
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-42-1.png" width="672" />

Save Female plots


``` r
ggsave(
  filename = here("tc_lta","figures", "transitions_female.png"),
  plot     = plot_female,
  width    = 8,
  height   = 8,
  dpi      = 300
)

```

### Examine Transition Probability Plot for Males


``` r
merged_male <- merged %>%
  filter(FEMALE == 0)

#### 2. Build Mplus-style transition object from modal assignments ####
male_obj <- make_transition_object(merged_male, "N1", "N2", "N3")

#### 3. Create plot with updated facet labels and font ####
plot_male <- plot_transitions_function(
  model_name       = male_obj,
  color_palette    = class_colors,
  facet_labels     = c(
    `1` = "Very Positive",
    `2` = "Qualified Positive",
    `3` = "Neutral",
    `4` = "Less Positive"
  ),
  timepoint_labels = c(
    `1` = "Grade 7",
    `2` = "Grade 10",
    `3` = "Grade 12"
  ),
  class_labels     = names(class_colors)
) +
  ggtitle("Attitudinal Profile Transitions\nMale Students: Grade 7 to 10 and Grade 10 to 12") +
  theme(
    strip.text = element_text(
      family = "Avenir Next",
      size = 11,
      face = "plain"
    ),
    plot.title = element_text(
      family = "Avenir Next",
      hjust = 0.5,
      size = 14,
      margin = margin(b = 10)
    ),
    text = element_text(family = "Avenir Next")
  )

#### 4. Display plot ####
plot_male
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-44-1.png" width="672" />

Save plots for males


``` r

ggsave(
  filename = here("tc_lta","figures", "transitions_male.png"),
  plot     = plot_male,
  width    = 8,
  height   = 8,
  dpi      = 300
)

```

### Examine Transition Probability Plot for Underrepresented Females


``` r
merged_female_minority <- merged %>%
  filter(FEMALE == 1, MINORITY == 1)

#### 2. Build Mplus-style transition object from modal assignments ####
female_minority_obj <- make_transition_object(merged_female_minority, "N1", "N2", "N3")

#### 3. Assign plot to object with correct facet labels and custom font ####
plot_female_minority <- plot_transitions_function(
  model_name       = female_minority_obj,
  color_palette    = class_colors,
  facet_labels     = c(
    `1` = "Very Positive",
    `2` = "Qualified Positive",
    `3` = "Neutral",
    `4` = "Less Positive"
  ),
  timepoint_labels = c(
    `1` = "Grade 7",
    `2` = "Grade 10",
    `3` = "Grade 12"
  ),
  class_labels     = names(class_colors)
) +
  ggtitle("Attitudinal Profile Transitions\nFemale Minority Students: Grade 7 to 10 and Grade 10 to 12") +
  theme(
    strip.text = element_text(
      family = "Avenir Next",
      size = 11,
      face = "plain"
    ),
    plot.title = element_text(
      family = "Avenir Next",
      hjust = 0.5,
      size = 14,
      margin = margin(b = 10)
    ),
    text = element_text(family = "Avenir Next")
  )

#### 4. Display plot ####
plot_female_minority
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-46-1.png" width="672" />

Save plot for minority females


``` r

ggsave(
  filename = here("tc_lta","figures", "transitions_female_minority.png"),
  plot     = plot_female_minority,
  width    = 8,
  height   = 8,
  dpi      = 300
)

```

### Examine Transition Probability Plot for Non-Underrepresented Females


``` r
merged_female_nonminority <- merged %>%
  filter(FEMALE == 1, MINORITY == 0)

#### 2. Build Mplus-style transition object from modal assignments ####
female_nonminority_obj <- make_transition_object(merged_female_nonminority, "N1", "N2", "N3")

#### 3. Assign plot to object with labeled facets and Avenir Next font ####
plot_female_nonminority <- plot_transitions_function(
  model_name       = female_nonminority_obj,
  color_palette    = class_colors,
  facet_labels     = c(
    `1` = "Very Positive",
    `2` = "Qualified Positive",
    `3` = "Neutral",
    `4` = "Less Positive"
  ),
  timepoint_labels = c(
    `1` = "Grade 7",
    `2` = "Grade 10",
    `3` = "Grade 12"
  ),
  class_labels     = names(class_colors)
) +
  ggtitle("Attitudinal Profile Transitions\nNon-Minority Female Students: Grade 7 to 10 and Grade 10 to 12") +
  theme(
    strip.text = element_text(
      family = "Avenir Next",
      size = 11,
      face = "plain"
    ),
    plot.title = element_text(
      family = "Avenir Next",
      hjust = 0.5,
      size = 14,
      margin = margin(b = 10)
    ),
    text = element_text(family = "Avenir Next")
  )

#### 4. Display plot ####
plot_female_nonminority
```

<img src="28-tc-lta_files/figure-html/unnamed-chunk-48-1.png" width="672" />

Save plots for non underrepresented females


``` r
ggsave(
  filename = here("tc_lta","figures", "transitions_female_nonminority.png"),
  plot     = plot_female_nonminority,
  width    = 8,
  height   = 8,
  dpi      = 300
)

```

------------------------------------------------------------------------

### Examine table of covariate results for the Step 3 LTA

We next examine the covariate results from the Step 3 LTA, which model how student characteristics predict both profile membership and transitions between profiles over time. The table reports logit estimates, standard errors, and odds ratios with confidence intervals for each covariate effect. These results allow us to identify which background factors are significantly associated with students’ starting profile in Grade 7, and how those factors relate to their likelihood of transitioning to different profiles across timepoints.


``` r
# Define class labels
class_names <- c(
  "1" = "Low Belonging",
  "2" = "Moderate Belonging",
  "3" = "High Support",
  "4" = "Engaged & Connected"
)

# Build covariate comparison data
covariate_data <- step3_full$parameters$unstandardized %>%
  filter(
    str_detect(paramHeader, "^C[1-3]#\\d+\\.ON$"),
    param %in% c("MINORITY", "FEMALE", "MATHG7", "MATHG10", "MATHG12")
  ) %>%
  mutate(
    LatentClass = str_extract(paramHeader, "C[1-3]#\\d+"),
    ClassNum = str_extract(LatentClass, "#\\d+") %>% str_remove("#"),
    ClassName = class_names[ClassNum],
    RefName = class_names["4"],
    Comparison = glue("{ClassName} vs. {RefName}"),
    Timepoint = case_when(
      str_detect(LatentClass, "C1") ~ "Grade 7",
      str_detect(LatentClass, "C2") ~ "Grade 10",
      str_detect(LatentClass, "C3") ~ "Grade 12"
    ),
    Timepoint = factor(Timepoint, levels = c("Grade 7", "Grade 10", "Grade 12")),
    OR = exp(est),
    OR_lower = exp(est - 1.96 * se),
    OR_upper = exp(est + 1.96 * se),
    OR_CI = sprintf("%.2f [%.2f, %.2f]", OR, OR_lower, OR_upper),
    Logit = sprintf("%.2f", est),
    SE = sprintf("%.2f", se),
    pval_fmt = ifelse(pval < .001, "<.001", sprintf("%.3f", pval)),
    pval_num = ifelse(pval < .001, 0.0009, pval)  # for styling logic
  ) %>%
  arrange(Timepoint, ClassNum) %>%
  mutate(RowGroup = glue("{Timepoint}: {Comparison}")) %>%
  select(RowGroup, Covariate = param, Logit, SE, `Odds Ratio [95% CI]` = OR_CI, `p-value` = pval_fmt, pval_num)

# Build gt table
covariate_table <- covariate_data %>%
  select(-pval_num) %>%
  gt(groupname_col = "RowGroup") %>%
  tab_header(
    title = "Step 3 LTA Covariate Effects",
    subtitle = "Log Odds of Class Membership Relative to Engaged & Connected"
  ) %>%
  cols_label(
    Covariate = "Covariate",
    Logit = "Logit",
    SE = "SE",
    `Odds Ratio [95% CI]` = "Odds Ratio [95% CI]",
    `p-value` = "p-value"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", style = "italic")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(`p-value`),
      rows = covariate_data$pval_num < 0.05
    )
  )


covariate_table
```


```{=html}
<div id="hevkvtxmbz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hevkvtxmbz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hevkvtxmbz thead, #hevkvtxmbz tbody, #hevkvtxmbz tfoot, #hevkvtxmbz tr, #hevkvtxmbz td, #hevkvtxmbz th {
  border-style: none;
}

#hevkvtxmbz p {
  margin: 0;
  padding: 0;
}

#hevkvtxmbz .gt_table {
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

#hevkvtxmbz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hevkvtxmbz .gt_title {
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

#hevkvtxmbz .gt_subtitle {
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

#hevkvtxmbz .gt_heading {
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

#hevkvtxmbz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hevkvtxmbz .gt_col_headings {
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

#hevkvtxmbz .gt_col_heading {
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

#hevkvtxmbz .gt_column_spanner_outer {
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

#hevkvtxmbz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hevkvtxmbz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hevkvtxmbz .gt_column_spanner {
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

#hevkvtxmbz .gt_spanner_row {
  border-bottom-style: hidden;
}

#hevkvtxmbz .gt_group_heading {
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

#hevkvtxmbz .gt_empty_group_heading {
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

#hevkvtxmbz .gt_from_md > :first-child {
  margin-top: 0;
}

#hevkvtxmbz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hevkvtxmbz .gt_row {
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

#hevkvtxmbz .gt_stub {
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

#hevkvtxmbz .gt_stub_row_group {
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

#hevkvtxmbz .gt_row_group_first td {
  border-top-width: 2px;
}

#hevkvtxmbz .gt_row_group_first th {
  border-top-width: 2px;
}

#hevkvtxmbz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hevkvtxmbz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hevkvtxmbz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hevkvtxmbz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hevkvtxmbz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hevkvtxmbz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hevkvtxmbz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hevkvtxmbz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hevkvtxmbz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hevkvtxmbz .gt_footnotes {
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

#hevkvtxmbz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hevkvtxmbz .gt_sourcenotes {
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

#hevkvtxmbz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hevkvtxmbz .gt_left {
  text-align: left;
}

#hevkvtxmbz .gt_center {
  text-align: center;
}

#hevkvtxmbz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hevkvtxmbz .gt_font_normal {
  font-weight: normal;
}

#hevkvtxmbz .gt_font_bold {
  font-weight: bold;
}

#hevkvtxmbz .gt_font_italic {
  font-style: italic;
}

#hevkvtxmbz .gt_super {
  font-size: 65%;
}

#hevkvtxmbz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hevkvtxmbz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hevkvtxmbz .gt_indent_1 {
  text-indent: 5px;
}

#hevkvtxmbz .gt_indent_2 {
  text-indent: 10px;
}

#hevkvtxmbz .gt_indent_3 {
  text-indent: 15px;
}

#hevkvtxmbz .gt_indent_4 {
  text-indent: 20px;
}

#hevkvtxmbz .gt_indent_5 {
  text-indent: 25px;
}

#hevkvtxmbz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hevkvtxmbz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal" style>Step 3 LTA Covariate Effects</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Log Odds of Class Membership Relative to Engaged &amp; Connected</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Covariate">Covariate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Logit">Logit</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SE">SE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Odds-Ratio-[95%-CI]">Odds Ratio [95% CI]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 7: Low Belonging vs. Engaged &amp;amp; Connected">Grade 7: Low Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 7: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.85</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.33</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">2.35 [1.22, 4.52]</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">0.011</td></tr>
    <tr><td headers="Grade 7: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.94</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.22</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.39 [0.26, 0.60]</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">&lt;.001</td></tr>
    <tr><td headers="Grade 7: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG7</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.04</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.04 [1.02, 1.07]</td>
<td headers="Grade 7: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">&lt;.001</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 7: Moderate Belonging vs. Engaged &amp;amp; Connected">Grade 7: Moderate Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.71</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.53</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">2.03 [0.72, 5.71]</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.182</td></tr>
    <tr><td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.12</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.40</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.13 [0.51, 2.48]</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.769</td></tr>
    <tr><td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG7</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.00</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.02</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.00 [0.96, 1.04]</td>
<td headers="Grade 7: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.939</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 7: High Support vs. Engaged &amp;amp; Connected">Grade 7: High Support vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 7: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">0.64</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.48</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.90 [0.74, 4.86]</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.182</td></tr>
    <tr><td headers="Grade 7: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.85</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.34</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.43 [0.22, 0.84]</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">0.013</td></tr>
    <tr><td headers="Grade 7: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG7</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.03</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.02</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.97 [0.93, 1.01]</td>
<td headers="Grade 7: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.108</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 10: Low Belonging vs. Engaged &amp;amp; Connected">Grade 10: Low Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 10: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.15</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.37</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.86 [0.42, 1.76]</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.675</td></tr>
    <tr><td headers="Grade 10: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.01</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.26</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.99 [0.59, 1.66]</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.962</td></tr>
    <tr><td headers="Grade 10: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG10</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.02</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.02 [1.00, 1.04]</td>
<td headers="Grade 10: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.094</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 10: Moderate Belonging vs. Engaged &amp;amp; Connected">Grade 10: Moderate Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.65</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.40</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.52 [0.24, 1.15]</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.105</td></tr>
    <tr><td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.09</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.32</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.10 [0.58, 2.07]</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.772</td></tr>
    <tr><td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG10</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.05</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.95 [0.93, 0.98]</td>
<td headers="Grade 10: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">&lt;.001</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 10: High Support vs. Engaged &amp;amp; Connected">Grade 10: High Support vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 10: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.40</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.50</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.67 [0.25, 1.78]</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.421</td></tr>
    <tr><td headers="Grade 10: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.10</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.35</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.90 [0.46, 1.79]</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.770</td></tr>
    <tr><td headers="Grade 10: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG10</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">0.00</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.00 [0.97, 1.03]</td>
<td headers="Grade 10: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.995</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 12: Low Belonging vs. Engaged &amp;amp; Connected">Grade 12: Low Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 12: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.37</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.41</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.69 [0.31, 1.54]</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.368</td></tr>
    <tr><td headers="Grade 12: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.30</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.31</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.74 [0.41, 1.37]</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.339</td></tr>
    <tr><td headers="Grade 12: Low Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG12</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">0.04</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">1.04 [1.01, 1.06]</td>
<td headers="Grade 12: Low Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left" style="font-weight: bold;">0.003</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 12: Moderate Belonging vs. Engaged &amp;amp; Connected">Grade 12: Moderate Belonging vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.82</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.47</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.44 [0.18, 1.10]</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.079</td></tr>
    <tr><td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.34</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.33</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.71 [0.37, 1.37]</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.308</td></tr>
    <tr><td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG12</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.02</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.98 [0.95, 1.00]</td>
<td headers="Grade 12: Moderate Belonging vs. Engaged & Connected  p-value" class="gt_row gt_left">0.052</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="5" class="gt_group_heading" style="font-style: italic; font-weight: bold;" scope="colgroup" id="Grade 12: High Support vs. Engaged &amp;amp; Connected">Grade 12: High Support vs. Engaged &amp; Connected</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="Grade 12: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MINORITY</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.14</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.48</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.87 [0.34, 2.23]</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.776</td></tr>
    <tr><td headers="Grade 12: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">FEMALE</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.22</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.40</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.80 [0.37, 1.74]</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.578</td></tr>
    <tr><td headers="Grade 12: High Support vs. Engaged & Connected  Covariate" class="gt_row gt_left">MATHG12</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Logit" class="gt_row gt_right">-0.01</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  SE" class="gt_row gt_right">0.01</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  Odds Ratio [95% CI]" class="gt_row gt_left">0.99 [0.97, 1.02]</td>
<td headers="Grade 12: High Support vs. Engaged & Connected  p-value" class="gt_row gt_left">0.712</td></tr>
  </tbody>
  
  
</table>
</div>
```


Save covariate table


``` r
gtsave(
  data = covariate_table,
  filename = here("tc_lta","figures", "step3_covariate_table.png")
)
```

------------------------------------------------------------------------

## Examine Outcome Variables

In a modern three-step LTA workflow, distal outcomes such as achievement are typically incorporated directly into the third step using model constraints. However, in line with the original paper, we replicate their approach by examining achievement outcomes post hoc using ANOVAs. Specifically, we test whether 12th-grade mathematics and science achievement differ by students’ final attitudinal profile. This allows us to assess the external validity of the profiles by linking them to meaningful academic indicators.


``` r
# 1. Prepare variables
merged <- merged %>%
  mutate(
    traj_final = factor(N3, levels = 1:4),
    stable_profile = (N1 == N2 & N2 == N3)
  )

# 2. Group means and SDs, exclude 9999 and NA in traj_final
distal_summary <- merged %>%
  filter(
    MATHG11 != 9999,
    SCIG11 != 9999,
    !is.na(traj_final)
  ) %>%
  group_by(traj_final) %>%
  summarise(
    n = n(),
    math_mean = mean(MATHG11),
    math_sd   = sd(MATHG11),
    sci_mean  = mean(SCIG11),
    sci_sd    = sd(SCIG11),
    .groups = "drop"
  )

# 3. Run ANOVAs (excluding 9999)
aov_math <- aov(MATHG11 ~ traj_final, data = merged %>% filter(MATHG11 != 9999))
aov_sci  <- aov(SCIG11  ~ traj_final, data = merged %>% filter(SCIG11 != 9999))

# 4. Extract test stats
f_math <- summary(aov_math)[[1]]$`F value`[1]
p_math <- summary(aov_math)[[1]]$`Pr(>F)`[1]
df_math1 <- summary(aov_math)[[1]]$Df[1]
df_math2 <- summary(aov_math)[[1]]$Df[2]

f_sci <- summary(aov_sci)[[1]]$`F value`[1]
p_sci <- summary(aov_sci)[[1]]$`Pr(>F)`[1]
df_sci1 <- summary(aov_sci)[[1]]$Df[1]
df_sci2 <- summary(aov_sci)[[1]]$Df[2]

# 5. Format table
gt(distal_summary) %>%
  tab_header(title = "12th Grade Achievement by Final Attitudinal Profile") %>%
  fmt_number(columns = ends_with("mean"), decimals = 2) %>%
  fmt_number(columns = ends_with("sd"), decimals = 2) %>%
  fmt_number(columns = "n", decimals = 0) %>%
  cols_label(
    traj_final = "Final Profile",
    n = "N",
    math_mean = "Math M",
    math_sd   = "Math SD",
    sci_mean  = "Science M",
    sci_sd    = "Science SD"
  ) %>%
  tab_source_note(
    source_note = glue::glue(
      "ANOVA — Math: F({df_math1}, {df_math2}) = {round(f_math, 2)}, p = {format.pval(p_math, digits = 3)}; ",
      "Science: F({df_sci1}, {df_sci2}) = {round(f_sci, 2)}, p = {format.pval(p_sci, digits = 3)}"
    )
  )
```


```{=html}
<div id="lcvpmbeipc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lcvpmbeipc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lcvpmbeipc thead, #lcvpmbeipc tbody, #lcvpmbeipc tfoot, #lcvpmbeipc tr, #lcvpmbeipc td, #lcvpmbeipc th {
  border-style: none;
}

#lcvpmbeipc p {
  margin: 0;
  padding: 0;
}

#lcvpmbeipc .gt_table {
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

#lcvpmbeipc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lcvpmbeipc .gt_title {
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

#lcvpmbeipc .gt_subtitle {
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

#lcvpmbeipc .gt_heading {
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

#lcvpmbeipc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lcvpmbeipc .gt_col_headings {
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

#lcvpmbeipc .gt_col_heading {
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

#lcvpmbeipc .gt_column_spanner_outer {
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

#lcvpmbeipc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lcvpmbeipc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lcvpmbeipc .gt_column_spanner {
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

#lcvpmbeipc .gt_spanner_row {
  border-bottom-style: hidden;
}

#lcvpmbeipc .gt_group_heading {
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

#lcvpmbeipc .gt_empty_group_heading {
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

#lcvpmbeipc .gt_from_md > :first-child {
  margin-top: 0;
}

#lcvpmbeipc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lcvpmbeipc .gt_row {
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

#lcvpmbeipc .gt_stub {
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

#lcvpmbeipc .gt_stub_row_group {
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

#lcvpmbeipc .gt_row_group_first td {
  border-top-width: 2px;
}

#lcvpmbeipc .gt_row_group_first th {
  border-top-width: 2px;
}

#lcvpmbeipc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lcvpmbeipc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lcvpmbeipc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lcvpmbeipc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lcvpmbeipc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lcvpmbeipc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lcvpmbeipc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lcvpmbeipc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lcvpmbeipc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lcvpmbeipc .gt_footnotes {
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

#lcvpmbeipc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lcvpmbeipc .gt_sourcenotes {
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

#lcvpmbeipc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lcvpmbeipc .gt_left {
  text-align: left;
}

#lcvpmbeipc .gt_center {
  text-align: center;
}

#lcvpmbeipc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lcvpmbeipc .gt_font_normal {
  font-weight: normal;
}

#lcvpmbeipc .gt_font_bold {
  font-weight: bold;
}

#lcvpmbeipc .gt_font_italic {
  font-style: italic;
}

#lcvpmbeipc .gt_super {
  font-size: 65%;
}

#lcvpmbeipc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lcvpmbeipc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lcvpmbeipc .gt_indent_1 {
  text-indent: 5px;
}

#lcvpmbeipc .gt_indent_2 {
  text-indent: 10px;
}

#lcvpmbeipc .gt_indent_3 {
  text-indent: 15px;
}

#lcvpmbeipc .gt_indent_4 {
  text-indent: 20px;
}

#lcvpmbeipc .gt_indent_5 {
  text-indent: 25px;
}

#lcvpmbeipc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lcvpmbeipc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>12th Grade Achievement by Final Attitudinal Profile</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="traj_final">Final Profile</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="math_mean">Math M</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="math_sd">Math SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sci_mean">Science M</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sci_sd">Science SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="traj_final" class="gt_row gt_center">1</td>
<td headers="n" class="gt_row gt_right">297</td>
<td headers="math_mean" class="gt_row gt_right">75.71</td>
<td headers="math_sd" class="gt_row gt_right">11.39</td>
<td headers="sci_mean" class="gt_row gt_right">71.20</td>
<td headers="sci_sd" class="gt_row gt_right">10.53</td></tr>
    <tr><td headers="traj_final" class="gt_row gt_center">2</td>
<td headers="n" class="gt_row gt_right">148</td>
<td headers="math_mean" class="gt_row gt_right">67.37</td>
<td headers="math_sd" class="gt_row gt_right">12.33</td>
<td headers="sci_mean" class="gt_row gt_right">64.19</td>
<td headers="sci_sd" class="gt_row gt_right">9.81</td></tr>
    <tr><td headers="traj_final" class="gt_row gt_center">3</td>
<td headers="n" class="gt_row gt_right">151</td>
<td headers="math_mean" class="gt_row gt_right">69.32</td>
<td headers="math_sd" class="gt_row gt_right">13.54</td>
<td headers="sci_mean" class="gt_row gt_right">66.26</td>
<td headers="sci_sd" class="gt_row gt_right">12.14</td></tr>
    <tr><td headers="traj_final" class="gt_row gt_center">4</td>
<td headers="n" class="gt_row gt_right">161</td>
<td headers="math_mean" class="gt_row gt_right">71.15</td>
<td headers="math_sd" class="gt_row gt_right">10.42</td>
<td headers="sci_mean" class="gt_row gt_right">66.20</td>
<td headers="sci_sd" class="gt_row gt_right">9.55</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="6">ANOVA — Math: F(3, 811) = 22.53, p = 5.12e-14; Science: F(3, 815) = 19.82, p = 2.08e-12</td>
    </tr>
  </tfoot>
  
</table>
</div>
```



``` r
# 1. Class label mapping
profile_labels <- c(
  "1" = "Supportive",
  "2" = "Disconnected",
  "3" = "Moderate",
  "4" = "Negative"
)

# 2. Clean and label profiles
df <- merged %>%
  filter(MATHG11 != 9999, SCIG11 != 9999) %>%
  mutate(profile = factor(N3, levels = 1:4))

# 3. ANOVA + Tukey — Math
aov_math <- aov(MATHG11 ~ profile, data = df)
tukey_math <- TukeyHSD(aov_math)$profile
tukey_math_df <- as.data.frame(tukey_math) %>%
  tibble::rownames_to_column("Comparison") %>%
  mutate(
    Outcome = "Math",
    Group1 = str_match(Comparison, "^(\\d+)-(\\d+)$")[,2],
    Group2 = str_match(Comparison, "^(\\d+)-(\\d+)$")[,3],
    Comparison = glue("{profile_labels[Group2]} vs. {profile_labels[Group1]}")
  ) %>%
  select(Outcome, Comparison, diff, lwr, upr, `p adj`) %>%
  rename(
    `Mean Difference` = diff,
    `Lower CI`        = lwr,
    `Upper CI`        = upr,
    `Adjusted p`      = `p adj`
  )

# 4. ANOVA + Tukey — Science
aov_sci <- aov(SCIG11 ~ profile, data = df)
tukey_sci <- TukeyHSD(aov_sci)$profile
tukey_sci_df <- as.data.frame(tukey_sci) %>%
  tibble::rownames_to_column("Comparison") %>%
  mutate(
    Outcome = "Science",
    Group1 = str_match(Comparison, "^(\\d+)-(\\d+)$")[,2],
    Group2 = str_match(Comparison, "^(\\d+)-(\\d+)$")[,3],
    Comparison = glue("{profile_labels[Group2]} vs. {profile_labels[Group1]}")
  ) %>%
  select(Outcome, Comparison, diff, lwr, upr, `p adj`) %>%
  rename(
    `Mean Difference` = diff,
    `Lower CI`        = lwr,
    `Upper CI`        = upr,
    `Adjusted p`      = `p adj`
  )

# 5. F values
math_f <- summary(aov_math)[[1]]
sci_f  <- summary(aov_sci)[[1]]

# 6. Build Math table
gt_math_table <- gt(tukey_math_df) %>%
  tab_header(title = "Pairwise Comparisons for 12th Grade Math Achievement") %>%
  fmt_number(columns = c("Mean Difference", "Lower CI", "Upper CI"), decimals = 2) %>%
  text_transform(
    locations = cells_body(columns = "Adjusted p"),
    fn = function(x) ifelse(as.numeric(x) < 0.001, "<.001", formatC(as.numeric(x), format = "f", digits = 3))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "Adjusted p", rows = as.numeric(`Adjusted p`) < 0.05)
  ) %>%
  tab_source_note(source_note = md(glue(
    "*F*({math_f$Df[1]}, {math_f$Df[2]}) = {round(math_f$`F value`[1], 2)}, *p* = {format.pval(math_f$`Pr(>F)`[1], digits = 3, eps = .001)}"
  )))

# 7. Build Science table
gt_sci_table <- gt(tukey_sci_df) %>%
  tab_header(title = "Pairwise Comparisons for 12th Grade Science Achievement") %>%
  fmt_number(columns = c("Mean Difference", "Lower CI", "Upper CI"), decimals = 2) %>%
  text_transform(
    locations = cells_body(columns = "Adjusted p"),
    fn = function(x) ifelse(as.numeric(x) < 0.001, "<.001", formatC(as.numeric(x), format = "f", digits = 3))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "Adjusted p", rows = as.numeric(`Adjusted p`) < 0.05)
  ) %>%
  tab_source_note(source_note = md(glue(
    "*F*({sci_f$Df[1]}, {sci_f$Df[2]}) = {round(sci_f$`F value`[1], 2)}, *p* = {format.pval(sci_f$`Pr(>F)`[1], digits = 3, eps = .001)}"
  )))

# 8. Render both tables
gt_math_table
```


```{=html}
<div id="napsxjxwfe" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#napsxjxwfe table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#napsxjxwfe thead, #napsxjxwfe tbody, #napsxjxwfe tfoot, #napsxjxwfe tr, #napsxjxwfe td, #napsxjxwfe th {
  border-style: none;
}

#napsxjxwfe p {
  margin: 0;
  padding: 0;
}

#napsxjxwfe .gt_table {
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

#napsxjxwfe .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#napsxjxwfe .gt_title {
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

#napsxjxwfe .gt_subtitle {
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

#napsxjxwfe .gt_heading {
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

#napsxjxwfe .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#napsxjxwfe .gt_col_headings {
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

#napsxjxwfe .gt_col_heading {
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

#napsxjxwfe .gt_column_spanner_outer {
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

#napsxjxwfe .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#napsxjxwfe .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#napsxjxwfe .gt_column_spanner {
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

#napsxjxwfe .gt_spanner_row {
  border-bottom-style: hidden;
}

#napsxjxwfe .gt_group_heading {
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

#napsxjxwfe .gt_empty_group_heading {
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

#napsxjxwfe .gt_from_md > :first-child {
  margin-top: 0;
}

#napsxjxwfe .gt_from_md > :last-child {
  margin-bottom: 0;
}

#napsxjxwfe .gt_row {
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

#napsxjxwfe .gt_stub {
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

#napsxjxwfe .gt_stub_row_group {
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

#napsxjxwfe .gt_row_group_first td {
  border-top-width: 2px;
}

#napsxjxwfe .gt_row_group_first th {
  border-top-width: 2px;
}

#napsxjxwfe .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#napsxjxwfe .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#napsxjxwfe .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#napsxjxwfe .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#napsxjxwfe .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#napsxjxwfe .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#napsxjxwfe .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#napsxjxwfe .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#napsxjxwfe .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#napsxjxwfe .gt_footnotes {
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

#napsxjxwfe .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#napsxjxwfe .gt_sourcenotes {
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

#napsxjxwfe .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#napsxjxwfe .gt_left {
  text-align: left;
}

#napsxjxwfe .gt_center {
  text-align: center;
}

#napsxjxwfe .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#napsxjxwfe .gt_font_normal {
  font-weight: normal;
}

#napsxjxwfe .gt_font_bold {
  font-weight: bold;
}

#napsxjxwfe .gt_font_italic {
  font-style: italic;
}

#napsxjxwfe .gt_super {
  font-size: 65%;
}

#napsxjxwfe .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#napsxjxwfe .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#napsxjxwfe .gt_indent_1 {
  text-indent: 5px;
}

#napsxjxwfe .gt_indent_2 {
  text-indent: 10px;
}

#napsxjxwfe .gt_indent_3 {
  text-indent: 15px;
}

#napsxjxwfe .gt_indent_4 {
  text-indent: 20px;
}

#napsxjxwfe .gt_indent_5 {
  text-indent: 25px;
}

#napsxjxwfe .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#napsxjxwfe div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Pairwise Comparisons for 12th Grade Math Achievement</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Outcome">Outcome</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Comparison">Comparison</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean-Difference">Mean Difference</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Lower-CI">Lower CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Upper-CI">Upper CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Adjusted-p">Adjusted p</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Disconnected</td>
<td headers="Mean Difference" class="gt_row gt_right">−8.34</td>
<td headers="Lower CI" class="gt_row gt_right">−11.41</td>
<td headers="Upper CI" class="gt_row gt_right">−5.27</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Moderate</td>
<td headers="Mean Difference" class="gt_row gt_right">−6.39</td>
<td headers="Lower CI" class="gt_row gt_right">−9.44</td>
<td headers="Upper CI" class="gt_row gt_right">−3.34</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">−4.56</td>
<td headers="Lower CI" class="gt_row gt_right">−7.54</td>
<td headers="Upper CI" class="gt_row gt_right">−1.57</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Disconnected vs. Moderate</td>
<td headers="Mean Difference" class="gt_row gt_right">1.95</td>
<td headers="Lower CI" class="gt_row gt_right">−1.57</td>
<td headers="Upper CI" class="gt_row gt_right">5.48</td>
<td headers="Adjusted p" class="gt_row gt_right">0.483</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Disconnected vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">3.78</td>
<td headers="Lower CI" class="gt_row gt_right">0.31</td>
<td headers="Upper CI" class="gt_row gt_right">7.26</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;">0.026</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Math</td>
<td headers="Comparison" class="gt_row gt_center">Moderate vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">1.83</td>
<td headers="Lower CI" class="gt_row gt_right">−1.63</td>
<td headers="Upper CI" class="gt_row gt_right">5.28</td>
<td headers="Adjusted p" class="gt_row gt_right">0.523</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="6"><span class='gt_from_md'><em>F</em>(3, 753) = 20.06, <em>p</em> = &lt;0.001</span></td>
    </tr>
  </tfoot>
  
</table>
</div>
```


``` r
gt_sci_table
```


```{=html}
<div id="daoebullas" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#daoebullas table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#daoebullas thead, #daoebullas tbody, #daoebullas tfoot, #daoebullas tr, #daoebullas td, #daoebullas th {
  border-style: none;
}

#daoebullas p {
  margin: 0;
  padding: 0;
}

#daoebullas .gt_table {
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

#daoebullas .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#daoebullas .gt_title {
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

#daoebullas .gt_subtitle {
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

#daoebullas .gt_heading {
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

#daoebullas .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daoebullas .gt_col_headings {
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

#daoebullas .gt_col_heading {
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

#daoebullas .gt_column_spanner_outer {
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

#daoebullas .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#daoebullas .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#daoebullas .gt_column_spanner {
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

#daoebullas .gt_spanner_row {
  border-bottom-style: hidden;
}

#daoebullas .gt_group_heading {
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

#daoebullas .gt_empty_group_heading {
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

#daoebullas .gt_from_md > :first-child {
  margin-top: 0;
}

#daoebullas .gt_from_md > :last-child {
  margin-bottom: 0;
}

#daoebullas .gt_row {
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

#daoebullas .gt_stub {
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

#daoebullas .gt_stub_row_group {
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

#daoebullas .gt_row_group_first td {
  border-top-width: 2px;
}

#daoebullas .gt_row_group_first th {
  border-top-width: 2px;
}

#daoebullas .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daoebullas .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#daoebullas .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#daoebullas .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daoebullas .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daoebullas .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#daoebullas .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#daoebullas .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#daoebullas .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daoebullas .gt_footnotes {
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

#daoebullas .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#daoebullas .gt_sourcenotes {
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

#daoebullas .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#daoebullas .gt_left {
  text-align: left;
}

#daoebullas .gt_center {
  text-align: center;
}

#daoebullas .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#daoebullas .gt_font_normal {
  font-weight: normal;
}

#daoebullas .gt_font_bold {
  font-weight: bold;
}

#daoebullas .gt_font_italic {
  font-style: italic;
}

#daoebullas .gt_super {
  font-size: 65%;
}

#daoebullas .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#daoebullas .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#daoebullas .gt_indent_1 {
  text-indent: 5px;
}

#daoebullas .gt_indent_2 {
  text-indent: 10px;
}

#daoebullas .gt_indent_3 {
  text-indent: 15px;
}

#daoebullas .gt_indent_4 {
  text-indent: 20px;
}

#daoebullas .gt_indent_5 {
  text-indent: 25px;
}

#daoebullas .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#daoebullas div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Pairwise Comparisons for 12th Grade Science Achievement</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Outcome">Outcome</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Comparison">Comparison</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean-Difference">Mean Difference</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Lower-CI">Lower CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Upper-CI">Upper CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Adjusted-p">Adjusted p</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Disconnected</td>
<td headers="Mean Difference" class="gt_row gt_right">−7.01</td>
<td headers="Lower CI" class="gt_row gt_right">−9.74</td>
<td headers="Upper CI" class="gt_row gt_right">−4.28</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Moderate</td>
<td headers="Mean Difference" class="gt_row gt_right">−4.94</td>
<td headers="Lower CI" class="gt_row gt_right">−7.65</td>
<td headers="Upper CI" class="gt_row gt_right">−2.22</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Supportive vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">−5.00</td>
<td headers="Lower CI" class="gt_row gt_right">−7.65</td>
<td headers="Upper CI" class="gt_row gt_right">−2.34</td>
<td headers="Adjusted p" class="gt_row gt_right" style="font-weight: bold;"><.001</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Disconnected vs. Moderate</td>
<td headers="Mean Difference" class="gt_row gt_right">2.07</td>
<td headers="Lower CI" class="gt_row gt_right">−1.07</td>
<td headers="Upper CI" class="gt_row gt_right">5.21</td>
<td headers="Adjusted p" class="gt_row gt_right">0.324</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Disconnected vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">2.01</td>
<td headers="Lower CI" class="gt_row gt_right">−1.08</td>
<td headers="Upper CI" class="gt_row gt_right">5.10</td>
<td headers="Adjusted p" class="gt_row gt_right">0.337</td></tr>
    <tr><td headers="Outcome" class="gt_row gt_left">Science</td>
<td headers="Comparison" class="gt_row gt_center">Moderate vs. Negative</td>
<td headers="Mean Difference" class="gt_row gt_right">−0.06</td>
<td headers="Lower CI" class="gt_row gt_right">−3.14</td>
<td headers="Upper CI" class="gt_row gt_right">3.01</td>
<td headers="Adjusted p" class="gt_row gt_right">1.000</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="6"><span class='gt_from_md'><em>F</em>(3, 753) = 18.39, <em>p</em> = &lt;0.001</span></td>
    </tr>
  </tfoot>
  
</table>
</div>
```


### Gender Differences in Transition Probabilities

To test for gender differences in attitudinal profile transitions, we computed pairwise z-tests comparing the proportion of female and male students transitioning between each pair of latent classes from Grade 7 to Grade 10. The table below presents cell counts, transition percentages by gender, and two-tailed z-tests for equality of proportions. Statistically significant differences (p \< .05) are bolded to highlight transitions where gender-based patterns emerge


``` r

# Step 1: Drop invalids and label properly
gender_data <- merged %>%
  filter(N1 %in% 1:4, N2 %in% 1:4, FEMALE %in% 0:1) %>%
  mutate(
    N1 = factor(N1, levels = 1:4),
    N2 = factor(N2, levels = 1:4),
    FEMALE = factor(FEMALE, levels = 0:1, labels = c("MALE", "FEMALE"))
  )

# Step 2: Count transitions
gender_counts <- gender_data %>%
  count(N1, N2, FEMALE) %>%
  tidyr::pivot_wider(
    names_from = FEMALE,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(n_FEMALE = FEMALE, n_MALE = MALE) %>%
  mutate(
    p_FEMALE = n_FEMALE / sum(n_FEMALE + n_MALE),
    p_MALE   = n_MALE / sum(n_FEMALE + n_MALE)
  )

# Step 3: z-tests
gender_counts <- gender_counts %>%
  mutate(
    p1 = n_FEMALE / (n_FEMALE + n_MALE),
    p2 = n_MALE / (n_FEMALE + n_MALE),
    n1 = n_FEMALE + n_MALE,
    se = sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n1)),
    z = (p1 - p2) / se,
    p_val_raw = 2 * (1 - pnorm(abs(z)))
  )

# Step 4: Recode class names directly as factors for clean display
gender_counts <- gender_counts %>%
  mutate(
    N1 = fct_recode(N1,
      "Disengaged"     = "1",
      "Moderate"       = "2",
      "Positive"       = "3",
      "Very Positive"  = "4"
    ),
    N2 = fct_recode(N2,
      "Disengaged"     = "1",
      "Moderate"       = "2",
      "Positive"       = "3",
      "Very Positive"  = "4"
    )
  )

# Step 5: Final display formatting
gender_transition_table <- gender_counts %>%
  mutate(
    `Female %` = round(p1 * 100, 1),
    `Male %`   = round(p2 * 100, 1),
    `p-value`  = ifelse(p_val_raw < 0.001, "<.001", sub("^0", "", sprintf("%.3f", p_val_raw)))
  ) %>%
  select(
    `From` = N1, `To` = N2,
    n_FEMALE, n_MALE,
    `Female %`, `Male %`,
    z, `p-value`, p_val_raw
  )

# Step 6: Create gt table with italicized labels and bold significant p-values
gt_gender_ztests <- gender_transition_table %>%
  gt() %>%
  tab_header(title = "Z-Tests for Gender Differences in Transition Probabilities") %>%
  fmt_number(columns = c("z"), decimals = 2) %>%
  cols_label(
    n_FEMALE = md("*n* (Female)"),
    n_MALE   = md("*n* (Male)"),
    z        = md("*z*"),
    `p-value` = md("*p*-value")
  ) %>%
  opt_row_striping() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(`p-value`),
      rows = p_val_raw < 0.05
    )
  ) %>%
  cols_hide(columns = p_val_raw)

# View table
gt_gender_ztests
```


```{=html}
<div id="jbntjojblv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jbntjojblv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jbntjojblv thead, #jbntjojblv tbody, #jbntjojblv tfoot, #jbntjojblv tr, #jbntjojblv td, #jbntjojblv th {
  border-style: none;
}

#jbntjojblv p {
  margin: 0;
  padding: 0;
}

#jbntjojblv .gt_table {
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

#jbntjojblv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jbntjojblv .gt_title {
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

#jbntjojblv .gt_subtitle {
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

#jbntjojblv .gt_heading {
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

#jbntjojblv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jbntjojblv .gt_col_headings {
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

#jbntjojblv .gt_col_heading {
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

#jbntjojblv .gt_column_spanner_outer {
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

#jbntjojblv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jbntjojblv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jbntjojblv .gt_column_spanner {
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

#jbntjojblv .gt_spanner_row {
  border-bottom-style: hidden;
}

#jbntjojblv .gt_group_heading {
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

#jbntjojblv .gt_empty_group_heading {
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

#jbntjojblv .gt_from_md > :first-child {
  margin-top: 0;
}

#jbntjojblv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jbntjojblv .gt_row {
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

#jbntjojblv .gt_stub {
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

#jbntjojblv .gt_stub_row_group {
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

#jbntjojblv .gt_row_group_first td {
  border-top-width: 2px;
}

#jbntjojblv .gt_row_group_first th {
  border-top-width: 2px;
}

#jbntjojblv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jbntjojblv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jbntjojblv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jbntjojblv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jbntjojblv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jbntjojblv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jbntjojblv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jbntjojblv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jbntjojblv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jbntjojblv .gt_footnotes {
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

#jbntjojblv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jbntjojblv .gt_sourcenotes {
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

#jbntjojblv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jbntjojblv .gt_left {
  text-align: left;
}

#jbntjojblv .gt_center {
  text-align: center;
}

#jbntjojblv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jbntjojblv .gt_font_normal {
  font-weight: normal;
}

#jbntjojblv .gt_font_bold {
  font-weight: bold;
}

#jbntjojblv .gt_font_italic {
  font-style: italic;
}

#jbntjojblv .gt_super {
  font-size: 65%;
}

#jbntjojblv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jbntjojblv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jbntjojblv .gt_indent_1 {
  text-indent: 5px;
}

#jbntjojblv .gt_indent_2 {
  text-indent: 10px;
}

#jbntjojblv .gt_indent_3 {
  text-indent: 15px;
}

#jbntjojblv .gt_indent_4 {
  text-indent: 20px;
}

#jbntjojblv .gt_indent_5 {
  text-indent: 25px;
}

#jbntjojblv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jbntjojblv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="8" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Z-Tests for Gender Differences in Transition Probabilities</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="From">From</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="To">To</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n_FEMALE"><span class='gt_from_md'><em>n</em> (Female)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n_MALE"><span class='gt_from_md'><em>n</em> (Male)</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Female-%">Female %</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Male-%">Male %</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="z"><span class='gt_from_md'><em>z</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="p-value"><span class='gt_from_md'><em>p</em>-value</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="From" class="gt_row gt_center">Disengaged</td>
<td headers="To" class="gt_row gt_center">Disengaged</td>
<td headers="n_FEMALE" class="gt_row gt_right">115</td>
<td headers="n_MALE" class="gt_row gt_right">134</td>
<td headers="Female %" class="gt_row gt_right">46.2</td>
<td headers="Male %" class="gt_row gt_right">53.8</td>
<td headers="z" class="gt_row gt_right">−1.71</td>
<td headers="p-value" class="gt_row gt_left">.088</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Disengaged</td>
<td headers="To" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">22</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">39</td>
<td headers="Female %" class="gt_row gt_right gt_striped">36.1</td>
<td headers="Male %" class="gt_row gt_right gt_striped">63.9</td>
<td headers="z" class="gt_row gt_right gt_striped">−3.21</td>
<td headers="p-value" class="gt_row gt_left gt_striped" style="font-weight: bold;">.001</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Disengaged</td>
<td headers="To" class="gt_row gt_center">Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right">40</td>
<td headers="n_MALE" class="gt_row gt_right">62</td>
<td headers="Female %" class="gt_row gt_right">39.2</td>
<td headers="Male %" class="gt_row gt_right">60.8</td>
<td headers="z" class="gt_row gt_right">−3.15</td>
<td headers="p-value" class="gt_row gt_left" style="font-weight: bold;">.002</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Disengaged</td>
<td headers="To" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">45</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">53</td>
<td headers="Female %" class="gt_row gt_right gt_striped">45.9</td>
<td headers="Male %" class="gt_row gt_right gt_striped">54.1</td>
<td headers="z" class="gt_row gt_right gt_striped">−1.15</td>
<td headers="p-value" class="gt_row gt_left gt_striped">.252</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Moderate</td>
<td headers="To" class="gt_row gt_center">Disengaged</td>
<td headers="n_FEMALE" class="gt_row gt_right">17</td>
<td headers="n_MALE" class="gt_row gt_right">17</td>
<td headers="Female %" class="gt_row gt_right">50.0</td>
<td headers="Male %" class="gt_row gt_right">50.0</td>
<td headers="z" class="gt_row gt_right">0.00</td>
<td headers="p-value" class="gt_row gt_left">1.000</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="To" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">46</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">25</td>
<td headers="Female %" class="gt_row gt_right gt_striped">64.8</td>
<td headers="Male %" class="gt_row gt_right gt_striped">35.2</td>
<td headers="z" class="gt_row gt_right gt_striped">3.69</td>
<td headers="p-value" class="gt_row gt_left gt_striped" style="font-weight: bold;">&lt;.001</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Moderate</td>
<td headers="To" class="gt_row gt_center">Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right">18</td>
<td headers="n_MALE" class="gt_row gt_right">16</td>
<td headers="Female %" class="gt_row gt_right">52.9</td>
<td headers="Male %" class="gt_row gt_right">47.1</td>
<td headers="z" class="gt_row gt_right">0.49</td>
<td headers="p-value" class="gt_row gt_left">.627</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="To" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">23</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">13</td>
<td headers="Female %" class="gt_row gt_right gt_striped">63.9</td>
<td headers="Male %" class="gt_row gt_right gt_striped">36.1</td>
<td headers="z" class="gt_row gt_right gt_striped">2.45</td>
<td headers="p-value" class="gt_row gt_left gt_striped" style="font-weight: bold;">.014</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Positive</td>
<td headers="To" class="gt_row gt_center">Disengaged</td>
<td headers="n_FEMALE" class="gt_row gt_right">36</td>
<td headers="n_MALE" class="gt_row gt_right">46</td>
<td headers="Female %" class="gt_row gt_right">43.9</td>
<td headers="Male %" class="gt_row gt_right">56.1</td>
<td headers="z" class="gt_row gt_right">−1.57</td>
<td headers="p-value" class="gt_row gt_left">.116</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Positive</td>
<td headers="To" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">31</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">29</td>
<td headers="Female %" class="gt_row gt_right gt_striped">51.7</td>
<td headers="Male %" class="gt_row gt_right gt_striped">48.3</td>
<td headers="z" class="gt_row gt_right gt_striped">0.37</td>
<td headers="p-value" class="gt_row gt_left gt_striped">.715</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Positive</td>
<td headers="To" class="gt_row gt_center">Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right">41</td>
<td headers="n_MALE" class="gt_row gt_right">32</td>
<td headers="Female %" class="gt_row gt_right">56.2</td>
<td headers="Male %" class="gt_row gt_right">43.8</td>
<td headers="z" class="gt_row gt_right">1.50</td>
<td headers="p-value" class="gt_row gt_left">.133</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Positive</td>
<td headers="To" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">30</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">27</td>
<td headers="Female %" class="gt_row gt_right gt_striped">52.6</td>
<td headers="Male %" class="gt_row gt_right gt_striped">47.4</td>
<td headers="z" class="gt_row gt_right gt_striped">0.56</td>
<td headers="p-value" class="gt_row gt_left gt_striped">.574</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Very Positive</td>
<td headers="To" class="gt_row gt_center">Disengaged</td>
<td headers="n_FEMALE" class="gt_row gt_right">82</td>
<td headers="n_MALE" class="gt_row gt_right">66</td>
<td headers="Female %" class="gt_row gt_right">55.4</td>
<td headers="Male %" class="gt_row gt_right">44.6</td>
<td headers="z" class="gt_row gt_right">1.87</td>
<td headers="p-value" class="gt_row gt_left">.061</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="To" class="gt_row gt_center gt_striped">Moderate</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">64</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">57</td>
<td headers="Female %" class="gt_row gt_right gt_striped">52.9</td>
<td headers="Male %" class="gt_row gt_right gt_striped">47.1</td>
<td headers="z" class="gt_row gt_right gt_striped">0.90</td>
<td headers="p-value" class="gt_row gt_left gt_striped">.367</td></tr>
    <tr><td headers="From" class="gt_row gt_center">Very Positive</td>
<td headers="To" class="gt_row gt_center">Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right">64</td>
<td headers="n_MALE" class="gt_row gt_right">44</td>
<td headers="Female %" class="gt_row gt_right">59.3</td>
<td headers="Male %" class="gt_row gt_right">40.7</td>
<td headers="z" class="gt_row gt_right">2.77</td>
<td headers="p-value" class="gt_row gt_left" style="font-weight: bold;">.006</td></tr>
    <tr><td headers="From" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="To" class="gt_row gt_center gt_striped">Very Positive</td>
<td headers="n_FEMALE" class="gt_row gt_right gt_striped">126</td>
<td headers="n_MALE" class="gt_row gt_right gt_striped">57</td>
<td headers="Female %" class="gt_row gt_right gt_striped">68.9</td>
<td headers="Male %" class="gt_row gt_right gt_striped">31.1</td>
<td headers="z" class="gt_row gt_right gt_striped">7.79</td>
<td headers="p-value" class="gt_row gt_left gt_striped" style="font-weight: bold;">&lt;.001</td></tr>
  </tbody>
  
  
</table>
</div>
```

