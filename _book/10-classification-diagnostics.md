


# Classification Diagnostics

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

## LCA

Prepare Data


``` r
df_bully <- read_csv(here("data", "crdc_lca_data.csv")) %>% 
  clean_names() %>% 
  dplyr::select(report_dis, report_race, report_sex, counselors_fte, psych_fte, law_fte) 
```

------------------------------------------------------------------------

Continuing the LCA example (\@ref(lca-enum)) in this bookdown, use Mplus to calculate k-class confidence intervals (Note: Change the syntax to make your chosen *k*-class model):


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
output_enum <- readModels(here("mplus", "class.out"))

# 95% k-Class and k-class 95% Confidence Intervals
k_ci <- output_enum$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters") %>% 
  unite(CI, c(low2.5,up2.5), sep=", ", remove = TRUE) %>% 
  mutate(CI = paste0("[", CI, "]")) %>% 
  rename(kclass=est) %>% 
  dplyr::select(kclass, CI)

# AvePPk = Average Latent Class Probabilities for Most Likely Latent Class Membership (Row) by Latent Class (Column)
avePPk <- tibble(avePPk = diag(output_enum$class_counts$avgProbs.mostLikely))

# mcaPk = modal class assignment proportion 
mcaPk <- round(output_enum$class_counts$mostLikely,3) %>% 
  mutate(model = paste0("Class ", class)) %>% 
  add_column(avePPk, k_ci) %>% 
  rename(mcaPk = proportion) %>% 
  dplyr::select(model, kclass, CI, mcaPk, avePPk)

# OCCk = odds of correct classification
OCCk <- mcaPk %>% 
  mutate(OCCk = round((avePPk/(1-avePPk))/(kclass/(1-kclass)),3))

# Put everything together
class_data <- data.frame(OCCk)
```

Now, use `{gt}` to make a nicely formatted table


``` r
class_table <- class_data %>% 
  gt() %>%
    tab_header(
    title = "Model Classification Diagnostics for the 3-Class Solution") %>%
    cols_label(
      model = md("*k*-Class"),
      kclass = md("*k*-Class Proportions"),
      CI = "95% CI",
      mcaPk = html("McaP<sub>k</sub>"),
      avePPk = md("AvePP<sub>k</sub>"),
      OCCk = md("OCC<sub>k</sub>")
    ) %>% 
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
<div id="idiraayrwl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#idiraayrwl table {
  font-family: 'Times New Roman';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#idiraayrwl thead, #idiraayrwl tbody, #idiraayrwl tfoot, #idiraayrwl tr, #idiraayrwl td, #idiraayrwl th {
  border-style: none;
}

#idiraayrwl p {
  margin: 0;
  padding: 0;
}

#idiraayrwl .gt_table {
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

#idiraayrwl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#idiraayrwl .gt_title {
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

#idiraayrwl .gt_subtitle {
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

#idiraayrwl .gt_heading {
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

#idiraayrwl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#idiraayrwl .gt_col_headings {
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

#idiraayrwl .gt_col_heading {
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

#idiraayrwl .gt_column_spanner_outer {
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

#idiraayrwl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#idiraayrwl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#idiraayrwl .gt_column_spanner {
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

#idiraayrwl .gt_spanner_row {
  border-bottom-style: hidden;
}

#idiraayrwl .gt_group_heading {
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

#idiraayrwl .gt_empty_group_heading {
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

#idiraayrwl .gt_from_md > :first-child {
  margin-top: 0;
}

#idiraayrwl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#idiraayrwl .gt_row {
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

#idiraayrwl .gt_stub {
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

#idiraayrwl .gt_stub_row_group {
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

#idiraayrwl .gt_row_group_first td {
  border-top-width: 2px;
}

#idiraayrwl .gt_row_group_first th {
  border-top-width: 2px;
}

#idiraayrwl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#idiraayrwl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#idiraayrwl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#idiraayrwl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#idiraayrwl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#idiraayrwl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#idiraayrwl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#idiraayrwl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#idiraayrwl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#idiraayrwl .gt_footnotes {
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

#idiraayrwl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#idiraayrwl .gt_sourcenotes {
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

#idiraayrwl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#idiraayrwl .gt_left {
  text-align: left;
}

#idiraayrwl .gt_center {
  text-align: center;
}

#idiraayrwl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#idiraayrwl .gt_font_normal {
  font-weight: normal;
}

#idiraayrwl .gt_font_bold {
  font-weight: bold;
}

#idiraayrwl .gt_font_italic {
  font-style: italic;
}

#idiraayrwl .gt_super {
  font-size: 65%;
}

#idiraayrwl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#idiraayrwl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#idiraayrwl .gt_indent_1 {
  text-indent: 5px;
}

#idiraayrwl .gt_indent_2 {
  text-indent: 10px;
}

#idiraayrwl .gt_indent_3 {
  text-indent: 15px;
}

#idiraayrwl .gt_indent_4 {
  text-indent: 20px;
}

#idiraayrwl .gt_indent_5 {
  text-indent: 25px;
}

#idiraayrwl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#idiraayrwl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Model Classification Diagnostics for the 3-Class Solution</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="model"><span class='gt_from_md'><em>k</em>-Class</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="kclass"><span class='gt_from_md'><em>k</em>-Class Proportions</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="mcaPk">McaP<sub>k</sub></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="avePPk"><span class='gt_from_md'>AvePP<sub>k</sub></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="OCCk"><span class='gt_from_md'>OCC<sub>k</sub></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="model" class="gt_row gt_center">Class 1</td>
<td headers="kclass" class="gt_row gt_center">0.249</td>
<td headers="CI" class="gt_row gt_center">[0.166, 0.329]</td>
<td headers="mcaPk" class="gt_row gt_center">0.282</td>
<td headers="avePPk" class="gt_row gt_center">0.675</td>
<td headers="OCCk" class="gt_row gt_center">6.264</td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 2</td>
<td headers="kclass" class="gt_row gt_center">0.106</td>
<td headers="CI" class="gt_row gt_center">[0.083, 0.136]</td>
<td headers="mcaPk" class="gt_row gt_center">0.095</td>
<td headers="avePPk" class="gt_row gt_center">0.904</td>
<td headers="OCCk" class="gt_row gt_center">79.420</td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 3</td>
<td headers="kclass" class="gt_row gt_center">0.644</td>
<td headers="CI" class="gt_row gt_center">[0.561, 0.731]</td>
<td headers="mcaPk" class="gt_row gt_center">0.623</td>
<td headers="avePPk" class="gt_row gt_center">0.893</td>
<td headers="OCCk" class="gt_row gt_center">4.614</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"> <i>Note.</i> McaP<sub>k</sub> = Modal class assignment proportion; AvePP<sub>k</sub> = Average posterior class probabilities; OCC<sub>k</sub> = Odds of correct classification; </td>
    </tr>
  </tfoot>
</table>
</div>
```


------------------------------------------------------------------------

## LPA

Prepare Data


``` r
pisa <- pisaUSA15[1:500,] %>%
  dplyr::select(broad_interest, enjoyment, instrumental_mot, self_efficacy)
```

------------------------------------------------------------------------

Continuing the LPA example (\@ref(lpa-enum)) in this bookdown, use Mplus to calculate k-class confidence intervals (Note: Change the syntax to make your chosen *k*-class model):


``` r
classification  <- mplusObject(
  
  TITLE = "LPA - Calculated k-Class 95% CI",
  
  VARIABLE = 
  "usevar =  broad_interest-self_efficacy;
   classes = c1(4);",
  
  ANALYSIS = 
   "estimator = ml; 
    type = mixture;    
    starts = 0; 
    processors = 10;
    optseed = 468036; ! This seed is taken from chosen model output
    bootstrap = 1000;",
  
  MODEL =
    " 
    ! This is copied and pasted from the chosen model input
  %c1#1%
  broad_interest (vbroad_interest);
  enjoyment (venjoyment);
  instrumental_mot (vinstrumental_mot);
  self_efficacy (vself_efficacy);

  broad_interest WITH enjoyment (broad_interestWenjoyment);
  broad_interest WITH instrumental_mot (broad_interestWinstrumental_mot);
  broad_interest WITH self_efficacy (broad_interestWself_efficacy);
  enjoyment WITH instrumental_mot (enjoymentWinstrumental_mot);
  enjoyment WITH self_efficacy (enjoymentWself_efficacy);
  instrumental_mot WITH self_efficacy (instrumental_motWself_efficacy);

  %c1#2%
  broad_interest (vbroad_interest);
  enjoyment (venjoyment);
  instrumental_mot (vinstrumental_mot);
  self_efficacy (vself_efficacy);

  broad_interest WITH enjoyment (broad_interestWenjoyment);
  broad_interest WITH instrumental_mot (broad_interestWinstrumental_mot);
  broad_interest WITH self_efficacy (broad_interestWself_efficacy);
  enjoyment WITH instrumental_mot (enjoymentWinstrumental_mot);
  enjoyment WITH self_efficacy (enjoymentWself_efficacy);
  instrumental_mot WITH self_efficacy (instrumental_motWself_efficacy);

  %c1#3%
  broad_interest (vbroad_interest);
  enjoyment (venjoyment);
  instrumental_mot (vinstrumental_mot);
  self_efficacy (vself_efficacy);

  broad_interest WITH enjoyment (broad_interestWenjoyment);
  broad_interest WITH instrumental_mot (broad_interestWinstrumental_mot);
  broad_interest WITH self_efficacy (broad_interestWself_efficacy);
  enjoyment WITH instrumental_mot (enjoymentWinstrumental_mot);
  enjoyment WITH self_efficacy (enjoymentWself_efficacy);
  instrumental_mot WITH self_efficacy (instrumental_motWself_efficacy);

  %c1#4%
  broad_interest (vbroad_interest);
  enjoyment (venjoyment);
  instrumental_mot (vinstrumental_mot);
  self_efficacy (vself_efficacy);

  broad_interest WITH enjoyment (broad_interestWenjoyment);
  broad_interest WITH instrumental_mot (broad_interestWinstrumental_mot);
  broad_interest WITH self_efficacy (broad_interestWself_efficacy);
  enjoyment WITH instrumental_mot (enjoymentWinstrumental_mot);
  enjoyment WITH self_efficacy (enjoymentWself_efficacy);
  instrumental_mot WITH self_efficacy (instrumental_motWself_efficacy);
  
  
  !CHANGE THIS SECTION TO YOUR CHOSEN k-CLASS MODEL
    
  %OVERALL%
  [C1#1](c1);
  [C1#2](c2);
  [C1#3](c3);

  Model Constraint:
  New(p1 p2 p3 p4);
  
  p1 = exp(c1)/(1+exp(c1)+exp(c2)+exp(c3));
  p2 = exp(c2)/(1+exp(c1)+exp(c2)+exp(c3));
  p3 = exp(c3)/(1+exp(c1)+exp(c2)+exp(c3));  
  p4 = 1/(1+exp(c1)+exp(c2)+exp(c3));",

  
  OUTPUT = "cinterval(bcbootstrap)",
  
  usevariables = colnames(pisa),
  rdata = pisa)

classification_fit <- mplusModeler(classification,
                dataout=here("lpa", "mplus", "class.dat"),
                modelout=here("lpa", "mplus", "class.inp") ,
                check=TRUE, run = TRUE, hashfilename = FALSE)
```

------------------------------------------------------------------------

Create table using `diagnostics_table` function


``` r
source(here("functions", "diagnostics_table.R"))

class_output <- readModels(here("mplus", "class.out"))

diagnostics_table(class_output)
```


```{=html}
<div id="bqgfmqklpq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bqgfmqklpq table {
  font-family: 'Times New Roman';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bqgfmqklpq thead, #bqgfmqklpq tbody, #bqgfmqklpq tfoot, #bqgfmqklpq tr, #bqgfmqklpq td, #bqgfmqklpq th {
  border-style: none;
}

#bqgfmqklpq p {
  margin: 0;
  padding: 0;
}

#bqgfmqklpq .gt_table {
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

#bqgfmqklpq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bqgfmqklpq .gt_title {
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

#bqgfmqklpq .gt_subtitle {
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

#bqgfmqklpq .gt_heading {
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

#bqgfmqklpq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqgfmqklpq .gt_col_headings {
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

#bqgfmqklpq .gt_col_heading {
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

#bqgfmqklpq .gt_column_spanner_outer {
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

#bqgfmqklpq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bqgfmqklpq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bqgfmqklpq .gt_column_spanner {
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

#bqgfmqklpq .gt_spanner_row {
  border-bottom-style: hidden;
}

#bqgfmqklpq .gt_group_heading {
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

#bqgfmqklpq .gt_empty_group_heading {
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

#bqgfmqklpq .gt_from_md > :first-child {
  margin-top: 0;
}

#bqgfmqklpq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bqgfmqklpq .gt_row {
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

#bqgfmqklpq .gt_stub {
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

#bqgfmqklpq .gt_stub_row_group {
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

#bqgfmqklpq .gt_row_group_first td {
  border-top-width: 2px;
}

#bqgfmqklpq .gt_row_group_first th {
  border-top-width: 2px;
}

#bqgfmqklpq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqgfmqklpq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bqgfmqklpq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bqgfmqklpq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqgfmqklpq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqgfmqklpq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bqgfmqklpq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bqgfmqklpq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bqgfmqklpq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bqgfmqklpq .gt_footnotes {
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

#bqgfmqklpq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqgfmqklpq .gt_sourcenotes {
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

#bqgfmqklpq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bqgfmqklpq .gt_left {
  text-align: left;
}

#bqgfmqklpq .gt_center {
  text-align: center;
}

#bqgfmqklpq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bqgfmqklpq .gt_font_normal {
  font-weight: normal;
}

#bqgfmqklpq .gt_font_bold {
  font-weight: bold;
}

#bqgfmqklpq .gt_font_italic {
  font-style: italic;
}

#bqgfmqklpq .gt_super {
  font-size: 65%;
}

#bqgfmqklpq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bqgfmqklpq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bqgfmqklpq .gt_indent_1 {
  text-indent: 5px;
}

#bqgfmqklpq .gt_indent_2 {
  text-indent: 10px;
}

#bqgfmqklpq .gt_indent_3 {
  text-indent: 15px;
}

#bqgfmqklpq .gt_indent_4 {
  text-indent: 20px;
}

#bqgfmqklpq .gt_indent_5 {
  text-indent: 25px;
}

#bqgfmqklpq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bqgfmqklpq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Model Classification Diagnostics</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="model"><span class='gt_from_md'><em>k</em>-Class</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="kclass"><span class='gt_from_md'><em>k</em>-Class Proportions</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="mcaPk"><span class='gt_from_md'><em>mcaPk</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="avePPk"><span class='gt_from_md'><em>AvePPk</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="OCCk"><span class='gt_from_md'><em>OCCk</em></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="model" class="gt_row gt_center">Class 1</td>
<td headers="kclass" class="gt_row gt_center">0.249</td>
<td headers="CI" class="gt_row gt_center">[0.166, 0.329]</td>
<td headers="mcaPk" class="gt_row gt_center">0.282</td>
<td headers="avePPk" class="gt_row gt_center">0.675</td>
<td headers="OCCk" class="gt_row gt_center">6.264</td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 2</td>
<td headers="kclass" class="gt_row gt_center">0.106</td>
<td headers="CI" class="gt_row gt_center">[0.083, 0.136]</td>
<td headers="mcaPk" class="gt_row gt_center">0.095</td>
<td headers="avePPk" class="gt_row gt_center">0.904</td>
<td headers="OCCk" class="gt_row gt_center">79.420</td></tr>
    <tr><td headers="model" class="gt_row gt_center">Class 3</td>
<td headers="kclass" class="gt_row gt_center">0.644</td>
<td headers="CI" class="gt_row gt_center">[0.561, 0.731]</td>
<td headers="mcaPk" class="gt_row gt_center">0.623</td>
<td headers="avePPk" class="gt_row gt_center">0.893</td>
<td headers="OCCk" class="gt_row gt_center">4.614</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"> <i>Note.</i> McaP<sub>k</sub> = Modal class assignment proportion; AvePP<sub>k</sub> = Average posterior class probabilities; OCC<sub>k</sub> = Odds of correct classification; </td>
    </tr>
  </tfoot>
</table>
</div>
```



<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
