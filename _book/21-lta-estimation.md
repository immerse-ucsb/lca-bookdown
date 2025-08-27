


# (PART) Latent Transition Analysis  {-}

# Measurement Invariance 

--------------------------------------------------------------------------------------

**Data Source**: The data used to illustrate these analyses include elementary school student *Science Attitude* survey items collected during 7th and 10th grades from the **Longitudinal Study of American Youth** (LSAY; Miller, 2015).

--------------------------------------------------------------------------------------

To install package {`rhdf5`} 


``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")

#BiocManager::install("rhdf5")
```

*Load packages*


``` r
library(MplusAutomation)
library(rhdf5)
library(tidyverse)       
library(here)            
library(glue)            
library(janitor)            
library(gt) 
library(reshape2)
library(cowplot)
library(ggrepel)
library(haven)
library(modelsummary)
library(corrplot)
library(DiagrammeR)
library(filesstrings)
library(PNWColors)
```

--------------------------------------------------------------------------------------

Read in LSAY data file, `lsay_new.csv`.


``` r

lsay_data <- read_csv(here("data","lsay_lta.csv"), na = c("9999")) %>% 
    mutate(across(everything(), as.numeric))
```

--------------------------------------------------------------------------------------

## Estimate Latent Transition Analysis (LTA) Model

--------------------------------------------------------------------------------------

### Estimate Invariant LTA Model


``` r

lta_inv <- mplusObject(
  
  TITLE = 
    "Invariant LTA", 
  
  VARIABLE = 
     "usev = ab39m ab39t ab39u ab39w ab39x  ! 7th grade indicators
             ga33a ga33h ga33i ga33k ga33l; ! 10th grade indicators
      
      categorical = ab39m-ab39x ga33a-ga33l;

      classes = c1(4) c2(4);",
    
  ANALYSIS = 
     "estimator = mlr;
      type = mixture;
      starts = 500 100;
      processors=10;",

  MODEL = 
     "%overall%
      c2 on c1;

      MODEL c1: 
      %c1#1%
      [AB39M$1-AB39X$1] (1-5);  !!! labels that are repeated will constrain parameters to equality !!!
      %c1#2%
      [AB39M$1-AB39X$1] (6-10);
      %c1#3%
      [AB39M$1-AB39X$1] (11-15);
      %c1#4%
      [AB39M$1-AB39X$1] (16-20);

      MODEL c2:
      %c2#1%
      [GA33A$1-GA33L$1] (1-5);
      %c2#2%
      [GA33A$1-GA33L$1] (6-10);
      %c2#3%
      [GA33A$1-GA33L$1] (11-15);
      %c2#4%
      [GA33A$1-GA33L$1] (16-20);",
   
  SAVEDATA = 
   "file = LTA_Inv_CPROBS.dat;
    save = cprob;
    missflag = 9999;",

  OUTPUT = "tech1 tech15 svalues;",
  
  usevariables = colnames(lsay_data),
  rdata = lsay_data)

lta_inv_fit <- mplusModeler(lta_inv,
                 dataout=here("lta","lta_model","lta.dat"),
                 modelout=here("lta","lta_model","4-class-invariant.inp"),
                 check=TRUE, run = TRUE, hashfilename = FALSE)
```

--------------------------------------------------------------------------------------

### Estimate Non-Invariant Estimated LTA Model


``` r

lta_non_inv <- mplusObject(
  
  TITLE = 
    "Non-Invariant LTA", 
  
  VARIABLE = 
     "usev = ab39m ab39t ab39u ab39w ab39x  ! 7th grade indicators
             ga33a ga33h ga33i ga33k ga33l; ! 10th grade indicators
      
      categorical = ab39m-ab39x ga33a-ga33l;

      classes = c1(4) c2(4);",
    
  ANALYSIS = 
     "estimator = mlr;
      type = mixture;
      starts = 500 100;
      processors=10;",

  MODEL = 
     "%overall%
      c2 on c1; !!! estimate all multinomial logistic regressions !!!
      
      !!! The above syntax can also be written as: !!!
               ! c2#1 on c1#1 c1#2 c1#3; !  
               ! c2#2 on c1#1 c1#2 c1#3; !
               ! c2#3 on c1#1 c1#2 c1#3; !

      MODEL c1: !!! the following syntax will allow item thresholds to be estimated for each class (e.g. noninvariance) !!!
      
      %c1#1%
      [AB39M$1-AB39X$1]; 
      %c1#2%
      [AB39M$1-AB39X$1];
      %c1#3%
      [AB39M$1-AB39X$1];
      %c1#4%
      [AB39M$1-AB39X$1];

      MODEL c2:
      %c2#1%
      [GA33A$1-GA33L$1];
      %c2#2%
      [GA33A$1-GA33L$1];
      %c2#3%
      [GA33A$1-GA33L$1];
      %c2#4%
      [GA33A$1-GA33L$1];",

  OUTPUT = "tech1 tech15 svalues;",
  
  usevariables = colnames(lsay_data),
  rdata = lsay_data)

lta_non_inv_fit <- mplusModeler(lta_non_inv,
                     dataout=here("lta","lta_model","lta.dat"),
                     modelout=here("lta","lta_model","4-class-non-invariant.inp"),
                     check=TRUE, run = TRUE, hashfilename = FALSE)
```

------------------------------------------------------------------------


### Conduct Sattorra-Bentler adjusted Log Likelihood Ratio Difference Testing

-   non-invariant (comparison): This model has **more** parameters.

-   invariant (nested): This model has **less** parameters.


``` r

# *0 = null or nested model & *1 = comparison  or parent model

lta_models <- readModels(here("lta","lta_model"), quiet = TRUE)

# Log Likelihood Values
L0 <- lta_models[["X4.class.invariant.out"]][["summaries"]][["LL"]]
L1 <- lta_models[["X4.class.non.invariant.out"]][["summaries"]][["LL"]] 

# LRT equation
lr <- -2*(L0-L1) 

# Parameters
p0 <- lta_models[["X4.class.invariant.out"]][["summaries"]][["Parameters"]] 
p1 <- lta_models[["X4.class.non.invariant.out"]][["summaries"]][["Parameters"]]

# Scaling Correction Factors
c0 <- lta_models[["X4.class.invariant.out"]][["summaries"]][["LLCorrectionFactor"]]
c1 <- lta_models[["X4.class.non.invariant.out"]][["summaries"]][["LLCorrectionFactor"]]

# Difference Test Scaling correction
cd <- ((p0*c0)-(p1*c1))/(p0-p1)

# Chi-square difference test(TRd)
TRd <- (lr)/(cd)

# Degrees of freedom
df <- abs(p0 - p1)


# Significance test
(p_diff <- pchisq(TRd, df, lower.tail=FALSE))
#> [1] 0.6245173
```

**RESULT**: The Log Likelihood $\chi^2$ difference test comparing the invariant and non-invariant LTA models was, $\chi^2 (20) = 21.542, p = .624$.

[Reference](https://stats.idre.ucla.edu/mplus/faq/how-can-i-compute-a-chi-square-test-for-nested-models-with-the-mlr-or-mlm-estimators/)
    

--------------------------------------------------------------------------------------

Read invariance model and extract parameters (intercepts and multinomial regression coefficients)


``` r

lta_inv1 <- readModels(here("lta","lta_model","4-Class-Invariant.out" ), quiet = TRUE)

par <- as_tibble(lta_inv1[["parameters"]][["unstandardized"]]) %>% 
  select(1:3) %>% 
  filter(grepl('ON|Means', paramHeader)) %>% 
  mutate(est = as.numeric(est))
```

Manual method to calculate transition probabilities:

Although possible to extract transition probabilities directly from the output the following code illustrates how the parameters are used to calculate each transition. This is useful for conducting advanced LTA model specifications such as making specific constraints within or between transition matrices, or testing the equivalence of specific transition probabilities.


``` r
# Name each parameter individually to make the subsequent calculations more readable
a1 <- unlist(par[13,3]); a2 <- unlist(par[14,3]); a3 <- unlist(par[15,3]); b11 <- unlist(par[1,3]);
b21 <- unlist(par[4,3]); b31 <- unlist(par[7,3]); b12 <- unlist(par[2,3]); b22 <- unlist(par[5,3]);
b32 <- unlist(par[8,3]); b13 <- unlist(par[3,3]); b23 <- unlist(par[6,3]); b33 <- unlist(par[9,3])

# Calculate transition probabilities from the logit parameters
t11 <- exp(a1+b11)/(exp(a1+b11)+exp(a2+b21)+exp(a3+b31)+exp(0))
t12 <- exp(a2+b21)/(exp(a1+b11)+exp(a2+b21)+exp(a3+b31)+exp(0))
t13 <- exp(a3+b31)/(exp(a1+b11)+exp(a2+b21)+exp(a3+b31)+exp(0))
t14 <- 1 - (t11 + t12 + t13)

t21 <- exp(a1+b12)/(exp(a1+b12)+exp(a2+b22)+exp(a3+b32)+exp(0))
t22 <- exp(a2+b22)/(exp(a1+b12)+exp(a2+b22)+exp(a3+b32)+exp(0))
t23 <- exp(a3+b32)/(exp(a1+b12)+exp(a2+b22)+exp(a3+b32)+exp(0))
t24 <- 1 - (t21 + t22 + t23)

t31 <- exp(a1+b13)/(exp(a1+b13)+exp(a2+b23)+exp(a3+b33)+exp(0))
t32 <- exp(a2+b23)/(exp(a1+b13)+exp(a2+b23)+exp(a3+b33)+exp(0))
t33 <- exp(a3+b33)/(exp(a1+b13)+exp(a2+b23)+exp(a3+b33)+exp(0))
t34 <- 1 - (t31 + t32 + t33)

t41 <- exp(a1)/(exp(a1)+exp(a2)+exp(a3)+exp(0))
t42 <- exp(a2)/(exp(a1)+exp(a2)+exp(a3)+exp(0))
t43 <- exp(a3)/(exp(a1)+exp(a2)+exp(a3)+exp(0))
t44 <- 1 - (t41 + t42 + t43)

```

### Create Transition Table


``` r

t_matrix <- tibble(
  "Time1" = c("C1=Anti-Science","C1=Amb. w/ Elevated","C1=Amb. w/ Minimal","C1=Pro-Science"),
  "C2=Anti-Science" = c(t11,t21,t31,t41),
  "C2=Amb. w/ Elevated" = c(t12,t22,t32,t42),
  "C2=Amb. w/ Minimal" = c(t13,t23,t33,t43),
  "C2=Pro-Science" = c(t14,t24,t34,t44))

t_matrix %>% 
  gt(rowname_col = "Time1") %>%
  tab_stubhead(label = "7th grade") %>% 
  tab_header(
    title = md("**Student transitions from 7th grade (rows) to 10th grade (columns)**"),
    subtitle = md("&nbsp;")) %>% 
  fmt_number(2:5,decimals = 2) %>% 
  tab_spanner(label = "10th grade",columns = 2:5) %>% 
  tab_footnote(
    footnote = md(
    "*Note.* Transition matrix values are the identical to Table 5, however Table 5 
    has the values rearranged by class for interpretation purposes. Classes may be arranged
    directly through Mplus syntax using start values."), 
    locations = cells_title())
```


```{=html}
<div id="hbvhedisrf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hbvhedisrf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hbvhedisrf thead, #hbvhedisrf tbody, #hbvhedisrf tfoot, #hbvhedisrf tr, #hbvhedisrf td, #hbvhedisrf th {
  border-style: none;
}

#hbvhedisrf p {
  margin: 0;
  padding: 0;
}

#hbvhedisrf .gt_table {
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

#hbvhedisrf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hbvhedisrf .gt_title {
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

#hbvhedisrf .gt_subtitle {
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

#hbvhedisrf .gt_heading {
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

#hbvhedisrf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hbvhedisrf .gt_col_headings {
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

#hbvhedisrf .gt_col_heading {
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

#hbvhedisrf .gt_column_spanner_outer {
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

#hbvhedisrf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hbvhedisrf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hbvhedisrf .gt_column_spanner {
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

#hbvhedisrf .gt_spanner_row {
  border-bottom-style: hidden;
}

#hbvhedisrf .gt_group_heading {
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

#hbvhedisrf .gt_empty_group_heading {
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

#hbvhedisrf .gt_from_md > :first-child {
  margin-top: 0;
}

#hbvhedisrf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hbvhedisrf .gt_row {
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

#hbvhedisrf .gt_stub {
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

#hbvhedisrf .gt_stub_row_group {
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

#hbvhedisrf .gt_row_group_first td {
  border-top-width: 2px;
}

#hbvhedisrf .gt_row_group_first th {
  border-top-width: 2px;
}

#hbvhedisrf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hbvhedisrf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hbvhedisrf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hbvhedisrf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hbvhedisrf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hbvhedisrf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hbvhedisrf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hbvhedisrf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hbvhedisrf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hbvhedisrf .gt_footnotes {
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

#hbvhedisrf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hbvhedisrf .gt_sourcenotes {
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

#hbvhedisrf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hbvhedisrf .gt_left {
  text-align: left;
}

#hbvhedisrf .gt_center {
  text-align: center;
}

#hbvhedisrf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hbvhedisrf .gt_font_normal {
  font-weight: normal;
}

#hbvhedisrf .gt_font_bold {
  font-weight: bold;
}

#hbvhedisrf .gt_font_italic {
  font-style: italic;
}

#hbvhedisrf .gt_super {
  font-size: 65%;
}

#hbvhedisrf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hbvhedisrf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hbvhedisrf .gt_indent_1 {
  text-indent: 5px;
}

#hbvhedisrf .gt_indent_2 {
  text-indent: 10px;
}

#hbvhedisrf .gt_indent_3 {
  text-indent: 15px;
}

#hbvhedisrf .gt_indent_4 {
  text-indent: 20px;
}

#hbvhedisrf .gt_indent_5 {
  text-indent: 25px;
}

#hbvhedisrf .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hbvhedisrf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>Student transitions from 7th grade (rows) to 10th grade (columns)</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'> </span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></td>
    </tr>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="a::stub">7th grade</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="10th grade">
        <div class="gt_column_spanner">10th grade</div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="C2=Anti-Science">C2=Anti-Science</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="C2=Amb.-w/-Elevated">C2=Amb. w/ Elevated</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="C2=Amb.-w/-Minimal">C2=Amb. w/ Minimal</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="C2=Pro-Science">C2=Pro-Science</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">C1=Anti-Science</th>
<td headers="stub_1_1 C2=Anti-Science" class="gt_row gt_right">0.27</td>
<td headers="stub_1_1 C2=Amb. w/ Elevated" class="gt_row gt_right">0.27</td>
<td headers="stub_1_1 C2=Amb. w/ Minimal" class="gt_row gt_right">0.32</td>
<td headers="stub_1_1 C2=Pro-Science" class="gt_row gt_right">0.15</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">C1=Amb. w/ Elevated</th>
<td headers="stub_1_2 C2=Anti-Science" class="gt_row gt_right">0.09</td>
<td headers="stub_1_2 C2=Amb. w/ Elevated" class="gt_row gt_right">0.56</td>
<td headers="stub_1_2 C2=Amb. w/ Minimal" class="gt_row gt_right">0.19</td>
<td headers="stub_1_2 C2=Pro-Science" class="gt_row gt_right">0.16</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">C1=Amb. w/ Minimal</th>
<td headers="stub_1_3 C2=Anti-Science" class="gt_row gt_right">0.15</td>
<td headers="stub_1_3 C2=Amb. w/ Elevated" class="gt_row gt_right">0.21</td>
<td headers="stub_1_3 C2=Amb. w/ Minimal" class="gt_row gt_right">0.52</td>
<td headers="stub_1_3 C2=Pro-Science" class="gt_row gt_right">0.12</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">C1=Pro-Science</th>
<td headers="stub_1_4 C2=Anti-Science" class="gt_row gt_right">0.08</td>
<td headers="stub_1_4 C2=Amb. w/ Elevated" class="gt_row gt_right">0.35</td>
<td headers="stub_1_4 C2=Amb. w/ Minimal" class="gt_row gt_right">0.27</td>
<td headers="stub_1_4 C2=Pro-Science" class="gt_row gt_right">0.30</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> Transition matrix values are the identical to Table 5, however Table 5
has the values rearranged by class for interpretation purposes. Classes may be arranged
directly through Mplus syntax using start values.</span></td>
    </tr>
  </tfoot>
</table>
</div>
```



<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
