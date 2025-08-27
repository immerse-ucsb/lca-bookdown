---
editor_options:
  markdown:
    wrap: sentence
---



# (PART) Introduction {-}

# R and RStudio

------------------------------------------------------------------------

This walkthrough is presented by the IMMERSE team and will go through some common tasks carried out in R.
There are many free resources available to get started with R and RStudio.
One of our favorites is [*R for Data Science*](https://r4ds.had.co.nz/).

------------------------------------------------------------------------

-   *R*[@rcore2017] is a free, open-source programming language and environment widely used for statistical computing, data analysis, and data visualization.

-   *RStudio*[@rstudio2020] is an integrated development environment (IDE) for R, providing an intuitive interface that makes coding, visualization, and project management more accessible.

-   *Mplus*[@muthen2017] is a statistical modeling program used for analyzing complex data, such as latent variable models, structural equation modeling, and growth modeling.
    This book uses an R package called `MplusAutomation` to automate the process of running models, extracting results, and generating data visualizations.

------------------------------------------------------------------------

## Installation

------------------------------------------------------------------------

### Step 0: Install R, RStudio, and Mplus

[Here](https://posit.co/download/rstudio-desktop/) you will find a guide to installing both R and R Studio.
You can also install Mplus [here](https://www.statmodel.com/orderonline/).

*Note*: The installation of Mplus requires a paid license with the mixture add-on.
IMMERSE fellows will be given their own copy of Mplus for use during the one year training.

------------------------------------------------------------------------

## Set-up

------------------------------------------------------------------------

### Step 1: Create a new R-project in RStudio

R-projects help us organize our folders , filepaths, and scripts.
To create a new R project:

-   File --\> New Project...

Click "New Directory" --\> New Project --\> Name your project

### Step 2: Create an R-markdown document

An R-markdown file provides an authoring framework for data science that allows us to organize our reports using texts and code chunks.
This document you are reading was made using R-markdown!

To create an R-markdown:

-   File --\> New File --\> R Markdown...

In the window that pops up, give the R-markdown a title such as "**Introduction to R and RStudio**" Click "OK." You should see a new markdown with some example text and code chunks.
We want a clean document to start off with so delete everything from line 10 down.
Go ahead and save this document in your R Project folder.

### Step 3: Load packages

Your first code chunk in any given markdown should be the packages you will be using.
To insert a code chunk, etiher use the keyboard shortcut ctrl + alt + i or Code --\> Insert Chunk or click the green box with the letter C on it.
There are a few packages we want our markdown to read in:


``` r
library(psych) # describe()
library(here) #helps with filepaths
library(gt) # create tables
library(tidyverse) #collection of R packages designed for data science
```

As a reminder, if a function does not work and you receive an error like this: `could not find function "random_function"`; or if you try to load a package and you receive an error like this: `` there is no package called `random_package` `` , then you will need to install the package using `install.packages("random_package")` in the console (the bottom-left window in R studio).
Once you have installed the package you will *never* need to install it again, however you must *always* load in the packages at the beginning of your R markdown using `library(random_package)`, as shown in this document.

The style of code and package we will be using is called [`tidyverse`](https://www.tidyverse.org/)[@wickham2019] .
Most functions are within the `tidyverse` package and if not, I've indicated the packages used in the code chunk above.

------------------------------------------------------------------------

## Explore the data

------------------------------------------------------------------------

### Step 4: Read in data

To demonstrate mixture modeling in the training program and online resource components of the IES grant we utilize the *Civil Rights Data Collection (CRDC)* (CRDC) data repository.
The CRDC is a federally mandated school-level data collection effort that occurs every other year.
This public data is currently available for selected latent class indicators across 4 years (2011, 2013, 2015, 2017) and all US states.
In this example, we use the Arizona state sample.
We utilize six focal indicators which constitute the latent class model in our example; three variables which report on harassment/bullying in schools based on disability, race, or sex, and three variables on full-time equivalent school staff hires (counselor, psychologist, law enforcement).
This data source also includes covariates on a variety of subjects and distal outcomes reported in 2018 such as math/reading assessments and graduation rates.


```{=html}
<div id="ufodrdcjaw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ufodrdcjaw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ufodrdcjaw thead, #ufodrdcjaw tbody, #ufodrdcjaw tfoot, #ufodrdcjaw tr, #ufodrdcjaw td, #ufodrdcjaw th {
  border-style: none;
}

#ufodrdcjaw p {
  margin: 0;
  padding: 0;
}

#ufodrdcjaw .gt_table {
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

#ufodrdcjaw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ufodrdcjaw .gt_title {
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

#ufodrdcjaw .gt_subtitle {
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

#ufodrdcjaw .gt_heading {
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

#ufodrdcjaw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ufodrdcjaw .gt_col_headings {
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

#ufodrdcjaw .gt_col_heading {
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

#ufodrdcjaw .gt_column_spanner_outer {
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

#ufodrdcjaw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ufodrdcjaw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ufodrdcjaw .gt_column_spanner {
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

#ufodrdcjaw .gt_spanner_row {
  border-bottom-style: hidden;
}

#ufodrdcjaw .gt_group_heading {
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

#ufodrdcjaw .gt_empty_group_heading {
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

#ufodrdcjaw .gt_from_md > :first-child {
  margin-top: 0;
}

#ufodrdcjaw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ufodrdcjaw .gt_row {
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

#ufodrdcjaw .gt_stub {
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

#ufodrdcjaw .gt_stub_row_group {
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

#ufodrdcjaw .gt_row_group_first td {
  border-top-width: 2px;
}

#ufodrdcjaw .gt_row_group_first th {
  border-top-width: 2px;
}

#ufodrdcjaw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufodrdcjaw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ufodrdcjaw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ufodrdcjaw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ufodrdcjaw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufodrdcjaw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ufodrdcjaw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ufodrdcjaw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ufodrdcjaw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ufodrdcjaw .gt_footnotes {
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

#ufodrdcjaw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufodrdcjaw .gt_sourcenotes {
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

#ufodrdcjaw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ufodrdcjaw .gt_left {
  text-align: left;
}

#ufodrdcjaw .gt_center {
  text-align: center;
}

#ufodrdcjaw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ufodrdcjaw .gt_font_normal {
  font-weight: normal;
}

#ufodrdcjaw .gt_font_bold {
  font-weight: bold;
}

#ufodrdcjaw .gt_font_italic {
  font-style: italic;
}

#ufodrdcjaw .gt_super {
  font-size: 65%;
}

#ufodrdcjaw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ufodrdcjaw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ufodrdcjaw .gt_indent_1 {
  text-indent: 5px;
}

#ufodrdcjaw .gt_indent_2 {
  text-indent: 10px;
}

#ufodrdcjaw .gt_indent_3 {
  text-indent: 15px;
}

#ufodrdcjaw .gt_indent_4 {
  text-indent: 20px;
}

#ufodrdcjaw .gt_indent_5 {
  text-indent: 25px;
}

#ufodrdcjaw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ufodrdcjaw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="Name" class="gt_row gt_left">report_sex</td>
<td headers="Label" class="gt_row gt_left">Number of full time equivalent psychologists hired as school staff</td>
<td headers="Values" class="gt_row gt_left">0 = No staff present, 1 = At least one staff present</td></tr>
    <tr><td headers="Name" class="gt_row gt_left">counselors_fte</td>
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


**To read in data in R**:


``` r
data <- read_csv(here("data", "crdc_lca_data.csv")) 
```

**Ways to view data in R**:

1. click on the data in your Global Environment (upper right pane) or use...


``` r
View(data)
```

2. `summary()` gives basic summary statistics & shows number of NA values (great for checking that data has been read in correctly)


``` r
summary(data)
#>     leaid             ncessch            report_dis    
#>  Length:2027        Length:2027        Min.   :0.0000  
#>  Class :character   Class :character   1st Qu.:0.0000  
#>  Mode  :character   Mode  :character   Median :0.0000  
#>                                        Mean   :0.0425  
#>                                        3rd Qu.:0.0000  
#>                                        Max.   :1.0000  
#>                                        NA's   :27      
#>   report_race      report_sex   counselors_fte  
#>  Min.   :0.000   Min.   :0.00   Min.   :0.0000  
#>  1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.0000  
#>  Median :0.000   Median :0.00   Median :0.0000  
#>  Mean   :0.103   Mean   :0.17   Mean   :0.4595  
#>  3rd Qu.:0.000   3rd Qu.:0.00   3rd Qu.:1.0000  
#>  Max.   :1.000   Max.   :1.00   Max.   :1.0000  
#>  NA's   :27      NA's   :27     NA's   :27      
#>    psych_fte         law_fte      
#>  Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :0.0000   Median :0.0000  
#>  Mean   :0.4742   Mean   :0.1255  
#>  3rd Qu.:1.0000   3rd Qu.:0.0000  
#>  Max.   :1.0000   Max.   :1.0000  
#>  NA's   :30       NA's   :27
```

3. `names()` provides a list of column names. Very useful if you don't have them memorized!


``` r
names(data)
#> [1] "leaid"          "ncessch"        "report_dis"    
#> [4] "report_race"    "report_sex"     "counselors_fte"
#> [7] "psych_fte"      "law_fte"
```

4. head() prints the top 6 rows of the dataframe


``` r
head(data)
#> # A tibble: 6 × 8
#>   leaid   ncessch      report_dis report_race report_sex
#>   <chr>   <chr>             <dbl>       <dbl>      <dbl>
#> 1 0400001 040000100120          0           0          0
#> 2 0400001 040000100616          0           0          1
#> 3 0400001 040000101204          0           0          1
#> 4 0400001 040000101871          0           1          1
#> 5 0400001 040000101872          0           0          0
#> 6 0400001 040000102344          0           0          0
#> # ℹ 3 more variables: counselors_fte <dbl>,
#> #   psych_fte <dbl>, law_fte <dbl>
```


### Step 5: Descriptive Statistics

Let's look at descriptive statistics for each variable.
Because looking at the ID variables' (`leaid`) and (`necessch`) descriptives is unnecessary, we use `select()` to remove the variable by using the minus (`-`) sign:


``` r
data %>% 
  select(-leaid, -ncessch) %>% 
  summary()
#>    report_dis      report_race      report_sex  
#>  Min.   :0.0000   Min.   :0.000   Min.   :0.00  
#>  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.00  
#>  Median :0.0000   Median :0.000   Median :0.00  
#>  Mean   :0.0425   Mean   :0.103   Mean   :0.17  
#>  3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.00  
#>  Max.   :1.0000   Max.   :1.000   Max.   :1.00  
#>  NA's   :27       NA's   :27      NA's   :27    
#>  counselors_fte     psych_fte         law_fte      
#>  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :0.0000   Median :0.0000   Median :0.0000  
#>  Mean   :0.4595   Mean   :0.4742   Mean   :0.1255  
#>  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
#>  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#>  NA's   :27       NA's   :30       NA's   :27
```

Alternatively, we can use the `psych::describe()` function to give more information:


``` r
data %>% 
  select(-leaid, -ncessch) %>% 
  describe()
#>                vars    n mean   sd median trimmed mad min
#> report_dis        1 2000 0.04 0.20      0    0.00   0   0
#> report_race       2 2000 0.10 0.30      0    0.00   0   0
#> report_sex        3 2000 0.17 0.38      0    0.09   0   0
#> counselors_fte    4 2000 0.46 0.50      0    0.45   0   0
#> psych_fte         5 1997 0.47 0.50      0    0.47   0   0
#> law_fte           6 2000 0.13 0.33      0    0.03   0   0
#>                max range skew kurtosis   se
#> report_dis       1     1 4.53    18.55 0.00
#> report_race      1     1 2.61     4.82 0.01
#> report_sex       1     1 1.76     1.08 0.01
#> counselors_fte   1     1 0.16    -1.97 0.01
#> psych_fte        1     1 0.10    -1.99 0.01
#> law_fte          1     1 2.26     3.11 0.01
```

What if we want to look at a subset of the data?
For example, what if we want to subset the data to observe a specific school district?
(`leaid`) We can use `tidyverse::filter()` to subset the data using certain criteria.


``` r
data %>% 
  filter(leaid == "0408800") %>% 
  describe() 
#>                vars  n  mean    sd median trimmed   mad min
#> leaid*            1 86  1.00  0.00    1.0    1.00  0.00   1
#> ncessch*          2 86 43.50 24.97   43.5   43.50 31.88   1
#> report_dis        3 86  0.05  0.21    0.0    0.00  0.00   0
#> report_race       4 86  0.15  0.36    0.0    0.07  0.00   0
#> report_sex        5 86  0.19  0.39    0.0    0.11  0.00   0
#> counselors_fte    6 86  0.95  0.21    1.0    1.00  0.00   0
#> psych_fte         7 86  0.19  0.39    0.0    0.11  0.00   0
#> law_fte           8 86  0.14  0.35    0.0    0.06  0.00   0
#>                max range  skew kurtosis   se
#> leaid*           1     0   NaN      NaN 0.00
#> ncessch*        86    85  0.00    -1.24 2.69
#> report_dis       1     1  4.23    16.10 0.02
#> report_race      1     1  1.91     1.68 0.04
#> report_sex       1     1  1.59     0.52 0.04
#> counselors_fte   1     1 -4.23    16.10 0.02
#> psych_fte        1     1  1.59     0.52 0.04
#> law_fte          1     1  2.04     2.21 0.04


#You can use any operator to filter: >, <, ==, >=, etc.
```

Since we have binary data (0,1), it would be helpful to look at the proportions:


``` r
data %>% 
  drop_na() %>% 
  pivot_longer(report_dis:law_fte, names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(prop = sum(value)/n(),
            n = n()) %>%
  arrange(desc(prop))
#> # A tibble: 6 × 3
#>   variable         prop     n
#>   <chr>           <dbl> <int>
#> 1 psych_fte      0.481   1970
#> 2 counselors_fte 0.459   1970
#> 3 report_sex     0.173   1970
#> 4 law_fte        0.127   1970
#> 5 report_race    0.105   1970
#> 6 report_dis     0.0431  1970
```

<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
