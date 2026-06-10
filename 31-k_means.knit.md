---
title-block-banner: true
title: "MMDBER_Physics Education"
format:
  html:
    toc: true
    toc-depth: 4 
    theme: Flatly
    page-layout: full
editor: visual
editor_options: 
  chunk_output_type: inline
---


``` r
library(tidyverse)
library(haven)
library(glue)
library(MplusAutomation)
library(rhdf5)
library(here)
library(janitor)
library(gt)
library(semPlot)
library(reshape2)
library(cowplot)
library(filesstrings)
library(hrbrthemes)
library(poLCA)
library(naniar)
library(psych)
library(flextable)
library(officer)
conflicted::conflicts_prefer(flextable::compose)
```


``` r
options(scipen=999, digits=4)
```


``` r
support_data <- read.csv(here("k-means", "SocialSupport.csv"))
```


``` r
vis_miss(support_data)
```

<img src="31-k_means_files/figure-html/unnamed-chunk-4-1.png" alt="" width="672" />


``` r
describe(support_data)
#>                  vars   n   mean     sd median trimmed
#> ResponseId*         1 567 284.00 163.82  284.0  284.00
#> Family              2 567   0.65   0.48    1.0    0.69
#> PhysPeers           3 567   0.85   0.35    1.0    0.94
#> NonPhysPeers        4 567   0.73   0.44    1.0    0.79
#> HSPhysTeach         5 567   0.25   0.43    0.0    0.18
#> NonPhysFaculty      6 567   0.24   0.43    0.0    0.17
#> PhysFaculty         7 567   0.74   0.44    1.0    0.80
#> ResearchGrp         8 567   0.54   0.50    1.0    0.55
#> WiPGroup            9 567   0.51   0.50    1.0    0.51
#> GIPGroup           10 567   0.27   0.45    0.0    0.22
#> ReligOrg           11 567   0.08   0.27    0.0    0.00
#> OtherIdent         12 567   0.04   0.19    0.0    0.00
#> SocialComm         13 567   0.22   0.42    0.0    0.15
#> MentalHlth         14 567   0.21   0.41    0.0    0.14
#> Woman              15 567   0.88   0.32    1.0    0.98
#> Transgender        16 567   0.07   0.25    0.0    0.00
#> NonBinary          17 567   0.17   0.37    0.0    0.08
#> White              18 567   0.71   0.45    1.0    0.76
#> Asian              19 567   0.22   0.41    0.0    0.15
#> Hispanic           20 567   0.13   0.34    0.0    0.04
#> Black              21 567   0.05   0.22    0.0    0.00
#> MidEastern         22 567   0.03   0.17    0.0    0.00
#> NatAmerican        23 567   0.01   0.10    0.0    0.00
#> PacificIslander    24 567   0.01   0.10    0.0    0.00
#> InstitutionType*   25 567   4.41   0.69    5.0    4.50
#> AcademicYear*      26 567   4.20   1.62    5.0    4.29
#> Identity           27 563   2.54   0.84    2.5    2.59
#>                     mad min max range  skew kurtosis   se
#> ResponseId*      210.53   1 567   566  0.00    -1.21 6.88
#> Family             0.00   0   1     1 -0.62    -1.61 0.02
#> PhysPeers          0.00   0   1     1 -2.00     1.99 0.01
#> NonPhysPeers       0.00   0   1     1 -1.05    -0.89 0.02
#> HSPhysTeach        0.00   0   1     1  1.18    -0.60 0.02
#> NonPhysFaculty     0.00   0   1     1  1.24    -0.47 0.02
#> PhysFaculty        0.00   0   1     1 -1.10    -0.80 0.02
#> ResearchGrp        0.00   0   1     1 -0.15    -1.98 0.02
#> WiPGroup           0.00   0   1     1 -0.03    -2.00 0.02
#> GIPGroup           0.00   0   1     1  1.01    -0.97 0.02
#> ReligOrg           0.00   0   1     1  3.15     7.93 0.01
#> OtherIdent         0.00   0   1     1  4.76    20.73 0.01
#> SocialComm         0.00   0   1     1  1.33    -0.22 0.02
#> MentalHlth         0.00   0   1     1  1.41    -0.02 0.02
#> Woman              0.00   0   1     1 -2.39     3.70 0.01
#> Transgender        0.00   0   1     1  3.45     9.95 0.01
#> NonBinary          0.00   0   1     1  1.79     1.22 0.02
#> White              0.00   0   1     1 -0.93    -1.14 0.02
#> Asian              0.00   0   1     1  1.38    -0.09 0.02
#> Hispanic           0.00   0   1     1  2.14     2.60 0.01
#> Black              0.00   0   1     1  3.98    13.90 0.01
#> MidEastern         0.00   0   1     1  5.68    30.35 0.01
#> NatAmerican        0.00   0   1     1  9.54    89.18 0.00
#> PacificIslander    0.00   0   1     1  9.54    89.18 0.00
#> InstitutionType*   0.00   1   5     4 -1.25     2.22 0.03
#> AcademicYear*      1.48   1   6     5 -0.31    -1.40 0.07
#> Identity           0.74   0   4     4 -0.54     0.07 0.04
```


``` r
cor(support_data$Woman, support_data$Transgender)
#> [1] -0.4965
cor(support_data$Woman, support_data$NonBinary)
#> [1] -0.6959
cor(support_data$Transgender, support_data$NonBinary)
#> [1] 0.3547
```


``` r
conflicted::conflicts_prefer(dplyr::filter)
#> [conflicted] Will prefer dplyr::filter over any other
#> package.
```


``` r
nonbinary_subset <- support_data %>% filter(NonBinary == 1) #94 people
trans_subset <- support_data %>% filter(Transgender == 1)
nowoman_subset <- support_data %>% filter(Woman == 0) #66 people
nowoman_nonbinary <- nowoman_subset %>% filter(NonBinary == 1) #58 people
filtered_data <- support_data %>%
  filter(Woman == 1 | NonBinary == 1 | Transgender == 1)
```

## LCA

### Enumeration


``` r
lca_7  <- lapply(1:7, function(k) {
  lca_enum  <- mplusObject(
      
    TITLE = glue("{k}-Class"), 
  
    VARIABLE = glue(
    "categorical = Family-MentalHlth; 
     usevar = Family-MentalHlth;
     classes = c({k}); "),
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 200 100; 
    processors = 10;",
  
  OUTPUT = "sampstat residual tech11 tech14 svalues;",
  
  PLOT = 
    "type = plot3; 
    series = Family-MentalHlth(*);",
  
  usevariables = colnames(support_data),
  rdata = support_data)

lca_enum_fit <- mplusModeler(lca_enum, 
                            dataout=glue("support.dat"),
                            modelout=glue("c{k}_support.inp") ,
                            check=TRUE, run = TRUE, hashfilename = FALSE)
})
```

### Table of Fit


``` r
source(here("k-means", "function/extract_mplus_info.R"))

output_dir <- here("k-means", "enum_mplus")

output_files <- list.files(output_dir, pattern = "\\.out$", full.names = TRUE)

final_data <- map_dfr(output_files, extract_mplus_info_extended)

sample_size <- unique(final_data$Sample_Size)
```






















































































