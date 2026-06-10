


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


### Nested Model Testing: Sattorra-Bentler adjusted Log Likelihood Ratio Difference Testing

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

#### Alternative: Nested Model Testing Using `compareModels` in MplusAutomation


``` r
compareModels(lta_models$X4.class.invariant.out, lta_models$X4.class.non.invariant.out, show="all", diffTest=TRUE)
#> 
#> ==============
#> 
#> Mplus model comparison
#> ----------------------
#> 
#> ------
#> Model 1:  /Users/traviscandieas/Library/CloudStorage/Box-Box/lca-bookdown/lta/lta_model/4-class-invariant.out 
#> Model 2:  /Users/traviscandieas/Library/CloudStorage/Box-Box/lca-bookdown/lta/lta_model/4-class-non-invariant.out 
#> ------
#> 
#> Model Summary Comparison
#> ------------------------
#> 
#>              m1            m2               
#> Title        Invariant LTA Non-Invariant LTA
#> Observations 3092          3092             
#> Estimator    MLR           MLR              
#> Parameters   35            55               
#> LL           -14542.768    -14531.997       
#> AIC          29155.536     29173.994        
#> BIC          29366.816     29506.005        
#> 
#>   MLR Chi-Square Difference Test for Nested Models Based on Loglikelihood
#>   -----------------------------------------------------------------------
#> 
#>   Difference Test Scaling Correction:  1.2355 
#>   Chi-square difference:  17.4359 
#>   Diff degrees of freedom:  20 
#>   P-value:  0.6245 
#> 
#>   Note: The chi-square difference test assumes that these models are nested.
#>   It is up to you to verify this assumption.
#> 
#>   MLR Chi-Square Difference test for nested models
#>   --------------------------------------------
#> 
#>   Difference Test Scaling Correction:  
#>   Chi-square difference:  
#>   Diff degrees of freedom:  
#>   P-value:  
#> 
#> Note: The chi-square difference test assumes that these models are nested.
#>   It is up to you to verify this assumption.
#> 
#> =========
#> 
#> Model parameter comparison
#> --------------------------
#>   Parameters present in both models
#> =========
#> 
#>   Approximately equal in both models (param. est. diff <= 1e-04)
#>   ----------------------------------------------
#> None
#> 
#> 
#>   Parameter estimates that differ between models (param. est. diff > 1e-04)
#>   ----------------------------------------------
#>  paramHeader   param                  LatentClass m1_est
#>      C2#1.ON    C1#1 Categorical.Latent.Variables  1.915
#>      C2#1.ON    C1#2 Categorical.Latent.Variables  0.789
#>      C2#1.ON    C1#3 Categorical.Latent.Variables  1.551
#>      C2#2.ON    C1#1 Categorical.Latent.Variables  0.420
#>      C2#2.ON    C1#2 Categorical.Latent.Variables  1.114
#>      C2#2.ON    C1#3 Categorical.Latent.Variables  0.451
#>      C2#3.ON    C1#1 Categorical.Latent.Variables  0.896
#>      C2#3.ON    C1#2 Categorical.Latent.Variables  0.349
#>      C2#3.ON    C1#3 Categorical.Latent.Variables  1.634
#>        Means    C1#1 Categorical.Latent.Variables  0.735
#>        Means    C1#2 Categorical.Latent.Variables  0.710
#>        Means    C1#3 Categorical.Latent.Variables  0.637
#>        Means    C2#1 Categorical.Latent.Variables -1.326
#>        Means    C2#2 Categorical.Latent.Variables  0.165
#>        Means    C2#3 Categorical.Latent.Variables -0.126
#>   Thresholds AB39M$1                         C1#1 -0.718
#>   Thresholds AB39M$1                         C1#2  0.964
#>   Thresholds AB39M$1                         C1#3 -2.101
#>   Thresholds AB39M$1                         C1#4 -0.298
#>   Thresholds AB39T$1                         C1#1  0.830
#>   Thresholds AB39T$1                         C1#2  3.759
#>   Thresholds AB39T$1                         C1#3 -2.553
#>   Thresholds AB39T$1                         C1#4  0.398
#>   Thresholds AB39U$1                         C1#1  0.287
#>   Thresholds AB39U$1                         C1#2  3.293
#>   Thresholds AB39U$1                         C1#3 -2.824
#>   Thresholds AB39U$1                         C1#4 -0.907
#>   Thresholds AB39W$1                         C1#1  0.289
#>   Thresholds AB39W$1                         C1#2  2.828
#>   Thresholds AB39W$1                         C1#3 -2.186
#>   Thresholds AB39W$1                         C1#4  1.572
#>   Thresholds AB39X$1                         C1#1 -0.802
#>   Thresholds AB39X$1                         C1#2  4.328
#>   Thresholds AB39X$1                         C1#3 -2.561
#>   Thresholds AB39X$1                         C1#4 14.639
#>   Thresholds GA33A$1                         C2#1 -0.718
#>   Thresholds GA33A$1                         C2#2  0.964
#>   Thresholds GA33A$1                         C2#3 -2.101
#>   Thresholds GA33A$1                         C2#4 -0.298
#>   Thresholds GA33H$1                         C2#1  0.830
#>   Thresholds GA33H$1                         C2#2  3.759
#>   Thresholds GA33H$1                         C2#3 -2.553
#>   Thresholds GA33H$1                         C2#4  0.398
#>   Thresholds GA33I$1                         C2#1  0.287
#>   Thresholds GA33I$1                         C2#2  3.293
#>   Thresholds GA33I$1                         C2#3 -2.824
#>   Thresholds GA33I$1                         C2#4 -0.907
#>   Thresholds GA33K$1                         C2#1  0.289
#>   Thresholds GA33K$1                         C2#2  2.828
#>   Thresholds GA33K$1                         C2#3 -2.186
#>   Thresholds GA33K$1                         C2#4  1.572
#>   Thresholds GA33L$1                         C2#1 -0.802
#>   Thresholds GA33L$1                         C2#2  4.328
#>   Thresholds GA33L$1                         C2#3 -2.561
#>   Thresholds GA33L$1                         C2#4 14.639
#>  m2_est . m1_se m2_se . m1_est_se m2_est_se . m1_pval
#>   1.137 | 1.591 2.298 |     1.204     0.495 |   0.229
#>   2.293 | 1.536 3.033 |     0.514     0.756 |   0.607
#>   2.559 | 1.486 2.767 |     1.044     0.925 |   0.297
#>   1.320 | 0.609 0.628 |     0.690     2.101 |   0.490
#>   0.873 | 0.468 1.039 |     2.382     0.841 |   0.017
#>   0.864 | 0.510 1.029 |     0.885     0.840 |   0.376
#>   0.477 | 0.624 0.616 |     1.435     0.775 |   0.151
#>   2.019 | 0.546 1.032 |     0.639     1.957 |   0.523
#>   1.220 | 0.542 0.878 |     3.015     1.391 |   0.003
#>   0.875 | 0.580 0.930 |     1.266     0.940 |   0.206
#>   0.788 | 0.505 0.911 |     1.408     0.865 |   0.159
#>   0.818 | 0.464 1.314 |     1.373     0.622 |   0.170
#>  -1.612 | 1.046 2.355 |    -1.267    -0.685 |   0.205
#>  -0.065 | 0.270 0.491 |     0.609    -0.133 |   0.543
#>  -0.370 | 0.269 0.487 |    -0.469    -0.759 |   0.639
#>   0.784 | 0.101 0.133 |    -7.092     5.902 |   0.000
#>  -2.033 | 0.097 0.144 |     9.911   -14.150 |   0.000
#>  -0.637 | 0.108 0.163 |   -19.444    -3.898 |   0.000
#>  -0.580 | 0.158 0.338 |    -1.882    -1.715 |   0.060
#>   3.150 | 0.147 0.656 |     5.636     4.802 |   0.000
#>  -2.236 | 1.017 0.363 |     3.697    -6.164 |   0.000
#>   0.937 | 0.266 0.223 |    -9.588     4.202 |   0.000
#>   0.192 | 0.239 0.717 |     1.667     0.267 |   0.096
#>   2.918 | 0.146 1.278 |     1.965     2.284 |   0.049
#>  -2.560 | 0.882 0.323 |     3.735    -7.916 |   0.000
#>   0.581 | 0.223 0.311 |   -12.652     1.865 |   0.000
#>  -2.164 | 0.781 5.381 |    -1.162    -0.402 |   0.245
#>   3.026 | 0.174 0.516 |     1.664     5.868 |   0.096
#>  -2.203 | 0.175 0.311 |    16.117    -7.089 |   0.000
#>   0.433 | 0.144 0.472 |   -15.195     0.917 |   0.000
#>   1.416 | 0.291 0.650 |     5.401     2.180 |   0.000
#>   4.450 | 0.563 2.507 |    -1.424     1.775 |   0.154
#>  -2.936 | 0.770 0.664 |     5.619    -4.418 |   0.000
#>  -0.378 | 0.186 0.874 |   -13.796    -0.433 |   0.000
#>   1.595 | 1.419 0.958 |    10.318     1.666 |   0.000
#>  -0.859 | 0.101 0.225 |    -7.092    -3.818 |   0.000
#>   1.132 | 0.097 0.137 |     9.911     8.270 |   0.000
#>  -2.197 | 0.108 0.166 |   -19.444   -13.253 |   0.000
#>  -0.141 | 0.158 0.237 |    -1.882    -0.597 |   0.060
#>   0.544 | 0.147 0.269 |     5.636     2.020 |   0.000
#>   3.836 | 1.017 1.458 |     3.697     2.631 |   0.000
#>  -2.883 | 0.266 0.600 |    -9.588    -4.809 |   0.000
#>   0.414 | 0.239 0.299 |     1.667     1.386 |   0.096
#>   0.103 | 0.146 0.298 |     1.965     0.344 |   0.049
#>   3.144 | 0.882 1.048 |     3.735     2.999 |   0.000
#>  -3.328 | 0.223 0.544 |   -12.652    -6.119 |   0.000
#>  -0.901 | 0.781 0.861 |    -1.162    -1.047 |   0.245
#>  -0.089 | 0.174 0.256 |     1.664    -0.349 |   0.096
#>   2.796 | 0.175 0.233 |    16.117    12.025 |   0.000
#>  -2.334 | 0.144 0.230 |   -15.195   -10.132 |   0.000
#>   1.726 | 0.291 0.523 |     5.401     3.303 |   0.000
#>  -0.847 | 0.563 0.673 |    -1.424    -1.259 |   0.154
#>   4.079 | 0.770 0.643 |     5.619     6.339 |   0.000
#>  -2.402 | 0.186 0.219 |   -13.796   -10.981 |   0.000
#>  15.000 | 1.419 0.000 |    10.318   999.000 |   0.000
#>  m2_pval
#>    0.621
#>    0.450
#>    0.355
#>    0.036
#>    0.401
#>    0.401
#>    0.438
#>    0.050
#>    0.164
#>    0.347
#>    0.387
#>    0.534
#>    0.494
#>    0.894
#>    0.448
#>    0.000
#>    0.000
#>    0.000
#>    0.086
#>    0.000
#>    0.000
#>    0.000
#>    0.789
#>    0.022
#>    0.000
#>    0.062
#>    0.688
#>    0.000
#>    0.000
#>    0.359
#>    0.029
#>    0.076
#>    0.000
#>    0.665
#>    0.096
#>    0.000
#>    0.000
#>    0.000
#>    0.550
#>    0.043
#>    0.009
#>    0.000
#>    0.166
#>    0.731
#>    0.003
#>    0.000
#>    0.295
#>    0.727
#>    0.000
#>    0.000
#>    0.001
#>    0.208
#>    0.000
#>    0.000
#>  999.000
#> 
#> 
#>   P-values that differ between models (p-value diff > 1e-04)
#>   -----------------------------------
#>  paramHeader   param                  LatentClass m1_est
#>      C2#1.ON    C1#1 Categorical.Latent.Variables  1.915
#>      C2#1.ON    C1#2 Categorical.Latent.Variables  0.789
#>      C2#1.ON    C1#3 Categorical.Latent.Variables  1.551
#>      C2#2.ON    C1#1 Categorical.Latent.Variables  0.420
#>      C2#2.ON    C1#2 Categorical.Latent.Variables  1.114
#>      C2#2.ON    C1#3 Categorical.Latent.Variables  0.451
#>      C2#3.ON    C1#1 Categorical.Latent.Variables  0.896
#>      C2#3.ON    C1#2 Categorical.Latent.Variables  0.349
#>      C2#3.ON    C1#3 Categorical.Latent.Variables  1.634
#>        Means    C1#1 Categorical.Latent.Variables  0.735
#>        Means    C1#2 Categorical.Latent.Variables  0.710
#>        Means    C1#3 Categorical.Latent.Variables  0.637
#>        Means    C2#1 Categorical.Latent.Variables -1.326
#>        Means    C2#2 Categorical.Latent.Variables  0.165
#>        Means    C2#3 Categorical.Latent.Variables -0.126
#>   Thresholds AB39M$1                         C1#4 -0.298
#>   Thresholds AB39T$1                         C1#4  0.398
#>   Thresholds AB39U$1                         C1#1  0.287
#>   Thresholds AB39U$1                         C1#3 -2.824
#>   Thresholds AB39U$1                         C1#4 -0.907
#>   Thresholds AB39W$1                         C1#1  0.289
#>   Thresholds AB39W$1                         C1#3 -2.186
#>   Thresholds AB39W$1                         C1#4  1.572
#>   Thresholds AB39X$1                         C1#1 -0.802
#>   Thresholds AB39X$1                         C1#3 -2.561
#>   Thresholds AB39X$1                         C1#4 14.639
#>   Thresholds GA33A$1                         C2#4 -0.298
#>   Thresholds GA33H$1                         C2#1  0.830
#>   Thresholds GA33H$1                         C2#2  3.759
#>   Thresholds GA33H$1                         C2#4  0.398
#>   Thresholds GA33I$1                         C2#1  0.287
#>   Thresholds GA33I$1                         C2#2  3.293
#>   Thresholds GA33I$1                         C2#4 -0.907
#>   Thresholds GA33K$1                         C2#1  0.289
#>   Thresholds GA33K$1                         C2#4  1.572
#>   Thresholds GA33L$1                         C2#1 -0.802
#>   Thresholds GA33L$1                         C2#4 14.639
#>  m2_est . m1_se m2_se . m1_est_se m2_est_se . m1_pval
#>   1.137 | 1.591 2.298 |     1.204     0.495 |   0.229
#>   2.293 | 1.536 3.033 |     0.514     0.756 |   0.607
#>   2.559 | 1.486 2.767 |     1.044     0.925 |   0.297
#>   1.320 | 0.609 0.628 |     0.690     2.101 |   0.490
#>   0.873 | 0.468 1.039 |     2.382     0.841 |   0.017
#>   0.864 | 0.510 1.029 |     0.885     0.840 |   0.376
#>   0.477 | 0.624 0.616 |     1.435     0.775 |   0.151
#>   2.019 | 0.546 1.032 |     0.639     1.957 |   0.523
#>   1.220 | 0.542 0.878 |     3.015     1.391 |   0.003
#>   0.875 | 0.580 0.930 |     1.266     0.940 |   0.206
#>   0.788 | 0.505 0.911 |     1.408     0.865 |   0.159
#>   0.818 | 0.464 1.314 |     1.373     0.622 |   0.170
#>  -1.612 | 1.046 2.355 |    -1.267    -0.685 |   0.205
#>  -0.065 | 0.270 0.491 |     0.609    -0.133 |   0.543
#>  -0.370 | 0.269 0.487 |    -0.469    -0.759 |   0.639
#>  -0.580 | 0.158 0.338 |    -1.882    -1.715 |   0.060
#>   0.192 | 0.239 0.717 |     1.667     0.267 |   0.096
#>   2.918 | 0.146 1.278 |     1.965     2.284 |   0.049
#>   0.581 | 0.223 0.311 |   -12.652     1.865 |   0.000
#>  -2.164 | 0.781 5.381 |    -1.162    -0.402 |   0.245
#>   3.026 | 0.174 0.516 |     1.664     5.868 |   0.096
#>   0.433 | 0.144 0.472 |   -15.195     0.917 |   0.000
#>   1.416 | 0.291 0.650 |     5.401     2.180 |   0.000
#>   4.450 | 0.563 2.507 |    -1.424     1.775 |   0.154
#>  -0.378 | 0.186 0.874 |   -13.796    -0.433 |   0.000
#>   1.595 | 1.419 0.958 |    10.318     1.666 |   0.000
#>  -0.141 | 0.158 0.237 |    -1.882    -0.597 |   0.060
#>   0.544 | 0.147 0.269 |     5.636     2.020 |   0.000
#>   3.836 | 1.017 1.458 |     3.697     2.631 |   0.000
#>   0.414 | 0.239 0.299 |     1.667     1.386 |   0.096
#>   0.103 | 0.146 0.298 |     1.965     0.344 |   0.049
#>   3.144 | 0.882 1.048 |     3.735     2.999 |   0.000
#>  -0.901 | 0.781 0.861 |    -1.162    -1.047 |   0.245
#>  -0.089 | 0.174 0.256 |     1.664    -0.349 |   0.096
#>   1.726 | 0.291 0.523 |     5.401     3.303 |   0.000
#>  -0.847 | 0.563 0.673 |    -1.424    -1.259 |   0.154
#>  15.000 | 1.419 0.000 |    10.318   999.000 |   0.000
#>  m2_pval
#>    0.621
#>    0.450
#>    0.355
#>    0.036
#>    0.401
#>    0.401
#>    0.438
#>    0.050
#>    0.164
#>    0.347
#>    0.387
#>    0.534
#>    0.494
#>    0.894
#>    0.448
#>    0.086
#>    0.789
#>    0.022
#>    0.062
#>    0.688
#>    0.000
#>    0.359
#>    0.029
#>    0.076
#>    0.665
#>    0.096
#>    0.550
#>    0.043
#>    0.009
#>    0.166
#>    0.731
#>    0.003
#>    0.295
#>    0.727
#>    0.001
#>    0.208
#>  999.000
#> 
#> 
#>   Parameters unique to model 1: 0
#>   -----------------------------
#> 
#>   None
#> 
#> 
#>   Parameters unique to model 2: 0
#>   -----------------------------
#> 
#>  None
#> 
#> 
#> ==============
```

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
<div id="oywhkvzwek" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#oywhkvzwek table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#oywhkvzwek thead, #oywhkvzwek tbody, #oywhkvzwek tfoot, #oywhkvzwek tr, #oywhkvzwek td, #oywhkvzwek th {
  border-style: none;
}

#oywhkvzwek p {
  margin: 0;
  padding: 0;
}

#oywhkvzwek .gt_table {
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

#oywhkvzwek .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#oywhkvzwek .gt_title {
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

#oywhkvzwek .gt_subtitle {
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

#oywhkvzwek .gt_heading {
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

#oywhkvzwek .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oywhkvzwek .gt_col_headings {
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

#oywhkvzwek .gt_col_heading {
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

#oywhkvzwek .gt_column_spanner_outer {
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

#oywhkvzwek .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oywhkvzwek .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oywhkvzwek .gt_column_spanner {
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

#oywhkvzwek .gt_spanner_row {
  border-bottom-style: hidden;
}

#oywhkvzwek .gt_group_heading {
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

#oywhkvzwek .gt_empty_group_heading {
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

#oywhkvzwek .gt_from_md > :first-child {
  margin-top: 0;
}

#oywhkvzwek .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oywhkvzwek .gt_row {
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

#oywhkvzwek .gt_stub {
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

#oywhkvzwek .gt_stub_row_group {
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

#oywhkvzwek .gt_row_group_first td {
  border-top-width: 2px;
}

#oywhkvzwek .gt_row_group_first th {
  border-top-width: 2px;
}

#oywhkvzwek .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oywhkvzwek .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#oywhkvzwek .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#oywhkvzwek .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oywhkvzwek .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oywhkvzwek .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oywhkvzwek .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#oywhkvzwek .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oywhkvzwek .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oywhkvzwek .gt_footnotes {
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

#oywhkvzwek .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#oywhkvzwek .gt_sourcenotes {
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

#oywhkvzwek .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#oywhkvzwek .gt_left {
  text-align: left;
}

#oywhkvzwek .gt_center {
  text-align: center;
}

#oywhkvzwek .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oywhkvzwek .gt_font_normal {
  font-weight: normal;
}

#oywhkvzwek .gt_font_bold {
  font-weight: bold;
}

#oywhkvzwek .gt_font_italic {
  font-style: italic;
}

#oywhkvzwek .gt_super {
  font-size: 65%;
}

#oywhkvzwek .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#oywhkvzwek .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#oywhkvzwek .gt_indent_1 {
  text-indent: 5px;
}

#oywhkvzwek .gt_indent_2 {
  text-indent: 10px;
}

#oywhkvzwek .gt_indent_3 {
  text-indent: 15px;
}

#oywhkvzwek .gt_indent_4 {
  text-indent: 20px;
}

#oywhkvzwek .gt_indent_5 {
  text-indent: 25px;
}

#oywhkvzwek .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#oywhkvzwek div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
  <tfoot>
    <tr class="gt_footnotes">
      <td class="gt_footnote" colspan="5"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'><em>Note.</em> Transition matrix values are the identical to Table 5, however Table 5
has the values rearranged by class for interpretation purposes. Classes may be arranged
directly through Mplus syntax using start values.</span></td>
    </tr>
  </tfoot>
</table>
</div>
```



<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
