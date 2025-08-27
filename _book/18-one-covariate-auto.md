


# Automatic 3-Step Covariate Only

------------------------------------------------------------------------

**Data source:**

This utilizes a dataset on undergraduate *Cheating* available from the `poLCA` package (Dayton, 1998): [See documentation here](https://cran.r-project.org/web/packages/poLCA/poLCA.pdf)

------------------------------------------------------------------------

## Load packages


``` r
library(MplusAutomation)
library(tidyverse) #collection of R packages designed for data science
library(here) #helps with filepaths
library(janitor) #clean_names
library(gt) # create tables
library(cowplot) # a ggplot theme
library(DiagrammeR) # create path diagrams
library(glue) # allows us to paste expressions into R code
library(data.table) # used for `melt()` function  
library(poLCA)
library(reshape2)
```

------------------------------------------------------------------------

## Automated Three-Step

------------------------------------------------------------------------

*Application: Undergraduate Cheating behavior*


"Dichotomous self-report responses by 319 undergraduates to four questions about cheating behavior" (poLCA, 2016).

------------------------------------------------------------------------

Prepare data


``` r

data(cheating)

cheating <- cheating %>% clean_names() 

df_cheat <-  cheating %>%                                  
  dplyr::select(1:4) %>%                                  
  mutate_all(funs(.-1)) %>%                                
  mutate(gpa = cheating$gpa)

# Detaching packages that mask the dpylr functions 
detach(package:poLCA, unload = TRUE)
detach(package:MASS, unload = TRUE)
```

------------------------------------------------------------------------

### R3STEP

**R3STEP** incorporates latent class predictors with mixture models. However, it is recommended to use the manual three-step.

------------------------------------------------------------------------


```{=html}
<div class="grViz html-widget html-fill-item" id="htmlwidget-19afcf04cdab44e274e7" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-19afcf04cdab44e274e7">{"x":{"diagram":" digraph cfa_model {\n\n# The `graph` statement - No editing needed\n\n    graph [layout = dot, overlap = true]\n \n# Two `node` statements\n \n# One for measured variables (box) \n\n    node [shape=box]\n    GPA LieExam LiePaper Fraud CopyExam;\n \n# One for latent variables (circle) \n \n    node [shape=circle]\n    bully [label=<Cheating <br/>Behavior <br/>C<sub>k=2<\/sub>>];\n    \n# `edge` statements\n \n    edge [minlen = 2]\n    bully -> {LieExam LiePaper Fraud CopyExam}\n    GPA -> bully [minlen = 4];\n    \n {rank = same; bully; GPA}\n \n }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```


------------------------------------------------------------------------

#### Run the **R3STEP** model with `gpa` as the latent class predictor


``` r

m_stepr  <- mplusObject(
  TITLE = "R3STEP - GPA as Predictor", 
  VARIABLE = 
   "categorical = lieexam-copyexam; 
    usevar = lieexam-copyexam;
    auxiliary = gpa (R3STEP);
    classes = c(2);",
  
  ANALYSIS = 
   "estimator = mlr; 
    type = mixture;
    starts = 500 100; 
    processors = 10;",
  
  OUTPUT = "sampstat patterns tech11 tech14;",
  
  PLOT = 
    "type = plot3; 
     series = lieexam-copyexam(*);",
  
  usevariables = colnames(df_cheat),
  rdata = df_cheat)

m_stepr_fit <- mplusModeler(m_stepr, 
                            dataout=here("three_step", "auto_3step", "r3step.dat"),
                            modelout=here("three_step", "auto_3step", "c2_r3step.inp") ,
                            check=TRUE, run = TRUE, hashfilename = FALSE)

```

------------------------------------------------------------------------

#### Regression slopes and odds ratios 


```
TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING
THE 3-STEP PROCEDURE

   WARNING:  LISTWISE DELETION IS APPLIED TO THE AUXILIARY VARIABLES IN THE
   ANALYSIS.  TO AVOID LISTWISE DELETION, DATA IMPUTATION CAN BE USED
   FOR THE AUXILIARY VARIABLES FOLLOWED BY ANALYSIS WITH TYPE=IMPUTATION.
   NUMBER OF DELETED OBSERVATIONS:  4
   NUMBER OF OBSERVATIONS USED:  315

                                                    Two-Tailed
                    Estimate       S.E.  Est./S.E.    P-Value

 C#1        ON
    GPA               -0.698      0.255     -2.739      0.006

 Intercepts
    C#1               -0.241      0.460     -0.523      0.601

Parameterization using Reference Class 1

 C#2        ON
    GPA                0.698      0.255      2.739      0.006

 Intercepts
    C#2                0.241      0.460      0.523      0.601


ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS
USING THE 3-STEP PROCEDURE

                                                95% C.I.
                    Estimate       S.E. Lower 2.5% Upper 2.5%

 C#1        ON
    GPA                0.498      0.127      0.302      0.820


Parameterization using Reference Class 1

 C#2        ON
    GPA                2.009      0.512      1.220      3.310
```



<div style="text-align: center;"><img src="images/ucsb_logo.png" width="75%" /></div>
