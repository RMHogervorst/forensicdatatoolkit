
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forensicdatatoolkit

<!-- badges: start -->

<!-- badges: end -->

The goal of forensicdatatoolkit is to combine several tools created for
investigating summary data and detect anomalies. This is not a tool for
finding fraud. Fraud is a legal term. These tools merely tell you if the
underlying data is probable or not. It can never tell us why the
statistics are wrong. Someone could lie or someone could mistype a
number. Only open data can tell us which it is.

## Installation

You can install the released version of forensicdatatoolkit from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("forensicdatatoolkit")
```

## Loading package

``` r
library(forensicdatatoolkit)
```

## Techniques available

What is in the package (citations in separate file) \[LINK to full
citations vignette\]

  - [x] Granularity-Related Inconsistency of Means (GRIM)
  - \[\] Granularity-Related Inconsistency of Means Mapped to Error
    Repeats (GRIMMER)
  - [x] Sample Parameter Reconstruction via Iterative Techniques
    (SPRITE)

We can use these tools to investigate summary statistics. For example if
we collect statistics from a set of papers we can investigate them.

Let’s imagin we have 2 published papers about food. The author let
several people taste chocolate bars and judge those bars. We use:

  - a 1-5 scale for food acceptance
  - a 1-9 scale for how dark the chocolate was and,
  - a 1-7 scale for if you like the chocolate

The study only reports the summary statistics (means and standard
deviations) for every scale.

``` r
example_data <- data.frame(
  study = c(1, 1, 1, 2, 2, 2),
  description = rep(c("food acceptance", "darkness", "likeability"), 2),
  n = c(20, 19, 19, 40, 41, 40),
  means = c(3.50, 5.60, 2.13, 2.54, 8.00, 3.05),
  sds = c(1.34, 1.34, 2.56, 0.45, 2, 1),
  scale_min = rep(c(1, 1, 1), 2),
  scale_max = rep(c(5, 9, 7))
)
example_data
#>   study     description  n means  sds scale_min scale_max
#> 1     1 food acceptance 20  3.50 1.34         1         5
#> 2     1        darkness 19  5.60 1.34         1         9
#> 3     1     likeability 19  2.13 2.56         1         7
#> 4     2 food acceptance 40  2.54 0.45         1         5
#> 5     2        darkness 41  8.00 2.00         1         9
#> 6     2     likeability 40  3.05 1.00         1         7
```

It is possible that the researcher dropped some chocolate on his work
when the values were being calculated. Fortunately we can do a
granularity check on the means to see if those means are possible given
the scale used, and the sample size. This check is known as the GRIM
test (Granularity-Related Inconsistency of Means). \[LINK TO PREPRINT
HERE\].

### GRIM (Granularity-Related Inconsistency of Means)

Let’s use an example: 20 people each picked an integer between 1 and 5
(because you cannot choose someting else). What happens to the mean
score if one person went from 1: disgusting to 2:awful? the mean score
would go up by 1/20th. The minimal stepsize is 1/20th or 0.05. That
means that every mean must be divisable by that stepsize. 3.5 is
therefore fine but 3.53 is not.

``` r
example_data$mean_possible <- grim(example_data$means, example_data$n)
example_data[, c("study", "description", "mean_possible")]
#>   study     description mean_possible
#> 1     1 food acceptance          TRUE
#> 2     1        darkness         FALSE
#> 3     1     likeability         FALSE
#> 4     2 food acceptance         FALSE
#> 5     2        darkness          TRUE
#> 6     2     likeability          TRUE
```

<details>

<summary> This also works with tidyverse verbs **click here to unhide
code** </summary>

``` r
library(dplyr)
example_data %>%
  mutate(
    mean_possible = grim(mean = means, n = n)
  ) %>%
  select(study, description, mean_possible)
```

</details>

CORVIDS ?

Please note that the ‘forensicdatatoolkit’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.

<details>

<summary> At the moment of creation (when I knitted this document ) this
was the state of my machine: **click here to expand** </summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 3.6.3 (2020-02-29)
#>  os       macOS Mojave 10.14.6        
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Europe/Amsterdam            
#>  date     2020-05-04                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package             * version    date       lib source        
#>  assertthat            0.2.1      2019-03-21 [1] CRAN (R 3.6.0)
#>  cli                   2.0.2      2020-02-28 [1] CRAN (R 3.6.0)
#>  colorspace            1.4-1      2019-03-18 [1] CRAN (R 3.6.0)
#>  crayon                1.3.4      2017-09-16 [1] CRAN (R 3.6.0)
#>  digest                0.6.25     2020-02-23 [1] CRAN (R 3.6.0)
#>  dplyr                 0.8.5      2020-03-07 [1] CRAN (R 3.6.0)
#>  ellipsis              0.3.0      2019-09-20 [1] CRAN (R 3.6.0)
#>  evaluate              0.14       2019-05-28 [1] CRAN (R 3.6.0)
#>  fansi                 0.4.1      2020-01-08 [1] CRAN (R 3.6.0)
#>  forensicdatatoolkit * 0.0.0.9000 2020-05-04 [1] local         
#>  ggplot2               3.3.0      2020-03-05 [1] CRAN (R 3.6.0)
#>  glue                  1.4.0      2020-04-03 [1] CRAN (R 3.6.2)
#>  gtable                0.3.0      2019-03-25 [1] CRAN (R 3.6.0)
#>  htmltools             0.4.0      2019-10-04 [1] CRAN (R 3.6.0)
#>  knitr                 1.28       2020-02-06 [1] CRAN (R 3.6.0)
#>  lifecycle             0.2.0      2020-03-06 [1] CRAN (R 3.6.0)
#>  magrittr              1.5        2014-11-22 [1] CRAN (R 3.6.0)
#>  munsell               0.5.0      2018-06-12 [1] CRAN (R 3.6.0)
#>  pillar                1.4.3      2019-12-20 [1] CRAN (R 3.6.0)
#>  pkgconfig             2.0.3      2019-09-22 [1] CRAN (R 3.6.0)
#>  purrr                 0.3.3      2019-10-18 [1] CRAN (R 3.6.0)
#>  R6                    2.4.1      2019-11-12 [1] CRAN (R 3.6.0)
#>  Rcpp                  1.0.4.6    2020-04-09 [1] CRAN (R 3.6.3)
#>  rlang                 0.4.5      2020-03-01 [1] CRAN (R 3.6.0)
#>  rmarkdown             2.1        2020-01-20 [1] CRAN (R 3.6.0)
#>  scales                1.1.0      2019-11-18 [1] CRAN (R 3.6.0)
#>  sessioninfo           1.1.1      2018-11-05 [1] CRAN (R 3.6.0)
#>  stringi               1.4.6      2020-02-17 [1] CRAN (R 3.6.0)
#>  stringr               1.4.0      2019-02-10 [1] CRAN (R 3.6.0)
#>  tibble                3.0.0      2020-03-30 [1] CRAN (R 3.6.2)
#>  tidyselect            1.0.0      2020-01-27 [1] CRAN (R 3.6.0)
#>  vctrs                 0.2.4      2020-03-10 [1] CRAN (R 3.6.0)
#>  withr                 2.1.2      2018-03-15 [1] CRAN (R 3.6.0)
#>  xfun                  0.13       2020-04-13 [1] CRAN (R 3.6.2)
#>  yaml                  2.2.1      2020-02-01 [1] CRAN (R 3.6.0)
#> 
#> [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
```

</details>
