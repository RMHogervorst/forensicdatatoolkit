
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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(forensicdatatoolkit)
## basic example code
```

What is in the package (citations in separate file)

  - [x] Granularity-Related Inconsistency of Means (GRIM)
  - \[\] Granularity-Related Inconsistency of Means Mapped to Error
    Repeats (GRIMMER)
  - \[\] Sample Parameter Reconstruction via Iterative Techniques
    (SPRITE)

We can use these tools to investigate summary statistics. For example if
we collect statistics from a set of papers we can investigate them.

``` r
example_data <- data.frame(
  study = c(1, 1, 1, 2, 2, 2),
  description = rep(c("food acceptance", "darkness","likeability"),2),
  n = c(20, 20, 19, 40, 41, 40),
  means = c(3.5, 5.6, 2, 2.5, 8, 3),
  sds = c(1.34, 1.34, 2.56, 0.45, 2, 1),
  scale_min = rep(c(1,1,1),2),
  scale_max = rep(c(5, 9, 7))
)
example_data
#>   study     description  n means  sds scale_min scale_max
#> 1     1 food acceptance 20   3.5 1.34         1         5
#> 2     1        darkness 20   5.6 1.34         1         9
#> 3     1     likeability 19   2.0 2.56         1         7
#> 4     2 food acceptance 40   2.5 0.45         1         5
#> 5     2        darkness 41   8.0 2.00         1         9
#> 6     2     likeability 40   3.0 1.00         1         7
```

``` r
example_data$mean_possible <- grim(example_data$means, example_data$n)
example_data[,c("study","description","mean_possible")]
#>   study     description mean_possible
#> 1     1 food acceptance          TRUE
#> 2     1        darkness          TRUE
#> 3     1     likeability          TRUE
#> 4     2 food acceptance          TRUE
#> 5     2        darkness          TRUE
#> 6     2     likeability          TRUE
```

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
example_data %>% 
  mutate(mean_possible =grim(mean = means, n=n)) %>% 
  select(study, description, mean_possible)
#>   study     description mean_possible
#> 1     1 food acceptance          TRUE
#> 2     1        darkness          TRUE
#> 3     1     likeability          TRUE
#> 4     2 food acceptance          TRUE
#> 5     2        darkness          TRUE
#> 6     2     likeability          TRUE
```

CORVIDS ?
