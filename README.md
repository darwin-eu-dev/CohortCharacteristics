
# CohortCharacteristics <a href="https://darwin-eu-dev.github.io/CohortCharacteristics/"><img src="man/figures/logo.png" align="right" height="130" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/CohortCharacteristics)](https://CRAN.R-project.org/package=CohortCharacteristics)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/CohortCharacteristics/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/CohortCharacteristics?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/CohortCharacteristics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/CohortCharacteristics/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Package overview

CohortCharacteristics contains functions for summarising characteristics
of cohorts of patients identified in an OMOP CDM dataset. Once a cohort
table has been created, CohortCharacteristics provides a number of
functions to help provide a summary of the characteristics of the
individuals within the cohort.

## Package installation

You can install the latest version of CohortCharacteristics from CRAN:

``` r
install.packages("CohortCharacteristics")
```

Or from github:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/CohortCharacteristics")
```

## Example usage

The CohortCharacteristics package is designed to work with data in the
OMOP CDM format, so our first step is to create a reference to the data
using the CDMConnector package. For this example we will work with the
example Eunomia dataset.

``` r
library(CDMConnector)
library(CohortCharacteristics)
library(dplyr)
```

``` r
cdm <- mockCohortCharacteristics(patient_size = 1000, drug_exposure_size = 1000)
cdm
```

We can see that in this example data we have a cohort table called
cohort1.

``` r
cdm$cohort1
#> # Source:   table<main.cohort1> [4 x 4]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <dbl>      <dbl> <date>            <date>         
#> 1                    1          1 2020-01-01        2020-04-01     
#> 2                    1          1 2020-06-01        2020-08-01     
#> 3                    1          2 2020-01-02        2020-02-02     
#> 4                    2          3 2020-01-01        2020-03-01
```

With one line of code from CohortCharacteristics we can generate summary
statistics on this cohort.

``` r
cohort1_characteristics <- summariseCharacteristics(cdm$cohort1)
cohort1_characteristics |> 
  glimpse()
#> Rows: 70
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK"…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "cohort_1", "cohort_2", "cohort_1", "cohort_2", "coho…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "Number records", "Number records", "Number subjects"…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "count", "count", "min", "min", "q2…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "date", "…
#> $ estimate_value   <chr> "3", "1", "2", "1", "2020-01-01", "2020-01-01", "2020…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

And with another line we can create a table of these results.

``` r
tableCharacteristics(cohort1_characteristics, type = "tibble")
#> # A tibble: 17 × 6
#>    `CDM name` `Variable name`    `Variable level` `Estimate name`   
#>    <chr>      <chr>              <chr>            <chr>             
#>  1 PP_MOCK    Number records     <NA>             N                 
#>  2 PP_MOCK    Number subjects    <NA>             N                 
#>  3 PP_MOCK    Cohort start date  <NA>             Median [Q25 - Q75]
#>  4 PP_MOCK    Cohort start date  <NA>             Range             
#>  5 PP_MOCK    Cohort end date    <NA>             Median [Q25 - Q75]
#>  6 PP_MOCK    Cohort end date    <NA>             Range             
#>  7 PP_MOCK    Sex                Female           N (%)             
#>  8 PP_MOCK    Sex                Male             N (%)             
#>  9 PP_MOCK    Age                <NA>             Median [Q25 - Q75]
#> 10 PP_MOCK    Age                <NA>             Mean (SD)         
#> 11 PP_MOCK    Age                <NA>             Range             
#> 12 PP_MOCK    Prior observation  <NA>             Median [Q25 - Q75]
#> 13 PP_MOCK    Prior observation  <NA>             Mean (SD)         
#> 14 PP_MOCK    Prior observation  <NA>             Range             
#> 15 PP_MOCK    Future observation <NA>             Median [Q25 - Q75]
#> 16 PP_MOCK    Future observation <NA>             Mean (SD)         
#> 17 PP_MOCK    Future observation <NA>             Range             
#> # ℹ 2 more variables: `[header]Cohort name\n[header_level]Cohort 1` <chr>,
#> #   `[header]Cohort name\n[header_level]Cohort 2` <chr>
```

CohortCharacteristics provides a number of other functions to help
summarise cohort tables and present the results in publication-ready
tables and figures. See the vignettes for more details.
