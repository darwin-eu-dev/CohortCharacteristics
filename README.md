
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

    #> To cite package 'CohortCharacteristics' in publications use:
    #> 
    #>   Catala M, Guo Y, Du M, Lopez-Guell K, Burn E, Alcalde M (????).
    #>   _CohortCharacteristics: Summarise and Visualise Characteristics of
    #>   Patients in the OMOP CDM_. R package version 0.3.0,
    #>   <https://darwin-eu-dev.github.io/CohortCharacteristics/>.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {CohortCharacteristics: Summarise and Visualise Characteristics of Patients in the OMOP CDM},
    #>     author = {Marti Catala and Yuchen Guo and Mike Du and Kim Lopez-Guell and Edward Burn and Marta Alcalde},
    #>     note = {R package version 0.3.0},
    #>     url = {https://darwin-eu-dev.github.io/CohortCharacteristics/},
    #>   }

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
cdm <- mockCohortCharacteristics(numberIndividuals = 1000)
cdm
```

We can see that in this example data we have a cohort table called
cohort1.

``` r
cdm$cohort1
#> # Source:   table<main.cohort1> [?? x 4]
#> # Database: DuckDB v1.0.0 [root@Darwin 23.6.0:R 4.4.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    1        601 1959-12-06        1963-11-09     
#>  2                    3        940 2009-04-18        2013-09-08     
#>  3                    2        628 1908-06-27        1924-02-28     
#>  4                    3        512 1983-08-19        1988-04-07     
#>  5                    1         83 1974-12-15        1975-10-21     
#>  6                    1        569 1954-08-04        1975-02-04     
#>  7                    1        173 1917-12-28        1920-11-22     
#>  8                    1         67 1931-03-08        1934-02-16     
#>  9                    2        121 1979-07-24        1995-02-06     
#> 10                    1        655 1938-11-28        1939-07-28     
#> # ℹ more rows
```

With one line of code from CohortCharacteristics we can generate summary
statistics on this cohort.

``` r
cohort1_characteristics <- summariseCharacteristics(cdm$cohort1)
cohort1_characteristics |>
  glimpse()
#> Rows: 111
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK"…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "cohort_1", "cohort_3", "cohort_2", "cohort_1", "coho…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "Number records", "Number records", "Number records",…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "count", "count", "count", "count",…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "320", "351", "329", "320", "351", "329", "1903-08-03…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

And with another line we can create a table of these results.

``` r
tableCharacteristics(cohort1_characteristics, type = "tibble")
#> # A tibble: 17 × 7
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
#> # ℹ 3 more variables: `[header_name]Cohort name\n[header_level]cohort_1` <chr>,
#> #   `[header_name]Cohort name\n[header_level]cohort_3` <chr>,
#> #   `[header_name]Cohort name\n[header_level]cohort_2` <chr>
```

CohortCharacteristics provides a number of other functions to help
summarise cohort tables and present the results in publication-ready
tables and figures. See the vignettes for more details.
