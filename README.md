
# CohortCharacteristics <a href="https://darwin-eu-dev.github.io/CohortCharacteristics/"><img src="man/figures/logo.png" align="right" height="130"/></a>

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
    #>   Catala M, Guo Y, Lopez-Guell K, Burn E, Mercade-Besora N, Alcalde M
    #>   (????). _CohortCharacteristics: Summarise and Visualise
    #>   Characteristics of Patients in the OMOP CDM_. R package version
    #>   0.3.0, <https://darwin-eu-dev.github.io/CohortCharacteristics/>.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {CohortCharacteristics: Summarise and Visualise Characteristics of Patients in the OMOP CDM},
    #>     author = {Marti Catala and Yuchen Guo and Kim Lopez-Guell and Edward Burn and Nuria Mercade-Besora and Marta Alcalde},
    #>     note = {R package version 0.3.0},
    #>     url = {https://darwin-eu-dev.github.io/CohortCharacteristics/},
    #>   }

## Package installation

You can install the latest version of CohortCharacteristics from CRAN:

``` r
install.packages("CohortCharacteristics")
```

Or install the development version from github:

``` r
install.packages("pak")
pak::pkg_install("darwin-eu-dev/CohortCharacteristics")
```

## Content

The package contain three types of functions:

- **summarise**\* type functions. These functions produce
  <summarised_result> standard output. See
  [omopgenerics](https://darwin-eu-dev.github.io/omopgenerics/articles/summarised_result.html)
  for more information on this standardised output format. These
  functions are the ones that do the work in terms of extracting the
  necessary data from the cdm and summarising it.
- **table**\* type functions. These functions work with the output of
  the summarise ones. They will produce a table visualisation created
  using the
  [visOmopresults](https://cran.r-project.org/package=visOmopResults)
  package.
- **plot**\* type functions. These functions work with the output of the
  summarise ones. They will produce a plot visualisation created using
  the
  [visOmopresults](https://cran.r-project.org/package=visOmopResults)
  package.

## Examples

### Mock data

The CohortCharacteristics package is designed to work with data in the
OMOP CDM format, so our first step is to create a reference to the data
using the CDMConnector package. For this example we will work with the
example Eunomia dataset.

``` r
library(CohortCharacteristics)
library(dplyr, warn.conflicts = FALSE)
```

``` r
cdm <- mockCohortCharacteristics(numberIndividuals = 1000)
#> Note: method with signature 'DBIConnection#Id' chosen for function 'dbExistsTable',
#>  target signature 'duckdb_connection#Id'.
#>  "duckdb_connection#ANY" would also be valid
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of PP_MOCK ────────────────────────────────────
#> • omop tables: person, observation_period, visit_occurrence,
#> condition_occurrence, drug_exposure, death
#> • cohort tables: cohort1, cohort2
#> • achilles tables: -
#> • other tables: -
```

We can see that in this example data we have a cohort table called
cohort1.

``` r
cdm$cohort1
#> # Source:   table<main.cohort1> [?? x 4]
#> # Database: DuckDB v1.0.0 [root@Darwin 23.6.0:R 4.4.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                   <int>      <int> <date>            <date>         
#>  1                    2        593 2010-01-26        2010-03-07     
#>  2                    3        695 1950-02-10        1954-01-25     
#>  3                    1        274 1959-11-29        1960-12-10     
#>  4                    2        507 1993-10-18        1995-09-03     
#>  5                    2        550 1980-11-11        1992-05-05     
#>  6                    3         46 1975-06-22        1979-07-03     
#>  7                    1         62 1976-04-26        1979-03-19     
#>  8                    2        749 1970-11-16        1976-10-08     
#>  9                    1        723 1966-12-28        1967-11-12     
#> 10                    3        963 1929-10-08        1930-11-24     
#> # ℹ more rows
```

### Cohort counts

### Cohort attrition

### Characteristics

With one line of code from CohortCharacteristics we can generate summary
statistics on this cohort.

``` r
cohort1_characteristics <- summariseCharacteristics(cdm$cohort1)
#> ℹ adding demographics columns
#> ℹ summarising data
#> ✔ summariseCharacteristics finished!
cohort1_characteristics |>
  glimpse()
#> Rows: 132
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK", "PP_MOCK"…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "cohort_1", "cohort_1", "cohort_1", "cohort_1", "coho…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "Number records", "Number subjects", "Cohort start da…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "min", "q25", "median", "q75", "max…
#> $ estimate_type    <chr> "integer", "integer", "date", "date", "date", "date",…
#> $ estimate_value   <chr> "355", "355", "1901-12-08", "1941-01-01", "1958-05-16…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

And with another line we can create a table of these results.

``` r
tableCharacteristics(cohort1_characteristics, type = "tibble")
#> ! Results have not been suppressed.
#> # A tibble: 20 × 6
#>    `Variable name`    `Variable level` `Estimate name`    [header_name]CDM nam…¹
#>    <chr>              <chr>            <chr>              <chr>                 
#>  1 Number records     <NA>             N                  355                   
#>  2 Number subjects    <NA>             N                  355                   
#>  3 Cohort start date  <NA>             Median [Q25 - Q75] 1958-05-16 [1941-01-0…
#>  4 Cohort start date  <NA>             Range              1901-12-08 to 2020-07…
#>  5 Cohort end date    <NA>             Median [Q25 - Q75] 1965-07-01 [1949-05-2…
#>  6 Cohort end date    <NA>             Range              1908-02-19 to 2027-01…
#>  7 Age                <NA>             Median [Q25 - Q75] 13 [5 - 24]           
#>  8 Age                <NA>             Mean (SD)          15.49 (12.22)         
#>  9 Age                <NA>             Range              0 to 53               
#> 10 Sex                Female           N (%)              168 (47.32%)          
#> 11 Sex                Male             N (%)              187 (52.68%)          
#> 12 Prior observation  <NA>             Median [Q25 - Q75] 4,848 [2,070 - 9,124] 
#> 13 Prior observation  <NA>             Mean (SD)          5,839.58 (4,466.56)   
#> 14 Prior observation  <NA>             Range              19 to 19,421          
#> 15 Future observation <NA>             Median [Q25 - Q75] 4,749 [2,336 - 8,313] 
#> 16 Future observation <NA>             Mean (SD)          5,686.77 (4,445.12)   
#> 17 Future observation <NA>             Range              15 to 21,907          
#> 18 Days in cohort     <NA>             Median [Q25 - Q75] 1,787 [583 - 4,118]   
#> 19 Days in cohort     <NA>             Mean (SD)          2,794.07 (2,917.60)   
#> 20 Days in cohort     <NA>             Range              4 to 14,127           
#> # ℹ abbreviated name:
#> #   ¹​`[header_name]CDM name\n[header_level]PP_MOCK\n[header_name]Cohort name\n[header_level]cohort_1`
#> # ℹ 2 more variables:
#> #   `[header_name]CDM name\n[header_level]PP_MOCK\n[header_name]Cohort name\n[header_level]cohort_2` <chr>,
#> #   `[header_name]CDM name\n[header_level]PP_MOCK\n[header_name]Cohort name\n[header_level]cohort_3` <chr>
```

CohortCharacteristics provides a number of other functions to help
summarise cohort tables and present the results in publication-ready
tables and figures. See the vignettes for more details.

### Timing between cohorts

### Overlap between cohort

### Large scale characteristics

### Disconnect

Disconnect from your database using `CDMConnector::cdmDisconnect()` to
close the connection or with `mockDisconnect()` to close connection and
delete the created mock data:

``` r
mockDisconnect(cdm)
```

### Recomendations

Although it is technically possible, we do not recommend to pipe table
or plot functions with the summarise ones. The main reason is that
summarise functions take some time to run, a large scale
characterisation in a big cdm object can take a few hours. If we pipe
the output to a table/plot function we loose the summarise result
object. In fact, some times we would send code around to be ran in
others database and what we want to export is the summarised_result
objects and not the table or plot which we would like to build after
compiling results from different cdm objects.

<p style="color:red">
Not recommended:
</p>

``` r
summariseCharacteristics() |>
  tableCharacteristics()
```
