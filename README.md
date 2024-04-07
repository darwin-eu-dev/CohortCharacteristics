
# CohortCharacteristics

[![CRAN
status](https://www.r-pkg.org/badges/version/CohortCharacteristics)](https://CRAN.R-project.org/package=CohortCharacteristics)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/CohortCharacteristics/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/CohortCharacteristics?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/CohortCharacteristics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/CohortCharacteristics/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Package overview

CohortCharacteristics contains functions for summarising characteristics
of OMOP CDM tables containing patient level data. The characteristics
that can be added include an individual´s demographics (sex, age, and
days of prior observation, …) and intersection with other tables
(standard tables, cohorts or concepts sets), from this intersections you
can summarise presence (flag), counts, time to event or date of event.
In addition, CohortCharacteristics also provides functionality visualise
this information in tables and figures.

## Package installation

You can install the latest version of CohortCharacteristics like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/CohortCharacteristics")
```
