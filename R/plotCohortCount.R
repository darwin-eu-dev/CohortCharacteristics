# Copyright 2024 DARWIN EU (C)
#
# This file is part of CohortCharacteristics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Plot the result of summariseCohortCount.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object. Output of summariseCohortCount().
#' @param facet Columns to facet by. See options with `tidyColumns(result)`.
#' Formula is also allowed to specify rows and columns.
#' @param colour Columns to color by. See options with `tidyColumns(result)`.
#' @param x Variables to use in x axis.
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockCohortCharacteristics(numberIndividuals = 100)
#' counts <- cdm$cohort2 |>
#'   addSex() |>
#'   addAge(ageGroup = list(c(0, 29), c(30, 59), c(60, Inf))) |>
#'   summariseCohortCount(strata = list("age_group", "sex", c("age_group", "sex"))) |>
#'   filter(variable_name == "Number subjects")
#' counts |>
#'   plotCohortCount(
#'     facet = cohort_name ~ age_group,
#'     colour = "sex",
#'     x = "sex")
#' }
#'
plotCohortCount <- function(result,
                            facet = c("cdm_name"),
                            colour = NULL,
                            x = NULL) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result) |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_count")
  if (nrow(result) == 0) {
    cli::cli_warn("No cohort count results found")
    return(emptyPlot())
  }

  opts <- oneVariable(result)

  p <- visOmopResults::barPlot(
    result = result,
    x = x,
    y = "count",
    colour = colour,
    facet = facet
  ) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = opts) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )

  return(p)
}

oneVariable <- function(result) {
  opts <- unique(result$variable_name)
  if (length(opts) > 1) {
    "Multiple variables present: {.var {opts}}. Please subset to one of them." |>
      cli::cli_abort()
  }
  return(opts)
}
