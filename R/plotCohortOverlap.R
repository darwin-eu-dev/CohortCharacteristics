# Copyright 2024 DARWIN EU (C)
#
# This file is part of CohortCharacteristics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Plot the result of summariseCohortOverlap.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summariseCohortOverlap result.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by
#' @param overlapLabel A glue expression to identify each plotted cohort
#' overlap.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- CohortCharacteristics::mockCohortCharacteristics()
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' plotCohortOverlap(overlap)
#' }
#'
plotCohortOverlap <- function(result,
                              facetVarX = "variable_name",
                              facetVarY = "strata_level",
                              colorVars = "variable_level",
                              overlapLabel = "{cohort_name_reference} &&& {cohort_name_comparator}",
                              uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(facetVarX, null.ok = TRUE)
  checkmate::assertCharacter(facetVarY, null.ok = TRUE)
  checkmate::assertCharacter(overlapLabel)
  checkmate::assertLogical(uniqueCombinations)




  # split table
  x <- result |>
    visOmopResults::tidy(splitStrata = FALSE) |>
    dplyr::mutate(group_level = glue::glue(.env$overlapLabel))

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  suppressMessages(data_to_plot <- result |>
                     dplyr::inner_join(x) |>
                     dplyr::filter(.data$estimate_type == "percentage") |>
                     dplyr::select(names(result)))

  return(
    plotCharacteristics(data_to_plot,
                        xAxis = "estimate_value",
                        yAxis = "group_level",
                        facetVarX = facetVarX,
                        facetVarY = facetVarY,
                        colorVars = colorVars,
                        plotStyle = "barplot")
  )
}
