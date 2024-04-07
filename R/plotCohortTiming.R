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

#' Plot summariseCohortTiming results.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summariseCohortTiming result.
#' @param plotType Type of desired formatted table, possibilities are "boxplot" and
#' "density".
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars Column names to distinct by colors. default set to group_level
#' @param timingLabel A glue expression to identify each plotted cohort
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
#' timing <- summariseCohortTiming(cdm$cohort2)
#' plotCohortTiming(timing)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotCohortTiming <- function(result,
                             plotType = "boxplot",
                             facetVarX = "variable_name",
                             facetVarY = "group_level",
                             colorVars = "group_level",
                             timingLabel = "{cohort_name_reference} &&& {cohort_name_comparator}",
                             uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(plotType, c("boxplot", "density"))
  checkmate::assertCharacter(facetVarX, null.ok = TRUE)
  checkmate::assertCharacter(facetVarY, null.ok = TRUE)
  checkmate::assertCharacter(colorVars, null.ok = TRUE)
  checkmate::assertCharacter(timingLabel)
  checkmate::assertLogical(uniqueCombinations)
  if (plotType == "density" & !"density"%in% result$variable_name) {
    cli::cli_abort("Please provide a cohort timing summarised result with density estimates (use `density = TRUE` in summariseCohortTiming).")
  }




  # split table
  x <- result |> visOmopResults::tidy(splitStrata = FALSE) |>
    dplyr::mutate(group_level = glue::glue(.env$timingLabel))



  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  suppressMessages(data_to_plot <- result |>
                     dplyr::inner_join(x) |>
                     dplyr::select(names(result)))



  # Plotting
  if (plotType == "boxplot") {
    return(
      gg <- plotCharacteristics(data_to_plot,
                                xAxis = "estimate_value",
                                yAxis = "group_level",
                                facetVarX = facetVarX,
                                facetVarY = facetVarY,
                                colorVars = colorVars,
                                plotStyle = "boxplot")
    )
  } else if (plotType == "density") {
    data_to_plot <- data_to_plot |> dplyr::filter(.data$variable_name == "density")
    gg <- plotCharacteristics(data_to_plot,
                              xAxis = "estimate_value",
                              yAxis = "group_level",
                              facetVarX = facetVarX,
                              facetVarY = facetVarY,
                              colorVars = colorVars,
                              vertical_x = TRUE,
                              plotStyle = "density")

  }

  return(gg)
}
