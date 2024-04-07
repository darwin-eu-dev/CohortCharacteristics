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

#' create a ggplot from the output of summariseLargeScaleCharacteristics.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseLargeScaleCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column.
#' Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column.
#' Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#'
#' @return A ggplot.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' concept <- dplyr::tibble(
#'   concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
#'   domain_id = NA_character_,
#'   vocabulary_id = NA_character_,
#'   concept_class_id = NA_character_,
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01")
#' ) |>
#'   dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#' cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' results <- cdm$cohort2 |>
#'   summariseLargeScaleCharacteristics(
#'     episodeInWindow = c("condition_occurrence"),
#'     minimumFrequency = 0
#'   )
#' graphs <- plotLargeScaleCharacteristics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotLargeScaleCharacteristics <- function(data,
                                          xAxis = "variable_name",
                                          yAxis = "estimate_value",
                                          facetVarX = c("variable_name"),
                                          facetVarY = c("group_level", "strata_level", "estimate_name"),
                                          colorVars = "variable_level",
                                          vertical_x = FALSE) {
  return(plotfunction(data,
    xAxis,
    yAxis,
    plotStyle = "scatterplot",
    facetVarX,
    facetVarY,
    colorVars,
    vertical_x = vertical_x
  ))
}
