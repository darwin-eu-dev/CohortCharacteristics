# Copyright 2022 DARWIN EU (C)
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

#' Create a ggplot from the output of summariseCharacteristics.
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' results <- summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#'   ),
#'   cohortIntersect = list(
#'     targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'   )
#' )
#'
#' plotCharacteristics(results)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCharacteristics <- function(data,
                                xAxis = "variable_name",
                                yAxis = "estimate_value",
                                plotStyle = "barplot",
                                facetVarX = NULL,
                                facetVarY = NULL,
                                colorVars = NULL,
                                vertical_x = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertTRUE(plotStyle %in% c("boxplot", "barplot", "density"), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)

  return(
    plotfunction(
      data,
      xAxis,
      yAxis,
      plotStyle = plotStyle,
      facetVarX,
      facetVarY,
      colorVars,
      vertical_x
    )
  )
}

#' Plot summariseDemographics output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- mockCohortCharacteristics()
#' results <- summariseDemographics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
#' )
#' graph <- plotDemographics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotDemographics <- function(data,
                             xAxis = "variable_name",
                             yAxis = "estimate_value",
                             plotStyle = "barplot",
                             facetVarX = "variable_name",
                             facetVarY = c("group_level", "strata_level"),
                             colorVars = "variable_level",
                             vertical_x = FALSE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_demographics"))

  if (plotStyle == "barplot") {
    return(
      CohortCharacteristics::plotCharacteristics(
        data = data |> dplyr::filter(.data$estimate_type == "percentage"),
        xAxis = xAxis,
        yAxis = yAxis,
        plotStyle = plotStyle,
        facetVarX = facetVarX,
        facetVarY = facetVarY,
        colorVars = colorVars,
        vertical_x = vertical_x
      )
    )
  }

  if (plotStyle == "boxplot") {
    return(
      CohortCharacteristics::plotCharacteristics(
        data = data,
        xAxis = xAxis,
        yAxis = yAxis,
        plotStyle = "boxplot",
        facetVarX = facetVarX,
        facetVarY = facetVarY,
        colorVars = colorVars,
        vertical_x = vertical_x
      )
    )
  }
}

#' Plot summariseCohortIntersect output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseCohortIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- mockCohortCharacteristics()
#' results <- summariseCohortIntersect(
#'   cohort = cdm$cohort1,
#'   cohortIntersect = list(
#'     "Medications in the prior year" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'     )
#'   )
#' )
#' graph <- plotCohortIntersect(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCohortIntersect <- function(data,
                                xAxis = "estimate_value",
                                yAxis = "variable_name",
                                plotStyle = "barplot",
                                facetVarX = "variable_name",
                                facetVarY = c("group_level", "strata_level"),
                                colorVars = "variable_level",
                                vertical_x = TRUE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_cohort_intersect"))

  gg <- CohortCharacteristics::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    colorVars = colorVars,
    facetVarX = facetVarX,
    facetVarY = facetVarY,
    vertical_x = vertical_x
  )

  return(gg)
}

#' Plot summariseTableIntersect output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseTableIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.

plotTableIntersect <- function(data,
                               xAxis = "variable_name",
                               yAxis = "estimate_value",
                               plotStyle = "boxplot",
                               facetVarX = "variable_name",
                               facetVarY = c("group_level", "strata_level"),
                               colorVars = NULL,
                               vertical_x = TRUE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_table_intersect"))

  gg <- CohortCharacteristics::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    facetVarX = facetVarX,
    facetVarY = facetVarY,
    colorVars = colorVars,
    vertical_x = vertical_x
  )

  return(gg)
}
