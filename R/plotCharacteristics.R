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
#' @param x what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facet Variables to facet by
#' @param colour  column in data to color by.
#' @param colourName Colour legend name
#' @param .options Additional plotting options.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' library(dplyr)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' results <- summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersectCount = list(
#'     tableName = "visit_occurrence", window = c(-365, -1)
#'   ),
#'   cohortIntersectFlag = list(
#'     targetCohortTable = "cohort2", window = c(-365, -1)
#'   )
#' )
#'
#' results |>
#'   filter(
#'     variable_name == "Cohort2 flag -365 to -1",
#'     estimate_name == "percentage"
#'   ) |>
#'   plotCharacteristics(
#'     plotStyle = "barplot",
#'     colour = "variable_level",
#'     x = "variable_level",
#'     facet = c(
#'       "cdm_name",
#'       "group_level",
#'       "strata_level"
#'     )
#'   )
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCharacteristics <- function(data,
                                x = "variable_name",
                                plotStyle = "barplot",
                                facet = NULL,
                                colour = NULL,
                                colourName = NULL,
                                .options = list()) {
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertTRUE(plotStyle %in% c("boxplot", "barplot", "density"), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)

  xAxis <- x
  yAxis <- "estimate_value"
  vertical_x <- FALSE

  # only allow one variable name to be used
  nVariableNames <- length(data |>
    dplyr::select("variable_name") |>
    dplyr::distinct() |>
    dplyr::pull())
  if (nVariableNames != 1) {
    emptyPlot(
      "Only one variable name can be plotted at a time.",
      "Please filter variable_name column in results before passing to plotCharacteristics()"
    )
  }

  nEstimateTypes <- length(data |>
    dplyr::select("estimate_type") |>
    dplyr::distinct() |>
    dplyr::pull())
  if (nEstimateTypes != 1) {
    emptyPlot(
      "Only one estimate type can be plotted at a time.",
      "Please filter estimate_type column in results before passing to plotCharacteristics()"
    )
  }

  estimateType <- data |>
    dplyr::select("estimate_type") |>
    dplyr::distinct() |>
    dplyr::pull()

  if (!estimateType %in% c("numeric", "percentage")) {
    emptyPlot(paste0(estimateType, " not currently supported by plotCharacteristics()"))
  }

  gg <- plotfunction(
    data,
    xAxis,
    yAxis,
    plotStyle = plotStyle,
    facetVarX = NULL,
    facetVarY = NULL,
    colorVars = colour,
    vertical_x,
    facet = facet,
    .options = .options
  )


  gg <- gg +
    ggplot2::theme_bw()


  if (estimateType == "numeric") {
    var <- unique(data$variable_name)

    if (xAxis == "estimate_value") {
      gg <- gg +
        ggplot2::ylab(var) +
        ggplot2::xlab("")
    }
    if (yAxis == "estimate_value") {
      gg <- gg +
        ggplot2::ylab(var) +
        ggplot2::xlab("")
    }
  }


  if (estimateType == "percentage") {
    if (xAxis == "estimate_value") {
      gg <- gg +
        ggplot2::xlab("Percentage") +
        ggplot2::ylab("")
    }
    if (yAxis == "estimate_value") {
      gg <- gg +
        ggplot2::ylab("Percentage") +
        ggplot2::xlab("")
    }
  }

  gg <- gg +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  if (!is.null(colourName)) {
    gg <- gg +
      ggplot2::labs(
        color = colourName,
        fill = colourName
      )
  } else {
    gg <- gg +
      ggplot2::labs(
        color = "",
        fill = ""
      )
  }


  gg
}
