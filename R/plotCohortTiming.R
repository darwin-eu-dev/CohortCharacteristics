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
#' @param timeScale Time scale to plot results. Can be days or years.
#' @param facet variables to facet by
#' @param colour Variables to use for colours
#' @param colourName Colour legend name
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#' @param .options Additional plotting options
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
                             timeScale = "days",
                             facet = NULL,
                             colour = NULL,
                             colourName = NULL,
                             uniqueCombinations = TRUE,
                             .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result)
  checkmate::assertChoice(plotType, c("boxplot", "density"))
  checkmate::assertChoice(timeScale, c("days", "years"))
  checkmate::assertCharacter(facet, null.ok = TRUE)
  checkmate::assertCharacter(colour, null.ok = TRUE)
  checkmate::assertCharacter(colourName, null.ok = TRUE, len = 1)
  checkmate::assertLogical(uniqueCombinations)
  if (plotType == "boxplot") {
    result <- result |>
      visOmopResults::filterSettings(.data$result_type == "cohort_timing")
  } else if (plotType == "density") {
    result <- result |>
      visOmopResults::filterSettings(.data$result_type == "cohort_timing_density")
    if (nrow(result) == 0) {
      cli::cli_abort("Please provide a cohort timing summarised result with density estimates (use `density = TRUE` in summariseCohortTiming).")
    }
  }

  colorVars <- colour
  facetVarX <- NULL
  facetVarY <- NULL

  if (is.null(.options[["facetNcols"]])) {
    .options[["facetNcols"]] <- 1
  }
  if (is.null(.options[["facetScales"]])) {
    .options[["facetScales"]] <- "free_y"
  }

  # split table
  timingLabel <- "{cohort_name_reference} &&& {cohort_name_comparator}"
  x <- result |>
    visOmopResults::tidy(splitStrata = FALSE) |>
    dplyr::mutate(group_level = glue::glue(.env$timingLabel))



  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  suppressMessages(data_to_plot <- result |>
    dplyr::inner_join(x) |>
    dplyr::select(names(result)))



  # Plotting
  data_to_plot <- data_to_plot |>
    dplyr::filter(.data$estimate_type == "numeric") |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    dplyr::mutate(group_level = stringr::str_replace_all(.data$group_level,
      pattern = "&&&",
      replacement = "to"
    ))

  if (timeScale == "years") {
    data_to_plot <- data_to_plot |>
      dplyr::mutate(estimate_value = .data$estimate_value / 365.25)
    if (plotType == "boxplot") {
      data_to_plot <- data_to_plot |>
        dplyr::mutate(variable_name = "years_between_cohort_entries")
    }
    xLab <- "Years between cohort entries"
  } else {
    xLab <- "Days between cohort entries"
  }

  if (plotType == "boxplot") {
    gg <- plotfunction(data_to_plot,
      xAxis = "estimate_value",
      yAxis = "group_level",
      facetVarX = facetVarX,
      facetVarY = facetVarY,
      colorVars = colorVars,
      plotStyle = "boxplot",
      facet = facet,
      .options = .options
    )
  } else if (plotType == "density") {
    data_to_plot <- data_to_plot |>
      dplyr::filter(.data$variable_name == "density")
    facet <- unique(c("group_level", facet))
    gg <- plotfunction(data_to_plot,
      xAxis = "estimate_value",
      yAxis = "group_level",
      facetVarX = facetVarX,
      facetVarY = facetVarY,
      colorVars = colorVars,
      vertical_x = TRUE,
      plotStyle = "density",
      facet = facet,
      .options = .options
    )
  }

  gg <- gg +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      y = xLab
    )

  if (!is.null(colourName)) {
    gg <- gg +
      ggplot2::labs(color = colourName)
  } else {
    gg <- gg +
      ggplot2::labs(color = "")
  }

  return(gg)
}
