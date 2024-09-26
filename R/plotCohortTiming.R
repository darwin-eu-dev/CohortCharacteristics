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
#' @param facet variables to facet by.
#' @param colour Variables to use for colours.
#' @param colourName deprecated.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#' @param .options deprecated.
#'
#' @return A ggplot.
#' @export
#'
plotCohortTiming <- function(result,
                             plotType = "boxplot",
                             timeScale = "days",
                             facet = c("cdm_name", "cohort_name_reference"),
                             colour = c("cohort_name_comparator"),
                             colourName = lifecycle::deprecated(),
                             uniqueCombinations = FALSE,
                             .options = lifecycle::deprecated()) {
  if (lifecycle::is_present(colourName)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "plotCohortTiming(colourName = )",
      with = "ggplot2::labs(color = )"
    )
  }
  if (lifecycle::is_present(.options)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "plotCohortTiming(.options = )",
      with = "ggplot2::facet_wrap()"
    )
  }

  result <- omopgenerics::validateResultArgument(result) |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_timing")

  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyPlot())
  }

  # initial checks
  omopgenerics::assertChoice(plotType, c("boxplot", "density"))
  omopgenerics::assertChoice(timeScale, c("days", "years"))
  omopgenerics::assertLogical(uniqueCombinations)

  if (plotType == "boxplot") {
    result <- result |>
      dplyr::filter(
        .data$variable_name == "days_between_cohort_entries",
        !.data$estimate_name %in% c("density_x", "density_y"))
    if (timeScale == "years") {
      result <- result |>
        dplyr::mutate(
          estimate_value = as.character(as.numeric(.data$estimate_value) / 365.25)
        )
    }
  } else if (plotType == "density") {
    result <- result |>
      dplyr::filter(
        .data$variable_name == "days_between_cohort_entries",
        .data$estimate_name %in% c("density_x", "density_y"))
    if (timeScale == "years") {
      result <- result |>
        dplyr::mutate(estimate_value = dplyr::if_else(
          .data$estimate_name == "x",
          as.character(as.numeric(.data$estimate_value) / 365.25),
          .data$estimate_value
        ))
    }
  }

  xLab <- switch (timeScale,
    "days" = "Days between cohort entries",
    "years" = "Years between cohort entries",
  )

  if (nrow(result) == 0) {
    cli::cli_warn("No timing results found")
    return(emptyPlot())
  }

  if (uniqueCombinations) {
    result <- result |>
      getUniqueCombinationsSr()
  }

  if (plotType == "boxplot") {
    p <- visOmopResults::boxPlot(result, facet = facet, colour = colour) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = ggplot2::element_blank(),
        x = ggplot2::element_blank(),
        y = xLab
      )
  } else if (plotType == "density") {
    # plot scatter needs to allow x to be an estimate
    p <- result |>
      visOmopResults::scatterPlot(
        x = "density_x", y = "density_y", ymin = NULL, ymax = NULL, line = TRUE, point = FALSE,
        ribbon = FALSE, facet = facet, colour = colour, group = colour) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = ggplot2::element_blank(),
        x = xLab,
        y = ggplot2::element_blank()
      )
  }

  p <- addLine(p)

  return(p)
}

addLine <- function(p) {
  p +
    ggplot2::geom_vline(
      xintercept = 0, colour = "black", linetype = "longdash", alpha = 0.5)
}
