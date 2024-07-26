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
#' @param facet Variables to facet by.
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
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' plotCohortOverlap(overlap)
#' }
#'
plotCohortOverlap <- function(result,
                              facet = NULL,
                              uniqueCombinations = TRUE,
                              .options = list()) {

  rlang::check_installed("ggplot2")
  rlang::check_installed("ggpubr")
  rlang::check_installed("scales")

  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(facet, null.ok = TRUE)
  checkmate::assertLogical(uniqueCombinations)

  overlapLabel <- "{cohort_name_reference} &&& {cohort_name_comparator}"
  colorVars <- "variable_name"
  facetVarX <- NULL
  facetVarY <- NULL

  if (is.null(.options[["facetNcols"]])) {
    .options[["facetNcols"]] <- 1
  }
  if (is.null(.options[["facetScales"]])) {
    .options[["facetScales"]] <- "free_y"
  }

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

  data_to_plot <- data_to_plot |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    dplyr::mutate(group_level = stringr::str_replace_all(.data$group_level,
      pattern = "&&&",
      replacement = "and"
    ))

  data_to_plot <- data_to_plot |>
    dplyr::mutate(
      variable_name = dplyr::case_when(
        .data$variable_name == "overlap" ~ "in_both_cohorts",
        .data$variable_name == "comparator" ~ "only_in_comparator_cohort",
        .data$variable_name == "reference" ~ "only_in_reference_cohort"
      )
    ) |>
    dplyr::mutate(variable_name = stringr::str_replace_all(.data$variable_name,
      pattern = "_",
      replacement = " "
    )) |>
    dplyr::mutate(
      variable_name =
        stringr::str_to_sentence(.data$variable_name)
    ) |>
    dplyr::mutate(variable_name = factor(.data$variable_name,
      levels = c(
        "Only in comparator cohort",
        "In both cohorts",
        "Only in reference cohort"
      )
    ))


  lev <- rev(sort(unique(data_to_plot$group_level)))
  data_to_plot <- data_to_plot |>
    dplyr::mutate(group_level = factor(.data$group_level,
      levels = lev
    ))

  gg <- plotfunction(data_to_plot,
    xAxis = "estimate_value",
    yAxis = "group_level",
    facetVarX = facetVarX,
    facetVarY = facetVarY,
    colorVars = colorVars,
    plotStyle = "barplot",
    facet = facet,
    .options = .options
  )

  gg <- gg +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = ggplot2::element_blank(),
      xlab = "Percentage"
    ) +
    ggplot2::xlab("Percentage") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse = TRUE))


  gg
}
