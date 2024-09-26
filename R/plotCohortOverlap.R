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

#' Plot the result of summariseCohortOverlap.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object. Output of summariseCohortOverlap().
#' @param facet Variables to facet by.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#' @param y Variables to use in y axis, if NULL all variables not present in
#' facet are used.
#' @param .options deprecated.
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
                              facet = c("cdm_name", "cohort_name_reference"),
                              uniqueCombinations = FALSE,
                              y = NULL,
                              .options = lifecycle::deprecated()) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result) |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_overlap") |>
    dplyr::filter(.data$estimate_name == "percentage")
  if (nrow(result) == 0) {
    cli::cli_warn("No cohort overlap results found")
    return(emptyPlot())
  }

  # initial checks
  omopgenerics::assertCharacter(facet, null = TRUE)
  omopgenerics::assertCharacter(y, null = TRUE)
  omopgenerics::assertLogical(uniqueCombinations, length = 1)

  if (uniqueCombinations) {
    result <- result |>
      getUniqueCombinationsSr()
  }

  result <- result |>
    dplyr::mutate(
      variable_level = dplyr::case_when(
        .data$variable_name == "reference" ~ "Only in reference cohort",
        .data$variable_name == "comparator" ~ "Only in comparator cohort",
        .data$variable_name == "overlap" ~ "In both cohorts",
      ),
      variable_name = "Individuals"
    )

  if (is.null(y)) {
    y <- c(
      "cdm_name", visOmopResults::groupColumns(result),
      visOmopResults::strataColumns(result))
    y <- y[!y %in% facet]
  }

  p <- visOmopResults::barPlot(
    result = result,
    x = y,
    y = "percentage",
    colour = "variable_level",
    facet = facet
  ) +
    ggplot2::coord_flip() +
    ggplot2::ylim(c(0, 100.5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )

  return(p)
}
