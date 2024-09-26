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

#' Format a summariseOverlapCohort result into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summariseOverlapCohort result.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param groupColumn Column to use as group labels.
#' @param hide Columns to drop from the output table.
#' @param uniqueCombinations Whether to display unique combinations
#' reference - comparator.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- mockCohortCharacteristics()
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' tableCohortOverlap(overlap)
#' mockDisconnect(cdm = cdm)
#' }
#'
#' @return A formatted table of the summariseOverlapCohort result.
#'
#' @export
#'
tableCohortOverlap <- function(result,
                               type = "gt",
                               header = c("variable_name"),
                               groupColumn = c("cdm_name"),
                               hide = c("variable_level"),
                               uniqueCombinations = TRUE) {
  # validate result
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_overlap")

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_cohort_overlap'` information.")
    return(emptyResultTable(type))
  }

  result <- result |>
    dplyr::mutate(variable_name = dplyr::case_when(
      variable_name == "overlap" ~ "In both cohorts",
      variable_name == "comparator" ~ "Only in comparator cohort",
      variable_name == "reference" ~ "Only in reference cohort"
    ))

  # unique reference - comparator combinations
  if (uniqueCombinations) result <- getUniqueCombinationsSr(result)

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N (%)" = "<count> (<percentage>%)"),
    header = header,
    groupColumn = groupColumn,
    type = type,
    hide = hide
  )

  return(tab)
}
