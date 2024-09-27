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
#' @param result A summarised_result object. Output of summariseCohortOverlap().
#' @param uniqueCombinations Whether to restrict to unique reference and
#' comparator comparisons.
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `tidyColumns(result)`.
#' @param groupColumn Columns to group by. See options with
#' `tidyColumns(result)`.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#'
#' tableCohortOverlap(overlap)
#'
#' mockDisconnect(cdm)
#' }
#'
#' @return A formatted table of the summariseOverlapCohort result.
#'
#' @export
#'
tableCohortOverlap <- function(result,
                               uniqueCombinations = TRUE,
                               type = "gt",
                               header = c("variable_name"),
                               groupColumn = c("cdm_name")) {
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
    hide = "variable_level"
  )

  return(tab)
}
