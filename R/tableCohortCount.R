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

#' Format a summarise_characteristics object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarise_characteristics object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param groupColumn Column to use as group labels.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' result <- summariseCohortCount(cdm$cohort1)
#'
#' tableCohortCount(result)
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the summariseCohortCount result
#' result.
#'
#' @export
#'
tableCohortCount <- function(result,
                             type = "gt",
                             header = "cohort_name",
                             groupColumn = NULL) {
  # validate result
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_count")

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_cohort_count'` information.")
    return(emptyResultTable(type))
  }

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N" = "<count>"),
    header = header,
    groupColumn = groupColumn,
    type = type,
    hide = "variable_level"
  )

  return(tab)
}
