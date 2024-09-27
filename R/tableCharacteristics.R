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
#' @param result A summarised_result object. Output of
#' summariseCharacteristics().
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `tidyColumns(result)`.
#' @param groupColumn Columns to group by. See options with
#' `tidyColumns(result)`.
#'
#' @examples
#' \donttest{
#' cdm <- mockCohortCharacteristics()
#'
#' result <- summariseCharacteristics(cdm$cohort1)
#'
#' tableCharacteristics(result)
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the summariseCharacteristics
#' result.
#'
#' @export
#'
tableCharacteristics <- function(result,
                                 type = "gt",
                                 header = c("cdm_name", "cohort_name"),
                                 groupColumn = NULL) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_characteristics")

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_characteristics'` information.")
    return(emptyResultTable(type))
  }

  estimateName <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "Mean (SD)" = "<mean> (<sd>)",
    "Range" = "<min> to <max>"
  )

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = estimateName,
    header = header,
    groupColumn = groupColumn,
    type = type
  )

  return(tab)
}
