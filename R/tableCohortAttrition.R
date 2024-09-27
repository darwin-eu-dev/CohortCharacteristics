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

#' Create a visual table from the output of summariseCohortAttrition.
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object. Output of
#' summariseCohortAttrition().
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `tidyColumns(result)`.
#' @param groupColumn Columns to group by. See options with
#' `tidyColumns(result)`.
#'
#' @return A visual table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' result <- summariseCohortAttrition(cdm$cohort2)
#'
#' tableCohortAttrition(result)
#'
#' mockDisconnect(cdm)
#' }
#'
tableCohortAttrition <- function(result,
                                 type = "gt",
                                 header = "variable_name",
                                 groupColumn = c("cdm_name", "cohort_name")) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_attrition")

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_cohort_attrition'` information.")
    return(emptyResultTable(type))
  }

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N" = "<count>"),
    header = header,
    groupColumn = groupColumn,
    type = type,
    hide = c("variable_level", "reason_id", "estimate_name")
  )

  return(tab)
}

emptyResultTable <- function(type) {
  x <- dplyr::tibble(`Table has no data` = character())
  if(type == "gt") {
    result <- gt::gt(x)
  } else if(type == "flextable") {
    result <- flextable::flextable(x)
  } else {
    result <- x
  }
  result
}
