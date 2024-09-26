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

#' Format a summariseCohortTiming result into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object. Output of summariseCohortTiming().
#' @param timeScale Time scale to plot results. Can be days or years.
#' @param uniqueCombinations Whether to restrict to unique reference and
#' comparator.
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with tidyColumns(result).
#' @param groupColumn Columns to use as group labels. See options with
#' tidyColumns(result).
#'
#' @return A formatted table of the summariseCohortTiming result.
#'
#' @export
#'
tableCohortTiming <- function(result,
                              timeScale = "days",
                              uniqueCombinations = TRUE,
                              type = "gt",
                              header = visOmopResults::strataColumns(result),
                              groupColumn = NULL) {
  # initial checks
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(timeScale, c("days", "years"))
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))
  omopgenerics::assertLogical(uniqueCombinations, length = 1)

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_cohort_timing") |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_cohort_timing'` information.")
    return(emptyResultTable(type))
  }

  if (timeScale == "years") {
    result <- changeDaysToYears(
      result, "days_between_cohort_entries", "years_between_cohort_entries"
    )
  }

  if (uniqueCombinations) result <- getUniqueCombinationsSr(result)

  # format table
  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c(
      "N" = "<count>",
      "Mean (SD)" = "<mean> (<sd>)",
      "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
      "Range" = "<min> to <max>"
    ),
    header = header,
    groupColumn = groupColumn,
    type = type
  )

  return(tab)
}
