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
#' @param result A summariseCohortTiming result
#' @param timeScale Time scale to plot results. Can be days or years.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options named list with additional formatting options.
#' CohortCharacteristics::optionsTableCohortTiming() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- CohortCharacteristics::mockCohortCharacteristics()
#' timing <- summariseCohortTiming(cdm$cohort2)
#' tableCohortTiming(timing)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A formatted table of the summariseCohortTiming result.
#'
#' @export
#'
tableCohortTiming <- function(result,
                              timeScale = "days",
                              type = "gt",
                              formatEstimateName = c(
                                "N" = "<count>",
                                "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                "Range" = "<min> - <max>"
                              ),
                              header = c("strata"),
                              split = c("group", "strata", "additional"),
                              groupColumn = NULL,
                              minCellCount = 5,
                              excludeColumns = c(
                                "result_id", "estimate_type", "variable_level"
                              ),
                              .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "cohort_timing")
  checkmate::assertList(.options)
  checkmate::assertChoice(timeScale, c("days", "years"))

  # defaults
  .options <- defaultTimingOptions(.options)

  if (.options$uniqueCombinations) {
    x <- result |>
      visOmopResults::splitGroup()
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level")))) |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    x <- result |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c("cdm_name", "group_name", "group_level", "strata_name", "strata_level"))))
  }




  if(timeScale == "years"){
    x <- dplyr::bind_rows(
      x |>
        dplyr::filter(.data$variable_name != "days_between_cohort_entries"),
      x |>
        dplyr::filter(.data$variable_name == "days_between_cohort_entries") |>
        dplyr::mutate(estimate_value =
                        as.character(as.numeric(.data$estimate_value)/365.25)) |>
        dplyr::mutate(variable_name = "years_between_cohort_entries")
          )

  }

  # format table
  result <- visOmopResults::visOmopTable(result = x,
                                        formatEstimateName = formatEstimateName,
                                        header = header,
                                        groupColumn = groupColumn,
                                        split = split,
                                        type = type,
                                        minCellCount = minCellCount,
                                        excludeColumns = excludeColumns,
                                        .options = .options)

  return(result)
}

defaultTimingOptions <- function(userOptions) {
  defaultOpts <- list(
    uniqueCombinations = TRUE,
    decimals = c(integer = 0, percentage = 2, numeric = 2, proportion = 2),
    decimalMark = ".",
    bigMark = ",",
    keepNotFormatted = TRUE,
    useFormatOrder = TRUE,
    delim = "\n",
    includeHeaderKey = TRUE,
    style = "default",
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}



#' Additional arguments for the function tableCohortTiming.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCohortTiming and their given default value.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableCohortTiming()
#' }
#'
#'
optionsTableCohortTiming <- function() {
  return(defaultTimingOptions(NULL))
}
