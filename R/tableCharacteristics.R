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

#' Format a summarised_characteristics object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_characteristics object.
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
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' CohortCharacteristics::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   tableCharacteristics()
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the summariseCharacteristics
#' result.
#'
#' @export
#'
tableCharacteristics <- function(result,
                                 type = "gt",
                                 formatEstimateName = c(
                                   "N (%)" = "<count> (<percentage>%)",
                                   "N" = "<count>",
                                   "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                   "Mean (SD)" = "<mean> (<sd>)",
                                   "Range" = "<min> to <max>"
                                 ),
                                 header = c("group"),
                                 split = c("group", "strata"),
                                 groupColumn = NULL,
                                 excludeColumns = c(
                                   "result_id", "estimate_type",
                                   "additional_name", "additional_level"
                                 ),
                                 .options = list()) {

  # check input
  intersects <- tidyr::expand_grid(
    "type" = c("cohort", "concept", "table"),
    "value" = c("flag", "count", "date", "days")
  ) |>
    dplyr::mutate("x" = paste0(
      "summarised_", .data$type, "_intersect_", .data$value
    )) |>
    dplyr::pull("x")
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in% c(
      "summarised_characteristics", "summarised_demographics", intersects
    ))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # ensure results are nicely ordered
  defaultVariableNames <- c(
    "Number records", "Number subjects",
    "Cohort start date", "Cohort end date",
    "Sex",
    "Age", "Age group",
    "Prior observation",
    "Future observation"
  )
  variableNames <- result |>
    dplyr::select("variable_name") |>
    dplyr::filter(!.data$variable_name %in% .env$defaultVariableNames) |>
    dplyr::distinct() |>
    dplyr::pull("variable_name")

  variableLevels <- sort(result |>
    dplyr::select("variable_level") |>
    dplyr::filter(!is.na(.data$variable_level)) |>
    dplyr::distinct() |>
    dplyr::pull("variable_level"))

  result <- result |>
    dplyr::mutate(variable_name = factor(.data$variable_name,
      levels = c(
        defaultVariableNames,
        variableNames
      )
    )) |>
    dplyr::mutate(variable_level = factor(.data$variable_level,
      levels = variableLevels
    )) |>
    dplyr::arrange(.data$variable_name, .data$variable_level) |>
    dplyr::mutate(variable_name = as.character(.data$variable_name)) |>
    dplyr::mutate(variable_level = as.character(.data$variable_level))

  # format table
  result <- visOmopResults::visOmopTable(
    result = result,
    formatEstimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    split = split,
    type = type,
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(result)
}

defaultCharacteristicsOptions <- function(.options) {
  defaults <- list(
    "decimals" = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    "decimalMark" = ".",
    "bigMark" = ",",
    "keepNotFormatted" = TRUE,
    "useFormatOrder" = TRUE,
    "delim" = "\n",
    "style" = "default",
    "na" = "-",
    "title" = NULL,
    "subtitle" = NULL,
    "caption" = NULL,
    "groupAsColumn" = FALSE,
    "groupOrder" = NULL,
    "colsToMergeRows" = "all_columns"
  )

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }
  return(defaults)
}

#' Additional arguments for the function tableCharacteristics.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCharacteristics, and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#'   optionsTableCharacteristics()
#' }
#'
optionsTableCharacteristics <- function() {
  return(defaultCharacteristicsOptions(NULL))
}
