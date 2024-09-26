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
#' @param hide Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' CohortCharacteristics::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#' @param formatEstimateName deprecated.
#' @param split deprecated.
#' @param excludeColumns deprecated.
#'
#' @examples
#' \donttest{
#' cdm <- mockCohortCharacteristics()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   tableCharacteristics()
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
                                 header = c("group"),
                                 groupColumn = NULL,
                                 hide = c(
                                   "result_id", "estimate_type",
                                   "additional_name", "additional_level"
                                 ),
                                 .options = list(),
                                 formatEstimateName = lifecycle::deprecated(),
                                 split = lifecycle::deprecated(),
                                 excludeColumns = lifecycle::deprecated()) {

  if (!inherits(result, "summarised_result")) {
    cli::cli_abort("result must be a summarised result")
  }
  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyResultTable(type = type))
  }

  estimateName <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "Mean (SD)" = "<mean> (<sd>)",
    "Range" = "<min> to <max>"
  )


   # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "summarise_characteristics")

  if (nrow(result) == 0) {
    cli::cli_warn("No characteristics results found")
    return(emptyResultTable(type = type))
  }

  omopgenerics::assertList(.options)

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

  if (nrow(result)==0){
    cli::cli_warn(
      "Output is empty, perhaps your result_type is not supported by this function."
    )
   suppressWarnings(
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
   )
  } else {

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
  }

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
