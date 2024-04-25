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
#' CohortCharacteristics::optionsTableCohortOverlap() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- CohortCharacteristics::mockCohortCharacteristics()
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' tableCohortOverlap(overlap)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A formatted table of the summariseOverlapCohort result.
#'
#' @export
#'
tableCohortOverlap  <- function(result,
                                type = "gt",
                                formatEstimateName = c("N (%)" = "<count> (<percentage>%)"),
                                header = c("strata"),
                                split = c("group", "strata", "additional"),
                                groupColumn = NULL,
                                excludeColumns = c("result_id", "estimate_type"),
                                .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "cohort_overlap")
  checkmate::assertList(.options)

  # default
  .options <- defaultOverlapOptions(.options)

  result <- result %>%
    dplyr::mutate(
      variable_name = dplyr::case_when(
        variable_name == "overlap" ~ "in_both_cohorts",
        variable_name == "comparator" ~ "only_in_comparator_cohort",
        variable_name == "reference" ~ "only_in_reference_cohort"
      )
    )

  # unique reference - comparator combinations
  if (.options$uniqueCombinations) {
    x <- result |>
      visOmopResults::splitGroup()
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference))) |>
      dplyr::mutate(variable_name = factor(.data$variable_name,
                                            levels = c("only_in_reference_cohort",
                                                       "in_both_cohorts",
                                                       "only_in_comparator_cohort"))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(
        c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level", "variable_name", "variable_level")
      ))) |>
      dplyr::mutate(
        variable_name = as.character(.data$variable_name),
        variable_level = NA_character_
      ) |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    x <- result |>
      dplyr::mutate(variable_name = factor(.data$variable_name,
                                            levels = c("only_in_reference_cohort",
                                                       "in_both_cohorts",
                                                       "only_in_comparator_cohort"))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(
        c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name", "variable_level")
      ))) |>
      dplyr::mutate(
        variable_name = as.character(.data$variable_name),
        variable_level = NA_character_
      )
  }

  # format table
  result <- visOmopResults::formatTable(result = x,
                                        formatEstimateName = formatEstimateName,
                                        header = c(header, "variable"),
                                        groupColumn = groupColumn,
                                        split = split,
                                        type = type,
                                        excludeColumns = excludeColumns,
                                        .options = .options)

  return(result)
}

defaultOverlapOptions <- function(userOptions) {
  defaultOpts <- list(
    uniqueCombinations = TRUE,
    c(integer = 0, percentage = 2, numeric = 2, proportion = 2),
    decimalMark = ".",
    bigMark = ",",
    style = "default",
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameCol = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}

#' Additional arguments for the function tableCohortOverlap.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCohortOverlap and their given default value.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableCohortOverlap()
#' }
#'
optionsTableCohortOverlap <- function() {
  return(defaultOverlapOptions(NULL))
}

formatOverlapEstimate <- function(count, percentage, .options) {
  paste0(
    niceNum(count, .options, "integer"),
    " (",
    niceNum(percentage, .options, "percentage"),
    "%)"
  )

}
niceNum <- function(num, .options, type) {
  trimws(format(round(num, .options$decimals[[type]]),
                big.mark = .options$bigMark,
                decimal.mark = .options$decimalMark,
                nsmall = .options$decimals[[type]],
                scientific = FALSE))
}
