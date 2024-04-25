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

#' Summarise counts for cohorts in a cohort table
#'
#' @param cohort A cohort table in the cdm.
#' @param cohortId A cohort definition id to restrict by. If NULL, all cohorts
#' will be included.
#'
#' @export
#'
#' @return  A summary of counts of the cohorts in the cohort table.
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- mockCohortCharacteristics()
#' summariseCohortCount(cohort = cdm$cohort1) |> dplyr::glimpse()
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
summariseCohortCount <- function(cohort,
                                 cohortId = NULL) {
  assertClass(cohort, "cohort_table")
  assertNumeric(cohortId, integerish = TRUE, min = 1, null = TRUE)

  cohort |> summaryInternal(cohortId = cohortId, resultType = "cohort_count")
}

summaryInternal <- function(cohort, cohortId, resultType) {
  res <- summary(cohort)
  if (!is.null(cohortId)) {
    res <- res |>
      visOmopResults::filterSettings(.data$cohort_definition_id %in% .env$cohortId)
  }
  res <- res |>
    visOmopResults::filterSettings(.data$result_type %in% .env$resultType) |>
    updateId()
  return(res)
}
updateId <- function(res) {
  resId <- res |>
    dplyr::select("result_id") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$result_id) |>
    dplyr::mutate("new_result_id" = dplyr::row_number())
  res <- res |>
    dplyr::inner_join(resId, by = "result_id") |>
    dplyr::select(-"result_id") |>
    dplyr::rename("result_id" = "new_result_id") |>
    omopgenerics::newSummarisedResult(
      settings = settings(res) |>
        dplyr::inner_join(resId, by = "result_id") |>
        dplyr::select(-"result_id") |>
        dplyr::rename("result_id" = "new_result_id")
    )
  return(res)
}
