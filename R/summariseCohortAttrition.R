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

#' Summarise attrition of a cohort_table
#'
#' @param cohort A cohort_table object.
#' @param cohortId A cohort_definition_id to restrict, if NULL, all cohorts are
#' used.
#'
#' @export
#'
#' @return A summarised_result_object with the attrition summarised.
#'
summariseCohortAttrition <- function(cohort,
                                     cohortId = NULL) {
  assertClass(cohort, "cohort_table")
  assertNumeric(cohortId, integerish = TRUE, min = 1, null = TRUE)

  cohort |>
    summaryInternal(cohortId = cohortId, resultType = "cohort_attrition")
}
