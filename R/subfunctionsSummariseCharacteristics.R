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

#' Summarise counts for each different cohort. You can add a list of
#' stratifications.
#'
#' @param cohort A cohort in the cdm.
#' @param strata Stratification list.
#'
#' @return A summary of the number of individuals in each cohort and strata.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockCohortCharacteristics()
#'
#' cdm$cohort1 |>
#'   PatientProfiles::addSex() |>
#'   summariseCohortCounts(strata = "sex")
#' }
#'
summariseCohortCounts <- function(cohort,
                                  strata = list()) {
  summariseCharacteristics(
    cohort = cohort, strata = strata, demographics = FALSE
  )
}
