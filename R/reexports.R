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

#' @importFrom omopgenerics suppress
#' @export
omopgenerics::suppress

#' @importFrom omopgenerics settings
#' @export
omopgenerics::settings

#' It creates a mock database for testing PatientProfiles package
#'
#' @param connectionDetails Connection an details to create the cdm mock object.
#' @param drug_exposure default null user can define its own table.
#' @param drug_strength default null user can define its own table.
#' @param observation_period default null user can define its own table.
#' @param condition_occurrence default null user can define its own table.
#' @param visit_occurrence default null user can define its own visit_occurrence table.
#' @param person default null user can define its own table.
#' @param drug_concept_id_size number of unique drug concept id.
#' @param ingredient_concept_id_size number of unique drug ingredient concept id.
#' @param drug_exposure_size number of unique drug exposure.
#' @param patient_size number of unique patient.
#' @param min_drug_exposure_start_date user define minimum drug exposure start date.
#' @param max_drug_exposure_start_date user define maximum drug exposure start date.
#' @param seed seed.
#' @param condition_concept_id_size number of unique row in the condition concept table.
#' @param visit_concept_id_size number of unique visit concept id.
#' @param visit_occurrence_id_size number of unique visit occurrence id.
#' @param earliest_date_of_birth the earliest date of birth of patient in person table format "dd-mm-yyyy".
#' @param latest_date_of_birth the latest date of birth for patient in person table format "dd-mm-yyyy".
#' @param earliest_observation_start_date the earliest observation start date for patient format "dd-mm-yyyy".
#' @param latest_observation_start_date the latest observation start date for patient format "dd-mm-yyyy".
#' @param min_days_to_observation_end the minimum number of days of the observational integer.
#' @param max_days_to_observation_end the maximum number of days of the observation period integer.
#' @param earliest_condition_start_date the earliest condition start date for patient format "dd-mm-yyyy".
#' @param earliest_visit_start_date the earliest visit start date for patient format "dd-mm-yyyy".
#' @param latest_condition_start_date the latest condition start date for patient format "dd-mm-yyyy".
#' @param latest_visit_start_date the latest visit start date for patient format "dd-mm-yyyy".
#' @param min_days_to_condition_end the minimum number of days of the condition integer.
#' @param min_days_to_visit_end the minimum number of days of the visit integer.
#' @param max_days_to_condition_end the maximum number of days of the condition integer.
#' @param max_days_to_visit_end the maximum number of days of the visit integer.
#' @param concept_ancestor the concept ancestor table.
#' @param ancestor_concept_id_size the size of concept ancestor table.
#' @param cohort1 cohort table for test to run in getindication.
#' @param cohort2 cohort table for test to run in getindication.
#' @param ... user self defined tibble table to put in cdm, it can input as many as the user want.
#' @return cdm of the mock database following user's specifications.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
mockCohortCharacteristics <- PatientProfiles::mockPatientProfiles
