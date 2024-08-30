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

#' Summarise characteristics of cohorts in a cohort table
#'
#' @param cohort A cohort table in the cdm.
#' @param cohortId Vector of cohort definition ids to include. If NULL all
#' cohort will be selected.
#' @param strata A list of variables to stratify results. These variables
#' must have been added as additional columns in the cohort table.
#' @param counts TRUE or FALSE. If TRUE, record and person counts will
#' be produced.
#' @param demographics TRUE or FALSE. If TRUE, patient demographics (cohort
#' start date, cohort end date, age, sex, prior observation, and future
#' observation will be summarised).
#' @param ageGroup A list of age groups to stratify results by.
#' @param tableIntersectFlag A list of arguments that uses
#' PatientProfiles::addTableIntersectFlag() to add variables to summarise.
#' @param tableIntersectCount A list of arguments that uses
#' PatientProfiles::addTableIntersectCount() to add variables to summarise.
#' @param tableIntersectDate A list of arguments that uses
#' PatientProfiles::addTableIntersectDate() to add variables to summarise.
#' @param tableIntersectDays A list of arguments that uses
#' PatientProfiles::addTableIntersectDays() to add variables to summarise.
#' @param cohortIntersectFlag A list of arguments that uses
#' PatientProfiles::addCohortIntersectFlag() to add variables to summarise.
#' @param cohortIntersectCount A list of arguments that uses
#' PatientProfiles::addCohortIntersectCount() to add variables to summarise.
#' @param cohortIntersectDate A list of arguments that uses
#' PatientProfiles::addCohortIntersectDate() to add variables to summarise.
#' @param cohortIntersectDays A list of arguments that uses
#' PatientProfiles::addCohortIntersectDays() to add variables to summarise.
#' @param conceptIntersectFlag A list of arguments that uses
#' PatientProfiles::addConceptIntersectFlag() to add variables to summarise.
#' @param conceptIntersectCount A list of arguments that uses
#' PatientProfiles::addConceptIntersectCount() to add variables to summarise.
#' @param conceptIntersectDate A list of arguments that uses
#' PatientProfiles::addConceptIntersectDate() to add variables to summarise.
#' @param conceptIntersectDays A list of arguments that uses
#' PatientProfiles::addConceptIntersectDays() to add variables to summarise.
#' @param otherVariables Other variables contained in cohort that you want to be
#' summarised.
#' @param otherVariablesEstimates Name of the estimates for the otherVariables
#' columns.
#'
#' @return A summary of the characteristics of the cohorts in the cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(CohortCharacteristics)
#' library(PatientProfiles)
#'
#' cdm <- mockCohortCharacteristics()
#' cdm$cohort1 |>
#'   addSex() |>
#'   addAge(
#'     ageGroup = list(c(0, 40), c(41, 150))
#'   ) |>
#'   summariseCharacteristics(
#'     strata = list("sex", "age_group"),
#'     cohortIntersectFlag = list (
#'       "Cohort 2 Flag" = list(
#'         targetCohortTable = "cohort2",
#'         window = c(-365, 0)
#'       )
#'     ),
#'     cohortIntersectCount = list (
#'       "Cohort 2 Count" = list(
#'         targetCohortTable = "cohort2",
#'         window = c(-365, 0)
#'       )
#'     )
#'   ) |>
#'   glimpse()
#'
#' mockDisconnect(cdm = cdm)
#' }
summariseCharacteristics <- function(cohort,
                                     cohortId = NULL,
                                     strata = list(),
                                     counts = TRUE,
                                     demographics = TRUE,
                                     ageGroup = NULL,
                                     tableIntersectFlag = list(),
                                     tableIntersectCount = list(),
                                     tableIntersectDate = list(),
                                     tableIntersectDays = list(),
                                     cohortIntersectFlag = list(),
                                     cohortIntersectCount = list(),
                                     cohortIntersectDate = list(),
                                     cohortIntersectDays = list(),
                                     conceptIntersectFlag = list(),
                                     conceptIntersectCount = list(),
                                     conceptIntersectDate = list(),
                                     conceptIntersectDays = list(),
                                     otherVariables = character(),
                                     otherVariablesEstimates = c("min", "q25", "median", "q75", "max", "count", "percentage")) {
  # check initial tables
  cdm <- omopgenerics::cdmReference(cohort)
  checkX(cohort)
  checkmate::assertLogical(demographics, any.missing = FALSE, len = 1)
  checkCdm(cdm)
  if (!is.list(strata)) {
    strata <- list(strata)
  }
  strata <- checkStrata(strata, cohort)
  ageGroup <- checkAgeGroup(ageGroup)
  assertLogical(counts)
  tableIntersectFlag <- assertIntersect(tableIntersectFlag)
  tableIntersectCount <- assertIntersect(tableIntersectCount)
  tableIntersectDate <- assertIntersect(tableIntersectDate)
  tableIntersectDays <- assertIntersect(tableIntersectDays)
  cohortIntersectFlag <- assertIntersect(cohortIntersectFlag)
  cohortIntersectCount <- assertIntersect(cohortIntersectCount)
  cohortIntersectDate <- assertIntersect(cohortIntersectDate)
  cohortIntersectDays <- assertIntersect(cohortIntersectDays)
  conceptIntersectFlag <- assertIntersect(conceptIntersectFlag)
  conceptIntersectCount <- assertIntersect(conceptIntersectCount)
  conceptIntersectDate <- assertIntersect(conceptIntersectDate)
  conceptIntersectDays <- assertIntersect(conceptIntersectDays)
  otherVariables <- checkOtherVariables(otherVariables, cohort)
  otherVariablesEstimates <- checkOtherVariablesEstimates(otherVariablesEstimates, otherVariables)
  assertNumeric(cohortId, integerish = TRUE, null = TRUE)
  ids <- omopgenerics::settings(cohort)$cohort_definition_id
  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- !cohortId %in% ids
    if (sum(indNot) > 0) {
      if (sum(indNot) == length(cohortId)) {
        cli::cli_abort("No valid cohort ids supplied.")
      } else {
        cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table and won't be used.")
        cohortId <- cohortId[!indNot]
      }
    }
  }

  srSet <- dplyr::tibble(
    "result_id" = 1L,
    "package_name" = "CohortCharacteristics",
    "package_version" = as.character(utils::packageVersion(
      "CohortCharacteristics"
    )),
    "result_type" = "summarise_characteristics"
  )

  # return empty result if no analyses chosen
  if (length(strata) == 0 &&
    isFALSE(counts) &&
    isFALSE(demographics) &&
    is.null(ageGroup) &&
    length(tableIntersectFlag) == 0 &&
    length(tableIntersectCount) == 0 &&
    length(tableIntersectDate) == 0 &&
    length(tableIntersectDays) == 0 &&
    length(cohortIntersectFlag) == 0 &&
    length(cohortIntersectCount) == 0 &&
    length(cohortIntersectDate) == 0 &&
    length(cohortIntersectDays) == 0 &&
    length(conceptIntersectFlag) == 0 &&
    length(conceptIntersectCount) == 0 &&
    length(conceptIntersectDate) == 0 &&
    length(conceptIntersectDays) == 0 &&
    all(lengths(otherVariables) == 0)) {
    return(
      omopgenerics::emptySummarisedResult() |>
        omopgenerics::newSummarisedResult(settings = srSet)
    )
  }

  # functions
  functions <- list(
    date = c("min", "q25", "median", "q75", "max"),
    numeric = c(
      "min", "q25", "median", "q75", "max", "mean", "sd"
    ),
    categorical = c("count", "percentage"),
    binary = c("count", "percentage")
  )

  # select necessary variables
  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", dplyr::all_of(unique(unlist(strata))),
      dplyr::all_of(unique(unlist(otherVariables)))
    )

  if (cohort |> dplyr::tally() |> dplyr::pull() == 0) {
    if (any(c("subject_id", "person_id") %in% colnames(cohort))) {
      variables <- c("number subjects", "number records")
    } else {
      variables <- "number records"
    }
    result <- dplyr::tibble(
      "result_id" = as.integer(1),
      "cdm_name" = CDMConnector::cdmName(cdm),
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "variable_name" = variables,
      "variable_level" = as.character(NA),
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "estimate_value" = "0",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
      omopgenerics::newSummarisedResult(settings = srSet)
    return(result)
  }

  dic <- dplyr::tibble(
    short_name = character(), new_variable_name = character(),
    new_variable_level = character(), table = character(), window = character(),
    value = character()
  )
  variables <- list()

  # demographics
  if (demographics) {
    cli::cli_alert_info("adding demographics columns")

    sex <- uniqueVariableName()
    age <- uniqueVariableName()
    priorObservation <- uniqueVariableName()
    futureObservation <- uniqueVariableName()
    demographicsCategorical <- sex

    if (!is.null(ageGroup)) {
      # default names
      ageGroup <- checkAgeGroup(ageGroup)

      # update names
      newNames <- uniqueVariableName(length(ageGroup))
      dic <- dic |>
        dplyr::union_all(dplyr::tibble(
          short_name = newNames,
          new_variable_name = names(ageGroup),
          new_variable_level = as.character(NA),
          table = as.character(NA),
          window = as.character(NA),
          value = as.character(NA)
        ))
      names(ageGroup) <- newNames
      demographicsCategorical <- c(demographicsCategorical, newNames)
    }
    dic <- dic |>
      dplyr::union_all(dplyr::tibble(
        short_name = c(sex, age, priorObservation, futureObservation),
        new_variable_name = c(
          "sex", "age", "prior_observation", "future_observation"
        ),
        new_variable_level = as.character(NA),
        table = as.character(NA),
        window = as.character(NA),
        value = as.character(NA)
      ))

    # add demographics
    cohort <- cohort |>
      PatientProfiles::addDemographics(
        ageGroup = ageGroup,
        sexName = sex,
        ageName = age,
        priorObservationName = priorObservation,
        futureObservationName = futureObservation
      )

    # update summary settings
    variables <- variables |>
      updateVariables(
        date = c("cohort_start_date", "cohort_end_date"),
        numeric = c(priorObservation, futureObservation, age),
        categorical = demographicsCategorical
      )
  }

  # intersects
  intersects <- c(
    "tableIntersectFlag", "tableIntersectCount", "tableIntersectDate",
    "tableIntersectDays", "cohortIntersectFlag", "cohortIntersectCount",
    "cohortIntersectDate", "cohortIntersectDays", "conceptIntersectFlag",
    "conceptIntersectCount", "conceptIntersectDate", "conceptIntersectDays"
  )
  for (intersect in intersects) {
    values <- eval(parse(text = intersect))
    funName <- paste0(
      "PatientProfiles::add", toupper(substr(intersect, 1, 1)),
      substr(intersect, 2, nchar(intersect))
    )
    value <- getValue(intersect)
    type <- getType(intersect)

    for (k in seq_along(values)) {
      cli::cli_inform(c("i" = "adding {intersect} {k}/{length(values)}"))

      val <- values[[k]]

      if (type == "cohort") {
        # cohort variables
        cohortInterest <- val$targetCohortTable
        set <- settings(cdm[[cohortInterest]])
        shortNames <- uniqueVariableName(nrow(set))
        attr(cdm[[cohortInterest]], "cohort_set") <- set |>
          dplyr::select("cohort_definition_id") |>
          dplyr::mutate("cohort_name" = shortNames)
        val$nameStyle <- "{cohort_name}"
        attr(cohort, "cdm_reference") <- cdm
        # update dic
        addDic <- dplyr::tibble(
          "new_variable_level" = set$cohort_name,
          "table" = cohortInterest
        )
      } else if (type == "concept") {
        # concept variables
        shortNames <- uniqueVariableName(length(val$conceptSet))
        val$nameStyle <- "{concept_name}"
        # update dic
        addDic <- dplyr::tibble(
          "new_variable_level" = names(val$conceptSet),
          "table" = NA
        )
        names(val$conceptSet) <- shortNames
      } else if (type == "table") {
        # table variables
        shortNames <- uniqueVariableName()
        val$nameStyle <- shortNames
        # update dic
        addDic <- dplyr::tibble(
          "new_variable_level" = NA,
          "table" = val$tableName
        )
      }
      dic <- dic |>
        dplyr::union_all(
          addDic |>
            dplyr::mutate(
              "short_name" = shortNames,
              "new_variable_name" = names(values)[k],
              "window" = names(val$window),
              "value" = value
            )
        )

      if (value == "date") {
        variables <- variables |> updateVariables(date = shortNames)
      } else if (value %in% c("days", "count")) {
        variables <- variables |> updateVariables(numeric = shortNames)
      } else if (value == "flag") {
        variables <- variables |> updateVariables(binary = shortNames)
      }

      val$x <- cohort

      cohort <- do.call(eval(parse(text = funName)), val)

      if (type == "cohort") {
        attr(cdm[[cohortInterest]], "cohort_set") <- set
      }
    }
  }

  # update cohort_names
  cohort <- cohort |> PatientProfiles::addCohortName()

  # detect other variables
  variables <- variables[lengths(variables) > 0]
  estimates <- functions[names(variables)]

  otherVariables <- otherVariables[lengths(otherVariables) > 0]
  otherVariablesEstimates <- otherVariablesEstimates[names(otherVariables)]

  variables <- c(variables, otherVariables)
  estimates <- c(estimates, otherVariablesEstimates)

  cli::cli_alert_info("summarising data")
  # summarise results

  suppressMessages(
    results <- cohort |>
      PatientProfiles::summariseResult(
        group = list("cohort_name"),
        strata = strata,
        variables = variables,
        estimates = estimates,
        counts = counts
      ) |>
      PatientProfiles::addCdmName(cdm = cdm)
  )

  # rename variables
  results <- results |>
    dplyr::left_join(
      dic |> dplyr::rename("variable_name" = "short_name"),
      by = "variable_name"
    ) |>
    dplyr::mutate(
      "variable_name" = dplyr::if_else(
        is.na(.data$new_variable_name),
        .data$variable_name,
        .data$new_variable_name
      ),
      "variable_level" = dplyr::if_else(
        is.na(.data$new_variable_level),
        .data$variable_level,
        .data$new_variable_level
      )
    ) |>
    dplyr::select(-c(
      "new_variable_name", "new_variable_level", "additional_name",
      "additional_level"
    )) |>
    dplyr::mutate(dplyr::across(
      c("variable_name", "variable_level"),
      ~ stringr::str_to_sentence(gsub("_", " ", .x))
    )) |>
    visOmopResults::uniteAdditional(cols = c("table", "window", "value")) |>
    dplyr::as_tibble()

  results <- results |>
    dplyr::mutate("result_id" = 1L) |>
    omopgenerics::newSummarisedResult(settings = srSet)

  cli::cli_alert_success("summariseCharacteristics finished!")

  return(results)
}

updateVariables <- function(variables,
                            date = NULL,
                            numeric = NULL,
                            binary = NULL,
                            categorical = NULL) {
  variables$date <- c(variables$date, date)
  variables$numeric <- c(variables$numeric, numeric)
  variables$binary <- c(variables$binary, binary)
  variables$categorical <- c(variables$categorical, categorical)
  return(variables)
}
binaryVariable <- function(x) {
  u <- unique(x)
  if (length(u) <= 3) {
    u <- as.character(u)
    return(all(u %in% c("0", "1", NA_character_)))
  }
  return(FALSE)
}
uniqueVariableName <- function(n = 1) {
  if (n != 0) {
    i <- getOption("unique_variable_name", 0) + 1:n
    options(unique_variable_name = i[length(i)])
    x <- sprintf("variable_%05i", i)
  } else {
    x <- NULL
  }
  return(x)
}
getValue <- function(name) {
  strsplit(name, "Intersect") |>
    unlist() |>
    dplyr::nth(2) |>
    tolower()
}
getType <- function(name) {
  strsplit(name, "Intersect") |>
    unlist() |>
    dplyr::first()
}
getSummaryName <- function(intersect) {
  paste0(
    c(
      "summarised",
      intersect |> snakecase::to_snake_case() |> strsplit("_") |> unlist()
    ),
    collapse = "_"
  )
}
