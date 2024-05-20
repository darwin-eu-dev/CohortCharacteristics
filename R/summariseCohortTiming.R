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

#' Summarise timing between entries into cohorts in a cohort table
#'
#' @param cohort  A cohort table in a cdm reference.
#' @param cohortId A cohort definition id to restrict by. If NULL, all cohorts
#' will be included.
#' @param strata A list of variables to stratify results. These variables
#' must have been added as additional columns in the cohort table.
#' @param restrictToFirstEntry If TRUE only an individual's first entry per
#' cohort will be considered. If FALSE all entries per individual will be
#' considered.
#' @param estimates Summary statistics to use when summarising timing.
#' @param density TRUE or FALSE. If TRUE, estimates for a density plot will
#' also be computed.
#'
#' @return A summary of timing between entries into cohorts in the cohort table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' cdm <- CohortCharacteristics::mockCohortCharacteristics()
#' summariseCohortTiming(cdm$cohort2) |> dplyr::glimpse()
#' mockDisconnect(cdm = cdm)
#' }
#'
#' @importFrom dplyr %>%
summariseCohortTiming <- function(cohort,
                                  cohortId = NULL,
                                  strata = list(),
                                  restrictToFirstEntry = TRUE,
                                  estimates = c("min", "q25", "median", "q75", "max"),
                                  density = FALSE) {
  # validate inputs
  assertClass(cohort, "cohort_table")
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkStrata(strata, cohort)
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))
  checkmate::assertLogical(restrictToFirstEntry, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::assertCharacter(estimates, any.missing = FALSE, null.ok = FALSE)
  timing <- estimates

  # add cohort names
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)

  if (is.na(name)) {
    cli::cli_abort("Please provide a permanent cohort table.")
  }

  ids <- cdm[[name]] |>
    omopgenerics::settings() |>
    dplyr::pull(.data$cohort_definition_id)

  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- which(!cohortId %in% ids)
    if (length(indNot) > 0) {
      cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table.")
    }
  }
  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if (isTRUE(restrictToFirstEntry)) {
    # to use cohortConstructor once released
    # cdm[[name]] <- cdm[[name]] |>
    #   restrictToFirstEntry()
    cdm[[name]] <- cdm[[name]] |>
      dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
      dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  strataCols <- unlist(strata)
  # should we use addCohortIntersectDate instead to avoid potentially large number of rows?
  cohort_timings <- cdm[[name]] |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::select(dplyr::all_of(c(
      strataCols, "cohort_name_reference",
      "cohort_start_date", "cohort_end_date",
      "subject_id"
    ))) |>
    dplyr::inner_join(
      cdm[[name]] |>
        dplyr::rename_with(~ paste0(.x, "_comparator"),
          .cols = c(
            "cohort_definition_id", "cohort_start_date",
            "cohort_end_date", "cohort_name"
          )
        ) |>
        dplyr::select(dplyr::all_of(c(
          strataCols, "cohort_name_comparator",
          "cohort_start_date_comparator", "cohort_end_date_comparator",
          "subject_id"
        ))),
      by = c("subject_id", unique(strataCols))
    ) |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) %>% # to be removed
    dplyr::mutate(days_between_cohort_entries = !!CDMConnector::datediff("cohort_start_date",
      "cohort_start_date_comparator",
      interval = "day"
    )) |>
    dplyr::collect()

  if (nrow(cohort_timings) == 0 || (length(timing) == 0 && !density)) {
    return(omopgenerics::emptySummarisedResult())
  }

  timingsResult <- list()
  rId <- integer()
  rT <- character()

  if (length(timing) > 0) {
    timingsResult$estimates <- cohort_timings |>
      PatientProfiles::summariseResult(
        group = c("cohort_name_reference", "cohort_name_comparator"),
        includeOverallGroup = FALSE,
        strata = strata,
        includeOverallStrata = TRUE,
        variables = "days_between_cohort_entries",
        estimates = timing
      ) |>
      dplyr::mutate(cdm_name = CDMConnector::cdmName(cdm))
    rId <- c(rId, 1L)
    rT <- c(rT, "cohort_timing")
  }

  if (density) {
    forDensity <- cohort_timings |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
    forDensity <- lapply(c(list(character(0)), strata), function(levels, data = forDensity) {
      data |> visOmopResults::uniteStrata(cols = levels)
    }) |>
      dplyr::bind_rows() |>
      dplyr::select(!dplyr::all_of(c(strataCols)))

    groups <- unique(forDensity$group_level)
    timingDensity <- NULL
    for (gLevel in groups) {
      # filter group comparison
      gData <- forDensity |> dplyr::filter(.data$group_level == .env$gLevel)
      strataNames <- unique(gData$strata_name)
      for (sName in strataNames) {
        # filter strata name
        sNameData <- gData |> dplyr::filter(.data$strata_name == .env$sName)
        strataLevels <- unique(sNameData$strata_level)
        # compute density for each strata level
        for (sLevel in strataLevels) {
          sLevelData <- sNameData |> dplyr::filter(.data$strata_level == .env$sLevel)
          if (nrow(sLevelData) > 2) {
            timingDensity <- timingDensity |>
              dplyr::union_all(
                getDensityData(sLevel, sLevelData) |>
                  dplyr::mutate(
                    group_level = gLevel,
                    strata_name = sName
                  )
              )
          }
        }
      }
    }

    timingsResult$density <- timingDensity |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        result_id = as.integer(2),
        cdm_name = CDMConnector::cdmName(cdm),
        group_name = "cohort_name_reference &&& cohort_name_comparator",
        variable_name = "density",
        estimate_type = "numeric",
        additional_name = "overall",
        additional_level = "overall"
      )
    rId <- c(rId, 2L)
    rT <- c(rT, "cohort_timing_density")
  }

  timingsResult <- timingsResult |>
    dplyr::bind_rows() |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = rId,
      "package_name" = "CohortCharacteristics",
      "package_version" = as.character(utils::packageVersion("CohortCharacteristics")),
      "result_type" = rT,
      "restrict_to_first_entry" = restrictToFirstEntry
    ))

  return(timingsResult)
}


getDensityData <- function(sLevel, data) {
  dStrata <- data$days_between_cohort_entries[data$strata_level == sLevel]
  d <- stats::density(dStrata)
  densityResult <- dplyr::tibble(
    variable_level = as.character(1:length(d$x)),
    estimate_name = "x",
    estimate_value = as.character(d$x)
  ) |>
    dplyr::union_all(
      dplyr::tibble(
        variable_level = as.character(1:length(d$x)),
        estimate_name = "y",
        estimate_value = as.character(d$y)
      )
    ) |>
    dplyr::mutate(
      strata_level = sLevel
    )
  return(densityResult)
}
