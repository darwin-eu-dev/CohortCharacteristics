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
#' cdm <- mockCohortCharacteristics(numberIndividuals = 100)
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
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cohortId <- omopgenerics::validateCohortIdArgument(cohortId, cohort)
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkStrata(strata, cohort)
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))
  checkmate::assertLogical(restrictToFirstEntry, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::assertCharacter(estimates, any.missing = FALSE, null.ok = FALSE)
  timing <- estimates

  if (length(timing) == 0 && !density) {
    return(omopgenerics::emptySummarisedResult())
  }

  cohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    PatientProfiles::addCohortName()

  if (isTRUE(restrictToFirstEntry)) {
    # to use cohortConstructor once released
    # cdm[[name]] <- cdm[[name]] |>
    #   restrictToFirstEntry()
    cohort <- cohort |>
      dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
      dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  strataCols <- unlist(strata) |> unique()
  # should we use addCohortIntersectDate instead to avoid potentially large number of rows?
  cohort_timings <- cohort |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::select(dplyr::all_of(c(
      strataCols, "cohort_name_reference",
      "cohort_start_date", "cohort_end_date",
      "subject_id"
    ))) |>
    dplyr::inner_join(
      cohort |>
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
      by = c("subject_id", strataCols)
    ) |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) %>% # to be removed
    dplyr::mutate(days_between_cohort_entries = !!CDMConnector::datediff(
      "cohort_start_date",
      "cohort_start_date_comparator",
      interval = "day"
    )) |>
    dplyr::select(!c(
      "cohort_start_date", "cohort_end_date", "cohort_start_date_comparator",
      "cohort_end_date_comparator"
    )) |>
    dplyr::collect()

  if (nrow(cohort_timings) == 0) {
    return(omopgenerics::emptySummarisedResult())
  }

  if (length(timing) > 0) {
    timingsEstimates <- cohort_timings |>
      PatientProfiles::summariseResult(
        group = c("cohort_name_reference", "cohort_name_comparator"),
        includeOverallGroup = FALSE,
        strata = strata,
        includeOverallStrata = TRUE,
        variables = "days_between_cohort_entries",
        estimates = timing
      )
  } else {
    timingsEstimates <- omopgenerics::emptySummarisedResult()
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
    timingsDensity <- NULL
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
          timingsDensity <- timingsDensity |>
            dplyr::union_all(
              getDensityData(sNameData$days_between_cohort_entries[
                sNameData$strata_level == sLevel]) |>
                dplyr::mutate(
                  "group_level" = gLevel,
                  "strata_name" = sName,
                  "strata_level" = sLevel
                )
            )
        }
      }
    }

    timingsDensity <- timingsDensity |>
      dplyr::mutate(
        result_id = 1L,
        group_name = "cohort_name_reference &&& cohort_name_comparator",
        variable_name = "density",
        estimate_type = "numeric"
      ) |>
      visOmopResults::uniteAdditional()

  } else {
    timingsDensity <- omopgenerics::emptySummarisedResult()
  }

  timingsResult <- timingsEstimates |>
    dplyr::bind_rows(timingsDensity) |>
    dplyr::mutate("cdm_name" = omopgenerics::cdmName(cohort)) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 1L,
      "package_name" = "CohortCharacteristics",
      "package_version" = as.character(utils::packageVersion("CohortCharacteristics")),
      "result_type" = "summarise_cohort_timing",
      "restrict_to_first_entry" = restrictToFirstEntry,
      "density" = density
    ))

  return(timingsResult)
}

getDensityData <- function(x) {
  nPoints <- 512
  nDigits <- ceiling(log(nPoints)/log(10))
  x <- x[!is.na(x)]
  if (length(x) == 1) {
    den <- stats::density(x, bw = 0.5)
  } else if (length(x) == 0) {
    den <- list(x = NA, y = NA)
  } else {
    den <- stats::density(x, n = nPoints)
  }
  lev <- stringr::str_pad(seq_len(nPoints), width = nDigits, side = "left", pad = "0")
  res <- dplyr::tibble(
    variable_level = lev,
    estimate_name = "x",
    estimate_value = as.character(den$x)
  ) |>
    dplyr::union_all(dplyr::tibble(
      variable_level = lev,
      estimate_name = "y",
      estimate_value = as.character(den$y)
    )) |>
    dplyr::arrange(.data$variable_level, .data$estimate_name)
  return(res)
}
