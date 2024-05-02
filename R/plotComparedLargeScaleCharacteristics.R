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

#' create a ggplot from the output of summariseLargeScaleCharacteristics.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseLargeScaleCharacteristics().
#' @param referenceGroupLevel group_level value to be used as the reference.
#' @param referenceStrataLevel strata_level value to be used as the reference.
#' @param referenceVariableLevel variable_level value to be used as the reference.
#' @param referenceCdmName cdm_name value to be used as the reference.
#' @param splitStrata boolean variable (TRUE/FALSE)
#' @param facet columns in data to facet. If the facet position wants to be specified, use the formula class for the input
#' (e.g., strata + table_name ~ group_level + cdm_name). Variables before "~" will be facet by on horizontal axis, whereas those after "~" on vertical axis.
#' Character format is also allowed (e.g., c("strata","table_name","group_level","cdm_name")).
#' Only the following columns are allowed to be facet by: c("cdm_name", "group_level", "strata_level", "variable_level", "strata", "table_name").
#' If splitStrata = TRUE, strata levels are also allowed.
#' @param colorVars column in data to color by. Only the following columns are allowed to be used: c("cdm_name", "group_level", "strata_level", "variable_level", "strata", "table_name").
#' If splitStrata = TRUE, strata levels are also allowed.
#' @param missings value to replace the missings with.
#'
#' @return A ggplot.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#' library(DrugUtilisation)
#' cdm <- DrugUtilisation::mockDrugUtilisation()
#'
#' lsc <- CohortCharacteristics::summariseLargeScaleCharacteristics(cdm$cohort1,
#'   eventInWindow = "condition_occurrence", episodeInWindow = "drug_exposure",
#'   minimumFrequency = 0.05
#' )
#'
#' plotComparedLargeScaleCharacteristics(
#'   data = lsc,
#'   referenceGroupLevel = "cohort_2", referenceStrataLevel = NULL,
#'   referenceVariableLevel = "-inf to -366", referenceCdmName = NULL,
#'   splitStrata = TRUE, facet = variable_level ~ group_level, colorVars = NULL,
#'   missings = 0
#' )
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotComparedLargeScaleCharacteristics <- function(data,
                                                  referenceGroupLevel = NULL,
                                                  referenceStrataLevel = NULL,
                                                  referenceVariableLevel = NULL,
                                                  referenceCdmName = NULL,
                                                  splitStrata = FALSE,
                                                  facet = NULL,
                                                  colorVars = NULL,
                                                  missings = 0) {
  if (length(data$result_id) != 0) {
    checkSettings(data)

    referenceGroupLevel <- checkReference(referenceGroupLevel, data, type = "group_level", argument = "referenceGroupLevel")
    referenceStrataLevel <- checkReference(referenceStrataLevel, data, type = "strata_level", argument = "referenceStrataLevel")
    referenceVariableLevel <- checkReference(referenceVariableLevel, data, type = "variable_level", argument = "referenceVariableLevel")
    referenceCdmName <- checkReference(referenceCdmName, data, type = "cdm_name", argument = "referenceCdmName")
    splitStrata <- checkSplitStrata(data, splitStrata)

    # Extract facet names
    x <- facetFunction(facet, splitStrata, data)
    facetVarX <- x$facetVarX
    facetVarY <- x$facetVarY
    data <- x$data

    # Color of the plot
    checkName(colorVars, splitStrata, data, type = "colorVars")

    # All that is not a facet variable will be a color variable if colorVar = NULL
    colorVars <- colorVarsIfNull(data, vars = c(facetVarX, facetVarY, referenceGroupLevel, referenceStrataLevel, referenceVariableLevel, referenceCdmName), splitStrata, colorVars)

    # Split strata
    if (splitStrata == TRUE) {
      strata_levels <- data$strata_name |> unique()
      strata_levels <- strata_levels[strata_levels != "overall"]
      data <- data |> visOmopResults::splitStrata()
    }

    # Tidying dataset
    data <- tidyData(data, referenceGroupLevel, referenceVariableLevel, referenceCdmName, referenceStrataLevel, missings, splitStrata, strata_levels)

    # Change facet names
    facetVarX <- changeNames(facetVarX, type = "facet")
    facetVarY <- changeNames(facetVarY, type = "facet")

    # Change colorVars names
    colorVars <- changeNames(colorVars, type = "colorVars")

    # Edit plot
    y <- editPlot(
      data = data, facetVarX = facetVarX, facetVarY = facetVarY,
      colorVars = colorVars, vertical_x = FALSE
    )
  } else {
    y <- plotfunction(data = data)
  }
  return(y)
}


checkReference <- function(reference, data, type, argument) {
  if (length(reference) > 1) {
    stop(sprintf(paste0(argument, " must have length = 1.")))
  }
  if (is.null(reference)) {
    # Check there is only one type
    if (length(unique(data[[type]])) > 1) {
      stop(sprintf(paste0(argument, " cannot be NULL, as there are multiple options to be used as the reference value.
                          Please, use the ", argument, " argument to define which ", type, " must be used as a reference: ", paste0(paste0("'", unique(data[[type]]), "'"), collapse = " or "))))
    } else {
      reference <- unique(data[[type]])
    }
  } else {
    # Format variable
    if (!inherits(reference, "character")) {
      stop(sprintf(paste0("'", argument, "' must be a character string.")))
    }
    # If variable is not present in the dataset
    if (!reference %in% data[[type]]) {
      stop(sprintf(paste0("'", reference, "' is not present in ", type, " column.")))
    }
  }
  return(reference)
}

checkSplitStrata <- function(data, splitStrata) {
  if (length(data$strata_level |> unique()) == 1) {
    warning("splitStrata cannot be TRUE when strata_level is unique. Changing splitStrata to FALSE.")
    splitStrata <- FALSE
  }

  return(splitStrata)
}

tidyData <- function(data, referenceGroupLevel, referenceVariableLevel, referenceCdmName, referenceStrataLevel, missings, splitStrata, strata_levels) {
  # Create table reference
  if (splitStrata) {
    table_reference <- data |>
      dplyr::filter(dplyr::if_any(strata_levels, ~ . == .env$referenceStrataLevel)) |>
      dplyr::filter(.data$group_level == .env$referenceGroupLevel) |>
      dplyr::filter(.data$variable_level == .env$referenceVariableLevel) |>
      dplyr::filter(.data$cdm_name == .env$referenceCdmName)
  } else {
    table_reference <- data |>
      dplyr::filter(.data$group_level == .env$referenceGroupLevel) |>
      dplyr::filter(.data$strata_level == .env$referenceStrataLevel) |>
      dplyr::filter(.data$variable_level == .env$referenceVariableLevel) |>
      dplyr::filter(.data$cdm_name == .env$referenceCdmName)
  }

  if ((table_reference$result_id |> length()) == 0) {
    stop(sprintf(paste0("There is no reference variables left in data.")))
  }

  # Create table comparator
  table_comparator <- data |>
    dplyr::anti_join(
      table_reference
    )

  if ((table_comparator$result_id |> length()) == 0) {
    stop(sprintf(paste0("There is no comparator variables left in data.")))
  }

  # Rename tables
  table_reference <- table_reference |>
    dplyr::rename_at(dplyr::vars(-c(
      "variable_name", "result_id", "result_type", "package_name", "package_version",
      "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level"
    )), ~ paste0(., "_reference"))
  table_comparator <- table_comparator |>
    dplyr::rename_at(dplyr::vars(-c(
      "variable_name", "result_id", "result_type", "package_name", "package_version",
      "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level"
    )), ~ paste0(., "_comparator"))

  # Join both tables
  data <- table_reference |>
    dplyr::full_join(
      table_comparator,
      by = c(
        "variable_name", "result_id", "result_type", "package_name", "package_version",
        "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level"
      )
    ) |>
    dplyr::distinct()

  # Cleaning the dataset
  data <- data |>
    dplyr::mutate(
      estimate_value_comparator = as.numeric(.data$estimate_value_comparator),
      estimate_value_reference = as.numeric(.data$estimate_value_reference)
    )

  # Replace reference missings
  data <- replaceReferenceMissings(data, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings, splitStrata, strata_levels)

  # Replace comparator missings
  data <- replaceComparatorMissings(data, table_comparator, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings, splitStrata, strata_levels)

  data <- data |>
    tidyr::replace_na(list(
      estimate_value_comparator = missings,
      estimate_value_reference = missings
    )) |>
    dplyr::rename(estimate_value = .data$estimate_value_reference) |>
    dplyr::mutate(estimate_value_comparator = .data$estimate_value_comparator / 100)

  return(data)
}

replaceReferenceMissings <- function(data, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings, splitStrata, strata_levels) {
  if (splitStrata) {
    data <- data |>
      dplyr::mutate(
        cdm_name_reference = referenceCdmName,
        group_name_reference = data |> dplyr::select("group_name_reference") |> dplyr::filter(!is.na(.data$group_name_reference)) |> dplyr::distinct() |> dplyr::pull(),
        group_level_reference = referenceGroupLevel,
        variable_level_reference = referenceVariableLevel,
        estimate_value_reference = dplyr::if_else(is.na(.data$estimate_value_reference), missings, .data$estimate_value_reference)
      )

    for (i in strata_levels) {
      data <- data |>
        dplyr::mutate_at(paste0(i, "_reference"), ~ data |>
          dplyr::select(paste0(i, "_reference")) |>
          dplyr::filter(!is.na(.data[[paste0(i, "_reference")]])) |>
          dplyr::distinct() |>
          dplyr::pull())
    }
  } else {
    data <- data |>
      dplyr::mutate(
        cdm_name_reference = referenceCdmName,
        group_name_reference = data |> dplyr::select("group_name_reference") |> dplyr::filter(!is.na(.data$group_name_reference)) |> dplyr::distinct() |> dplyr::pull(),
        group_level_reference = referenceGroupLevel,
        variable_level_reference = referenceVariableLevel,
        strata_name_reference = data |> dplyr::select("strata_name_reference") |> dplyr::filter(!is.na(.data$strata_name_reference)) |> dplyr::distinct() |> dplyr::pull(),
        strata_level_reference = referenceStrataLevel,
        estimate_value_reference = dplyr::if_else(is.na(.data$estimate_value_reference), missings, .data$estimate_value_reference)
      )
  }
  return(data)
}

replaceComparatorMissings <- function(data, table_comparator, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings, splitStrata, strata_levels) {
  if (splitStrata) {
    comparator_groups <- table_comparator |>
      dplyr::select(
        "cdm_name_comparator", "group_name_comparator", "group_level_comparator",
        paste0(strata_levels, "_comparator"), "variable_level_comparator"
      ) |>
      dplyr::distinct()
  } else {
    comparator_groups <- table_comparator |>
      dplyr::select(
        "cdm_name_comparator", "group_name_comparator", "group_level_comparator",
        "strata_name_comparator", "strata_level_comparator", "variable_level_comparator"
      ) |>
      dplyr::distinct()
  }

  data <- data |>
    dplyr::select(-c(dplyr::ends_with("comparator"))) |>
    dplyr::distinct() |>
    dplyr::cross_join(comparator_groups) |>
    dplyr::left_join(
      data |>
        dplyr::select("variable_name", dplyr::ends_with("comparator"))
    ) |>
    dplyr::mutate(estimate_value_comparator = dplyr::if_else(is.na(.data$estimate_value_comparator), missings, .data$estimate_value_comparator))

  return(data)
}

changeNames <- function(var, type) {
  if (!is.null(var)) {
    var <- paste0(var, "_comparator")
    var <- gsub("table_name_comparator", "table_name", var)
  }

  return(var)
}

editPlot <- function(data, facetVarX = NULL, facetVarY = NULL, colorVars = NULL, vertical_x = FALSE) {
  x <- plotfunction(
    data = data,
    xAxis = "estimate_value",
    yAxis = "estimate_value_comparator",
    plotStyle = "scatterplot",
    facetVarX,
    facetVarY,
    colorVars,
    vertical_x
  ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::geom_abline(slope = 1, colour = "black", linetype = 2, size = 0.5) +
    ggplot2::labs(x = "Reference", y = "Comparator")
}
