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

#' Format a summarised_large_scale_characteristics object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_large_scale_characteristics object.
#' @param type Output type ("gt" or "flextable").
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param splitStrata Whether to split strata_group and strata_level to multiple
#' columns.
#' @param header Specify the headers of the table.
#' @param topConcepts Number of concepts to restrict the table.
#'
#' @export
#'
#' @return A formatted table.
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(CDMConnector)
#'
#' con <- dbConnect(duckdb(), eunomia_dir())
#' cdm <- cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")
#' cdm <- generateConceptCohortSet(
#'   cdm = cdm,
#'   conceptSet = list("viral_pharyngitis" = 4112343),
#'   name = "my_cohort"
#' )
#' result <- summariseLargeScaleCharacteristics(
#'   cohort = cdm$my_cohort,
#'   eventInWindow = "condition_occurrence",
#'   episodeInWindow = "drug_exposure"
#' )
#' tableLargeScaleCharacteristics(result)
#' }
#'
tableLargeScaleCharacteristics <- function(result,
                                           type = "gt",
                                           formatEstimateName = c("N (%)" = "<count> (<percentage>%)"),
                                           splitStrata = TRUE,
                                           header = c(
                                             "cdm name", "cohort name",
                                             "strata", "window name"
                                           ),
                                           topConcepts = NULL) {

  if (!inherits(result, "summarised_result")) {
    cli::cli_abort("result must be a summarised result")
  }
  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyResultTable(type = type))
  }

  assertLogical(splitStrata, length = 1)
  if (is.character(header)) {
    header <- tolower(header)
    header <- gsub("_", " ", header)
  }
  assertChoice(header, choices = c("cdm name", "cohort name", "strata", "window name"))
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarised_large_scale_characteristics"
    )
  if (nrow(result) == 0) {
    cli::cli_warn(
      "No summarised large scale characteristics found in this result object"
    )
    return(emptyResultTable(type = type))
  }

  # min cell count
  settings <- omopgenerics::settings(result) |>
    dplyr::filter(.data$result_type == "summarised_large_scale_characteristics")
  if ("min_cell_count" %in% colnames(settings)) {
    result <- result |>
      dplyr::left_join(
        settings |>
          dplyr::select("result_id", "min_cell_count"),
        by = "result_id"
      ) |>
      dplyr::mutate(estimate_value = dplyr::if_else(
        is.na(.data$estimate_value), paste0("<", .data$min_cell_count), .data$estimate_value
      )) |>
      dplyr::select(!"min_cell_count")
  } else {
    cli::cli_inform(c("!" = "Results have not been suppressed."))
  }

  sets <- settings(result) |>
    dplyr::mutate("group" = paste0(
      "Table: ", .data$table_name, " (", .data$type, " in window)"
    )) |>
    dplyr::select("result_id", "group")
  res <- result |>
    visOmopResults::splitGroup() |>
    visOmopResults::splitAdditional() |>
    dplyr::inner_join(sets, by = "result_id") |>
    dplyr::rename("window_name" = "variable_level") |>
    dplyr::select(!"result_id")
  if (splitStrata) {
    res <- res |> visOmopResults::splitStrata()
    strataColumns <- visOmopResults::strataColumns(result)
  } else {
    strataColumns <- c("strata_name", "strata_level")
  }
  # get only topN
  top <- res |>
    dplyr::filter(.data$estimate_name == "count") |>
    dplyr::select("concept_id", "estimate_value", "group") |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
    dplyr::select("concept_id", "group") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate("order_id" = dplyr::row_number()) |>
    dplyr::ungroup()
  if (!is.null(topConcepts)) {
    top <- top |>
      dplyr::filter(.data$order_id <= .env$topConcepts)
  }
  res <- res |>
    dplyr::inner_join(
      top |>
        dplyr::select(-"group"),
      by = "concept_id"
    )

  res <- res |>
    visOmopResults::formatEstimateValue() |>
    dplyr::as_tibble() |>
    visOmopResults::formatEstimateName(estimateNameFormat = formatEstimateName) |>
    orderWindow() |>
    dplyr::mutate(
      "Concept" = paste0(.data$variable_name, " (", .data$concept_id, ")")
    ) |>
    dplyr::arrange(!!!rlang::syms(c(
      "cdm_name", "cohort_name", "Concept",
      strataColumns, "group", "window_id",
      "order_id"
    ))) |>
    dplyr::select(dplyr::all_of(c(
      "group",
      "CDM name" = "cdm_name", "Cohort name" = "cohort_name",
      strataColumns, "Concept", "Window" = "window_name", "estimate_value"
    )))

  header <- cleanHeader(header, strataColumns)
  tab <- visOmopResults::formatHeader(result = res, header = header)
  if (type == "gt") {
    res <- visOmopResults::gtTable(tab, groupColumn = "group")
  } else {
    res <- visOmopResults::fxTable(tab, groupColumn = "group")
  }

  return(res)
}
cleanHeader <- function(header, strata) {
  header[header == "cdm name"] <- "CDM name"
  header[header == "cohort name"] <- "Cohort name"
  header[header == "window name"] <- "Window"
  if ("strata" %in% header) {
    id <- which(header == "strata")
    header <- append(header, strata, after = id)
    header <- header[header != "strata"]
  }
}
orderWindow <- function(res) {
  windows <- res |>
    dplyr::select("window_name") |>
    dplyr::distinct() |>
    dplyr::pull()
  win <- windows |>
    stringr::str_split(pattern = " ") |>
    lapply(function(x) {
      if (length(x) == 3) {
        if (x[2] == "to") {
          return(dplyr::tibble(
            lower = as.numeric(x[1]), upper = as.numeric(x[3])
          ))
        }
      }
      return(dplyr::tibble(lower = NA, upper = NA))
    })
  names(win) <- windows
  tib <- dplyr::bind_rows(win, .id = "window_name") |>
    dplyr::arrange(.data$lower, .data$upper) |>
    dplyr::mutate("window_id" = dplyr::row_number())
  res <- res |>
    dplyr::left_join(tib, by = "window_name")
  return(res)
}
