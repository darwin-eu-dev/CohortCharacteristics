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

#' Format a summarise_large_scale_characteristics object into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised_result object. Output of
#' summariseLargeScaleCharacteristics().
#' @param topConcepts Number of concepts to restrict the table.
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `tidyColumns(result)`.
#' @param groupColumn Columns to group by. See options with
#' `tidyColumns(result)`.
#' @param hide Columns to hide from the visualisation. See options with
#' `tidyColumns(result)`.
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
#' con <- dbConnect(duckdb(), eunomiaDir())
#' cdm <- cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")
#' cdm <- generateConceptCohortSet(
#'   cdm = cdm,
#'   conceptSet = list("viral_pharyngitis" = 4112343),
#'   name = "my_cohort"
#' )
#'
#' result <- summariseLargeScaleCharacteristics(
#'   cohort = cdm$my_cohort,
#'   eventInWindow = "condition_occurrence",
#'   episodeInWindow = "drug_exposure"
#' )
#'
#' tableLargeScaleCharacteristics(result)
#'
#' cdmDisconnect(cdm)
#' }
#'
tableLargeScaleCharacteristics <- function(result,
                                           topConcepts = NULL,
                                           type = "gt",
                                           header = c(
                                             "cdm_name", "cohort_name",
                                             visOmopResults::strataColumns(result),
                                             "variable_level"
                                           ),
                                           groupColumn = c("table_name", "type", "analysis"),
                                           hide = character()) {
  # validate result
  result <- omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, c("gt", "flextable", "tibble"))
  omopgenerics::assertNumeric(topConcepts, integerish = TRUE, length = 1, null = TRUE)

  # check settings
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_large_scale_characteristics"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("`result` object does not contain any `result_type == 'summarise_large_scale_characteristics'` information.")
    return(emptyResultTable(type))
  }

  # get only topN
  if (!is.null(topConcepts)) {
    top <- result |>
      dplyr::filter(.data$estimate_name == "count") |>
      dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
      dplyr::group_by(dplyr::across(!c("variable_name", "estimate_value"))) |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      utils::head(topConcepts) |>
      dplyr::ungroup() |>
      dplyr::select(!dplyr::starts_with("estimate"))
    result <- result |>
      dplyr::semi_join(top, by = colnames(top))
  }

  tab <- visOmopResults::visOmopTable(
    result = result,
    estimateName = c("N(%)" = "<count>(<percentage>%)"),
    header = header,
    settingsColumns = c("table_name", "type", "analysis"),
    groupColumn = groupColumn,
    type = type,
    hide = hide
  )

  return(tab)
}
