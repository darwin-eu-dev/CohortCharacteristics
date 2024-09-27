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

#' @noRd
checkX <- function(x) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a valid table")
  }
  if ("person_id" %in% colnames(x) && "subject_id" %in% colnames(x)) {
    cli::cli_abort(paste0(
      "x can only contain an individual identifier, please remove 'person_id'",
      " or 'subject_id'"
    ))
  }
  if (!("person_id" %in% colnames(x)) && !("subject_id" %in% colnames(x))) {
    cli::cli_abort(paste0(
      "x must contain an individual identifier ('person_id'",
      " or 'subject_id')"
    ))
  }
  personVariable <- dplyr::if_else(
    "person_id" %in% colnames(x), "person_id", "subject_id"
  )
  invisible(personVariable)
}

#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm a cdm_reference object.")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        " are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}

#' @noRd
checkVariableInX <- function(indexDate, x, nullOk = FALSE, name = "indexDate") {
  omopgenerics::assertCharacter(indexDate, lenngth = 1, null = nullOk)
  if (!is.null(indexDate) && !(indexDate %in% colnames(x))) {
    cli::cli_abort(glue::glue("{name} ({indexDate}) should be a column in x"))
  }
  invisible(NULL)
}

#' @noRd
checkCategory <- function(category, overlap = FALSE, type = "numeric") {
  omopgenerics::assertList(category, unique = TRUE)

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    invisible(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    cli::cli_abort("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category_label = names(.env$category)) |>
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      dplyr::case_when(
        is.infinite(.data$lower_bound) & is.infinite(.data$upper_bound) ~ "any",
        is.infinite(.data$lower_bound) ~ paste(.data$upper_bound, "or below"),
        is.infinite(.data$upper_bound) ~ paste(.data$lower_bound, "or above"),
        TRUE ~ paste(.data$lower_bound, "to", .data$upper_bound)
      ),
      .data$category_label
    )) |>
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (!overlap) {
    if (nrow(result) > 1) {
      lower <- result$lower_bound[2:nrow(result)]
      upper <- result$upper_bound[1:(nrow(result) - 1)]
      if (!all(lower > upper)) {
        cli::cli_abort("There can not be overlap between categories")
      }
    }
  }

  invisible(result)
}

#' @noRd
checkNewName <- function(name, x) {
  renamed <- name[name %in% colnames(x)]
  if (length(renamed) > 0) {
    mes <- paste0(
      "The following columns will be overwritten: ",
      paste0(renamed, collapse = ", ")
    )
    cli::cli_warn(message = mes)
  }
  invisible(name)
}

#' @noRd
getWindowNames <- function(window) {
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- gsub("-", "m", element)
    invisible(paste0(element[1], "_to_", element[2]))
  }
  windowNames <- names(window)
  if (is.null(windowNames)) {
    windowNames <- lapply(window, getname)
  } else {
    windowNames[windowNames == ""] <- lapply(window[windowNames == ""], getname)
  }
  invisible(windowNames)
}

#' @noRd
checkNameStyle <- function(nameStyle, filterTbl, windowTbl, value) {
  omopgenerics::assertCharacter(nameStyle, length = 1, minNumCharacter = 1)
  filterChange <- !is.null(filterTbl) && nrow(filterTbl) > 1
  windowChange <- !is.null(windowTbl) && nrow(windowTbl) > 1
  valueChange <- length(value) > 1
  changed <- c(
    c("{id_name}")[filterChange],
    c("{window_name}")[windowChange],
    c("{value}")[valueChange]
  )
  containWindow <- grepl("\\{window_name\\}", nameStyle)
  containFilter <- grepl("\\{id_name\\}", nameStyle)
  containValue <- grepl("\\{value\\}", nameStyle)
  contained <- c(
    c("{id_name}")[containFilter],
    c("{window_name}")[containWindow],
    c("{value}")[containValue]
  )
  if (!all(changed %in% contained)) {
    variablesNotContained <- changed[!(changed %in% contained)]
    variablesNotContained <- gsub("[{}]", "", variablesNotContained)
    variablesNotContained <- gsub("id_name", "cohort_name", variablesNotContained)
    cli::cli_abort(paste0(
      "Variables: ",
      paste0(variablesNotContained, collapse = ", "),
      " have multiple possibilities and should be cotained in nameStyle"
    ))
  }
}

#' @noRd
checkValue <- function(value, x, name) {
  omopgenerics::assertCharacter(value, minNumCharacter = 1)
  omopgenerics::assertTrue(
    all(value %in% c("flag", "count", "date", "days", colnames(x)))
  )
  valueOptions <- c("flag", "count", "date", "days")
  valueOptions <- valueOptions[valueOptions %in% colnames(x)]
  if (length(valueOptions) > 0) {
    cli::cli_warn(paste0(
      "Variables: ",
      paste0(valueOptions, collapse = ", "),
      " are also present in ",
      name,
      ". But have their own functionality inside the package. If you want to
      obtain that column please rename and run again."
    ))
  }
  invisible(value[!(value %in% c("flag", "count", "date", "days"))])
}

#' @noRd
checkCohortNames <- function(x, targetCohortId, name) {
  if (!("cohort_table" %in% class(x))) {
    cli::cli_abort("cdm[[targetCohortTable]]) must be a 'cohort_table'.")
  }
  cohort <- omopgenerics::settings(x)
  filterVariable <- "cohort_definition_id"
  if (is.null(targetCohortId)) {
    if (is.null(cohort)) {
      idName <- NULL
      filterVariable <- NULL
      targetCohortId <- NULL
    } else {
      cohort <- dplyr::collect(cohort)
      idName <- cohort$cohort_name
      targetCohortId <- cohort$cohort_definition_id
    }
  } else {
    if (is.null(cohort)) {
      idName <- paste0(name, "_", targetCohortId)
    } else {
      idName <- cohort |>
        dplyr::filter(
          as.integer(.data$cohort_definition_id) %in%
            as.integer(.env$targetCohortId)
        ) |>
        dplyr::arrange(.data$cohort_definition_id) |>
        dplyr::pull("cohort_name")
      if (length(idName) != length(targetCohortId)) {
        cli::cli_abort(
          "some of the cohort ids given do not exist in the cohortSet of
          cdm[[targetCohortName]]"
        )
      }
    }
  }
  parameters <- list(
    "filter_variable" = filterVariable,
    "filter_id" = sort(targetCohortId),
    "id_name" = idName
  )
  invisible(parameters)
}

#' @noRd
checkSnakeCase <- function(name, verbose = TRUE) {
  wrong <- FALSE
  for (i in seq_along(name)) {
    n <- name[i]
    n <- gsub("[a-z]", "", n)
    n <- gsub("[0-9]", "", n)
    n <- gsub("_", "", n)
    if (nchar(n) > 0) {
      oldname <- name[i]
      name[i] <- gsub("([[:upper:]])", "\\L\\1", perl = TRUE, name[i])
      name[i] <- gsub("[^a-z,0-9.-]", "_", name[i])
      name[i] <- gsub("-", "_", name[i])
      if (verbose) {
        cli::cli_alert(paste0(oldname, " has been changed to ", name[i]))
      }
      wrong <- TRUE
    }
  }
  if (wrong && verbose) {
    cli::cli_alert("some provided names were not in snake_case")
    cli::cli_alert("names have been changed to lower case")
    cli::cli_alert("special symbols in names have been changed to '_'")
  }
  invisible(name)
}

#' @noRd
checkExclude <- function(exclude) {
  if (!is.null(exclude) && !is.character(exclude)) {
    cli::cli_abort("eclude must a character vector or NULL")
  }
}

#' @noRd
checkTable <- function(table) {
  if (!("tbl" %in% class(table))) {
    cli::cli_abort("table should be a tibble")
  }
}

#' @noRd
checkStrata <- function(strata, table, type = "strata") {
  errorMessage <- paste0(type, " should be a list that point to columns in table")
  if (!is.list(strata)) {
    strata <- list(strata)
  }
  if (length(strata) > 0) {
    if (!is.character(unlist(strata))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(strata) %in% colnames(table))) {
      notPresent <- strata |>
        unlist() |>
        unique()
      notPresent <- notPresent[!notPresent %in% colnames(table)]
      cli::cli_abort(paste0(
        errorMessage,
        ". The following columns were not found in the data: ",
        paste0(notPresent, collapse = ", ")
      ))
    }
  }
  return(invisible(strata))
}

#' @noRd
checkSuppressCellCount <- function(suppressCellCount) {
  omopgenerics::assertNumeric(
    suppressCellCount, integerish = TRUE, min = 0, length = 1
  )
}

#' @noRd
checkBigMark <- function(bigMark) {
  omopgenerics::assertCharacter(bigMark, length = 1)
}

#' @noRd
checkDecimalMark <- function(decimalMark) {
  omopgenerics::assertCharacter(decimalMark, length = 1, minNumCharacter = 1)
}

#' @noRd
checkSignificantDecimals <- function(significantDecimals) {
  omopgenerics::assertNumeric(
    significantDecimals, integerish = TRUE, min = 0, length = 1
  )
}

#' @noRd
checkCensorDate <- function(x, censorDate) {
  check <- x |>
    dplyr::select(dplyr::all_of(censorDate)) |>
    utils::head(1) |>
    dplyr::pull() |>
    inherits("Date")
  if (!check) {
    cli::cli_abort("{censorDate} is not a date variable")
  }
}

#' @noRd
checkOtherVariables <- function(otherVariables, cohort, call = rlang::env_parent()) {
  if (!is.list(otherVariables)) {
    otherVariables <- list(otherVariables)
  }
  omopgenerics::assertList(otherVariables, class = "character", call = call)
  if (!all(unlist(otherVariables) %in% colnames(cohort))) {
    cli::cli_abort("otherVariables must point to columns in cohort.", call = call)
  }
  if (is.null(names(otherVariables))) {
    names(otherVariables) <- paste0("other_", seq_along(otherVariables))
  }
  return(invisible(otherVariables))
}

#' @noRd
checkOtherVariablesEstimates <- function(otherVariablesEstimates, otherVariables, call = rlang::env_parent()) {
  if (!is.list(otherVariablesEstimates)) {
    otherVariablesEstimates <- list(otherVariablesEstimates)
  }
  omopgenerics::assertList(otherVariablesEstimates, class = "character", call = call)
  allEstimates <- PatientProfiles::availableEstimates(fullQuantiles = TRUE) |>
    dplyr::pull("estimate_name") |>
    unique()
  notEstimate <- unique(unlist(otherVariablesEstimates))
  notEstimate <- notEstimate[!notEstimate %in% allEstimates]
  if (length(notEstimate) > 0) {
    cli::cli_abort(
      "Not valid estimates found in otherVariablesEstimates: {notEstimate}.
      Please see valid estimates in: PatientProfiles::availableEstimates()",
      call = call
    )
  }
  if (length(otherVariablesEstimates) == 1 && length(otherVariables) > 1) {
    otherVariablesEstimates <- rep(otherVariablesEstimates, length(otherVariables))
  }
  if (is.null(names(otherVariablesEstimates))) {
    names(otherVariablesEstimates) <- paste0("other_", seq_along(otherVariablesEstimates))
  }
  return(invisible(otherVariablesEstimates))
}

correctStrata <- function(strata, overall) {
  if (length(strata) == 0 || overall) {
    strata <- c(list(character()), strata)
  }
  strata <- unique(strata)
  return(strata)
}

warnOverwriteColumns <- function(x, nameStyle, values = list()) {
  if (length(values) > 0) {
    nameStyle <- tidyr::expand_grid(!!!values) |>
      dplyr::mutate("tmp_12345" = glue::glue(.env$nameStyle)) |>
      dplyr::pull("tmp_12345") |>
      as.character() |>
      unique()
  }

  extraColumns <- colnames(x)[colnames(x) %in% nameStyle]
  if (length(extraColumns) > 0) {
    ms <- extraColumns
    names(ms) <- rep("*", length(ms))
    cli::cli_inform(message = c(
      "!" = "The following columns will be overwritten:", ms
    ))
    x <- x |> dplyr::select(!dplyr::all_of(extraColumns))
  }

  return(x)
}

assertIntersect <- function(intersect) {
  name <- substitute(intersect)
  functionName <- paste0(
    "PatientProfiles::add", toupper(substr(name, 1, 1)),
    substr(name, 2, nchar(name))
  )
  arguments <- formals(eval(parse(text = functionName)))
  arguments <- arguments[!names(arguments) %in% c("x", "cdm")]

  if (any(c("targetCohortTable", "tableName", "conceptSet") %in% names(intersect))) {
    intersect <- list(intersect)
  }

  namesIntersect <- names(intersect)
  if (is.null(namesIntersect)) {
    namesIntersect <- rep("", length(intersect))
  }

  for (k in seq_along(intersect)) {
    # get variables
    nams <- names(intersect[[k]])

    # validate
    extraArguments <- names(intersect[[k]])
    extraArguments <- extraArguments[!extraArguments %in% names(arguments)]
    if (length(extraArguments) > 0) {
      cli::cli_abort(c(
        "{extraArguments} are not arguments of {functionName}()."
      ))
    }
    required <- character()
    for (kk in seq_along(arguments)) {
      x <- arguments[[kk]]
      if (missing(x)) {
        required <- c(required, names(arguments)[kk])
      }
    }
    notPresent <- required[!required %in% names(intersect[[k]])]
    if (length(notPresent) > 0) {
      cli::cli_abort(c(
        "{notPresent} need to be provided for {functionName}()."
      ))
    }
    if ("window" %in% nams) {
      if (!is.list(intersect[[k]]$window)) {
        intersect[[k]]$window <- list(intersect[[k]]$window)
      }
      if (length(intersect[[k]]$window) != 1) {
        cli::cli_abort("{name}: only one window can be provided, please see examples.")
      }
      if (is.null(names(intersect[[k]]$window))) {
        names(intersect[[k]]$window) <- getWindowName(intersect[[k]]$window)
      } else if (names(intersect[[k]]$window) == "") {
        names(intersect[[k]]$window) <- getWindowName(intersect[[k]]$window)
      }
    } else {
      cli::cli_abort("{name}: please provide window argument.")
    }

    # add names if missing
    if (namesIntersect[k] == "") {
      if ("tableName" %in% nams) {
        tblName <- intersect[[k]]$tableName
      } else if ("conceptSet" %in% nams) {
        tblName <- "Concepts"
      } else {
        tblName <- intersect[[k]]$targetCohortTable
      }
      value <- name |>
        as.character() |>
        getValue()
      winName <- getWindowNames(intersect[[k]]$window)
      namesIntersect[k] <- paste(tblName, value, winName)
    }
  }

  names(intersect) <- namesIntersect

  return(invisible(intersect))
}
getWindowName <- function(win) {
  paste0(win[[1]][1], " to ", win[[1]][2])
}
