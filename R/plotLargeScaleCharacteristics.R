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
#' @param position if set to *horizontal* the horizontal axis will plot "variable_name" column and the vertical axis "estimate_value" column.
#' If *vertical*, axis will be the other way around.
#' @param splitStrata boolean variable (TRUE/FALSE)
#' @param facet columns in data to facet. If the facet position wants to be specified, use the formula class for the input
#' (e.g., strata + table_name ~ group_level + cdm_name). Variables before "~" will be facet by on horizontal axis, whereas those after "~" on vertical axis.
#' Character format is also allowed (e.g., c("strata","table_name","group_level","cdm_name")).
#' Only the following columns are allowed to be facet by: c("cdm_name", "group_level", "strata_level", "variable_level", "strata", "table_name").
#' If splitStrata = TRUE, strata levels are also allowed.
#' @param colorVars column in data to color by. Only the following columns are allowed to be used: c("cdm_name", "group_level", "strata_level", "variable_level", "strata", "table_name").
#' If splitStrata = TRUE, strata levels are also allowed.
#'
#' @return A ggplot.
#'
#' @export
#'
plotLargeScaleCharacteristics <- function(data,
                                          position = "horizontal",
                                          splitStrata = FALSE,
                                          facet = NULL,
                                          colorVars = "variable_level") {
  if (length(data$result_id) != 0) {
    checkSettings(data)

    # Position of the plot
    x <- positionFunction(position)
    xAxis <- x$xAxis
    yAxis <- x$yAxis
    verticalX <- x$verticalX

    # Facet of the plot
    x <- facetFunction(facet, splitStrata, data)
    facetVarX <- x$facetVarX
    facetVarY <- x$facetVarY
    data <- x$data

    # Color of the plot
    checkName(colorVars, splitStrata, data, type = "colorVars")

    # All that is not a facet variable will be a color variable if colorVar = NULL
    colorVars <- colorVarsIfNull(data, vars = c(facetVarX, facetVarY), splitStrata, colorVars)

    # Split strata
    if (splitStrata == TRUE) {
      data <- data |> visOmopResults::splitStrata()
    }

    y <- plotfunction(data,
      xAxis,
      yAxis,
      plotStyle = "scatterplot",
      facetVarX,
      facetVarY,
      colorVars,
      vertical_x = verticalX
    )

    y <- addAxis(y, position)
  } else {
    y <- plotfunction(data)
  }
  return(y)
}

checkSettings <- function(data) {
  if (length(settings(data)$result_id) == 0) {
    stop(sprintf("Settings table is not present in the data. Please, when filtering the large scale characterisation table, include the following argument: filter( ... | variable_name == 'settings')"))
  }
}

positionFunction <- function(position) {
  if (position == "horizontal") {
    xAxis <- "estimate_value"
    yAxis <- "variable_name"
    verticalX <- FALSE
  } else if (position == "vertical") {
    xAxis <- "variable_name"
    yAxis <- "estimate_value"
    verticalX <- TRUE
  } else {
    stop(sprintf("'position' input must be either 'horizontal' or 'vertical'."))
  }
  return(list("xAxis" = xAxis, "yAxis" = yAxis, "verticalX" = verticalX))
}

facetFunction <- function(facet, splitStrata, data) {
  if (!is.null(facet)) {
    checkmate::assertTRUE(inherits(facet, c("formula", "character")))

    if (inherits(facet, "formula")) {
      facet <- Reduce(paste, deparse(facet))
    }

    # Extract facet names
    x <- extractFacetVar(facet)
    facetVarX <- x$facetVarX
    facetVarY <- x$facetVarY

    # Check facet names validity
    facetVarX <- checkFacetNames(facetVarX, splitStrata, data)
    facetVarY <- checkFacetNames(facetVarY, splitStrata, data)
  } else {
    facetVarX <- NULL
    facetVarY <- NULL
  }

  # Add table_name column
  data <- data |>
    visOmopResults::addSettings() |>
    dplyr::filter(
      .data$estimate_type == "percentage",
      .data$result_type == "summarised_large_scale_characteristics"
    )
  return(list("facetVarX" = facetVarX, "facetVarY" = facetVarY, "data" = data))
}


extractFacetVar <- function(facet) {
  if (unique(stringr::str_detect(facet, "~"))) {
    # Separate x and y from the formula
    facetVarX <- gsub("~.*", "", facet)
    facetVarY <- gsub(".*~", "", facet)

    # Remove
    facetVarX <- stringr::str_split(facetVarX, pattern = "\\+")[[1]]
    facetVarY <- stringr::str_split(facetVarY, pattern = "\\+")[[1]]
  } else {
    if (length(facet) == 1) {
      facetVarX <- facet
      facetVarY <- NULL
    } else {
      # Assign "randomly" the positions
      horizontal <- 1:round(length(facet) / 2)
      vertical <- (round(length(facet) / 2) + 1):length(facet)

      facetVarX <- facet[horizontal]
      facetVarY <- facet[vertical]
    }
  }

  return(list("facetVarX" = facetVarX, "facetVarY" = facetVarY))
}

checkFacetNames <- function(facetVar, splitStrata, data) {
  if (!is.null(facetVar)) {
    # Remove spaces at the beginning or at the end
    facetVar <- gsub(" $", "", facetVar)
    facetVar <- gsub("^ ", "", facetVar)

    # Replace empty spaces with "_"
    facetVar <- gsub(" ", "_", facetVar)

    # Turn to lower case
    facetVar <- tolower(facetVar)

    facetVar[facetVar == "cohort_name"] <- "group_level"
    facetVar[facetVar == "window_name"] <- "variable_level"
    facetVar[facetVar == "strata"] <- "strata_level"

    # Replace empty or "." facet by NULL
    if (TRUE %in% (facetVar %in% c("", ".", as.character()))) {
      facetVar <- NULL
    }

    # Check correct column names
    checkName(facetVar, splitStrata, data, type = "facet")

    # Specific cases - strata
    facetVar <- checkStrataName(facetVar, splitStrata, data)
  }
  return(facetVar)
}

checkStrataName <- function(facetVar, splitStrata, data) {
  if (("strata_level" %in% c(facetVar)) && splitStrata == TRUE) {
    facetVar[facetVar == "strata_level"] <- paste(c(data |> visOmopResults::strataColumns()), collapse = " &&& ")
    facetVar <- unlist(stringr::str_split(facetVar, pattern = " &&& "))
  }
  return(facetVar)
}

checkName <- function(var, splitStrata, data, type) {
  # Check correct column names
  if (!is.null(var)) {
    if (splitStrata == TRUE) {
      x <- var %in% c(
        "cdm_name", "group_level", "strata_level",
        "variable_level", "table_name", data |> visOmopResults::strataColumns()
      )
      if (FALSE %in% x) {
        stop(sprintf(paste0(var[!x], " is not a valid variable for ", type)))
      }
    } else if (splitStrata == FALSE) {
      x <- var %in% c("cdm_name", "group_level", "strata_level", "variable_level", "table_name")
      y <- var %in% (data |> visOmopResults::strataColumns())

      if (FALSE %in% x) {
        if (TRUE %in% y) {
          stop("'", sprintf(paste0(var[y], "' is not a valid value for ", type, " name when 'splitStrata = FALSE'. Try 'splitStrata = TRUE'")))
        } else {
          stop("'", sprintf(paste0(var[!x], "' is not a valid value for ", type)))
        }
      }
    }
  }
}

colorVarsIfNull <- function(data, vars, splitStrata, colorVars) {
  if (is.null(colorVars) && splitStrata == TRUE) {
    colorVars <- c(
      "cdm_name", "group_level",
      "variable_level", "table_name", data |> visOmopResults::strataColumns()
    )
    colorVars <- colorVars[!c(colorVars %in% vars)]
  } else if (is.null(colorVars) && splitStrata == FALSE) {
    colorVars <- c(
      "cdm_name", "group_level", "strata_level",
      "variable_level", "table_name"
    )
    colorVars <- colorVars[!c(colorVars %in% vars)]
  }

  return(colorVars)
}

addAxis <- function(y, position) {
  if (position == "horizontal") {
    y <- y +
      ggplot2::xlab("Estimate") +
      ggplot2::ylab("Concept id")
  } else {
    y <- y +
      ggplot2::ylab("Estimate") +
      ggplot2::xlab("Concept id")
  }
  return(y)
}
