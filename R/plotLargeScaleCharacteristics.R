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
#' @param data output of summariseLargeScaleCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column.
#' Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column.
#' Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#'
#' @return A ggplot.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortCharacteristics)
#'
#' cdm <- mockCohortCharacteristics()
#'
#' concept <- dplyr::tibble(
#'   concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
#'   domain_id = NA_character_,
#'   vocabulary_id = NA_character_,
#'   concept_class_id = NA_character_,
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01")
#' ) |>
#'   dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#' cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' results <- cdm$cohort2 |>
#'   summariseLargeScaleCharacteristics(
#'     episodeInWindow = c("condition_occurrence"),
#'     minimumFrequency = 0
#'   )
#' graphs <- plotLargeScaleCharacteristics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'

plotLargeScaleCharacteristics <- function(data,
                                          position    = "horizontal",
                                          splitStrata = FALSE,
                                          facet       = NULL,
                                          colorVars   = "variable_level") {

  # Select percentage values of large scale characteristics
  data <- data |> dplyr::filter(.data$estimate_name == "percentage")

  # Position of the plot
  x <- positionFunction(position)
  xAxis <- x$xAxis
  yAxis <- x$yAxis

  # Split strata
  # if(splitStrata == TRUE){
  #   strata_names <- data$strata_name |> unique()
  #   data <- data |> visOmopResults::splitStrata()
  # }

  # Facet of the plot
  x <- facetFunction(facet, splitStrata, data)
  facetVarX <- x$facetVarX
  facetVarY <- x$facetVarY
  data      <- x$data

  return(plotfunction(data,
    xAxis,
    yAxis,
    plotStyle = "scatterplot",
    facetVarX,
    facetVarY,
    colorVars,
    vertical_x = FALSE
  ))
}

positionFunction <- function(position){
  if(position == "horizontal"){
    xAxis = "estimate_value"
    yAxis = "variable_name"
  }else if(position == "vertical"){
    xAxis = "variable_name"
    yAxis = "estimate_value"
  }else{
    stop(sprintf("'position' input must be either 'horizontal' or 'vertical'."))
  }
  return(list("xAxis" = xAxis, "yAxis" = yAxis))
}

facetFunction <- function(facet, splitStrata, data){
  checkmate::assertTRUE(inherits(facet, c("formula","character")))

  if(class(facet) == "formula"){
    facet <- Reduce(paste, deparse(facet))
  }

  # Extract facet names
  x <- extractFacetVar(facet)
  facetVarX <- x$facetVarX
  facetVarY <- x$facetVarY

  # Check facet names validity
  facetVarX <- checkFacetNames(facetVarX, splitStrata, data)
  facetVarY <- checkFacetNames(facetVarY, splitStrata, data)

  # Specific case - table name
  if("table_name" %in% c(facetVarX, facetVarY)){
    data <- data |> left_join(settings(lsc), by = c("result_id", "result_type", "cdm_name"))
  }

  return(list("facetVarX" = facetVarX, "facetVarY" = facetVarY, "data" = data))
}

extractFacetVar <- function(facet){

  if(unique(stringr::str_detect(facet,"~"))){
    # Separate x and y from the formula
    facetVarX <- gsub("~.*","",facet)
    facetVarY <- gsub(".*~","",facet)

    # Remove
    facetVarX <- stringr::str_split(facetVarX, pattern = "\\+")[[1]]
    facetVarY <- stringr::str_split(facetVarY, pattern = "\\+")[[1]]
  }else{
    if(length(facet) == 1){
      facetVarX <- facet
      facetVarY <- NULL
    }else{
      # Assign "randomly" the positions
      horizontal <- 1:round(length(facet)/2)
      vertical   <- (round(length(facet)/2)+1):length(facet)

      facetVarX <- facet[horizontal]
      facetVarY <- facet[vertical]
    }
  }

  return(list("facetVarX" = facetVarX, "facetVarY" = facetVarY))
}

checkFacetNames <- function(facetVar, splitStrata, data){
  if(!is.null(facetVar)){
    # Remove spaces at the beginning or at the end
    facetVar <- gsub(" $","",facetVar)
    facetVar <- gsub("^ ","",facetVar)

    # Replace empty spaces with "_"
    facetVar <- gsub(" ","_",facetVar)

    # Replace empty or "." facet by NULL
    if(TRUE %in% (facetVar %in% c("",".",as.character()))){
      facetVar <- NULL
    }

    # Turn to lower case
    facetVar <- tolower(facetVar)

    facetVar[facetVar == "cohort_name"] <- "group_level"
    facetVar[facetVar == "window_name"] <- "variable_level"

    # Check correct column names
    x <- unique(facetVar %in% c(NULL, "cdm_name", "group_level", "strata_level",
                                "variable_level", "strata", "table_name",  data |> visOmopResults::strataColumns()))

    if(c("FALSE") %in% as.character(x)){
      stop(sprintf(paste0(facetVar[!x]," is not a valid facet variable")))
    }

    # Specific cases - strata
    facetVar <- checkStrataName(facetVar, splitStrata)
  }
  return(facetVar)
}

checkStrataName <- function(facetVar,splitStrata){
  if("strata" %in% c(facetVar) & splitStrata == FALSE){
    facetVar <- gsub("strata","strata_level",facetVar)
  }else if("strata" %in% c(facetVar) & splitStrata == TRUE){
    facetVar[facetVar == "strata"] <- paste(c(data |> visOmopResults::strataColumns()), collapse = " &&& ")
    facetVar <- unlist(stringr::str_split(facetVar, pattern = " &&& "))
  }
  return(facetVar)
}
