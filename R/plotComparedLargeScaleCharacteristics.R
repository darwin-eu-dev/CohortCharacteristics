
plotComparedLargeScaleCharacteristics <- function(data,
                                                 referenceGroupLevel    = NULL,
                                                 referenceStrataLevel   = NULL,
                                                 # referenceStrataLevel = c("<40 &&& Female")
                                                 # referenceStrataLevel = c("<40", "Female")
                                                 # referenceStrataLevel = list(age_group = "<40", sex = "Female") || c("overall") || c("<40) - but if it's a character it can only have length 1!
                                                 referenceVariableLevel = NULL,
                                                 referenceCdmName       = NULL,
                                                 facet       = NULL,
                                                 splitStrata = FALSE,
                                                 colorVars   = NULL,
                                                 missings    = 0){

  checkSettings(data)
  referenceGroupLevel    <- checkReference(referenceGroupLevel,    data, type = "group_level",    argument = "referenceGroupLevel")
  referenceVariableLevel <- checkReference(referenceVariableLevel, data, type = "variable_level", argument = "referenceVariableLevel")
  referenceCdmName       <- checkReference(referenceCdmName,       data, type = "cdm_name",       argument = "referenceCdmName")

  referenceStrataLevel <- checkReferenceStrataLevel(referenceStrataLevel, data)

  # Extract facet names
  x <- facetFunction(facet, splitStrata, data)
  facetVarX <- x$facetVarX
  facetVarY <- x$facetVarY
  data      <- x$data

  # Color of the plot
  checkName(colorVars, splitStrata, data, type = "colorVars")

  # All that is not a facet variable will be a color variable if colorVar = NULL
  colorVars <- colorVarsIfNull(data, vars = c(facetVarX, facetVarY, referenceGroupLevel, referenceVariableLevel, referenceCdmName), splitStrata, colorVars)

  # Split strata
  if(splitStrata == TRUE){
    data <- data |> visOmopResults::splitStrata()
  }

  # Tidying dataset
  data <- tidyData(data, referenceGroupLevel, referenceVariableLevel, referenceCdmName, referenceStrataLevel, missings)

  # Change facet names
  facetVarX <- changeNames(reference, facetVarX, type = "facet")
  facetVarY <- changeNames(reference, facetVarY, type = "facet")

  # Change colorVars names
  colorVars <- changeNames(reference, colorVars, type = "colorVars")

  # Edit plot
  y <- editPlot(data = data, facetVarX = facetVarX, facetVarY = facetVarY, colorVars = colorVars, vertical_x = FALSE)
  return(y)
}


checkReference <- function(reference, data, type, argument){
  if(length(reference) > 1){
    stop(sprintf(paste0(argument, " must have length = 1.")))
  }
  if(is.null(reference)){
    # Check there is only one type
    if(length(unique(data[[type]])) > 1){
      stop(sprintf(paste0("Please, use the ", argument," argument to define which ", type, " must be used as a reference: ", paste0(paste0("'",unique(data[[type]]),"'"), collapse = " or "))))
    }else{
      reference <- unique(data[[type]])
    }
  }else{
    # Format variable
    if(!inherits(reference, "character")){
      stop(sprintf(paste0("'", argument,"' must be a character string.")))
    }
    # If variable is not present in the dataset
    if(!reference %in% data[[type]]){
      stop(sprintf(paste0("'", reference,"' is not present in ", type," column.")))
    }
  }
  return(reference)
}

checkReferenceStrataLevel <- function(referenceStrataLevel, data){
  referenceStrataLevel <- "overall"
}

tidyData <- function(data, referenceGroupLevel, referenceVariableLevel, referenceCdmName, referenceStrataLevel, missings){
  # Create table reference
  table_reference  <- data |>
    dplyr::filter(.data$group_level %in% .env$referenceGroupLevel) |>
    dplyr::filter(.data$variable_level %in% .env$referenceVariableLevel) |>
    dplyr::filter(.data$cdm_name %in% .env$referenceCdmName)

  # Create table comparator
  table_comparator <- data |>
    dplyr::anti_join(
      table_reference
    )

  if((table_comparator$result_id |> length())== 0){
    stop(sprintf(paste0("There is no comparator variables in the data.")))
  }

  # Rename tables
  table_reference <- table_reference |>
    dplyr::rename_at(dplyr::vars(-c("variable_name", "result_id", "result_type", "package_name", "package_version",
                              "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level")), ~paste0(.,"_reference"))
  table_comparator <- table_comparator |>
    dplyr::rename_at(dplyr::vars(-c("variable_name", "result_id", "result_type", "package_name", "package_version",
                                    "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level")), ~paste0(.,"_comparator"))

  # Join both tables
  data <- table_reference |>
    dplyr::full_join(
      table_comparator,
      by = c("variable_name", "result_id", "result_type", "package_name", "package_version",
             "estimate_name", "estimate_type", "table_name", "type", "analysis", "additional_name", "additional_level")
    ) |>
    distinct()

  # Cleaning the dataset
  data <- data |>
    dplyr::mutate(estimate_value_comparator = as.numeric(.data$estimate_value_comparator),
                  estimate_value_reference  = as.numeric(.data$estimate_value_reference))

  # Replace reference missings
  data <- replaceReferenceMissings(data, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings)

  # Replace comparator missings
  data <- replaceComparatorMissings(data, table_comparator, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings)

  for(i in colnames(data)){
    if(grepl("comparator", i) & i != "estimate_value_comparator"){
      data <- data |>
        dplyr::mutate(dplyr::across(
          .cols = .data[[i]],
          .fns  = ~dplyr::if_else(is.na(.x), .data[[gsub("comparator","reference",i)]], .x)
        ))
    }
  }

  data <- data |>
    tidyr::replace_na(list(estimate_value_comparator = missings,
                           estimate_value_reference = missings)) |>
    dplyr::rename(estimate_value = estimate_value_reference)
}

replaceReferenceMissings <- function(data, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings){
  data <- data |>
    mutate(cdm_name_reference       = referenceCdmName,
           group_name_reference     = data |> select(group_name_reference) |> filter(!is.na(group_name_reference)) |> distinct() |> pull(),
           group_level_reference    = referenceGroupLevel,
           variable_level_reference = referenceVariableLevel,
           strata_name_reference    = "overall",
           strata_level_reference   = referenceStrataLevel,
           estimate_value_reference = if_else(is.na(estimate_value_reference), missings, estimate_value_reference))

  return(data)
}

replaceComparatorMissings <- function(data, table_comparator, referenceCdmName, referenceGroupLevel, referenceVariableLevel, referenceStrataLevel, missings){
  comparator_groups <- table_comparator |>
    select(cdm_name_comparator, group_name_comparator, group_level_comparator, strata_name_comparator, strata_level_comparator, variable_level_comparator) |>
    distinct()

  data <- data |>
    select(-c(ends_with("comparator"))) |>
    distinct() |>
    cross_join(comparator_groups) |>
    left_join(
      data |>
        select("variable_name", ends_with("comparator"))
    ) |>
    mutate(estimate_value_comparator = if_else(is.na(estimate_value_comparator), missings, estimate_value_comparator))

  return(data)
}

changeNames <- function(reference, var, type){
  if(!is.null(var)){
    # Change facet names to do the plot
    if(TRUE %in% (names(reference) %in% var)){
      warning(paste0(type," variable and reference variable are identic. Consider using different variables."))
    }else{
      var <- paste0(var,"_comparator")
    }
  }

  var <- gsub("table_name_comparator","table_name",var)
  return(var)
}

editPlot <- function(data, facetVarX = NULL, facetVarY = NULL, colorVars = NULL, vertical_x = FALSE){
    plotfunction(data  = data,
                 xAxis = "estimate_value",
                 yAxis = "estimate_value_comparator",
                 plotStyle = "scatterplot",
                 facetVarX,
                 facetVarY,
                 colorVars,
                 vertical_x) +
      ggplot2::scale_x_continuous(limits = c(0, 100)) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::geom_abline(slope = 1, colour = "red", linetype = 2, size = 0.25) +
      ggplot2::labs(x = "Reference", y = "Comparator")
}
