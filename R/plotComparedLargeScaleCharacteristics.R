
plotComparedLargeScaleCharacteristics <- function(data,
                                                 reference, # c("cohort_1") | list(cdm_name = "X",
                                                 #                      group_level  = "X",
                                                 #                      strata_level = "X",
                                                 #                      window_level = "X" / variable_level = "X")
                                                 # comparator  = NULL, # NULL := everything that is not the reference cohort,
                                                 # c("cohort_2") | list(cdm_name = "X",
                                                 #                      group_level  = "X",
                                                 #                      strata_level = "X",
                                                 #                      window_level = "X"),
                                                 facet       = NULL,
                                                 splitStrata = FALSE,
                                                 colorVars   = NULL,
                                                 missings    = 0){

  # Check inputs
  checkReference(reference, data, type = "reference")

  # Extract facet names
  x <- facetFunction(facet, splitStrata, data)
  facetVarX <- x$facetVarX
  facetVarY <- x$facetVarY
  data      <- x$data

  # Color of the plot
  checkName(colorVars, splitStrata, data, type = "colorVars")

  # All that is not a facet variable will be a color variable if colorVar = NULL
  colorVars <- colorVarsIfNull(data, vars = c(facetVarX, facetVarY, names(reference)), splitStrata, colorVars)

  # Split strata
  if(splitStrata == TRUE){
    data <- data |> visOmopResults::splitStrata()
  }

  # Tidying dataset
  data <- tidyData(data, reference, missings)

  # Change facet names
  facetVarX <- changeNames(reference, facetVarX, type = "facet")
  facetVarY <- changeNames(reference, facetVarY, type = "facet")

  # Change colorVars names
  colorVars <- changeNames(reference, colorVars, type = "colorVars")

  # Edit plot
  y <- editPlot(data = data, facetVarX = facetVarX, facetVarY = facetVarY, colorVars = colorVars, vertical_x = FALSE)
  return(y)
}

checkReference <- function(reference, data, type){
  if(!is.null(reference)){
    # Reference can be a character or a list
    if(!inherits(reference, c("list","character"))){
      stop(sprintf(paste0("'", type, "' must be either a list or a character.")))
    }

    # If is a character
    if(inherits(reference, c("character"))){
      if(length(reference) > 1){
        stop(sprintf("'", type,"' argument must be length = 1. If willing to add more details, please use list() format."))
      }

      if(!reference %in% data$group_level){
        stop(sprintf(paste0("'", reference, "' must be a group_level value.")))
      }
    }

    # If is a list
    if(inherits(reference, c("list"))){
      # Change window_level to variable_level for internal usage
      names(reference)[names(reference) == "window_level"] <- "variable_level"

      # Check that inputs correspond to tables of the lsc
      x <- names(reference) %in% colnames(data)
      if(FALSE %in% x){
        stop(sprintf(paste0(names(reference)[x], " must be a column in large scale characterisation table.\n")))
      }

      # Check that inputs are elements in the lsc table
      for(x in names(reference)){
        y <- data |> dplyr::select(.data[[x]])

        if(!reference[x] %in% (data |> dplyr::select(.data[[x]]) |> dplyr::distinct() |> dplyr::pull())){
          stop(sprintf(paste0(reference[x], " is not present in column ", x)))
        }
      }
    }
  }

}

tidyData <- function(data, reference, missings){
  # Create table reference
  table_reference  <- data
  for(x in names(reference)){
    table_reference <- table_reference |>
      dplyr::filter(.data[[x]] == reference[[x]])
  }

  # Create table comparator
  table_comparator <- data |>
    dplyr::anti_join(table_reference)

  # Rename tables
  table_reference <- table_reference |>
    dplyr::rename_at(dplyr::vars(-c("variable_name", "result_type",
                             "package_version", "estimate_name", "estimate_type", dplyr::all_of(names(reference)))), ~paste0(.,"_reference"))
  table_comparator <- table_comparator |>
    dplyr::rename_at(dplyr::vars(-c("variable_name", "result_type",
                             "package_version", "estimate_name", "estimate_type", dplyr::all_of(names(reference)))), ~paste0(.,"_comparator"))

  # Join both tables
  data <- table_reference |>
    dplyr::full_join(
      table_comparator,
      by = c("variable_name", "result_type", "package_version", "estimate_name", "estimate_type", dplyr::all_of(names(reference)))
    )

  # Cleaning the dataset
  data <- data |>
    dplyr::mutate(estimate_value_comparator = as.numeric(.data$estimate_value_comparator),
                  estimate_value_reference  = as.numeric(.data$estimate_value_reference))

  # Replace missings
  for(i in colnames(data)){
    if(grepl("reference", i) & i != "estimate_value_comparator"){
      data <- data |>
        dplyr::mutate(dplyr::across(
          .cols = .data[[i]],
          .fns  = ~dplyr::if_else(is.na(.x), .data[[gsub("reference","comparator",i)]], .x)
        ))
    }
  }

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
    tidyr::replace_na(list(estimate_value_comparator = .env$missings,
                           estimate_value_reference = .env$missings)) |>
    dplyr::rename(estimate_value = .data$estimate_value_reference)
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
