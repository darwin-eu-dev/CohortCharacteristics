
plotCompareLargeScaleCharacteristics <- function(data,
                                                 reference, # c("cohort_1") | list(cdm_name = "X",
                                                            #                      group_level  = "X",
                                                            #                      strata_level = "X",
                                                            #                      window_level = "X" / variable_level = "X")
                                                 comparator  = NULL, # NULL := everything that is not the reference cohort,
                                                                     # c("cohort_1") | list(cdm_name = "X",
                                                                     #                      group_level  = "X",
                                                                     #                      strata_level = "X",
                                                                     #                      window_level = "X"),
                                                 facet       = NULL,
                                                 colorVars   = NULL,
                                                 missings    = 0){


  # Check inputs
  checkReferenceAndComparator(reference,  data, type = "reference")
  checkReferenceAndComparator(comparator, data, type = "comparator")

  # Tidying dataset
  data <- tidyData(data, reference, comparator, missings)


  return( plotfunction(data,
                       xAxis = "estimate_value",
                       yAxis = "estimate_value_comparator",
                       plotStyle = "scatterplot",
                       facetVarX = NULL,
                       facetVarY = NULL,
                       colorVars = NULL,
                       vertical_x = FALSE))
}

checkReferenceAndComparator <- function(reference, data, type){

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

      if(!reference %in% group_level){
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
        y <- data |> select(x)

        if(!reference[x] %in% (data |> select(x) |> distinct() |> pull())){
          stop(sprintf(paste0(reference[x], " is not present in column ", x)))
        }
      }
    }
  }

}

tidyData <- function(data, reference, comparator, missings){
  data <- data |> filter(estimate_type == "percentage",
                         result_type == "summarised_large_scale_characteristics",
                         package_name == "PatientProfiles")

  # Create table reference
  table_reference  <- data |> filter(result_id == 0.5)
  for(x in names(reference)){
    table_reference <- table_reference |>
      union_all(
        data |>
          filter(.data[[x]] == reference[[x]])
      )
  }

  # Create table comparator
  if(!is.null(comparator)){
    table_comparator <- table_reference
    for(x in names(comparator)){
      table_comparator <- table_comparator |>
        union_all(
          data |>
            filter(x %in% comparator[x])
        )
    }
  }else{
    table_comparator <- data |>
      anti_join(table_reference)
  }

  # Rename tables
  table_reference <- table_reference |>
    rename_at(vars(-c("variable_name", "package_name", "result_type", "package_name",
                      "package_version", "estimate_name", "estimate_type", all_of(names(comparator)))), ~paste0(.,"_reference"))
  table_comparator <- table_comparator |>
    rename_at(vars(-c("variable_name", "package_name", "result_type", "package_name",
                      "package_version", "estimate_name", "estimate_type", all_of(names(comparator)))), ~paste0(.,"_comparator"))

  # Join both tables
  data <- table_reference |>
    full_join(
      table_comparator,
      by = c("variable_name", "package_name", "result_type", "package_version", "estimate_name", "estimate_type", all_of(names(comparator)))
    )

  # Cleaning the dataset
  data <- data |>
    mutate(estimate_value_comparator = as.numeric(estimate_value_comparator),
           estimate_value_reference  = as.numeric(estimate_value_reference)) |>
    mutate(result_id_reference        = if_else(is.na(result_id_reference),                result_id_comparator,        result_id_reference),
           cdm_name_reference         = if_else(is.na(cdm_name_reference),                  cdm_name_comparator,         cdm_name_reference),
           group_name_reference       = if_else(is.na(group_name_reference),              group_name_comparator,       group_name_reference),
           group_level_reference      = if_else(is.na(group_level_reference),            group_level_comparator,      group_level_reference),
           strata_name_reference      = if_else(is.na(strata_name_reference),            strata_name_comparator,      strata_name_reference),
           strata_level_reference     = if_else(is.na(strata_level_reference),          strata_level_comparator,     strata_level_reference),
           variable_level_reference   = if_else(is.na(variable_level_reference),      variable_level_comparator,   variable_level_reference),
           additional_name_reference  = if_else(is.na(additional_name_reference),    additional_name_comparator,  additional_name_reference),
           additional_level_reference = if_else(is.na(additional_level_reference),  additional_level_comparator, additional_level_reference)) |>
    mutate(result_id_comparator        = if_else(is.na(result_id_comparator),                result_id_reference,        result_id_comparator),
           cdm_name_comparator         = if_else(is.na(cdm_name_comparator),                  cdm_name_reference,         cdm_name_comparator),
           group_name_comparator       = if_else(is.na(group_name_comparator),              group_name_reference,       group_name_comparator),
           group_level_comparator      = if_else(is.na(group_level_comparator),            group_level_reference,      group_level_comparator),
           strata_name_comparator      = if_else(is.na(strata_name_comparator),            strata_name_reference,      strata_name_comparator),
           strata_level_comparator     = if_else(is.na(strata_level_comparator),          strata_level_reference,     strata_level_comparator),
           variable_level_comparator   = if_else(is.na(variable_level_comparator),      variable_level_reference,   variable_level_comparator),
           additional_name_comparator  = if_else(is.na(additional_name_comparator),    additional_name_reference,  additional_name_comparator),
           additional_level_comparator = if_else(is.na(additional_level_comparator),  additional_level_reference, additional_level_comparator)) |>
    tidyr::replace_na(list(estimate_value_comparator = missings, estimate_value_reference = missings)) |>
    rename(estimate_value = estimate_value_reference)


}
