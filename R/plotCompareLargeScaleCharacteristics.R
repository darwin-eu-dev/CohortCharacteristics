
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
                                                 colorVars   = NULL)


plotCompareLargeScaleCharacteristics <- function(data,
                                                 reference,
                                                 comparator  = NULL,
                                                 facet       = NULL,
                                                 colorVars   = NULL) {

  checkReferenceAndComparator(reference,  data, type = "reference")
  checkReferenceAndComparator(comparator, data, type = "comparator")


  return(y)
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
      if(FALSE %in% unique()){
        stop(sprintf(paste0(names(reference)[x], " must be a column in large scale characterisation table.\n")))
      }

      # Check that inputs are elements in the lsc table
      for(x in names(reference)){
        y <- data |> select(x)

        if(reference[x] %in% (data |> select(x) |> distinct() |> pull())){
          stop(sprintf(paste0(reference[x], " is not present in column ", x)))
        }
      }
    }
  }

}
