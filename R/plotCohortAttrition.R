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
#' @param x attrition table
#'
#' @return A dgr_graph
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(CodelistGenerator)
#' library(dplyr)
#' con <- DBI::dbConnect(duckdb::duckdb(),
#' dbdir = CDMConnector::eunomia_dir())
#' cdm <- CDMConnector::cdm_from_con(con,
#' cdm_schem = "main",
#' write_schema = "main")
#' meds_cs <- getDrugIngredientCodes(cdm = cdm, name = c("acetaminophen",
#' "morphine"))
#' cdm <- generateConceptCohortSet(
#' cdm = cdm,
#' name = "meds",
#' conceptSet = meds_cs,
#' end = "event_end_date",
#' limit = "all",
#' overwrite = TRUE)
#'
#' cdm[["meds"]] <- cdm[["meds"]] |>
#' filter(year(cohort_end_date) <= 1949) |>
#' recordCohortAttrition("Restrict to cohort_start_date <= 1949") |>
#' filter(year(cohort_start_date) >= 1920) |>
#' recordCohortAttrition("Restrict to cohort_end_date >= 1920") |>
#' compute(temporary = FALSE, name = "meds")
#'
#' ca <- attrition(cdm[["meds"]]) |> filter(cohort_definition_id == 2) |>
#' mutate(number_records = 161137831)
#'
#' render_graph(plotCohortAttrition(ca))
#' }

plotCohortAttrition <- function(x) {
  # Turn everything as a character
  x <- x |> dplyr::mutate_all(~as.character(.))

  # Create table to be used in the graph
  xn <- createLabels(x)

  y <- selectLabels(xn)
  xn  <- y$xn
  att <- y$att

  # Create graph
  n  <- nrow(x)
  xg <- create_graph()

  att <- validateReason(att)
  h2  <- getHeightMiddleBox(att)

  w1 <- getWidthMainBox(xn)
  p1 <- getPositionMainBox(xn,n,h2)

  w2 <- getWidthMiddleBox(att)
  p2 <- getPositionMiddleBox(p1)

  xg <- getNodes(xn,att,n,xg,h2,w1,p1,w2,p2)

  return(xg)
}


formatNum <- function(col) {
  dplyr::if_else(
    !is.na(as.numeric(col)),
    gsub(" ", "", format(as.integer(col), big.mark=",")),
    col
  )
}

createLabels <- function(x){
  x <- x |>
    dplyr::arrange(reason_id) |>
    dplyr::mutate(
      number_subjects = formatNum(number_subjects),
      number_records  = formatNum(number_records),
      excluded_subjects = formatNum(excluded_subjects),
      label = paste0(
        "N subjects = ", number_subjects, "\nN records = ", number_records
      )
    )
  return(x)
}

selectLabels <- function(xn){
  if(nrow(xn) == 1){
    xn <- xn |>
      dplyr::mutate(label = paste0("Qualifying events", "\n", label)) |>
      dplyr::select(label)

    att <- NULL
  }else{
    att <- xn |>
      dplyr::filter(reason_id > min(reason_id)) |>
      dplyr::mutate(label = paste0(
        "N subjects = ", excluded_subjects, "\nN records = ", excluded_records
      )
      ) |>
      dplyr::select(reason, label)

    xn <- xn |>
      dplyr::mutate(
        label = dplyr::if_else(
          reason_id == min(reason_id),
          paste0("𝗜𝗻𝗶𝘁𝗶𝗮𝗹 𝗲𝘃𝗲𝗻𝘁𝘀", "\n", label),
          dplyr::if_else(
            reason_id == max(reason_id),
            paste0("𝗙𝗶𝗻𝗮𝗹 𝗲𝘃𝗲𝗻𝘁𝘀", "\n", label),
            label
          )
        )
      ) |>
      dplyr::select(label)
  }
  return(list("xn" = xn, "att" = att))
}

getWidthMainBox <- function(xn){
  return(0.08*max(nchar(strsplit(xn$label[1], split = "\n")[[1]])))
}

getPositionMainBox <- function(xn,n,h2){
  return(n + 1 - seq_len(n) -cumsum(append(0,h2-0.2)))
}

getPositionMiddleBox <- function(p1){
  return((p1[1:(length(p1)-1)] - p1[2:length(p1)])/2 + (p1[2:(length(p1))]))
}

validateReason <- function(att){

  n_char <- nchar(att$reason)
  n_char_count <- round(n_char/35)
  n_char_count[n_char_count > 1] = n_char_count[n_char_count > 1] - 1

  for(k in seq_len(nrow(att))){

    cut <- seq_len(n_char_count[k])
    cut <- cut*35
    positions <- unlist(gregexpr(' ', att$reason[k]))

    matrix_positions <- matrix(positions,length(cut), length(positions), byrow = TRUE)

    positions <- unique(matrix_positions[seq_len(length(cut)),  apply(abs(matrix_positions - cut),1,which.min)])

    for(kk in positions){
      substr(att$reason[k], start = kk, stop = kk+1) <- "\n"
    }

    # Ensure that we do not have placed \n at the end of the string
    att$reason[k] <- sub("\n$","",att$reason[k])
  }



  return(att)
}

getHeightMiddleBox <- function(att){
  return((stringr::str_count(att$reason, pattern = "\n")+1)*0.2)
}

getWidthMiddleBox <- function(att){
  return(min(2.25,0.08*max(nchar(unlist(strsplit(att$reason,"\n"))))))
}

getNodes <- function(xn,att,n,xg,h2,w1,p1,w2,p2){
  for (k in seq_len(n)) {
    xg <- xg %>%
      add_node(
        label = xn$label[k],
        node_aes = node_aes(
          shape = "box",
          x = 1,
          width = w1,
          y = p1[k] + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 11, fontcolor = "black",
          fontname = "Calibri",
          penwidth = ifelse(k == 1 | k == n, 2, 1),
          color = "black",
          fillcolor = "#F0F8FF"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        add_edge(from = k - 1, to = k, edge_aes = edge_aes(color = "black"))
    }
  }

  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      xg <- xg %>%
        add_node(
          label = att$label[k],
          node_aes = node_aes(
            shape = "box",
            x = 3,
            width = 1.2,
            y = p2[k],
            height = 0.4,
            fontsize = 9,
            fillcolor = "grey",
            fontcolor = "black",
            color = "black",
            fontname = "Calibri"
          )
        ) %>%
        add_node(
          label = att$reason[k],
          node_aes = node_aes(
            shape = "box",
            x = 1,
            width = w2,
            y = p2[k],
            height = h2[k],
            fillcolor = "white",
            color = "black",
            fontcolor = "back",
            fontname = "Calibri",
            fontsize = 10
          )
        ) %>%
        add_edge(
          from = 2*k + n, to = 2*k + n -1, edge_aes = edge_aes(color = "black")
        )
    }
  }
  return(xg)
}
