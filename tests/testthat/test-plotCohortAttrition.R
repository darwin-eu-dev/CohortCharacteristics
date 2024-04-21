test_that("multiplication works", {
  library(CDMConnector)
  library(DrugUtilisation)
  library(dplyr)
  library(DiagrammeR)
  library(PatientProfiles)

  cdm <- mockDrugUtilisation(n = 1000)

  cdm[["cohort1"]] <- cdm[["cohort1"]] |>
   filter(year(cohort_start_date) >= 2000) |>
    recordCohortAttrition("Restrict to cohort_start_date >= 2000") |>
    filter(year(cohort_end_date) < 2020) |>
    recordCohortAttrition("Restrict to cohort_end_date < 2020") |>
    compute(temporary = FALSE, name = "cohort1")

  ca <- attrition(cdm[["cohort1"]]) |>
    filter(cohort_definition_id == 2)

  x <- render_graph(plotCohortAttrition(ca))

  expect_true(inherits(x,c("grViz","htmlwidget")))

  # Test empty data
  ca <- ca |> filter(cohort_definition_id == 10)
  x <- render_graph(plotCohortAttrition(ca))
  expect_true(inherits(x,c("grViz","htmlwidget")))
})
