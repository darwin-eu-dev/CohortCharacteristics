test_that("plot cohort attrition", {
  cdm <- mockCohortCharacteristics(numberIndividuals = 1000)

  cdm[["cohort1"]] <- cdm[["cohort1"]] |>
    dplyr::filter(cohort_start_date >= as.Date("2000-01-01")) |>
    omopgenerics::recordCohortAttrition("Restrict to cohort_start_date >= 2000") |>
    dplyr::filter(cohort_end_date < as.Date("2020-01-01")) |>
    omopgenerics::recordCohortAttrition("Restrict to cohort_end_date < 2020") |>
    dplyr::compute(temporary = FALSE, name = "cohort1")

  ca <- cdm[["cohort1"]] |> summariseCohortAttrition()

  x <- plotCohortAttrition(ca, cohortId = 2)

  expect_true(inherits(x, c("grViz", "htmlwidget")))

  # Test empty data
  ca <- ca |> dplyr::filter(result_id == 10)
  x <- plotCohortAttrition(ca)
  expect_true(inherits(x, c("grViz", "htmlwidget")))
})
