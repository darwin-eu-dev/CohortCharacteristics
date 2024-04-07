test_that("summarise cohortIntersect", {
  skip_on_cran()
  cdm <- CohortCharacteristics::mockCohortCharacteristics()
  expect_error(res <- CohortCharacteristics::summariseCohortIntersect(
    cohort = cdm$cohort1
  ))

  expect_no_error(CohortCharacteristics::summariseCohortIntersect(
    cohort = cdm$cohort1,
    cohortIntersect = list(
      targetCohortTable = "cohort2",value = "flag", window = c(0,0)
    )
  ))

})

