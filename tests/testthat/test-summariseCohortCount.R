test_that("multiplication works", {
  skip_on_cran()
  set.seed(123456)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence(recordPerson = 3) |>
    omock::mockDrugExposure(recordPerson = 4.5) |>
    omock::mockCohort(
      numberCohorts = 3, cohortName = c("covid", "tb", "asthma")
    )

  cdm1 <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main"
  )

  cdm2 <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main"
  )

  result1 <- cdm1$cohort |>
    summariseCohortCount()

  result2 <- cdm2$cohort |>
    summariseCohortCount()

  expect_identical(result1, result2)

  PatientProfiles::mockDisconnect(cdm = cdm)
})
