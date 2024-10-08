test_that("summariseCohortOverlap", {
  person <- dplyr::tibble(
    person_id = c(1:20, 199) |> as.integer(),
    gender_concept_id = 8532L,
    year_of_birth = sample(1950:2000L, size = 21, replace = T),
    month_of_birth = sample(1:12L, size = 21, replace = T),
    day_of_birth = sample(1:28L, size = 21, replace = T),
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5), 5) |>
      as.integer(),
    subject_id = c(
      20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10,
      6, 18, 5, 1, 20, 14, 13, 8, 17, 3,
      16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18,
      5, 12, 3, 14, 13, 199
    ) |> as.integer(),
    cohort_start_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5), "1989-03-31"
    )),
    cohort_end_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5), "1989-03-31"
    ))
  )

  obs <- dplyr::tibble(
    observation_period_id = c(1:20, 199) |> as.integer(),
    person_id = c(1:20, 199) |> as.integer(),
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date = as.Date("2025-01-01"),
    period_type_concept_id = 0L
  )

  cdm <- mockCohortCharacteristics(
    con = connection(), writeSchema = writeSchema(),
    person = person, observation_period = obs, table = table
  )

  overlap1 <- summariseCohortOverlap(cdm$table)
  expect_equal(
    omopgenerics::resultColumns("summarised_result"),
    colnames(overlap1)
  )
  expect_equal(
    overlap1$group_name |> unique(),
    "cohort_name_reference &&& cohort_name_comparator"
  )
  expect_true(nrow(overlap1) == 5 * 4 * 6)
  expect_true(overlap1 |> dplyr::filter(grepl("cohort_5", .data$group_level)) |> dplyr::pull(estimate_value) |> unique() == "0")
  expect_equal(unique(overlap1$strata_name), "overall")
  expect_equal(unique(overlap1$strata_level), "overall")
  expect_equal(unique(overlap1$additional_name), "overall")
  expect_equal(unique(overlap1$additional_level), "overall")
  expect_true(all(c("count", "percentage") %in% unique(overlap1$estimate_name)))


  # strata and cohortID ----
  overlap2 <- summariseCohortOverlap(cdm$table,
    cohortId = 1:2
  )
  expect_true(nrow(overlap2) == 2 * 6)
  expect_true(all(
    c("cohort_2 &&& cohort_1", "cohort_1 &&& cohort_2") %in%
      unique(overlap2$group_level)
  ))

  cdm$table <- cdm$table |>
    PatientProfiles::addAge(ageGroup = list(c(0, 40), c(41, 100))) |>
    PatientProfiles::addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  s <- cdm$table |>
    dplyr::filter(cohort_definition_id %in% 1:2) |>
    dplyr::distinct(age_group, sex)
  s1 <- nrow(s |> dplyr::collect())
  s2 <- nrow(s |> dplyr::distinct(age_group) |> dplyr::collect())

  overlap3 <- summariseCohortOverlap(cdm$table,
    cohortId = 1:2,
    strata = list("age_group", c("age_group", "sex"))
  )
  expect_true(all(unique(overlap3$group_level) %in%
    unique(overlap1$group_level)))
  expect_true(all(c("overall", "age_group", "age_group &&& sex") %in%
    unique(overlap3$strata_name)))
  expect_true(nrow(overlap3) == 2 * 6 + 2 * 6 * s1 + 2 * 6 * s2)

  mockDisconnect(cdm)
})

test_that("expect result is deterministic", {
  set.seed(123456)
  cdm <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 3, cohortName = c("covid", "tb", "asthma"))
  cdm$cohort <- cdm$cohort |>
    dplyr::inner_join(
      cdm$person |>
        dplyr::select("subject_id" = "person_id") |>
        dplyr::mutate(idep = paste0("Q", sample(1:4L, dplyr::n(), replace = TRUE))),
      by = "subject_id"
    )
  aG <- list("<=20" = c(0, 20), ">20" = c(21, Inf))
  st <- list("sex", "idep", "age_group", c("age_group", "sex"))

  cdm1 <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main"
  )
  cdm1$cohort <- cdm1$cohort |>
    PatientProfiles::addDemographics(
      age = FALSE, priorObservation = FALSE, futureObservation = FALSE,
      ageGroup = aG, name = "cohort"
    ) |>
    omopgenerics::newCohortTable()

  cdm2 <- CDMConnector::copyCdmTo(
    con = duckdb::dbConnect(duckdb::duckdb()), cdm = cdm, schema = "main"
  )
  cdm2$cohort <- cdm2$cohort |>
    PatientProfiles::addDemographics(
      age = FALSE, priorObservation = FALSE, futureObservation = FALSE,
      ageGroup = aG, name = "cohort"
    ) |>
    omopgenerics::newCohortTable()

  result1 <- cdm1$cohort |>
    summariseCohortOverlap(strata = st)

  result2 <- cdm2$cohort |>
    summariseCohortOverlap(strata = st)

  expect_identical(result1, result2)

  CDMConnector::cdmDisconnect(cdm1)
  CDMConnector::cdmDisconnect(cdm2)
})
