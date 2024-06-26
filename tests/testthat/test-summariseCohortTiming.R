test_that("summariseCohortTiming", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n = 20, min = 1950, max = 2000),
    month_of_birth = runif(n = 20, min = 1, max = 12),
    day_of_birth = runif(n = 20, min = 1, max = 28),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )


  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(
      sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5),
      sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5)
    ),
    cohort_start_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5)
    )),
    cohort_end_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5)
    ))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:20,
    person_id = 1:20,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date = as.Date("2025-01-01"),
    period_type_concept_id = NA
  )

  cdm <- mockCohortCharacteristics(
    con = connection(), writeSchema = writeSchema(), person = person,
    observation_period = obs, table = table
  )

  timing1 <- summariseCohortTiming(cdm$table,
    restrictToFirstEntry = TRUE
  )

  expect_equal(
    omopgenerics::resultColumns("summarised_result"),
    colnames(timing1)
  )

  expect_true(all(c("min", "q25", "median", "q75", "max", "count") %in%
    timing1$estimate_name |> unique()))
  expect_true(omopgenerics::settings(timing1)$restrict_to_first_entry)

  timing2 <- summariseCohortTiming(cdm$table,
    restrictToFirstEntry = FALSE,
    estimates = c("min", "max")
  )
  expect_equal(
    omopgenerics::resultColumns("summarised_result"),
    colnames(timing2)
  )
  expect_true(all(timing2$estimate_name |> unique() %in%
    c("min", "max", "count")))

  timing3 <- summariseCohortTiming(cdm$table,
    restrictToFirstEntry = FALSE,
    estimates = character(),
    density = TRUE
  )
  expect_true(all(c("density") %in%
    unique(timing3$variable_name)))
  expect_true(all(c("x", "y") %in%
    unique(timing3$estimate_name)))
  expect_true("overall" == unique(timing3$strata_level))
  expect_no_error(res1 <- tidyr::pivot_wider(timing3, names_from = "estimate_name", values_from = "estimate_value"))
  expect_true(all(c("x", "y") %in% colnames(res1)))
  expect_true(class(res1$x) == "character")

  ## Strata and cohortId----
  cdm$table <- cdm$table |>
    PatientProfiles::addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    PatientProfiles::addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing4 <- summariseCohortTiming(cdm$table,
    strata = list("age_group", c("age_group", "sex"))
  )
  expect_true(all(c("overall", "age_group", "age_group &&& sex") %in%
    unique(timing4$strata_name)))

  # add density tests
  timing5 <- summariseCohortTiming(cdm$table,
    strata = list("age_group", c("age_group", "sex")),
    estimates = character(),
    density = TRUE
  )
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "age_group &&& sex"]) %in% c("x", "y", "count")))
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "overall"]) %in% c("x", "y", "count")))
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "age_group"]) %in% c("x", "y", "count")))
  expect_no_error(res2 <- tidyr::pivot_wider(timing5, names_from = "estimate_name", values_from = "estimate_value"))
  expect_true(all(c("x", "y") %in% colnames(res2)))
  expect_true(class(res2$x) == "character")

  timing6 <- summariseCohortTiming(cdm$table, cohortId = 1)
  expect_true(nrow(timing6) == 0)

  expect_warning(timing7 <- summariseCohortTiming(cdm$table,
    cohortId = 5:7
  ))
  expect_true(nrow(timing7) == 0)

  timing8 <- summariseCohortTiming(cdm$table, cohortId = 1, density = TRUE)
  expect_true(nrow(timing8) == 0)

  mockDisconnect(cdm)
})
