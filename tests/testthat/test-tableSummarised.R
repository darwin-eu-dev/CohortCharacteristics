test_that("tableCharacteristics", {
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = c(1, 2, 3) |> as.integer(),
    gender_concept_id = c(8507, 8532, 8532) |> as.integer(),
    year_of_birth = c(1985, 2000, 1962) |> as.integer(),
    month_of_birth = c(10, 5, 9) |> as.integer(),
    day_of_birth = c(30, 10, 24) |> as.integer(),
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2) |> as.integer(),
    subject_id = c(1, 1, 2, 3) |> as.integer(),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1) |> as.integer(),
    subject_id = c(1, 1, 3, 3) |> as.integer(),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1) |> as.integer(),
    subject_id = c(1, 1, 2, 3) |> as.integer(),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3) |> as.integer(),
    person_id = c(1, 2, 3) |> as.integer(),
    observation_period_start_date = as.Date(c(
      "1975-01-01", "1959-04-29", "1944-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2021-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0L
  )

  cdm <- mockCohortCharacteristics(
    con = connection(), writeSchema = writeSchema(),
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1L, 2L),
      cohort_name = c("exposed", "unexposed")
    )
  )
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1L, 2L),
      cohort_name = c("covid", "headache")
    )
  )
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1L, 2L, 3L),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )

  result1 <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersectFlag = list(
      "Medications" = list(
        targetCohortTable = "medication", window = c(-365, 0)
      ),
      "Comorbidities" = list(
        targetCohortTable = "comorbidities", window = c(-Inf, 0)
      )
    )
  )


  expect_no_error(gt1 <- tableCharacteristics(result1, excludeColumns = c(
    "result_id",
    "estimate_type", "additional_name",
    "additional_level", "cdm_name"
  )))
  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(c("Variable name", "Variable level", "Estimate name") %in%
    colnames(gt1$`_data`)))

  fx1 <- tableCharacteristics(result1, header = c("cdm_name", "group", "strata"), type = "flextable")
  expect_true(class(fx1) == "flextable")
  expect_true(all(c(
    "Variable name", "Variable level", "Estimate name",
    "CDM name\nPP_MOCK\nCohort name\nexposed", "CDM name\nPP_MOCK\nCohort name\nunexposed"
  ) %in%
    colnames(fx1$body$dataset)))
  expect_true(all(fx1$body$dataset$`Variable name` |> unique() %in%
    c(
      "Number records", "Number subjects", "Cohort start date",
      "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation", "Days in cohort", "Medications", "Comorbidities"
    )))

  tibble1 <- tableCharacteristics(result1, type = "tibble", split = "strata", header = character())
  expect_true(all(class(tibble1) %in% c("tbl_df", "tbl", "data.frame")))
  expect_true(all(c(
    "Variable name", "Variable level", "Estimate name",
    "CDM name", "Estimate value"
  ) %in%
    colnames(tibble1)))
})

test_that("tableCharacteristics, empty output warning message", {
  skip_on_cran()

  cdm <- CodelistGenerator::mockVocabRef()
  ac_result <- CodelistGenerator::summariseAchillesCodeUse(list("oa" = c(3, 4, 5)), cdm)
  expect_warning(tableCharacteristics(result = ac_result, type = "gt")    )
  PatientProfiles::mockDisconnect(cdm)
})

test_that("tableCohortOverlap", {
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1:20L,
    gender_concept_id = sample(c(8532L, 8507L), size = 20, replace = T),
    year_of_birth = sample(1950:2000L, size = 20, replace = TRUE),
    month_of_birth = sample(1:12L, size = 20, replace = TRUE),
    day_of_birth = sample(1:28L, size = 20, replace = TRUE),
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 15), rep(2L, 10), rep(3L, 15), rep(4L, 5)),
    subject_id = c(
      20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10,
      6, 18, 5, 1, 20, 14, 13, 8, 17, 3,
      16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18,
      5, 12, 3, 14, 13
    ) |>
      as.integer(),
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
    observation_period_id = 1:20L,
    person_id = 1:20L,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date = as.Date("2025-01-01"),
    period_type_concept_id = 0L
  )

  cdm <- mockCohortCharacteristics(
    con = connection(), writeSchema = writeSchema(), person = person,
    observation_period = obs, table = table
  )

  overlap <- summariseCohortOverlap(cdm$table)

  gtResult1 <- tableCohortOverlap(overlap)
  expect_true("gt_tbl" %in% class(gtResult1))
  expect_equal(
    gtResult1$`_data`$`CDM name`,
    c("PP_MOCK", rep("", nrow(gtResult1$`_data`) - 1))
  )

  fxResult1 <- tableCohortOverlap(overlap,
    type = "flextable",
    split = character(),
    header = "group",
    excludeColumns = c(
      "result_id", "estimate_type", "strata_name",
      "strata_level", "additional_name",
      "additional_level"
    )
  )
  expect_true("flextable" %in% class(fxResult1))

  cdm$table <- cdm$table |>
    PatientProfiles::addAge(ageGroup = list(c(0, 40), c(41, 100))) |>
    PatientProfiles::addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  overlap3 <- summariseCohortOverlap(
    cdm$table, strata = list("age_group", c("age_group", "sex"))
  )
  tibbleResult1 <- tableCohortOverlap(overlap3, type = "tibble")
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(tibbleResult1)))

  gtResult2 <- tableCohortOverlap(overlap3, type = "gt")
  expect_true("gt_tbl" %in% class(gtResult2))

  mockDisconnect(cdm)
})

test_that("tableCohortTiming", {
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1:20L,
    gender_concept_id = 8532L,
    year_of_birth = sample(1950:1970L, size = 20, replace = TRUE),
    month_of_birth = sample(1:12L, size = 20, replace = TRUE),
    day_of_birth = sample(1:28L, size = 20, replace = TRUE),
    race_concept_id= 0L,
    ethnicity_concept_id = 0L
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1L, 15), rep(2L, 10), rep(3L, 15), rep(4L, 5)),
    subject_id = c(
      20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10, 6, 18, 5, 1, 20, 14,
      13, 8, 17, 3, 16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18, 5, 12,
      3, 14, 13
    ) |>
      as.integer(),
    cohort_start_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5))),
    cohort_end_date = as.Date(c(
      rep("2000-01-01", 5), rep("2010-09-05", 5), rep("2006-05-01", 5),
      rep("2003-03-31", 5), rep("2008-07-02", 5), rep("2000-01-01", 5),
      rep("2012-09-05", 5), rep("1996-05-01", 5), rep("1989-03-31", 5)))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:20L,
    person_id = 1:20L,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date =  as.Date("2025-01-01"),
    period_type_concept_id = 0L
  )

  cdm <- mockCohortCharacteristics(
    person = person, observation_period = obs, table = table)

  timing1 <- summariseCohortTiming(cdm$table,
                                   restrictToFirstEntry = TRUE)
  tibble1 <- tableCohortTiming(timing1, type = "tibble", header = c("strata"), split = c("group", "additional"))
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator",
                    "Variable name", "Estimate name", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(nrow(tibble1 |> dplyr::distinct(`Cohort name reference`, `Cohort name comparator`)) == 6)
  expect_true(all(unique(tibble1$`Estimate name`) %in% c("N", "Median [Q25 - Q75]", "Range")))

  tibble2 <- tableCohortTiming(timing1, type = "tibble", header = "cohort_name")

  tibble3 <- tableCohortTiming(timing1, type = "tibble", .options = list(uniqueCombinations = FALSE))
  expect_true(all(unique(tibble3$`Cohort name comparator`) %in%
                    unique(tibble3$`Cohort name reference`)))

  tibble4 <- tableCohortTiming(timing1, type = "tibble", header = "cohort_name", split = character())
  gt1 <- tableCohortTiming(timing1, type = "gt")
  expect_true("gt_tbl" %in% class(gt1))

  fx1 <- tableCohortTiming(timing1, type = "flextable")
  expect_true("flextable" %in% class(fx1))

  # strata ----
  cdm$table <- cdm$table |>
    PatientProfiles::addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    PatientProfiles::addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing2 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")))

  fx2 <- tableCohortTiming(timing2,
                           type = "flextable")
  expect_true(class(fx2) == "flextable")

  gt2 <- tableCohortTiming(timing2)
  expect_true("gt_tbl" %in% class(gt2))

  gt3 <- tableCohortTiming(
    timing2 |>
      dplyr::filter(grepl("cohort_1", group_level)) |>
      dplyr::filter(grepl("2|3", group_level)),
    header = c("cdm_name", "cohort_name", "age_group", "sex"))
  expect_true("gt_tbl" %in% class(gt3))
  mockDisconnect(cdm)
})
