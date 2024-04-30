test_that("test summariseCharacteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockCohortCharacteristics(
    connectionDetails,
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period, cohort1 = emptyCohort,
    cohort2 = emptyCohort
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    ))
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
    ))
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersectFlag = list(
      "Medications" = list(
        targetCohortTable = "medication", window = c(-365, 0)
      ),
      "Comorbidities" = list(
        targetCohortTable = "comorbidities", window = c(-Inf, 0)
      )
    )
  ) |>
    suppress(minCellCount = 1))
  expect_true(inherits(result, "summarised_result"))
  expect_identical(
    result |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::filter(variable_level == "Covid") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    2
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::filter(variable_level == "Headache") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    1
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::filter(variable_level == "Acetaminophen") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    2
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::filter(variable_level == "Ibuprophen") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::filter(variable_level == "Naloxone") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "unexposed") |>
      dplyr::filter(variable_level == "Covid") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "unexposed") |>
      dplyr::filter(variable_level == "Headache") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    1
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "unexposed") |>
      dplyr::filter(variable_level == "Acetaminophen") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "unexposed") |>
      dplyr::filter(variable_level == "Ibuprophen") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )
  expect_identical(
    result |>
      dplyr::filter(group_level == "unexposed") |>
      dplyr::filter(variable_level == "Naloxone") |>
      dplyr::filter(estimate_name == "count") |>
      dplyr::pull("estimate_value") |>
      as.numeric(),
    0
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersectFlag = list(
      "Medications short" = list(
        targetCohortTable = "medication", window = list("short" = c(-30, 0))
      ),
      "Medications long" = list(
        targetCohortTable = "medication", window = list("long" = c(-365, 0))
      ),
      "Comorbidities" = list(
        targetCohortTable = "comorbidities", window = c(-Inf, 0)
      )
    )
  )|>
    suppress(minCellCount = 1))
  expect_true(inherits(result, "summarised_result"))
  expect_true(
    result |>
      visOmopResults::splitAdditional() |>
      dplyr::filter(window == "short") |>
      dplyr::tally() |>
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 4 # 2 group_level 4 estimate type
  )
  expect_true(
    result |>
      visOmopResults::splitAdditional() |>
      dplyr::filter(window == "long") |>
      dplyr::tally() |>
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 4 # 2 group_level 4 estimate type
  )
  expect_true(
    result |>
      visOmopResults::splitAdditional() |>
      dplyr::filter(table == "medication") |>
      dplyr::tally() |>
      dplyr::pull() ==
      omopgenerics::settings(cdm$medication) |> nrow() * 8 # 2 group_level 4 estimate type 2 window
  )
  expect_true(
    result |>
      visOmopResults::splitAdditional() |>
      dplyr::filter(table == "comorbidities") |>
      dplyr::tally() |>
      dplyr::pull() ==
      omopgenerics::settings(cdm$comorbidities) |> nrow() * 4 # 2 group_level 4 estimate type
  )

  result_notables <- summariseCharacteristics(cdm$dus_cohort)|>
    suppress(minCellCount = 1)
  expect_true(inherits(result, "summarised_result"))

  # counts - both records and persons
  sc_person_record <- summariseCharacteristics(
    cdm$dus_cohort, counts = TRUE, demographics = FALSE
  )
  expect_true(nrow(sc_person_record |>
                    dplyr::filter(variable_name == "Number records")) > 0)
  expect_true(nrow(sc_person_record |>
                     dplyr::filter(variable_name == "Number subjects")) > 0)
  # counts - none
  sc_no_counts <- summariseCharacteristics(
    cdm$dus_cohort, counts = FALSE, demographics = TRUE
  )
  expect_true(nrow(sc_no_counts |>
                     dplyr::filter(variable_name == "Number records")) == 0)
  expect_true(nrow(sc_no_counts |>
                     dplyr::filter(variable_name == "Number subjects")) == 0)
  expect_error(summariseCharacteristics(
    cdm$dus_cohort, counts = "not an option", demographics = FALSE
  ))

  # no options chosen
  expect_no_error(empty <- summariseCharacteristics(
    cdm$dus_cohort, counts = FALSE, demographics = FALSE
  ))
  expect_equal(empty, omopgenerics::emptySummarisedResult())

  # demographics
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = TRUE,
    cohortIntersectFlag = list(
      "Medications" = list(
        targetCohortTable = "medication", window = c(-365, 0)
      )
    )
  ))
  expect_true(all(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = TRUE
  ))
  expect_true(all(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = FALSE,
    cohortIntersectFlag = list(
      "Medications" = list(
        targetCohortTable = "medication", window = c(-365, 0)
      )
    )
  ))
  expect_false(any(
    c("Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
      "Future observation") %in% result$variable_name
  ))
  expect_identical(
    summariseCharacteristics(cdm$dus_cohort, demographics = FALSE) |>
      dplyr::arrange(.data$group_level, .data$variable_name),
    summariseCohortCounts(cdm$dus_cohort) |>
      dplyr::arrange(.data$group_level, .data$variable_name)
  )

})

test_that("test empty cohort", {
  cdm <- mockCohortCharacteristics(connectionDetails = connectionDetails)

  expect_no_error(
    cdm$cohort1 |> dplyr::filter(cohort_definition_id == 0) |>
      summariseCharacteristics(cohortIntersectFlag = list(
        "Medications" = list(
          targetCohortTable = "cohort2", window = c(-365, 0)
        ),
        "Comorbidities" = list(
          targetCohortTable = "cohort2", window = c(-Inf, 0)
        )
      ))
  )

  expect_no_error(
    res <- cdm$cohort1 |>
      summariseCharacteristics(cohortIntersectFlag = list(
        "Medications" = list(
          targetCohortTable = "cohort2", window = c(-365, 0), targetCohortId = 1
        ),
        "Comorbidities" = list(
          targetCohortTable = "cohort2", window = c(-Inf, 0)
        )
      ))
  )

  expect_true(
    res |>
      dplyr::filter(variable_name == "Medications") |>
      dplyr::pull("variable_level") |>
      unique() == "Cohort 1"
  )
  expect_true(all(
    res |>
      dplyr::filter(variable_name == "Comorbidities") |>
      dplyr::pull("variable_level") |>
      unique() |>
      sort() == c("Cohort 1", "Cohort 2")
  ))

  expect_no_error(
    x1 <- cdm$cohort1 |>
      summariseCharacteristics(tableIntersectFlag = list("Visits" = list(
        tableName = "visit_occurrence", window = c(-365, 0)
      )))
  )

  # expect_no_error(
  #   x3 <- cdm$cohort1 |>
  #     summariseCharacteristics(tableIntersect = list("Visits" = list(
  #       tableName = "visit_occurrence", value = "visit_concept_id",
  #       window = c(-Inf, Inf)
  #     )))
  # )
})

test_that("test cohort id", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockCohortCharacteristics(
    connectionDetails,
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period, cohort1 = emptyCohort,
    cohort2 = emptyCohort
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    ))
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
    ))
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )

  result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortId = 1,
    cohortIntersectFlag = list(
      "Medications" = list(
        targetCohortTable = "medication", window = c(-365, 0)
      ),
      "Comorbidities" = list(
        targetCohortTable = "comorbidities", window = c(-Inf, 0)
      )
    )
  )
  resultAll <- summariseCharacteristics(
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
  expect_true(inherits(result, "summarised_result"))
  expect_true(unique(result$group_level) == "exposed")
  expect_identical(
    resultAll |>
      dplyr::filter(group_level == "exposed") |>
      dplyr::arrange(.data$variable_name, .data$estimate_name, .data$estimate_value),
    result |>
      dplyr::arrange(.data$variable_name, .data$estimate_name, .data$estimate_value)
  )

  expect_warning(
    summariseCharacteristics(
      cdm$dus_cohort,
      cohortId = c(1,5),
      cohortIntersectFlag = list(
        "Medications" = list(
          targetCohortTable = "medication", window = c(-365, 0)
        ),
        "Comorbidities" = list(
          targetCohortTable = "comorbidities", window = c(-Inf, 0)
        )
      )
    )
  )

  expect_error(
    summariseCharacteristics(
      cdm$dus_cohort,
      cohortId = 5,
      cohortIntersectFlag = list(
        "Medications" = list(
          targetCohortTable = "medication", window = c(-365, 0)
        ),
        "Comorbidities" = list(
          targetCohortTable = "comorbidities", window = c(-Inf, 0)
        )
      )
    )
  )

})

test_that("arguments tableIntersect",{
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5), gender_concept_id = c(8507, 8532, 8532, 8507, 8507),
    year_of_birth = c(1985, 2000, 1962, 1999, 1979), month_of_birth = c(10, 5, 9, 2, 4),
    day_of_birth = c(30, 10, 24, 26, 25),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2, 2, 2),
    subject_id = c(1, 1, 2, 3, 4, 5),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25", "2010-01-01", "2009-09-09"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25", "2010-01-01", "2009-09-09"
    ))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5),
    person_id = c(1, 2, 3, 4, 5),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03", "2003-01-01", "2005-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10", "2024-12-31", "2024-12-31"
    )),
    period_type_concept_id = 0
  )

  visit_occurrence <- dplyr::tibble(
    visit_occurrence_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    person_id = c(1, 1, 1, 1, 5, 2, 2, 4, 5, 4),
    visit_start_date = as.Date(c(
      "2009-01-01", "2011-01-02", "1994-12-03", "2013-01-01", "2005-01-01", "2008-08-08", "2009-09-09", "2010-10-10", "2011-11-11", "2008-09-01"
    )),
    visit_concept_id = 0,
    visit_end_date = visit_start_date,
    visit_type_concept_id = 0
  )

  cdm <- mockCohortCharacteristics(
    dus_cohort = dus_cohort, person = person,
    observation_period = observation_period,
    visit_occurrence = visit_occurrence,
    cohort1 = emptyCohort,
    cohort2 = emptyCohort
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    ))

  expect_no_error(
    results <- summariseCharacteristics(
      cohort = cdm$dus_cohort,
      ageGroup = list(c(0, 50), c(51, 150)),
      tableIntersectCount = list(
        "Number visits anytime before" = list(
          tableName = "visit_occurrence", window = c(-Inf, -1)
        )
      )
    )
  )

  expect_true(
    "Number visits anytime before" %in%
      (results %>% dplyr::pull("variable_name"))
  )

  expect_true(
    "0 to 50" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_false(
    "51 to 150" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    2
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "median") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "median") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )

  expect_no_error(
    results <- summariseCharacteristics(
      cohort = cdm$dus_cohort,
      ageGroup = list(c(0, 20), c(21, 150)),
      tableIntersectFlag = list(
        "Flag visits anytime before" = list(
          tableName = "visit_occurrence", window = c(-Inf, -1)
        )
      )
    )
  )

  expect_true(
    "Flag visits anytime before" %in%
      (results %>% dplyr::pull("variable_name"))
  )

  expect_true(
    "0 to 20" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_true(
    "21 to 150" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Flag visits anytime before",
                    estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Flag visits anytime before",
                    estimate_name == "count") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    2
  )

  expect_no_error(
    results <- summariseCharacteristics(
      cohort = cdm$dus_cohort,
      ageGroup = list(c(0, 50), c(51, 150)),
      tableIntersectCount = list(
        "Number visits anytime before" = list(
          tableName = "visit_occurrence", window = c(-Inf, -1)
        )
      )
    )
  )

  expect_true(
    "Number visits anytime before" %in%
      (results %>% dplyr::pull("variable_name"))
  )

  expect_true(
    "0 to 50" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_false(
    "51 to 150" %in%
      (results %>% dplyr::pull("variable_level"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    2
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "median") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    0
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Number visits anytime before",
                    estimate_name == "median") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    1
  )

  expect_no_error(
    results <- summariseCharacteristics(
      cohort = cdm$dus_cohort,
      tableIntersectDate = list(
        "Date visits anytime before" = list(
          tableName = "visit_occurrence",
          order = "last",
          window = c(-Inf, -1)
        )
      )
    )
  )

  expect_true(
    "Date visits anytime before" %in%
      (results %>% dplyr::pull("variable_name"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Date visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.Date(),
    as.Date("2009-09-09")
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Date visits anytime before",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.Date(),
    as.Date("2005-01-01")
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Date visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.Date(),
    as.Date("2009-09-09")
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Date visits anytime before",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.Date(),
    as.Date("2008-09-01")
  )

  expect_no_error(
    results <- summariseCharacteristics(
      cohort = cdm$dus_cohort,
      tableIntersectDays = list(
        "Days visits anytime after" = list(
          tableName = "visit_occurrence",
          order = "first",
          window = c(1, Inf)
        )
      )
    )
  )

  expect_true(
    "Days visits anytime after" %in%
      (results %>% dplyr::pull("variable_name"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Days visits anytime after",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    as.numeric(as.Date("1994-12-03") - as.Date("1991-04-19"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Days visits anytime after",
                    estimate_name == "min") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    as.numeric(as.Date("2010-10-10") - as.Date("2010-01-01"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "exposed",
                    variable_name == "Days visits anytime after",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    as.numeric(as.Date("1994-12-03") - as.Date("1990-04-19"))
  )

  expect_identical(
    results %>%
      dplyr::filter(group_level == "unexposed",
                    variable_name == "Days visits anytime after",
                    estimate_name == "max") %>%
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    as.numeric(as.Date("2011-11-11") - as.Date("2009-09-09"))
  )

  CDMConnector::cdm_disconnect(cdm = cdm)
})
