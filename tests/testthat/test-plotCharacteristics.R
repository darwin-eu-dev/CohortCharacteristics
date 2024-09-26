test_that("test plot", {
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1:3L,
    gender_concept_id = as.integer(c(8507, 8532, 8532)),
    year_of_birth = as.integer(c(1985, 2000, 1962)),
    month_of_birth = as.integer(c(10, 5, 9)),
    day_of_birth = as.integer(c(30, 10, 24)),
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = as.integer(c(1, 1, 1, 2)),
    subject_id = as.integer(c(1, 1, 2, 3)),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = as.integer(c(1, 2, 2, 1)),
    subject_id = as.integer(c(1, 1, 3, 3)),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = as.integer(c(1, 1, 2, 1)),
    subject_id = as.integer(c(1, 1, 2, 3)),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:3L,
    person_id = 1:3L,
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
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
      cohort_definition_id = c(1L, 2L), cohort_name = c("exposed", "unexposed")
    )
  )
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1L, 2L), cohort_name = c("covid", "headache")
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
  test_data <- summariseCharacteristics(
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

  # barplot
  plot <- plotCharacteristics(
    result = test_data |>
      dplyr::filter(
        variable_name == "Medications",
        estimate_type == "percentage"
      ),
    plotStyle = "barplot",
    facet = c("cohort_name"),
    colour = c("variable_name", "variable_level")
  )

  expect_true(ggplot2::is.ggplot(plot))

  # boxplot
  plot2 <- plotCharacteristics(
    result = test_data |>
      dplyr::filter(variable_name == "Age"),
    plotStyle = "boxplot",
    facet = "variable_name",
    colour = c("cohort_name")
  )

  expect_true(ggplot2::is.ggplot(plot2))

  expect_no_error(plotCharacteristics(
    result = test_data |>
      dplyr::filter(variable_name == "Age"),
    plotStyle = "boxplot"
  ))

  expect_no_error(plotCharacteristics(
    result = test_data |>
      dplyr::filter(variable_name == "Age"),
    plotStyle = "barplot"
  ))

  expect_no_error(plotCharacteristics(
    result = test_data |>
      dplyr::filter(variable_name == "Age"),
    plotStyle = "barplot"
  ))
})
