library(CDMConnector)
library(CodelistGenerator)
library(PatientProfiles)
library(dplyr)
library(ggplot2)

con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = CDMConnector::eunomia_dir())
cdm <- CDMConnector::cdm_from_con(con,
                                  cdm_schem = "main",
                                  write_schema = "main")

meds_cs <- getDrugIngredientCodes(cdm = cdm,
                                  name = c("acetaminophen",
                                           "morphine"))
cdm <- generateConceptCohortSet(
  cdm = cdm,
  name = "meds",
  conceptSet = meds_cs,
  end = "event_end_date",
  limit = "all",
  overwrite = TRUE
)

lsc <- cdm$meds %>%
  summariseLargeScaleCharacteristics(
    window = c(-Inf,0),
    eventInWindow ="condition_occurrence",
    minimumFrequency = 0.05
  )

lsc
