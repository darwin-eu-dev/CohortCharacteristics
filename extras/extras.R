library(CDMConnector)
library(CodelistGenerator)
library(PatientProfiles)
library(dplyr)
library(ggplot2)
library(DiagrammeR)
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

cdm[["meds"]] <- cdm[["meds"]] |>
  filter(year(cohort_start_date) <= 1949) |>
  recordCohortAttrition("t here would") |>
  filter(year(cohort_start_date) >= 1920) |>
  recordCohortAttrition("end if I try it here would should I do aaa I do not know e would should I do aaa I do not know") |>
  filter(year(cohort_start_date) >= 1930) |>
  recordCohortAttrition("nd ") |>
  compute(temporary = FALSE, name = "meds")

ca <- attrition(cdm[["meds"]]) |>
  filter(cohort_definition_id == 2) |>
  mutate(number_records = 161137831)

render_graph(plotCohortAttrition(ca))

