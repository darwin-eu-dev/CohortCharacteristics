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
  addAge(ageGroup = list("<40" = c(0,39), ">=40" = c(40,Inf))) |>
  addSex() |>
  summariseLargeScaleCharacteristics(
    window = list("-Inf to 0" = c(-Inf,0),  "0 to Inf" = c(0,Inf)),
    strata = list("age_group","sex"),
    eventInWindow ="condition_occurrence",
    episodeInWindow = "drug_exposure",
    minimumFrequency = 0.05
  )

lsc1 <- lsc |>
  filter((group_level == "morphine") | variable_name == "settings")

lsc1 <- lsc |> filter((group_level == "morphine") |
                        variable_name == "settings")

data = lsc1
referenceGroupLevel  = NULL
referenceStrataLevel = "Male"
referenceVariableLevel = NULL
referenceCdmName       = NULL
facet       = strata ~ table_name
splitStrata = FALSE
colorVars   = NULL
missings    = 0
position    = "horizontal"


plotComparedLargeScaleCharacteristics(data = data,
                                      referenceGroupLevel    = referenceGroupLevel,
                                      referenceStrataLevel   = referenceStrataLevel ,
                                      referenceVariableLevel = referenceVariableLevel,
                                      referenceCdmName       = referenceCdmName,
                                      facet       = facet,
                                      splitStrata =  splitStrata ,
                                      colorVars   = colorVars ,
                                      missings    = missings )

plotLargeScaleCharacteristics(data = lsc,
                              position = "horizontal",
                              facet     = . ~ strata_level,
                              splitStrata = FALSE,
                              colorVars   = NULL)




