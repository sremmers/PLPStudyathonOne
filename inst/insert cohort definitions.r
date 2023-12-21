library(ROhdsiWebApi)

baseUrl <-"https://pioneer.hzdr.de/WebAPI"

token <- 'Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJzLnJlbW1lcnNAZXJhc211c21jLm5sIiwiU2Vzc2lvbi1JRCI6bnVsbCwiZXhwIjoxNzAzMTIwNzgxfQ.eyHR8UZqunn9AzuuLnEIChtbnHmKXg-macBDMuLaY58x0Qcbil-TfvkTSsfGYg6A9KYhI8_4sTZ_1ERXipW10w'
setAuthHeader(baseUrl = baseUrl, token)

# after inserting the cohorts

# Insert cohort definitions from ATLAS into package -----------------------
ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "inst/settings/CohortsToCreate.csv",
baseUrl = baseUrl ,
insertTableSql = TRUE,
insertCohortCreationR = TRUE,
generateStats = FALSE,
packageName = "PLPstudy")
