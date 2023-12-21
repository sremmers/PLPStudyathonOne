execute_plp=function(connectionDetails=connectionDetails,
                    cdmDatabaseSchema=cdmDatabaseSchema,
                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable =  cohortTable,
                    GenerateCohorts = TRUE,
                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                    outputFolder=outputFolder,
                    incrementalFolder = file.path(outputFolder, "incrementalFolder"),
                    databaseId =   databaseId,
                    packageWithCohortDefinitions = "PLPStudyathonOne",
                    cohortIds = NULL,
                    databaseName = databaseId,
                    databaseDescription = databaseDescription,
                    extraLog = NULL) 
 {


if(GenerateCohorts==TRUE)
    { 
        cohort_generation(connectionDetails= connectionDetails ,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                    outputFolder= outputFolder,
                    incrementalFolder = file.path(outputFolder, "incrementalFolder"),
                    databaseId = databaseId,
                    packageWithCohortDefinitions = "PLPStudyathonOne",
                    cohortIds = NULL,
                    minCellCount = 5,
                    databaseName = databaseName,
                    databaseDescription = databaseDescription,
                    extraLog = NULL) 

    }

# Create the PLP database details 
DatabaseDetails=PatientLevelPrediction::createDatabaseDetails (connectionDetails,
                            cdmDatabaseSchema=cdmDatabaseSchema, 
                            cdmDatabaseName=databaseName, 
                            cdmDatabaseId= databaseId,
                            tempEmulationSchema = cdmDatabaseSchema,
                            cohortDatabaseSchema = cohortDatabaseSchema, 
                            cohortTable = cohortTable,
                            outcomeDatabaseSchema = cohortDatabaseSchema, 
                            outcomeTable = cohortTable )

  # The first step is to select the variables that will be used in the model:
  # this is done by creating a covariate setting object using the createCovariateSettings
  # function from the FeatureExtraction package.

  #Covariate setting 1:
  covSet1=FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useConditionGroupEraLongTerm = TRUE,
    useMeasurementMediumTerm = TRUE,
    useCharlsonIndex = TRUE)

  #Covariate setting 2:
  covSet2=FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useMeasurementMediumTerm = TRUE,
    useMeasurementLongTerm = TRUE,
    useMeasurementValueLongTerm = TRUE,
    useMeasurementValueMediumTerm = TRUE,
    useConditionGroupEraLongTerm = TRUE,
    useConditionGroupEraMediumTerm = TRUE,
    useConditionOccurrenceLongTerm = TRUE,
    useConditionOccurrenceMediumTerm = TRUE,
    useDrugEraLongTerm = TRUE,
    useDrugEraMediumTerm = TRUE,
    useDrugGroupEraLongTerm = TRUE,
    useDrugGroupEraMediumTerm = TRUE,
    useDrugGroupEraShortTerm = TRUE,
    useProcedureOccurrenceLongTerm = TRUE,
    useProcedureOccurrenceMediumTerm = TRUE,
    useCharlsonIndex = TRUE)


  #population settings:
  populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
    washoutPeriod = 0,
    firstExposureOnly = FALSE,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 1,
    riskWindowStart = -1,
    riskWindowEnd = 365,
    startAnchor =  'cohort start',
    endAnchor =  'cohort start',
    minTimeAtRisk = 30,
    requireTimeAtRisk = FALSE,
    includeAllOutcomes = TRUE
  )

  #split settings
  splitSettings <-  PatientLevelPrediction::createDefaultSplitSetting(
    trainFraction = 0.75,
    testFraction = 0.25,
    type = 'stratified',
    nfold = 3,
    splitSeed = 1234
  )

  # preprocess settings
  preprocessSettings <- PatientLevelPrediction::createPreprocessSettings(
    minFraction = 0.001,
    normalize = T,
    removeRedundancy = T
  )

  # first model design #using Lasso

  outcomeIds <- c(196, 197, 195, 192)

  # cov setting 1
  for (i in 1:length(outcomeIds)) {
    assign(paste0("model", i), PatientLevelPrediction::createModelDesign(
      targetId  =199,
      outcomeId = outcomeIds[i],
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      populationSettings = populationSettings,
      covariateSettings =  covSet1,
      featureEngineeringSettings = NULL,
      sampleSettings = NULL,
      preprocessSettings = preprocessSettings,
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
      splitSettings = splitSettings) )
  }

  # cov setting 2
  for (i in 1:length(outcomeIds)) {
    assign(paste0("model", length(outcomeIds)+i), PatientLevelPrediction::createModelDesign(
      targetId  = 199,
      outcomeId = outcomeIds[i],
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      populationSettings = populationSettings,
      covariateSettings =  covSet2,
      featureEngineeringSettings = NULL,
      sampleSettings = NULL,
      preprocessSettings = preprocessSettings,
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
      splitSettings = splitSettings ) )
  }


  # custom covariates
  for (i in 1:length(outcomeIds)) {
    assign(paste0("model", 2*length(outcomeIds) + i), PatientLevelPrediction::createModelDesign(
      targetId = 199,
      outcomeId = outcomeIds[i],
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
      populationSettings = createStudyPopulationSettings(
        washoutPeriod = 0,
        firstExposureOnly = FALSE,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 1,
        riskWindowStart = -1,
        riskWindowEnd = 365,
        startAnchor =  'cohort start',
        endAnchor =  'cohort start',
        minTimeAtRisk = 30,
        requireTimeAtRisk = FALSE,
        includeAllOutcomes = TRUE
      ),
      covariateSettings = list ( PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "grade 1",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 67,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "grade 2",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 68,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "grade 3",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 69,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "grade 4",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 70,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "grade 5",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 71,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "psa_10",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 1023,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "psa_10_20",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 60,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "psa_20",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 1023,
        startDay = -30,
        endDay = 1,
        analysisId = 300+1
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "age55",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 111,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "age55_80",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 110,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "age_80",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 109,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "charlson 0",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 120,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "charlson 1",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 121,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "charlson 2 and higher",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 1025,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "T1",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 63,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "T2",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 64,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "T3_4",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 65,
        startDay = -1,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "Family history of PCa",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 72,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "Stroke",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 153,
        startDay = -30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "Type 2 diabetes",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 154,
        startDay = 30,
        endDay = 1,
        analysisId = 300+i
      ),
      PatientLevelPrediction::createCohortCovariateSettings(
        cohortName = "M1 disease",
        settingId = 1, cohortTable = NULL,
        cohortDatabaseSchema = NULL,
        cohortId = 55,
        startDay = -30,
        endDay = 10,
        analysisId = 300+i
      ) )) )
  }

  #run the models
  PatientLevelPrediction::runMultiplePlp (databaseDetails =DatabaseDetails  ,
                                          modelDesignList = list( mget(paste0("model", 1:12))),
                                          saveDirectory =  outputFolder,
                                          sqliteLocation = file.path(outputFolder, "sqlite"))
}
