
Cause-specific mortality package
========================================================


Instructions To Build Package
===================

- Build the package by clicking the R studio 'Install and Restart' button in the built tab 



Instructions To Run Package
===================


```r
  library(CauseSpecificMortality)
  # USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "./CauseSpecificMortalityResults"

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "location with space to save big data")

# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'CauseSpecificMortalityCohort'


#=======================

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = T,
        runAnalyses = T,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,
        minCellCount= 5)
```
- To predict cause of death after PLP analysis (model 1 = Lasso logistic regression, 2 = Gradient boosting machine)
  
```r

CausePrediction(outputFolder, TAR = 30, nTree = 500, seedNum = NULL)

```

- You can then easily transport the trained models into a network validation study package by running :

```r
  
  execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = F,
        runAnalyses = F,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = T,
        minCellCount= 5)

```

- To create the shiny app and view run:

```r
  
populateShinyApp(resultDirectory = outputFolder,
                 minCellCount = 10, 
                 databaseName = 'friendly name'
                 ) 
        
viewShiny('CauseSpecificMortality')
  

```

# Development status
Under development. Do not use

