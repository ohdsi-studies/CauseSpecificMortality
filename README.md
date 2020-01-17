Cause-specific mortality prediction
=============

<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **Patient-Level Prediction**
- Study type: **Clinical Application**
- Tags: **-**
- Study lead: **Chungsoo Kim**
- Study lead forums tag: **[[CSKim]](https://forums.ohdsi.org/u/[Chungsoo_Kim])**
- Study start date: **June, 2019**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**


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

# parameter settings for causePrediction 

#=======================

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = T,
        runAnalyses = T,
        causePrediction = T,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,
        minCellCount= 5)
```

- If you want to run the causePrediction function with multiple parameter, you can use this
- TAR: 30, 60, 90, 180, 365 days
- Algorithm: [values](http://topepo.github.io/caret/available-models.html) in caret package


```r
TAR <- c(30,60,90,180,365)
algorithm <- "rf"
lapply(TAR, function(x) causePrediction(outputFolder, TAR = x, algorithm))
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

