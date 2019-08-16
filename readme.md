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
# TAR = (30,90,180,365)
# nTree = tree numbers of random forest algorithm, seedNum = Seed number

TAR <- 30
nTree <- 200 
seedNum <- NULL

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

```r

TAR <- c(30,60,90,180,365)
nTree <- 200 
lapply(TAR, function(x) causePrediction(outputFolder, TAR = x))

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
