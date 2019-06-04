library(CauseSpecificMortality)
<<<<<<< HEAD

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
=======
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- Sys.getenv("outputFolder")

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = Sys.getenv("temp"))

# Details for connecting to the server:
dbms <- Sys.getenv("dbms")
user <- Sys.getenv("userID")
pw <- Sys.getenv("userPW")
server <- Sys.getenv("server16")
port <- Sys.getenv("port")
>>>>>>> master

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
<<<<<<< HEAD
cdmDatabaseSchema <- 'cdm database schema'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'
=======
cdmDatabaseSchema <- Sys.getenv("cdmDatabaseSchema")
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <-  Sys.getenv("cohortDatabaseSchema")
>>>>>>> master

oracleTempSchema <- NULL

# table name where the cohorts will be generated
<<<<<<< HEAD
cohortTable <- 'CauseSpecificMortalityCohort'

# your Database end date
DB_END_DATE <- 'your DB end date'
=======
cohortTable <- Sys.getenv("cohortTable")
>>>>>>> master
#=======================

DB_END_DATE <- 'your DB end date'

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        DB_END_DATE = DB_END_DATE,
        createProtocol = F,
        createCohorts = T,
        runAnalyses = T,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,
        minCellCount= 5)

<<<<<<< HEAD
# TAR = (30,90,180,365), model = (1,2), nTree = tree number of random forest, seedNum
CausePrediction(outputFolder, TAR = 30, model = 1, nTree = 500, seedNum = NULL)


=======
PatientLevelPrediction::viewMultiplePlp(outputFolder)

CauseClassification(outputFolder, TAR = 30, model = 1, nTree = 500, seedNum = 1234)
>>>>>>> master
