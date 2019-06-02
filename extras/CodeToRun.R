library(CauseSpecificMortality)
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

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- Sys.getenv("cdmDatabaseSchema")
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <-  Sys.getenv("cohortDatabaseSchema")

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- Sys.getenv("cohortTable")
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

PatientLevelPrediction::viewMultiplePlp(outputFolder)

CauseClassification(outputFolder, TAR = 30, model = 1, nTree = 500, seedNum = 1234)
