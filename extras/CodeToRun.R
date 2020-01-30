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
        causePrediction = T,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,
        minCellCount= 5)

# Cause of death prediction (If you want to change defalut parameter setting)

# TAR = 30, 60, 90, 180, 365 days
# algorithm = algorithm you want to use for stacking ensemble model
# available algorithms in caret packages (http://topepo.github.io/caret/available-models.html)
# seedNum = Seed number

causePrediction(outputFolder, TAR = 30, algorithm = "rf", seedNum = NULL)

# If you want to run the causePrediction function with multiple parameters, you can use lapply like this :
TAR <- c(30,60,90,180,365)
algorithm <- "rf"
lapply(TAR, function(x) causePrediction(outputFolder, TAR = x, algorithm))
