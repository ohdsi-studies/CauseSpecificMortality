library(mortality)
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "c:/Users/User/Desktop/Progress/Mortality_Result"

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "c:/Users/User/Desktop/Progress/Mortality_temp")

# Details for connecting to the server:
dbms <- "sql server"
user <- 'ted9219'
pw <- 'rddo678$'
server <- '128.1.99.53'
port <- '1433'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'NHIS_NSC.dbo'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'NHIS_NSC_Result.dbo'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'CS_mortality'
#=======================

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = T,
        runAnalyses = F,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,
        minCellCount= 5)
