# Run all scripts in right order

# Set working directory
setwd("~/GitHub/BachelorThesis/MODULAR")
# setwd("/Users/hassearrow/BachelorThesis/MODULAR")


# --- Data retrieval and cleaning ---
source("DataRetrieval.R")
source("DataExplorationAndTransformation.R")
source("DataImputation.R")

# --- Specific for non-network specific models ---
source("DataPrepTidyData.R")
source("LogisticRegression.R")

# --- Network specific models ---
# source("DataExploration_network.R")
source("DataPrepSNA.R")





# Files to be run manually:

"DependencyExploration_Reciprocality.R"

"FitModel_ERGM.R"
"FitModel_LSM.R"
"LSM_Evaluation.R"

