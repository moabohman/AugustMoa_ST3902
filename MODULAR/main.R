# Run all scripts in right order

# Set working directory
setwd("~/GitHub/BachelorThesis/MODULAR")



# --- Data retrieval and cleaning ---
source("DataRetrieval.R")
source("DataExplorationAndTransformation.R")
source("DataImputation.R")

# --- Specific for non-network specific models ---
source("DataPrepLR.R")
source("LogisticRegression.R")
# source("DataPrepMN.R")
# source("MN_Test.R")

# --- Network specific models ---
source("DataExploration_AdjMat.R")
source("DataPrepSNA.R")
source("ERGM.R")
source("LSM.R")


source(".R")
source(".R")
source(".R")
source(".R")
source(".R")
source(".R")
source(".R")
source(".R")
source(".R")

