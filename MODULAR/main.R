# Run all scripts in right order

# Set working directory
setwd("C:/Users/moaka/OneDrive - Stockholm University/Documents/Studier_Arkiv/UppsatsenHT25/GitHub/BachelorThesis/MODULAR")
# setwd("/Users/hassearrow/BachelorThesis/MODULAR")

# Samling av (förhoppningsvis) alla nödvändiga paket ---
source("LibraryCollection.R")

# =========================================================

# --- Data retrieval and cleaning ---
source("DataRetrieval.R")
source("DataExplorationAndTransformation.R")
source("DataImputation.R")

# --- Specific for non-network specific models ---
source("DataPrepTidyData.R")
source("LogisticRegression.R")

# --- Network specific models ---
# source("DataExploration_network.R")     -- Run manually!
source("DataPrepSNA.R")





# Files to be run manually:

"DependencyExploration_Reciprocality.R"

"FitModel_LSM.R"
"LSM_Evaluation.R"

# Ofärdiga script: (laboration pågår)
"Gof_enhanced.R"
"Diagnostics.R"   # (ModelExploration)
"3D_plots.R"