##################################################
#                                                #
#                 LSM_Evaluation                 #
#                                                #
##################################################


# ======= - Conversion function: Matrix to Tidydata - =======

AdjMat_To_TidyDataDf <- function(InputMatrix) {
  
  # Turn matrix into dataframe
  TidyData_df <- data.frame(as.table(InputMatrix))
  colnames(TidyData_df) <- c("Origin", "Destination", "Value")
  
  # Clean Origin == Destination
  TidyData_df <- TidyData_df[!(TidyData_df$Origin==TidyData_df$Destination),]
  
  return(TidyData_df)
}

# =========================================================


# --- Retrieve predicted values ---
LSM26_pred <- predict(LSM26)

# --- Classify observations ---
# Choose cut-off level
c_LSM <- 0.5
LSM26_classification <- ifelse(LSM26_pred > c_LSM, 1, 0)


# --- Data transformation ---
colnames(LSM26_pred) <- VertexName
rownames(LSM26_pred) <- VertexName

colnames(LSM26_classification) <- VertexName
rownames(LSM26_classification) <- VertexName


LSM26_pred_Tidy <- AdjMat_To_TidyDataDf(LSM26_pred)
LSM26_Classi_Tidy <- AdjMat_To_TidyDataDf(LSM26_classification)
AdjMat_Tidy <- AdjMat_To_TidyDataDf(AdjMat)


# --- Make a confusion matrix ---
confusionMatrix(as.factor(LSM26_Classi_Tidy$Value), 
                as.factor(AdjMat_Tidy$Value))

# --- Analyze the ROC curve ---
roc(AdjMat_Tidy$Value ~ LSM26_pred_Tidy$Value, 
    plot = TRUE, print.auc = TRUE)


par(par_temp)
