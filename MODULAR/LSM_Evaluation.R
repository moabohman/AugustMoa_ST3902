
AdjMat_To_TidyDataDf <- function(InputMatrix) {
  
  # Turn matrix into dataframe
  TidyData_df <- data.frame(as.table(InputMatrix))
  colnames(TidyData_df) <- c("Origin", "Destination", "Visafree")
  
  # Clean Origin == Destination
  TidyData_df <- TidyData_df[!(TidyData_df$Origin==TidyData_df$Destination),]
  
  return(TidyData_df)
}

# =========================================================


# --- Retrieve predicted values ---
LSM20_pred <- predict(LSM20)

# --- Classify observations ---
# Choose cut-off level
c_LSM <- 0.5
LSM20_classification <- ifelse(LSM20_pred > c_LSM, 1, 0)


# --- Data transformation ---
colnames(LSM20_pred) <- VertexName
rownames(LSM20_pred) <- VertexName

colnames(LSM20_classification) <- VertexName
rownames(LSM20_classification) <- VertexName


LSM20_pred_Tidy <- AdjMat_To_TidyDataDf(LSM20_pred)
LSM20_Classi_Tidy <- AdjMat_To_TidyDataDf(LSM20_classification)
AdjMat_Tidy <- AdjMat_To_TidyDataDf(AdjMat)


# --- Make a confusion matrix ---
confusionMatrix(as.factor(LSM20_Classi_Tidy$Visafree), 
                as.factor(AdjMat_Tidy$Visafree))

# --- Analyze the ROC curve ---
roc(AdjMat_Tidy$Visafree ~ LSM20_pred_Tidy$Visafree, 
    plot = TRUE, print.auc = TRUE)


par(par_temp)
