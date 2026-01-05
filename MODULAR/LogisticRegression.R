##################################################
#                                                #
#               LogisticRegression               #
#                                                #
##################################################


# ============== - Input Variables - =======================

TidyData <- TidyData
# names(TidyData)

# ====================== - Data exploration - =======================
# Mainly for multicollinearity (and outliers, as well as correlation to Visafree)

TidyData_asFactor <- TidyData
TidyData_asFactor$Visafree <- as.factor(TidyData_asFactor$Visafree)
# esquisser(TidyData_asFactor, viewer = "browser")

cor(TidyData[,!names(TidyData) %in% c(
  "Origin","Destination","border",
  "OriginLanguage", "DestinationLanguage",
  "OriginLangGroup", "DestinationLangGroup",
  "OriginColoniser","OriginColonised",
  "DestinationColoniser","DestinationColonised")])

corrplot.mixed(cor(TidyData[,!names(TidyData) %in% c(
    "Origin","Destination","border",
    "OriginLanguage", "DestinationLanguage",
    "OriginLangGroup", "DestinationLangGroup",
    "OriginColoniser","OriginColonised",
    "DestinationColoniser","DestinationColonised")]),
  upper = 'number',
  lower = "circle",
  tl.pos = "lt",
  tl.col = "black",
  tl.cex = 0.8,
  addCoefasPercent = TRUE,
  number.cex=0.8)


## Correlations:
##   - avg_distance_km and avg_distance_norm - of course!
##   - Transformed_Area and ln_GDP

# ========== - Conduct Logistic Regression - ================

# Find best logistic regression model
LR_FullModel <- glm(Visafree ~ avg_distance_km
                    + border
                    + reldist_weighted
                    + OriginLangGroup
                    + OriginColoniser
                    + OriginColonised
                    + Originln_GDP
                    # + OriginTransformed_Area
                    + DestinationLangGroup
                    + DestinationColoniser
                    + DestinationColonised
                    + Destinationln_GDP
                    # + DestinationTransformed_Area
                    + OriginNonViolence
                    + OriginTrans_NetMigration
                    + DestinationNonViolence
                    + DestinationTrans_NetMigration,
                    # also Transformed_Area or ln_GDP for each country?
                    family = "binomial", data = TidyData)
summary(LR_FullModel)
# anova(LR_FullModel)

LR_StartModel <- glm(Visafree ~ 1, family = "binomial", data = TidyData)
summary(LR_StartModel)
anova(LR_StartModel, LR_FullModel)


# # Framåtval
# step(LR_StartModel,
#      scope = list(lower = LR_StartModel, upper = LR_FullModel),
#      direction = "forward")
# # Bakåteliminering
# step(LR_FullModel,
#      direction = "backward")
# # Stegvis regression
# step(LR_StartModel,
#      scope = list(lower = LR_StartModel, upper = LR_FullModel),
#      direction = "both")


# Best fit: (same as LR_FullModel)
LR_Choice <- glm(Visafree ~ DestinationLangGroup
                 + Destinationln_GDP
                 + avg_distance_km
                 + OriginLangGroup
                 + OriginColoniser
                 # + OriginTransformed_Area
                 + Originln_GDP
                 # + DestinationTransformed_Area
                 + border
                 + DestinationColonised
                 + reldist_weighted
                 + DestinationColoniser
                 + OriginColonised
                 + OriginNonViolence
                 # + OriginTrans_NetMigration
                 + DestinationNonViolence
                 + DestinationTrans_NetMigration,
                 # also Transformed_Area or ln_GDP for each country?
                 family = "binomial", data = TidyData)

summary(LR_Choice)
anova(LR_Choice)

# Check for multicollinearity in chosen model
vif(LR_Choice)
# OK! (Values < 5 )



# ========== - Evaluation - ================

# --- Retrieve predicted values ---
TidyData_Eval <- TidyData[,names(TidyData) %in% c("Origin","Destination","Visafree")]
TidyData_Eval$Prediction <- predict(LR_Choice, type = "response")

# --- Classify observations ---
# Choose cut-off level
c <- 0.5
TidyData_Eval$Classification <- ifelse(TidyData_Eval$Prediction > c, 1, 0)

# --- Make a confusion matrix ---
confusionMatrix(as.factor(TidyData_Eval$Classification), 
                       as.factor(TidyData_Eval$Visafree))

# --- Analyze the ROC curve ---
roc(as.factor(TidyData_Eval$Visafree) ~ TidyData_Eval$Prediction, 
    plot = TRUE, print.auc = TRUE)

# =========================================================
# =============== - Reciprocality - =======================
# =========================================================

# --- Extend data with information on the reciprocal ---
TidyData_Reci <- merge(TidyData_Eval, TidyData_Eval,
                       by.x = c("Origin","Destination"), 
                       by.y = c("Destination","Origin"), all.x = TRUE)

# Clean out duplicates by creating key independent of direction
key.creator <- function(columns) {
  key <- paste(sort(columns), collapse = "")
  return(key)
}

TidyData_Reci$Key = apply(TidyData_Reci[, c("Origin", "Destination")], 
                          1, FUN = key.creator)
TidyData_Reci <- TidyData_Reci %>% distinct(Key, .keep_all = TRUE)

# Calculate true and predicted values for reciprocal Visafree

# True values
TidyData_Reci$Reci_VisaFree <- TidyData_Reci$Visafree.x * TidyData_Reci$Visafree.y
TidyData_Reci$Reci_VisaFree <- as.factor(TidyData_Reci$Reci_VisaFree)

# Predicted probabilities (under the assumption independent observations)
TidyData_Reci$Reci_Prediction <- TidyData_Reci$Prediction.x * TidyData_Reci$Prediction.y

# Classification

# Choose cut-off level (default is same as c above)
c_reci <- 0.25
TidyData_Reci$Reci_Classification <- ifelse(TidyData_Reci$Reci_Prediction > c_reci, 1, 0)
TidyData_Reci$Reci_Classification <- as.factor(TidyData_Reci$Reci_Classification)

# summary(TidyData_Reci)


# ===== - Compare true and predicted probailities for reciprocality - ======
# If the observation are independent they should be roughly similar

# --- Make a confusion matrix ---
confusionMatrix(TidyData_Reci$Reci_Classification, 
                TidyData_Reci$Reci_VisaFree)

# --- Analyze the ROC curve ---
roc(as.factor(TidyData_Reci$Reci_VisaFree) ~ TidyData_Reci$Reci_Prediction, 
    plot = TRUE, print.auc = TRUE)




# ============== - Output Variables - =======================


# # Variables cleaning - Comment/Uncomment at will
# rm(LR_FullModel, LR_StartModel, key.creator)
rm(LR_StartModel, key.creator)




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - The data is unbalanced! Do wee need do handle that?
# - Re-do analysis with comparison between 
#       - GDP
#       - Area
#       - "SameLanguage" (till exempel?)

