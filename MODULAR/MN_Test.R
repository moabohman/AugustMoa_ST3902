# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(nnet)

library(caret)
library(pROC)

# ============== - Input Variables - =======================

TidyData_MN <- TidyData_MN
# names(TidyData_MN)


# ====================== - Data exploration - =======================
# Mainly for multicollinearity (and outliers, as well as correlation to Visafree)

library(esquisse)
# esquisser(TidyData_MN, viewer = "browser")


## Correlations (from LR analysis):
##   - avg_distance_km and avg_distance_norm - of course!
##   - Transformed_Area and ln_GDP

# ========== - Conduct Logistic Regression - ================

# Find best multinomial logit regression model
MN_FullModel <- multinom(VisafreeDyadic ~ . 
                    - CountryDyad 
                    - avg_distance_norm
                    - OriginLanguage
                    - DestinationLanguage, 
                    # also Transformed_Area or ln_GDP for each country?
                    data = TidyData_MN)
summary(MN_FullModel)
# anova(MN_FullModel)

MN_StartModel <- multinom(VisafreeDyadic ~ 1, data = TidyData_MN)
summary(MN_StartModel)
anova(MN_StartModel, MN_FullModel)


# Framåtval
step(MN_StartModel, 
     scope = list(lower = MN_StartModel, upper = MN_FullModel), 
     direction = "forward")
# Bakåteliminering
step(MN_FullModel, 
     direction = "backward")
# Stegvis regression
step(MN_StartModel,
     scope = list(lower = MN_StartModel, upper = MN_FullModel),
     direction = "both")


# Best fit:
MN_Choice <- multinom(VisafreeDyadic ~ avg_distance_km + Originln_GDP + 
                   Destinationln_GDP + DestinationTransformed_Area + reldist_weighted + 
                   DestinationColoniser + OriginTransformed_Area + OriginColoniser + 
                   DestinationColonised + border, data = TidyData_MN)

summary(MN_Choice)


# ========== - Evaluation - ================


table(TidyData_MN$VisafreeDyadic)
xtabs( ~ predict(MN_Choice) + TidyData_MN$VisafreeDyadic)

# Bad predictions when 1:s are involved!


# ============== - Output Variables - =======================


# # Variables cleaning - Comment/Uncomment at will
rm(MN_FullModel, MN_StartModel)




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - The data is unbalanced! Do wee need do handle that?
# - Re-do analysis with comparison between 
#       - GDP
#       - Area
#       - "SameLanguage" (till exempel?)
#     - Also Language_Grouped (perhaps from earlier?)

