# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(statnet)
# library(ergm)

# ============== - Input Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# ========== - Fit ERGM - ================

library(statnet)

ERGM_FullModel <- ergm(Visafree_network ~ edges 
                       + nodecov("ln_GDP")
                       + nodefactor("Coloniser")
                       + nodefactor("Colonised")
                       + nodefactor("LangGroup")
                       + nodecov("Transformed_Area")
                       # + nodefactor("Colonised")
                       + dyadcov(Dya_GeoDistance)
                       + dyadcov(Dya_RelDistance)
                       + dyadcov(Dya_Border)
                       + mutual)
# ERGM_FullModel <- ergm(Visafree_network ~ edges + mutual + nodematch("Language", diff = TRUE))

summary(ERGM_FullModel)
mcmc.diagnostics(ERGM_FullModel)
Gof_ERGM_FullModel <- gof(ERGM_FullModel)
summary(Gof_ERGM_FullModel)
plot(Gof_ERGM_FullModel)


ERGM_SmallModel <- ergm(Visafree_network ~ edges) 
summary(ERGM_SmallModel)
mcmc.diagnostics(ERGM_SmallModel)
Gof_ERGM_SmallModel <- gof(ERGM_SmallModel)
summary(Gof_ERGM_SmallModel)
plot(Gof_ERGM_SmallModel)

ERGM_Model <- ergm(Visafree_network ~ edges + mutual
                        + nodecov("ln_GDP") 
                        # + nodefactor("Coloniser")
                        # + nodecov("Transformed_Area")
                        # + nodefactor("Colonised")
                        + dyadcov(Dya_GeoDistance)
                        # + dyadcov(Dya_RelDistance)
                        # + dyadcov(Dya_Border)
                   )

summary(ERGM_Model)
mcmc.diagnostics(ERGM_Model)
Gof_ERGM_Model <- gof(ERGM_Model)
summary(Gof_ERGM_Model)
plot(Gof_ERGM_Model)


ERGM_Choice
ERGM_as_LR_Choice <- ergm(Visafree_network ~ edges 
                       + nodecov("ln_GDP") + nodefactor("Coloniser")
                       + nodecov("Transformed_Area")
                       # + nodefactor("Colonised")
                       + dyadcov(Dya_GeoDistance)
                       + dyadcov(Dya_RelDistance)
                       + dyadcov(Dya_Border)
                       + mutual)

summary(ERGM_as_LR_Choice)
mcmc.diagnostics(ERGM_as_LR_Choice)
Gof_ERGM_as_LR_Choice <- gof(ERGM_as_LR_Choice)
summary(Gof_ERGM_as_LR_Choice)
plot(Gof_ERGM_as_LR_Choice)

# =========================================================
### Gör en ERGM som ska motsvara LR, men med rho också ###
summary(LR_FullModel)
summary(LR_Choice)
# =========================================================


# Jämför glm vs ERGM vs LSM
AIC(LR_FullModel)
AIC(ERGM_FullModel)
AIC(model_lsm)

# Goodness-of-fit för ERGM
gof_ergm <- gof(ERGM_FullModel)
plot(gof_ergm)













# ============== - Output Variables - =======================


# # Variables cleaning - Comment/Uncomment at will
rm(LR_FullModel, LR_StartModel, key.creator)




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - The data is unbalanced! Do wee need do handle that?
# - Re-do analysis with comparison between 
#       - GDP
#       - Area
#       - "SameLanguage" (till exempel?)
#     - Also Language_Grouped (perhaps from earlier?)

