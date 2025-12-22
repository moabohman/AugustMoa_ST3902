# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(statnet)
library(latentnet)
library(sna)
library(plotly)
library(rgl)

library(tidyverse)
library(statnet)
library(Rglpk)
# library(ergm)

# ============== - Input Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_GeoDistanceNorm <- Dya_GeoDistanceNorm
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# ============== - ERGM1 - =======================

summary(Visafree_network ~ edges + mutual)
ERGM1 <- ergm(Visafree_network ~ edges + mutual)

# save(ERGM1, file = "ERGM1.RData")

summary(ERGM1)
gof_ERGM1 <- gof(ERGM1)
plot(gof_ERGM1)
# save(gof_ERGM1, file = "gof_ERGM1.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM1)
# ---- .. ----



# ============== - ERGM2 - =======================

summary(Visafree_network ~ edges
        + nodecov("ln_GDP")
)
ERGM2 <- ergm(Visafree_network ~ edges
              + nodecov("ln_GDP")
              )

# save(ERGM2, file = "ERGM2.RData")

summary(ERGM2)
gof_ERGM2 <- gof(ERGM2)
plot(gof_ERGM2)
# save(gof_ERGM2, file = "gof_ERGM2.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM2)
# # We can also plot the burn-in:
# for(i in ERGM2$control$pilot.runs) mcmc.diagnostics(ERGM2,burnin=i)
# ---- .. ----

# ============== - ERGM3 - =======================

summary(Visafree_network ~ edges
        + nodecov("NonViolence")
)
ERGM3 <- ergm(Visafree_network ~ edges
              + nodecov("NonViolence")
)

# save(ERGM3, file = "ERGM3.RData")

summary(ERGM3)
gof_ERGM3 <- gof(ERGM3)
plot(gof_ERGM3)
# save(gof_ERGM3, file = "gof_ERGM3.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM3)
# ---- .. ----

# ============== - ERGM4 - =======================

summary(Visafree_network ~ edges
        + nodematch("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
)
ERGM4 <- ergm(Visafree_network ~ edges
              + nodematch("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
)

# save(ERGM4, file = "ERGM4.RData")

summary(ERGM4)
gof_ERGM4 <- gof(ERGM4)
plot(gof_ERGM4)
# save(gof_ERGM4, file = "gof_ERGM4.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM4)
# ---- .. ----


# ============== - ERGM5 - =======================

summary(Visafree_network ~ edges
        + nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_GeoDistanceNorm)
        + dyadcov(Dya_Border)
        + dyadcov(Dya_RelDistance)
)
ERGM5 <- ergm(Visafree_network ~ edges
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_GeoDistanceNorm)
              + dyadcov(Dya_Border)
              + dyadcov(Dya_RelDistance)
)

# save(ERGM5, file = "ERGM5.RData")

summary(ERGM5)
gof_ERGM5 <- gof(ERGM5)
plot(gof_ERGM5)
# save(gof_ERGM5, file = "gof_ERGM5.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM5)
# ---- .. ----

# ============== - ERGM6 - =======================

summary(Visafree_network ~ edges
        + nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_GeoDistanceNorm)
        + dyadcov(Dya_Border)
        + dyadcov(Dya_RelDistance)
        + mutual
)
ERGM6 <- ergm(Visafree_network ~ edges
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_GeoDistanceNorm)
              + dyadcov(Dya_Border)
              + dyadcov(Dya_RelDistance)
              + mutual
)

# save(ERGM6, file = "ERGM6.RData")

summary(ERGM6)
gof_ERGM6 <- gof(ERGM6)
plot(gof_ERGM6)
# save(gof_ERGM6, file = "gof_ERGM6.RData")

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(ERGM6)
# ---- .. ----











# =========================================================
# =========================================================

# par_temp <- par()
# par(par_temp)
# par(mfrow=c(3,2))






















