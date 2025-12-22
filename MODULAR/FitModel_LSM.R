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

# ============== - Input Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_GeoDistanceNorm <- Dya_GeoDistanceNorm
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# ============== - LSM1 - =======================

LSM1 <- ergmm(Visafree_network ~ euclidean(d = 2))

# save(LSM1, file = "LSM1.RData")

summary(LSM1)
gof_LSM1 <- gof(LSM1)
plot(gof_LSM1)
# save(gof_LSM1, file = "gof_LSM1.RData")

plot(LSM1,use.rgl=FALSE, labels = TRUE)
plot(LSM1,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM1$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM_Model9)
# # We can also plot the burn-in:
# for(i in LSM_Model9$control$pilot.runs) mcmc.diagnostics(LSM_Model9,burnin=i)
# ---- .. ----



# ============== - LSM2 - =======================

LSM2 <- ergmm(Visafree_network ~ euclidean(d = 3)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM2, file = "LSM2.RData")

summary(LSM2)
gof_LSM2 <- gof(LSM2)
plot(gof_LSM2)
# save(gof_LSM2, file = "gof_LSM2.RData")

plot(LSM2,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM2$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM2)
# # We can also plot the burn-in:
# for(i in LSM2$control$pilot.runs) mcmc.diagnostics(LSM2,burnin=i)
# ---- .. ----


# ============== - LSM3 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_GeoDistanceNorm)
        + dyadcov(Dya_Border)
        + dyadcov(Dya_RelDistance)
)
LSM3 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_GeoDistanceNorm)
              + dyadcov(Dya_Border)
              + dyadcov(Dya_RelDistance)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM3, file = "LSM3.RData")

summary(LSM3)
gof_LSM3 <- gof(LSM3)
plot(gof_LSM3)
# save(gof_LSM3, file = "gof_LSM3.RData")

plot(LSM3,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM3$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM3)
# # We can also plot the burn-in:
# for(i in LSM3$control$pilot.runs) mcmc.diagnostics(LSM3,burnin=i)
# ---- .. ----


# ============== - LSM3_A - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_GeoDistanceNorm)
)
LSM3_A <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_GeoDistanceNorm)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM3_A, file = "LSM3_A.RData")

summary(LSM3_A)
gof_LSM3_A <- gof(LSM3_A)
plot(gof_LSM3_A)
# save(gof_LSM3_A, file = "gof_LSM3_A.RData")

plot(LSM3_A,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM3_A$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM3_A)
# # We can also plot the burn-in:
# for(i in LSM3_A$control$pilot.runs) mcmc.diagnostics(LSM3_A,burnin=i)
# ---- .. ----


# ============== - LSM3_B - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM3_B <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM3_B, file = "LSM3_B.RData")

summary(LSM3_B)
gof_LSM3_B <- gof(LSM3_B)
plot(gof_LSM3_B)
# save(gof_LSM3_B, file = "gof_LSM3_B.RData")

par(par_temp)
par(mfrow=c(2,2))

plot(LSM3_B,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM3_B$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM3_B)
# # We can also plot the burn-in:
# for(i in LSM3_B$control$pilot.runs) mcmc.diagnostics(LSM3_B,burnin=i)
# ---- .. ----



# ============== - LSM3_C - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_RelDistance)
)
LSM3_C <- ergmm(Visafree_network ~ euclidean(d = 3)
                + nodefactor("LangGroup")
                + nodefactor("Coloniser")
                + nodefactor("Colonised")
                + nodecov("NonViolence")
                + nodecov("ln_GDP")
                + nodecov("Trans_NetMigration")
                + dyadcov(Dya_RelDistance)
                , control=ergmm.control(store.burnin=TRUE))

# save(LSM3_C, file = "LSM3_C.RData")

summary(LSM3_C)
gof_LSM3_C <- gof(LSM3_C)
plot(gof_LSM3_C)
# save(gof_LSM3_C, file = "gof_LSM3_C.RData")

plot(LSM3_C,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM3_C$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM3_C)
# # We can also plot the burn-in:
# for(i in LSM3_C$control$pilot.runs) mcmc.diagnostics(LSM3_C,burnin=i)
# ---- .. ----




# ============== - LSM4 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
)
LSM4 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodecov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM4, file = "LSM4.RData")

summary(LSM4)
gof_LSM4 <- gof(LSM4)
plot(gof_LSM4)
# save(gof_LSM4, file = "gof_LSM4.RData")

plot(LSM4,use.rgl=FALSE, labels = TRUE)
plot(LSM4,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1, what = "pmean")

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM4$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM4)
# # We can also plot the burn-in:
# for(i in LSM4$control$pilot.runs) mcmc.diagnostics(LSM4,burnin=i)
# ---- .. ----



# ============== - LSM5 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodeofactor("Coloniser")
        + nodeifactor("Coloniser")
        + nodeofactor("Colonised")
        + nodeifactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodeocov("ln_GDP")
        + nodeicov("ln_GDP")
        + nodeocov("Trans_NetMigration")
        + nodeicov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM5 <- ergmm(Visafree_network ~ euclidean(d = 3)
                + nodefactor("LangGroup")
                + nodeofactor("Coloniser")
                + nodeifactor("Coloniser")
                + nodeofactor("Colonised")
                + nodeifactor("Colonised")
                + nodeocov("NonViolence")
                + nodeicov("NonViolence")
                + nodeocov("ln_GDP")
                + nodeicov("ln_GDP")
                + nodeocov("Trans_NetMigration")
                + nodeicov("Trans_NetMigration")
                + dyadcov(Dya_Border)
                , control=ergmm.control(store.burnin=TRUE))

# save(LSM5, file = "LSM5.RData")

summary(LSM5)
gof_LSM5 <- gof(LSM5)
plot(gof_LSM5)
# save(gof_LSM5, file = "gof_LSM5.RData")

plot(LSM5,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM5$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM5)
# # We can also plot the burn-in:
# for(i in LSM5$control$pilot.runs) mcmc.diagnostics(LSM5,burnin=i)
# ---- .. ----



# ============== - LSM6 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
)
LSM6 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodeocov("NonViolence")
              + nodeicov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM6, file = "LSM6.RData")

summary(LSM6)
gof_LSM6 <- gof(LSM6)
plot(gof_LSM6)
# save(gof_LSM6, file = "gof_LSM6.RData")

plot(LSM6,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM6$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM6)
# # We can also plot the burn-in:
# for(i in LSM6$control$pilot.runs) mcmc.diagnostics(LSM6,burnin=i)
# ---- .. ----



# ============== - LSM7 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM7 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodeocov("NonViolence")
              + nodeicov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM7, file = "LSM7.RData")

summary(LSM7)
gof_LSM7 <- gof(LSM7)
plot(gof_LSM7)
# save(gof_LSM7, file = "gof_LSM7.RData")

plot(LSM7,use.rgl=FALSE, labels = TRUE)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM7$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM7)
# # We can also plot the burn-in:
# for(i in LSM7$control$pilot.runs) mcmc.diagnostics(LSM7,burnin=i)
# ---- .. ----




# ============== - LSM8 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodeocov("ln_GDP")
        + nodeicov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM8 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodeocov("NonViolence")
              + nodeicov("NonViolence")
              + nodeocov("ln_GDP")
              + nodeicov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM8, file = "LSM8.RData")

summary(LSM8)
gof_LSM8 <- gof(LSM8)
plot(gof_LSM8)
# save(gof_LSM8, file = "gof_LSM8.RData")

plot(LSM8,use.rgl=FALSE, labels = TRUE)
plot(LSM8,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1, what = "pmean")

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM8$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM8)
# # We can also plot the burn-in:
# for(i in LSM8$control$pilot.runs) mcmc.diagnostics(LSM8,burnin=i)
# ---- .. ----



# ============== - LSM9 - =======================
summary(Visafree_network ~ dyadcov(Dya_Border)
)
LSM9 <- ergmm(Visafree_network ~ euclidean(d = 3)
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM9, file = "LSM9.RData")

summary(LSM9)
gof_LSM9 <- gof(LSM9)
plot(gof_LSM9)
# save(gof_LSM9, file = "gof_LSM9.RData")

plot(LSM9,use.rgl=FALSE, labels = TRUE)
plot(LSM9,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1, what = "pmean")

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM9$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM9)
# # We can also plot the burn-in:
# for(i in LSM9$control$pilot.runs) mcmc.diagnostics(LSM9,burnin=i)
# ---- .. ----



# ============== - LSM10 - =======================

# summary()
LSM10 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM10, file = "LSM10.RData")

summary(LSM10)
gof_LSM10 <- gof(LSM10)
plot(gof_LSM10)
# save(gof_LSM10, file = "gof_LSM10.RData")

plot(LSM10,use.rgl=FALSE, labels = TRUE)
plot(LSM10,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM10$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM10)
# # We can also plot the burn-in:
# for(i in LSM10$control$pilot.runs) mcmc.diagnostics(LSM10,burnin=i)
# ---- .. ----




# ============== - LSM11 - =======================

# summary()
LSM11 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 3)
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM11, file = "LSM11.RData")

summary(LSM11)
gof_LSM11 <- gof(LSM11)
plot(gof_LSM11)
# save(gof_LSM11, file = "gof_LSM11.RData")

plot(LSM11,use.rgl=FALSE, labels = TRUE)
plot(LSM11,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM11$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM11)
# # We can also plot the burn-in:
# for(i in LSM11$control$pilot.runs) mcmc.diagnostics(LSM11,burnin=i)
# ---- .. ----



# ============== - LSM12 - =======================


summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM12 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
                + nodefactor("LangGroup")
                + nodefactor("Coloniser")
                + nodefactor("Colonised")
                + nodecov("NonViolence")
                + nodecov("ln_GDP")
                + nodecov("Trans_NetMigration")
                + dyadcov(Dya_Border)
                , control=ergmm.control(store.burnin=TRUE))

# save(LSM12, file = "LSM12.RData")

summary(LSM12)
gof_LSM12 <- gof(LSM12)
plot(gof_LSM12)
# save(gof_LSM12, file = "gof_LSM12.RData")

plot(LSM12,use.rgl=FALSE, labels = TRUE)
plot(LSM12,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM12$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM12)
# # We can also plot the burn-in:
# for(i in LSM12$control$pilot.runs) mcmc.diagnostics(LSM12,burnin=i)
# ---- .. ----


# ============== - LSM13 - =======================

summary(Visafree_network ~ nodefactor("LangGroup")
        + nodeofactor("Coloniser")
        + nodeifactor("Coloniser")
        + nodeofactor("Colonised")
        + nodeifactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodeocov("ln_GDP")
        + nodeicov("ln_GDP")
        + nodeocov("Trans_NetMigration")
        + nodeicov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM13 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
              + nodefactor("LangGroup")
              + nodeofactor("Coloniser")
              + nodeifactor("Coloniser")
              + nodeofactor("Colonised")
              + nodeifactor("Colonised")
              + nodeocov("NonViolence")
              + nodeicov("NonViolence")
              + nodeocov("ln_GDP")
              + nodeicov("ln_GDP")
              + nodeocov("Trans_NetMigration")
              + nodeicov("Trans_NetMigration")
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM13, file = "LSM13.RData")

summary(LSM13)
gof_LSM13 <- gof(LSM13)
plot(gof_LSM13)
# save(gof_LSM13, file = "gof_LSM13.RData")

plot(LSM13,use.rgl=FALSE, labels = TRUE)
plot(LSM13,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM13$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM13)
# # We can also plot the burn-in:
# for(i in LSM13$control$pilot.runs) mcmc.diagnostics(LSM13,burnin=i)
# ---- .. ----



# ============== - LSM14 - =======================

# summary()
LSM14 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
               + rreceiver
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM14, file = "LSM14.RData")

summary(LSM14)
gof_LSM14 <- gof(LSM14)
plot(gof_LSM14)
# save(gof_LSM14, file = "gof_LSM14.RData")

plot(LSM14,use.rgl=FALSE, labels = TRUE)
plot(LSM14,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM14$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM14)
# # We can also plot the burn-in:
# for(i in LSM14$control$pilot.runs) mcmc.diagnostics(LSM14,burnin=i)
# ---- .. ----




# ============== - LSM15 - =======================

# summary()
LSM15 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
               + rsender
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM15, file = "LSM15.RData")

summary(LSM15)
gof_LSM15 <- gof(LSM15)
plot(gof_LSM15)
# save(gof_LSM15, file = "gof_LSM15.RData")

plot(LSM15,use.rgl=FALSE, labels = TRUE)
plot(LSM15,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM15$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM15)
# # We can also plot the burn-in:
# for(i in LSM15$control$pilot.runs) mcmc.diagnostics(LSM15,burnin=i)
# ---- .. ----




# ============== - LSM16 - =======================

# summary()
LSM16 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
               + rsender
               + rreceiver
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM16, file = "LSM16.RData")

summary(LSM16)
gof_LSM16 <- gof(LSM16)
plot(gof_LSM16)
# save(gof_LSM16, file = "gof_LSM16.RData")

plot(LSM16,use.rgl=FALSE, labels = TRUE)
plot(LSM16,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM16$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM16)
# # We can also plot the burn-in:
# for(i in LSM16$control$pilot.runs) mcmc.diagnostics(LSM16,burnin=i)
# ---- .. ----




# ============== - LSM17 - =======================

# summary()
LSM17 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 3)
               + rsender
               + rreceiver
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM17, file = "LSM17.RData")

summary(LSM17)
gof_LSM17 <- gof(LSM17)
plot(gof_LSM17)
# save(gof_LSM17, file = "gof_LSM17.RData")

plot(LSM17,use.rgl=FALSE, labels = TRUE)
plot(LSM17,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM17$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM17)
# # We can also plot the burn-in:
# for(i in LSM17$control$pilot.runs) mcmc.diagnostics(LSM17,burnin=i)
# ---- .. ----




# ============== - LSM18 - =======================

# summary()
LSM18 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 4)
               + rsender
               + rreceiver
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM18, file = "LSM18.RData")

summary(LSM18)
gof_LSM18 <- gof(LSM18)
plot(gof_LSM18)
# save(gof_LSM18, file = "gof_LSM18.RData")

plot(LSM18,use.rgl=FALSE, labels = TRUE)
plot(LSM18,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM18$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM18)
# # We can also plot the burn-in:
# for(i in LSM18$control$pilot.runs) mcmc.diagnostics(LSM18,burnin=i)
# ---- .. ----




# ============== - LSM19 - =======================

summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)

LSM19 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
               + rsender
               + rreceiver
               + nodefactor("LangGroup")
               + nodefactor("Coloniser")
               + nodefactor("Colonised")
               + nodecov("NonViolence")
               + nodecov("ln_GDP")
               + nodecov("Trans_NetMigration")
               + dyadcov(Dya_Border)
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM19, file = "LSM19.RData")

summary(LSM19)
gof_LSM19 <- gof(LSM19)
plot(gof_LSM19)
# save(gof_LSM19, file = "gof_LSM19.RData")

par(par_temp)
par(mfrow=c(2,2))

plot(LSM19,use.rgl=FALSE, labels = TRUE)
plot(LSM19,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM19$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM19)
# # We can also plot the burn-in:
# for(i in LSM19$control$pilot.runs) mcmc.diagnostics(LSM19,burnin=i)
# ---- .. ----




# ============== - LSM20 - =======================
summary(Visafree_network ~ nodefactor("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodeocov("NonViolence")
        + nodeicov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)
LSM20 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
              + rsender
              + rreceiver
              + nodefactor("LangGroup")
              + nodefactor("Coloniser")
              + nodefactor("Colonised")
              + nodeocov("NonViolence")
              + nodeicov("NonViolence")
              + nodecov("ln_GDP")
              + nodecov("Trans_NetMigration")
              + dyadcov(Dya_Border)
              , control=ergmm.control(store.burnin=TRUE))

# save(LSM20, file = "LSM20.RData")

summary(LSM20)
gof_LSM20 <- gof(LSM20)
plot(gof_LSM20)
# save(gof_LSM20, file = "gof_LSM20.RData")

par(par_temp)
par(mfrow=c(2,2))

plot(LSM20,use.rgl=FALSE, labels = TRUE)
plot(LSM20,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM20$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM20)
# # We can also plot the burn-in:
# for(i in LSM20$control$pilot.runs) mcmc.diagnostics(LSM20,burnin=i)
# ---- .. ----



# ============== - LSM21 - =======================

summary(Visafree_network ~ nodematch("LangGroup")
        + nodefactor("Coloniser")
        + nodefactor("Colonised")
        + nodecov("NonViolence")
        + nodecov("ln_GDP")
        + nodecov("Trans_NetMigration")
        + dyadcov(Dya_Border)
)

LSM21 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
               + rsender
               + rreceiver
               + nodematch("LangGroup")
               + nodefactor("Coloniser")
               + nodefactor("Colonised")
               + nodecov("NonViolence")
               + nodecov("ln_GDP")
               + nodecov("Trans_NetMigration")
               + dyadcov(Dya_Border)
               , control=ergmm.control(store.burnin=TRUE))

# save(LSM21, file = "LSM21.RData")

summary(LSM21)
gof_LSM21 <- gof(LSM21)
plot(gof_LSM21)
# save(gof_LSM21, file = "gof_LSM21.RData")

par(par_temp)
par(mfrow=c(2,2))

plot(LSM21,use.rgl=FALSE, labels = TRUE)
plot(LSM21,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM21$mkl$Z)
names(Coordinates) <- c("x1","x2","x3")
Coordinates <- cbind(Coordinates, VertexName)
plot_ly(Coordinates, x = ~x1, y = ~x2, z = ~x3, color= ~VertexName)
# ---- .. ----

# ---- Diagnostics ----
# # See if we have convergence in the MCMC
# mcmc.diagnostics(LSM21)
# # We can also plot the burn-in:
# for(i in LSM21$control$pilot.runs) mcmc.diagnostics(LSM21,burnin=i)
# ---- .. ----






# =========================================================
# =========================================================
# par_temp <- par()
# par(par_temp)
# par(mfrow=c(2,2))

Vertex_RealNames



















