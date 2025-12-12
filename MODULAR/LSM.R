# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(statnet)
library(latentnet)

# ============== - Input Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# ======================= - Fit Latent Space Model - =======================


LSM_Model <- ergmm(Visafree_network ~ euclidean(d = 2))

model_2 <- ergmm(network_1 ~ euclidean(d = 3))

model_3 <- ergmm(network_1 ~ euclidean(d = 3) +
                   nodefactor("Language") +
                   nodecov("Coloniser") +
                   nodecov("Colonised") +
                   nodecov("Area") +
                   nodecov("InternalDistance") +
                   nodecov("ln_GDP"))
summary(model_1)
summary(model_2)
summary(model_3)

plot(model_3)

gof_model_3 <- gof(model_3, GOF = ~idegree + odegree + distance + triadcensus)
gof_model_3

plot(gof_model_3)


# x <- gof(model_3)
# plot(x)
