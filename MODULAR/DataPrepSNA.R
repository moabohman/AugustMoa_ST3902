# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(statnet)

# ============== - Input Variables - =======================

AdjMat <- AdjMat
VerAtt_SNA <- VertexAttributes
DyaAtt_SNA <- DyadAttributes

# Network object from previous? [instead of Ver/Dya-matrices!]

# ========== - Select covariates - ================
# Let's skip Language for now

VerAtt_SNA <- VerAtt_SNA[,!names(VerAtt_SNA) %in% c("Language")]

# ========== - Create network object - ================

# Skapa nätverk

Visafree_network <- as.network(AdjMat, directed = TRUE, matrix.type = "adjacency")
Visafree_network

# ========== - Add vertex attributes - ================

# Check matching order
sum(network.vertex.names(Visafree_network) == VerAtt_SNA$VertexName)

network::set.vertex.attribute(Visafree_network, 
                     names(VerAtt_SNA[,!names(VerAtt_SNA) %in% c("VertexName")]), 
                     VerAtt_SNA[,!names(VerAtt_SNA) %in% c("VertexName")])


# ========== - Create adjacency matrices for dyad attributes - ================

# --- GeoDistance ---
Dya_GeoDistance <- xtabs(avg_distance_km ~ Origin + Destination, data = DyaAtt_SNA,
                    sparse = TRUE)
Dya_GeoDistance <- as.matrix(Dya_GeoDistance)

# --- RelDistance ---
Dya_RelDistance <- xtabs(reldist_weighted ~ Origin + Destination, data = DyaAtt_SNA,
                            sparse = TRUE)
Dya_RelDistance <- as.matrix(Dya_RelDistance)

# --- Border ---
Dya_Border <- xtabs(as.character(border) ~ Origin + Destination, data = DyaAtt_SNA,
                            sparse = TRUE)
Dya_Border <- as.matrix(Dya_Border)


# ============== - Output Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# Variables cleaning - Comment/Uncomment at will
rm(VerAtt_SNA, DyaAtt_SNA)



# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Anything regarding dyad attributes?
# - Another script: Visualization/EDA of the network?