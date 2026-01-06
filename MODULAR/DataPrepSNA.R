##################################################
#                                                #
#                  DataPrepSNA                   #
#                                                #
##################################################


# ============== - Input Variables - =======================

AdjMat <- AdjMat
VerAtt_SNA <- VertexAttributes
DyaAtt_SNA <- DyadAttributes

# ========== - Select covariates - ================
# Let's skip Language and Transformed_Area for now

VerAtt_SNA <- VerAtt_SNA[,!names(VerAtt_SNA) %in% c("Language", "Transformed_Area")]

# ========== - Create network object - ================

# Create network
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
Dya_GeoDistance <- xtabs(avg_distance_km ~ Origin + Destination, 
                         data = DyaAtt_SNA, sparse = TRUE)
Dya_GeoDistance <- as.matrix(Dya_GeoDistance)
# Dya_GeoDistance <- symmetrize(Dya_GeoDistance, rule = "upper")

# --- GeoDistanceNorm ---
Dya_GeoDistanceNorm <- xtabs(avg_distance_norm ~ Origin + Destination, 
                             data = DyaAtt_SNA, sparse = TRUE)
Dya_GeoDistanceNorm <- as.matrix(Dya_GeoDistanceNorm)
# Dya_GeoDistanceNorm <- symmetrize(Dya_GeoDistanceNorm, rule = "upper")

# --- RelDistance ---
Dya_RelDistance <- xtabs(reldist_weighted ~ Origin + Destination, 
                         data = DyaAtt_SNA, sparse = TRUE)
Dya_RelDistance <- as.matrix(Dya_RelDistance)
# Dya_RelDistance <- symmetrize(Dya_RelDistance, rule = "upper")

# --- Border ---
Dya_Border <- xtabs(as.character(border) ~ Origin + Destination, 
                    data = DyaAtt_SNA, sparse = TRUE)
Dya_Border <- as.matrix(Dya_Border)
# Dya_Border <- symmetrize(Dya_Border, rule = "weak")


# isSymmetric.matrix(Dya_Border)


# (--- Issue with non-symmetric adjmat for RelDistance ---)
# sumeq <- matrix(0, nrow = 199, ncol = 199)
# for (i in 1:199) {
#   for (j in 1:199) {
#     sumeq[i,j] <- Dya_RelDistance[i,j] == Dya_RelDistance[j,i]
#   }
# }
# sum(colSums(sumeq)) == 199*199
# (-------------------------------------------------------)



# ============== - Output Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_GeoDistanceNorm <- Dya_GeoDistanceNorm
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# Variables cleaning - Comment/Uncomment at will
rm(VerAtt_SNA, DyaAtt_SNA)


# save(Visafree_network, file = "Visafree_network.RData")
# save(Dya_GeoDistance, file = "Dya_GeoDistance.RData")
# save(Dya_GeoDistanceNorm, file = "Dya_GeoDistanceNorm")
# save(Dya_RelDistance, file = "Dya_RelDistance.RData")
# save(Dya_Border, file = "Dya_Border.RData")




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

