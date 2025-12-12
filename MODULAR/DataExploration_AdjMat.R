# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(sna)
library(network)
library(latentnet)

library(esquisse)
library(fBasics)
library(interp)

# ============== - Input Variables - =======================

AdjMat <- AdjMat



# ========== - Visualize the adjacency matrix/the network - ================


# Load igraph
library(igraph)

# Create a sample graph
Visafree_graph <- graph_from_adjacency_matrix(AdjMat, mode = "directed")


# --- Measures ---
# Degree centrality
deg <- degree(Visafree_graph)
print(deg)
hist(deg)

# Betweenness centrality
bet <- betweenness(Visafree_graph)
print(bet)
# hist(bet)

# Closeness centrality
clo <- closeness(Visafree_graph)
print(clo)
hist(clo)

# Eigenvector centrality
eig <- eigen_centrality(Visafree_graph)$vector
print(eig)
hist(eig)

# Density
dens <- edge_density(Visafree_graph)
print(dens)

# Clustering coefficient
clust <- transitivity(Visafree_graph, type="global")
print(clust)

# --- Plots ---
# Basic force-directed layout
plot(Visafree_graph, layout=layout_with_fr, vertex.size=deg*0.05,
     vertex.label = NA, vertex.color="skyblue")

# Circular layout
plot(Visafree_graph, layout=layout_in_circle, vertex.size=deg*0.05,
     vertex.label = NA, vertex.color="orange")

plot(Visafree_graph)
plot(Visafree_graph, layout=layout.circle, 
     vertex.size=3, vertex.label = NA, vertex.color="orange",
     edge.arrow.size=0.5)
plot(Visafree_graph, layout=layout.kamada.kawai)
plot(Visafree_graph, layout=layout_with_fr(Visafree_graph))


is_weighted(Visafree_graph)
ecount(Visafree_graph)









# ============== - Output Variables - =======================





# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Create network object and use statnet to visualize as well?
#     - In that case: Let the network be outvar. here and invar. in next.