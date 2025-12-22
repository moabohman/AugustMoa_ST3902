# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(sna)
library(network)
library(latentnet)

# ============== - Input Variables - =======================

AdjMat <- AdjMat
Visafree_network <- Visafree_network


# ========== - Visualize the adjacency matrix/the network - ================

Visafree_network
# summary(Visafree_network, mixingmatrices = TRUE)
class(Visafree_network)
# plot(Visafree_network)
# plot( Visafree_network , # the network object
#       # vertex.cex = degree(Visafree_network) , # how should nodes (vertices) be scaled
#       displaylabels =  TRUE) # display the labels of vertices


# --- Measures ---
# Degree centrality
indeg <- degree(Visafree_network, cmode = "indegree")
outdeg <- degree(Visafree_network, cmode = "outdegree")
alldeg <- degree(Visafree_network)

print(sort(indeg))
print(sort(outdeg))
print(sort(alldeg))

hist(indeg)
hist(outdeg)
hist(alldeg)

# Betweenness centrality
bet <- betweenness(Visafree_network)
print(sort(bet))
# hist(bet)

# Closeness centrality
clo <- closeness(Visafree_network, cmode="undirected")
print(sort(clo))
hist(clo)

# Eigenvector centrality
eig <- evcent(Visafree_network)
print(sort(eig))
hist(eig)

# Density
dens <- gden(Visafree_network)
print(dens)

# Clustering coefficient?
clust <- gtrans(Visafree_network)
print(clust)

# --- Plots ---
# Basic force-directed layout
plot(Visafree_network, mode = "fruchtermanreingold", vertex.cex=alldeg*0.02,
     displaylabels = FALSE, vertex.col="skyblue",
     arrowhead.cex=0.1, edge.lwd = 0.01)

# Circular layout
plot(Visafree_network, mode="circle", vertex.cex=alldeg*0.02,
     displaylabels = FALSE, vertex.col="orange", edge.lwd = 0.01)

plot(Visafree_network)
plot(Visafree_network, mode="circle", 
     vertex.cex=3, displaylabels = FALSE, vertex.col="orange",
     arrowhead.cex=0.5, edge.lwd = 0.01)
plot(Visafree_network, mode="kamadakawai")



is.multiplex(Visafree_network)
is.directed(Visafree_network)
has.loops(Visafree_network)
ecount(Visafree_network)

library("sna")
plot.sociomatrix(AdjMat, main = "Sociomatrix")

# png("PlotSociomatrix.png", width=8000, height=6000)
# plot.sociomatrix(AdjMat, main = "Sociomatrix")
# dev.off()
# pdf("PlotSociomatrix.pdf", width=80, height=60)
# plot.sociomatrix(AdjMat, main = "Sociomatrix")
# dev.off()






# ============== - Output Variables - =======================

# Variables cleaning - Comment/Uncomment at will
rm(clo, clust, eig)



# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Create network object and use statnet to visualize as well?