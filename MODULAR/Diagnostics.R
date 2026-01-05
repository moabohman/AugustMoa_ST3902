##################################################
#                                                #
#                  Diagnostics                   #
#                                                #
##################################################


# ====== - Examine distribution of sociality effects - ======
#  - Look for outliers, and shape/spred of the distribution
#  - Are there a few extreme values affecting the fit?

hist((LSM22[["mkl"]][["receiver"]]))
hist((LSM22[["mkl"]][["sender"]]))
# hist((LSM22[["mcmc.mle"]][["receiver"]]))
# hist((LSM22[["mcmc.mle"]][["sender"]]))
# hist((LSM22[["mcmc.pmode"]][["receiver"]]))
# hist((LSM22[["mcmc.pmode"]][["sender"]]))

hist((LSM22[["mkl"]][["receiver"]]), breaks = 20)
hist((LSM22[["mkl"]][["sender"]]), breaks = 20)
# hist((LSM22[["mcmc.mle"]][["receiver"]]), breaks = 20)
# hist((LSM22[["mcmc.mle"]][["sender"]]), breaks = 20)
# hist((LSM22[["mcmc.pmode"]][["receiver"]]), breaks = 20)
# hist((LSM22[["mcmc.pmode"]][["sender"]]), breaks = 20)


# =========================================================

Coloniser_numeric <- as.numeric(VertexAttributes$Coloniser)
Colonised_numeric <- as.numeric(VertexAttributes$Colonised)

# ===== - Examine correlation between sociality effects and kovariates - =====
# #  - Are they explaining the same thing?


corrplot.mixed(cor(cbind(VertexAttributes[,names(VertexAttributes) %in% c(
  "NonViolence","ln_GDP", "Transformed_Area","Trans_NetMigration")], 
  Coloniser_numeric,
  Colonised_numeric,
  (LSM22[["mkl"]][["receiver"]]),
  (LSM22[["mkl"]][["sender"]]))), 
     upper = 'number',
     lower = "circle",
     tl.pos = "lt",
     tl.col = "black",
     tl.cex = 0.8,
     addCoefasPercent = TRUE,
     number.cex=0.8)

# Same for mcmc.mle and mcmc.pmode

# =========================================================

hist((LSM24[["mkl"]][["receiver"]]))
hist((LSM24[["mkl"]][["sender"]]))
# hist((LSM24[["mcmc.mle"]][["receiver"]]))
# hist((LSM24[["mcmc.mle"]][["sender"]]))
# hist((LSM24[["mcmc.pmode"]][["receiver"]]))
# hist((LSM24[["mcmc.pmode"]][["sender"]]))

hist((LSM24[["mkl"]][["receiver"]]), breaks = 20)
hist((LSM24[["mkl"]][["sender"]]), breaks = 20)
# hist((LSM24[["mcmc.mle"]][["receiver"]]), breaks = 20)
# hist((LSM24[["mcmc.mle"]][["sender"]]), breaks = 20)
# hist((LSM24[["mcmc.pmode"]][["receiver"]]), breaks = 20)
# hist((LSM24[["mcmc.pmode"]][["sender"]]), breaks = 20)


corrplot.mixed(cor(cbind(VertexAttributes[,names(VertexAttributes) %in% c(
  "NonViolence","ln_GDP", "Transformed_Area","Trans_NetMigration")], 
  Coloniser_numeric,
  Colonised_numeric,
  (LSM24[["mkl"]][["receiver"]]),
  (LSM24[["mkl"]][["sender"]]))), 
  upper = 'number',
  lower = "circle",
  tl.pos = "lt",
  tl.col = "black",
  tl.cex = 0.8,
  addCoefasPercent = TRUE,
  number.cex=0.8)

# =========================================================

key.creator <- function(columns) {
  key <- paste(sort(columns), collapse = "")
  return(key)
}


DyadAttributes_Copy <- DyadAttributes[,names(DyadAttributes) %in% c("Origin","Destination",
                                                                    "avg_distance_km")]
DyadAttributes_Copy$Key = apply(DyadAttributes_Copy[, c("Origin", "Destination")], 
                                1, FUN = key.creator)
DyadAttributes_Copy <- DyadAttributes_Copy %>% distinct(Key, .keep_all = TRUE)
DyadAttributes_Copy <- DyadAttributes_Copy[,names(DyadAttributes_Copy) %in% c("Key","avg_distance_km")]







latent_dist <- as.matrix(dist(LSM10[["mkl"]][["Z"]]))
rownames(latent_dist) <- VertexName
colnames(latent_dist) <- VertexName
LatentDist_Tidy <- AdjMat_To_TidyDataDf(latent_dist)

# Clean out duplicates by creating key independent of direction
LatentDist_Tidy_Copy <- LatentDist_Tidy
LatentDist_Tidy_Copy$Key = apply(LatentDist_Tidy_Copy[, c("Origin", "Destination")], 
                          1, FUN = key.creator)
LatentDist_Tidy_Copy <- LatentDist_Tidy_Copy %>% distinct(Key, .keep_all = TRUE)
LatentDist_Tidy_Copy <- LatentDist_Tidy_Copy[,names(LatentDist_Tidy_Copy) %in% c("Key","Value")]

GeoLatentDist_Corr <- merge(DyadAttributes_Copy, LatentDist_Tidy_Copy, by = "Key", all.x = TRUE)

cor(GeoLatentDist_Corr[,names(GeoLatentDist_Corr) %in% c("avg_distance_km","Value")])

corrplot.mixed(cor(GeoLatentDist_Corr[,names(GeoLatentDist_Corr) %in% c("avg_distance_km","Value")]), 
  upper = 'number',
  lower = "circle",
  tl.pos = "lt",
  tl.col = "black",
  tl.cex = 0.8,
  addCoefasPercent = TRUE,
  number.cex=0.8)


# Verkar inte finnas någon koppling mellan de latenta positionerna och GeoDist


plot(
  (GeoLatentDist_Corr[,names(GeoLatentDist_Corr) %in% c("avg_distance_km","Value")]),
  pch = 19, col = rgb(0,0,0,0.3),
  xlab = "Geografiskt avstånd",
  ylab = "Latent distans",
  main = "Samband mellan latent distans och geografiskt avstånd"
)

abline(lm(GeoLatentDist_Corr$avg_distance_km ~ GeoLatentDist_Corr$Value), col = "red", lwd = 2)


# =========================================================







