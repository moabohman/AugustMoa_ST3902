# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
library(esquisse)
library(fBasics)
library(interp)

# ============== - Input Variables - =======================

# source("DataRetrieval.R")
VerAtt_Vis <- VertexAttributes_Raw
DyaAtt_Vis <- DyadAttributes_Raw


# ========== - Visualize vertex attributes - ================

summary(VerAtt_Vis)


# -----------
# --- GDP ---
# -----------

hist(VerAtt_Vis$GDP)
# Scew (Big numbers!)

# Transform to logarithm and re-visualize
VerAtt_Vis <- VerAtt_Vis %>% 
  mutate(ln_GDP = log(GDP))
hist(VerAtt_Vis$ln_GDP)
# Much better! Near normal as well!

# Formal tests
pchiTest(VerAtt_Vis$ln_GDP) # - Not normal
pchiTest(na.exclude(VerAtt_Vis$ln_GDP)) # Still not normal, but closer: P-value 0.04905
jbTest(na.exclude(VerAtt_Vis$ln_GDP)) # - Normal
shapiro.test(VerAtt_Vis$ln_GDP) # - Normal


# ----------------
# --- Language ---
# ----------------

sort(table(VerAtt_Vis$Language), decreasing = TRUE)

# Create language groups after imputation!

# -----------------
# --- Coloniser ---
# -----------------

table(VerAtt_Vis$Coloniser)


# -----------------
# --- Colonised ---
# -----------------

table(VerAtt_Vis$Colonised)


# ------------
# --- Area ---
# ------------

hist(VerAtt_Vis$Area)
# Scew!

# Transform and re-visualize
  # Trials
    # hist(log(VerAtt_Vis$Area))
    # hist((VerAtt_Vis$Area)^(1/9))


VerAtt_Vis <- VerAtt_Vis %>% 
  mutate(Transformed_Area = (Area)^(1/9))

hist(VerAtt_Vis$Transformed_Area)
# Better! (Could use bestNormalized, but leads to diminishing improvement)
# (Could we, with missing data? Maybe transcan() or aregImpute() 
# of the Hmisc package in R? (vanBuuren, pp. 116)

# Formal tests
pchiTest(VerAtt_Vis$Transformed_Area) # - Not normal
pchiTest(na.exclude(VerAtt_Vis$Transformed_Area)) # Not adjusted is barely signifcant: P-value 0.05741
jbTest(na.exclude(VerAtt_Vis$Transformed_Area)) # - Normal
shapiro.test(VerAtt_Vis$Transformed_Area) # - Not normal

# -------------------
# --- NonViolence ---
# -------------------

hist(VerAtt_Vis$NonViolence)
# Scew. Not normal.

# --------------------
# --- NetMigration ---
# --------------------

hist(VerAtt_Vis$NetMigration)
# 


hist((VerAtt_Vis$NetMigration)^(1/5))

# Transform and re-visualize
VerAtt_Vis <- VerAtt_Vis %>% 
  mutate(Trans_NetMigration = (NetMigration)^(1/5))
hist(VerAtt_Vis$Trans_NetMigration)
# Much better! Near normal as well!

# Formal tests
pchiTest(VerAtt_Vis$Trans_NetMigration) # - Not normal
pchiTest(na.exclude(VerAtt_Vis$Trans_NetMigration)) # Normal
jbTest(na.exclude(VerAtt_Vis$Trans_NetMigration)) # - Normal
shapiro.test(VerAtt_Vis$Trans_NetMigration) # - Normal



# --- Keep transfomed variables and clean out original ones ---

VerAtt_Vis <- VerAtt_Vis[,!names(VerAtt_Vis) %in% c("GDP","Area", "NetMigration")]


# Final visualization



# esquisser(VerAtt_Vis, viewer = "browser")
ggplot(VerAtt_Vis) +
  aes(x = Transformed_Area, y = ln_GDP, colour = Coloniser) +
  geom_point(size = 2L) +
  scale_color_hue(direction = 1) +
  theme_minimal()
# Obvious relationship between GDP and Area
# Interesting to see how Colonisers have a higer intercept in general



# ========== - Visualize dyad attributes - ================

summary(DyaAtt_Vis)

# -----------------------
# --- avg_distance_km ---
# -----------------------

hist(DyaAtt_Vis$avg_distance_km)
summary(DyaAtt_Vis$avg_distance_km)
# Scew (Big numbers!)

# Transform and re-visualize
# DyaAtt_Vis <- DyaAtt_Vis %>%
#   mutate(Transformed_GeoDist = (avg_distance_km)^(2/3))
# hist((DyaAtt_Vis$avg_distance_km)^(2/3))


# -------------------------
# --- avg_distance_norm ---
# -------------------------

hist(DyaAtt_Vis$avg_distance_norm)

# -----------------
# --- border ---
# -----------------

table(DyaAtt_Vis$border)


# ------------------------
# --- reldist_weighted ---
# ------------------------

hist(DyaAtt_Vis$reldist_weighted)




# --- Keep transfomed variables and clean out original ones ---

# DyaAtt_Vis <- DyaAtt_Vis[,!names(DyaAtt_Vis) %in% c("avg_distance_km","Area")]




# ============== - Output Variables - =======================

VertexAttributes_Trans <- VerAtt_Vis
DyadAttributes_Trans <- DyaAtt_Vis

# Variables cleaning - Comment/Uncomment at will
# rm()


# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Re-cathegorise Language into fewer cathegories
#     (maybe keep original as well) - DONE
# - Do dyad attributes need to get transformed for any reason?

