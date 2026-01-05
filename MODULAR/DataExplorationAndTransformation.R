##################################################
#                                                #
#        DataExplorationAndTransformation        #
#                                                #
##################################################


# ============== - Input Variables - =======================

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
# Better!



# -------------------
# --- NonViolence ---
# -------------------

hist(VerAtt_Vis$NonViolence)
# Scew. Not normal.

# --------------------
# --- NetMigration ---
# --------------------

hist(VerAtt_Vis$NetMigration)


# Transform and re-visualize
VerAtt_Vis <- VerAtt_Vis %>% 
  mutate(Trans_NetMigration = (NetMigration)^(1/5))
hist(VerAtt_Vis$Trans_NetMigration)


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



# ============== - Output Variables - =======================

VertexAttributes_Trans <- VerAtt_Vis
DyadAttributes_Trans <- DyaAtt_Vis

# Variables cleaning - Comment/Uncomment at will
# rm()


# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~



