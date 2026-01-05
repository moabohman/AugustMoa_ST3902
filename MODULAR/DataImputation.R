##################################################
#                                                #
#                 DataImputation                 #
#                                                #
##################################################


# ============== - Input Variables - =======================

VertexAttributes_Imp <- VertexAttributes_Trans

# =========================================================
# =========================================================
# =========== - NA handling / Imputation - ================
# =========================================================
# =========================================================

### Due to scewed data, transformation of variables were made
### before imputation (vanBuuren, pp. 74-75, 116) - 
### 
### "Vroomen et al.(2016) investigated imputation of cost data, 
### and found that predictive mean matching of the log-transformed 
### outperformed plain predictive mean match-ing, a two-step method 
### and complete-case analysis, and hence recommend log-transformed 
### method for monetary data." 
### (vanBuuren, pp. 93) - Gäller detta bara Semi-cont. data??
### 
### "Thus, transformation toward normality and back-transformation 
### into the original scale improves statistical inference." 
### vanBuuren, pp. 146

# ======================= - LANGUAGE - =======================

# View(VertexAttributes_Imp[is.na(VertexAttributes_Imp$Language),])

# Manually adding languages
VertexAttributes_Imp <- VertexAttributes_Imp %>%
  mutate(
    Language = case_when(
      VertexName == "COD" ~ "French",
      VertexName == "LIE" ~ "German",
      VertexName == "MCO" ~ "French",
      VertexName == "MNE" ~ "Montenegrin",
      VertexName == "PSE" ~ "Arabic",
      VertexName == "ROU" ~ "Romanian",
      VertexName == "SRB" ~ "Serbian",
      VertexName == "SSD" ~ "English",
      VertexName == "TLS" ~ "Tetum",
      VertexName == "VAT" ~ "Italian",
      VertexName == "XKX" ~ "Albanian",
      TRUE ~ Language
    )
  )

# =========================================================
# ======================= - GDP - =======================

# Manual for some special cases
# https://grokipedia.com/page/List_of_sovereign_states_in_Europe_by_GDP_(nominal)
# Visserligen värden för 2025 medan övriga är från 2024 (om vi inte ändrar detta till nåt ännu värre...)
VertexAttributes_Imp <- VertexAttributes_Imp %>%
  mutate(
    ln_GDP = case_when(
      VertexName == "LIE" ~ log(7*10^9),
      VertexName == "MCO" ~ log(9*10^9),
      VertexName == "SMR" ~ log(2*10^9),
      VertexName == "VAT" ~ log(0.002*10^9),
      TRUE ~ ln_GDP
    )
  )


# =========================================================
# ============= - MICE Imputation - =======================


# Kör en tom först
empty_mice <- mice(VertexAttributes_Imp, maxit = 0,
                   printFlag = FALSE)

# Ändra så att vi inte predicerar med Language (>50 klasser)
meth <- empty_mice$method
pred <- empty_mice$pred

# pred[,"Language"] <- 0
# KOMMENTERADE BORT DENNA (OVAN)


# pred[,"Language"] <- 1
# diag(pred) <- 0
# meth["Language"] <- "pmm"
# meth["Language"] <- ""

# Imputera värden med mice
imp_mice <- mice(VertexAttributes_Imp, m = 5, maxit = 5, 
                 method = meth, pred = pred, seed = 666,
                 printFlag = FALSE)

bootmean_ln_GDP <- rowMeans(imp_mice$imp$ln_GDP)
bootmean_Transformed_Area <- rowMeans(imp_mice$imp$Transformed_Area)
bootmean_NonViolence <- rowMeans(imp_mice$imp$NonViolence)
bootmean_Trans_NetMigration <- rowMeans(imp_mice$imp$Trans_NetMigration)

# VertexAttributes_Imp[names(bootmean_ln_GDP),"VertexName"]
# exp(bootmean_ln_GDP)
# VertexAttributes_Imp[names(bootmean_Transformed_Area),"VertexName"]
# (bootmean_Transformed_Area)^9

VertexAttributes_Complete <- VertexAttributes_Imp

VertexAttributes_Complete[names(bootmean_ln_GDP),"ln_GDP"] <- bootmean_ln_GDP
VertexAttributes_Complete[names(bootmean_Transformed_Area),"Transformed_Area"] <- bootmean_Transformed_Area
VertexAttributes_Complete[names(bootmean_NonViolence),"NonViolence"] <- bootmean_NonViolence
VertexAttributes_Complete[names(bootmean_Trans_NetMigration),"Trans_NetMigration"] <- bootmean_Trans_NetMigration


# ====== - Exploration of complete data  - ================

summary(VerAtt_Vis)
summary(VertexAttributes_Complete)


# -----------
# --- GDP ---
# -----------

hist(VerAtt_Vis$ln_GDP)
hist(VertexAttributes_Complete$ln_GDP)
# Complete hist DO look a little skewed to the left - But more even distributed!
# Manually imputed values probably affects the distribution substantially.

# ------------
# --- Area ---
# ------------

hist(VerAtt_Vis$Transformed_Area)
hist(VertexAttributes_Complete$Transformed_Area)
# Rather equal


# -------------------
# --- NonViolence ---
# -------------------

hist(VerAtt_Vis$NonViolence)
hist(VertexAttributes_Complete$NonViolence)
# Rather equal

# --------------------------
# --- Trans_NetMigration ---
# --------------------------

hist(VerAtt_Vis$Trans_NetMigration)
hist(VertexAttributes_Complete$Trans_NetMigration)
# More scewed...


# ================================================================
# --- Create language groups to decrease number of cathegories ---
# ================================================================


# sort(table(VertexAttributes_Complete$Language), decreasing = TRUE)
CommonLanguages <- table(VertexAttributes_Complete$Language)[table(VertexAttributes_Complete$Language)>2]
VertexAttributes_Complete$LangGroup <- VertexAttributes_Complete$Language
VertexAttributes_Complete <- VertexAttributes_Complete %>% relocate(LangGroup, .after = Language)
VertexAttributes_Complete$LangGroup[!(VertexAttributes_Complete$Language %in% names(CommonLanguages))] <- "Other" 

# =========================================================
# =========================================================
# =========================================================
# =========================================================
# =========================================================

### Save data
write.csv(VertexAttributes_Complete,file='VertexAttributes_Complete.csv')


# ============== - Output Variables - =======================

AdjMat <- AdjMat
VertexAttributes <- VertexAttributes_Complete
DyadAttributes <- DyadAttributes_Trans

# Variables cleaning - Comment/Uncomment at will
rm(empty_mice, VertexAttributes_Imp, bootmean_ln_GDP, bootmean_Transformed_Area,
   bootmean_NonViolence, bootmean_Trans_NetMigration, pred, meth, CommonLanguages)

save(AdjMat, file = "AdjMat.RData")
save(VertexAttributes, file = "VertexAttributes.RData")
save(DyadAttributes, file = "DyadAttributes.RData")


# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Kan vi använda norm istället för pmm? Eller norm.boot?