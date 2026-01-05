##################################################
#                                                #
#                 DataRetrieval                  #
#                                                #
##################################################


# ============== - Main variable/The Adjacency Matrix - ==============


passportISO3 <- read.csv(
  url("https://raw.githubusercontent.com/ilyankou/passport-index-dataset/master/passport-index-matrix-iso3.csv"))

# Transfer nodenames to rownames instead of first column
rownames(passportISO3) <- passportISO3$Passport
passportISO3 <- passportISO3[,-1]

# Sort rows and columns to match each other
passportISO3 <-  passportISO3[order(row.names(passportISO3)),]
passportISO3 <-  passportISO3[,order(names(passportISO3))]


# Confirm adjmat (symmetry)
boolsum <- 0
for (i in 1:199) {
  boolsum <- boolsum + (passportISO3[i,i] == -1)
}

# Dichotomise values
PassportMat <- as.matrix(passportISO3)
AdjMat <- passportISO3
AdjMat[,] <- 0
AdjMat[(PassportMat == "visa free")] <- 1
AdjMat[(PassportMat == "visa on arrival")] <- 1

AdjMat <- as.matrix(AdjMat)

# Extract vertex name (Country ISO-3)
VertexName <- row.names(AdjMat)

# Also create a list with real names for future analysis
ISO3_Countrynames_Converter <- read_xlsx("DataRetrieval/ISO3_Countrynames_Converter.xlsx")
VertexName_CountryName <- merge(as.data.frame(VertexName), ISO3_Countrynames_Converter, 
                          by.x = "VertexName", 
                          by.y = "ISO3", all.x = TRUE)
Vertex_RealNames <- VertexName_CountryName[,2]
rm(ISO3_Countrynames_Converter)

 
# ======================= - Vertex attribute - =======================


### GDP


# GDP_WBG <- read_excel("~/BachelorThesis/MoasLabbar/DataRetrieval/GDP_WBG.xlsx")

GDP_WBG <- read_excel("DataRetrieval/GDP_WBG.xlsx",
                      col_types = c("skip", "text", "text",
                                    "skip", rep("skip", 50), # skip 1960-2009
                                    rep("numeric", 15)))
# Only keep counties which represent vertices
GDP_WBG_selected <- merge(as.data.frame(VertexName), GDP_WBG, 
                          by.x = "VertexName", 
                          by.y = "Country Code", all.x = TRUE)

# select a year for our analysis!
# --- UNCOMMENT BELOW TO SEE CHOICES ---
# summary(GDP_WBG_selected)

# GDP_WBG_selected[is.na(GDP_WBG_selected$`2024`),]

# --- CHOOSE YEAR IN FILTERING BELOW --- 
GDP_final <- GDP_WBG_selected[,names(GDP_WBG_selected) %in% c("VertexName","2024")]
colnames(GDP_final) <- c("VertexName","GDP")

# ----------

### CEPII - Language and more

geo_cepii <- read_excel("DataRetrieval/geo_cepii.xls",
                        col_types = c("skip", "text", "skip",
                                      "skip", "skip", "numeric", "numeric",
                                      "skip", "text", "skip", "skip", "numeric",
                                      "numeric", "skip", "skip", "skip",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "text", "text", "text", "text", "text",
                                      "skip", "skip", "skip"), na = ".")

# geo_cepii <- read_excel("~/BachelorThesis/MoasLabbar/DataRetrieval/geo_cepii.xls",
#                         col_types = c("skip", "text", "skip",
#                                       "skip", "skip", "numeric", "numeric",
#                                       "skip", "text", "skip", "skip", "numeric",
#                                       "numeric", "skip", "skip", "skip",
#                                       "text", "text", "text", "text", "text",
#                                       "text", "text", "text", "text", "text",
#                                       "text", "text", "text", "text", "text",
#                                       "skip", "skip", "skip"), na = ".")

# Only keep counties which represent vertices
geo_cepii_selected <- merge(as.data.frame(VertexName), geo_cepii, 
                          by.x = "VertexName", 
                          by.y = "iso3", all.x = TRUE)




Language_final <- geo_cepii_selected[,names(geo_cepii_selected) %in% c("VertexName","langoff_1")]
colnames(Language_final) <- c("VertexName","Language")
Language_final <- distinct(Language_final)


# ----------


### Coloniser
Colonisers <- na.omit(unique(c(geo_cepii$colonizer1,geo_cepii$colonizer2,geo_cepii$colonizer3,geo_cepii$colonizer4)))
Coloniser <- as.data.frame(cbind(VertexName, rep(0, length(VertexName))))
colnames(Coloniser) <- c("VertexName","Coloniser")
Coloniser[Coloniser$VertexName %in% Colonisers,]$Coloniser <- 1
Coloniser$Coloniser <- as.factor(Coloniser$Coloniser)
Coloniser_final <- Coloniser

### Colonised 
ColonisingInfo <- geo_cepii_selected[,names(geo_cepii_selected) %in% c("VertexName","colonizer1","colonizer2","colonizer3","colonizer4")]
Colonised <- merge(as.data.frame(VertexName), ColonisingInfo, 
                   by.x = "VertexName", 
                   by.y = "VertexName", all.x = TRUE)
Colonised$Colonised <- 1
Colonised[is.na(Colonised$colonizer1),]$Colonised <- 0
Colonised$Colonised <- as.factor(Colonised$Colonised)
Colonised_final <- Colonised[,names(Colonised) %in% c("VertexName","Colonised")]
Colonised_final <- distinct(Colonised_final)
# ----------

### Size measures

CountrySize_final <- geo_cepii_selected[,names(geo_cepii_selected) %in% c("VertexName","area")]
colnames(CountrySize_final) <- c("VertexName","Area")
CountrySize_final <- distinct(CountrySize_final)
# ----------

### QoG - TerrorismIndex and more

qog_std_cs_jan25 <- read_excel("DataRetrieval/qog_std_cs_jan25.xlsx")
# colSums(is.na(qog_std_cs_jan25[,colSums(is.na(qog_std_cs_jan25))==1]))
# colSums(is.na(qog_std_cs_jan25[,"gti_gti"]))
# gpi_conf

QoG_selected <- qog_std_cs_jan25[,names(qog_std_cs_jan25) %in%
                                   c("ccodealp",
                                    # "gti_gti", # Starkt korrelerad med wbgi_pve (-62)
                                    # "bmr_dem", # Starkt korrelerad med wbgi_pve (53)
                                     "wbgi_pve", # Mindre bortfall än gti_gti!
                                     "wdi_migration"
                                   )]

QoG_selected$wbgi_pve <- as.numeric(QoG_selected$wbgi_pve)
QoG_selected$wdi_migration <- as.numeric(QoG_selected$wdi_migration)

QoG_selected <- merge(as.data.frame(VertexName), QoG_selected, 
                      by.x = "VertexName", 
                      by.y = "ccodealp", all.x = TRUE)



# QoG_selected$gti_gti <- as.numeric(QoG_selected$gti_gti)
# QoG_selected$bmr_dem <- as.numeric(QoG_selected$bmr_dem)
# corrplot.mixed(cor(na.omit(QoG_selected[,!names(QoG_selected) %in% c("VertexName")])), 
#                upper = 'number',
#                lower = "circle",
#                tl.pos = "lt",
#                tl.col = "black",
#                tl.cex = 0.8,
#                addCoefasPercent = TRUE,
#                number.cex=0.8)

# Även wbgi_pve och wdi_migration är korrelerade (25)

colnames(QoG_selected) <- c("VertexName","NonViolence","NetMigration")
colSums(is.na(QoG_selected[,colSums(is.na(QoG_selected))>0]))


# ======================= - Dyad attribute - =======================


# ----------


### GeoDistance

# geodist_PSW24 <- read_dta("~/BachelorThesis/MoasLabbar/DataRetrieval/geodist_PSW24.dta")
geodist_PSW24 <- read_dta("DataRetrieval/geodist_PSW24.dta")
# GeoDistance_final <- geodist_PSW24

geodist_PSW24$border <- as.factor(geodist_PSW24$border)
GeoDistance_final <- geodist_PSW24[,names(geodist_PSW24) %in% c("countrycode_1",
                                                                "countrycode_2",
                                                                "avg_distance_km",
                                                                "avg_distance_norm",
                                                                "border")]


# write.csv(geodist_PSW24,file='geodist_PSW24.csv')
# write.csv(GeoDist,file='GeoDist.csv')
# ----------



### RelDistance
# religious_distance_PSW2024 <- read_dta("~/BachelorThesis/MoasLabbar/DataRetrieval/religious_distance_PSW2024.dta")
religious_distance_PSW2024 <- read_dta("DataRetrieval/religious_distance_PSW2024.dta")
RelDistance_final <- religious_distance_PSW2024[religious_distance_PSW2024$year == 2020,]

RelDistance_final <- RelDistance_final[,!names(RelDistance_final) %in% c("year")]


# write.csv(religious_distance_PSW2024,file='religious_distance_PSW2024.csv')
# ----------



# Create table for vertex attributes
VertexAttributes_Raw <- as.data.frame(VertexName)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, GDP_final, by = "VertexName", all.x = TRUE)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, Language_final, by = "VertexName", all.x = TRUE)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, Coloniser_final, by = "VertexName", all.x = TRUE)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, Colonised_final, by = "VertexName", all.x = TRUE)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, CountrySize_final, by = "VertexName", all.x = TRUE)
VertexAttributes_Raw <- merge(VertexAttributes_Raw, QoG_selected, by = "VertexName", all.x = TRUE)


# Create table for dyad attributes
DyadAttributes_Raw <- expand.grid(Origin = VertexName, Destination = VertexName, stringsAsFactors = FALSE)
DyadAttributes_Raw <- DyadAttributes_Raw[DyadAttributes_Raw$Origin != DyadAttributes_Raw$Destination, ]
# GeoDistance
DyadAttributes_Raw <- merge(DyadAttributes_Raw, GeoDistance_final, 
                        by.x = c("Origin","Destination"), 
                        by.y = c("countrycode_1","countrycode_2"), all.x = TRUE)

#RelDistance
DyadAttributes_Raw <- merge(DyadAttributes_Raw, RelDistance_final, 
                        by.x = c("Origin","Destination"), 
                        by.y = c("countrycode_1","countrycode_2"), all.x = TRUE)

DyadAttributes_Raw$reldist_weighted <- round(DyadAttributes_Raw$reldist_weighted, digits = 4)

# ============== - Output Variables - =======================

AdjMat <- AdjMat
VertexAttributes_Raw <- VertexAttributes_Raw
DyadAttributes_Raw <- DyadAttributes_Raw

# Variables cleaning - Comment/Uncomment at will
rm(Colonised, Coloniser, ColonisingInfo, GDP_WBG, GDP_WBG_selected, qog_std_cs_jan25,
   geo_cepii, geo_cepii_selected, geodist_PSW24, passportISO3, PassportMat, 
   religious_distance_PSW2024, boolsum, Colonisers, i)

rm(Colonised_final, Coloniser_final, CountrySize_final, GDP_final, QoG_selected,
   GeoDistance_final, Language_final, RelDistance_final)


# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Choosing year for GDP