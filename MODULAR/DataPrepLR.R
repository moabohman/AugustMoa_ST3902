# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)

# ============== - Input Variables - =======================

AdjMat <- AdjMat
VerAtt_LR <- VertexAttributes
DyaAtt_LR <- DyadAttributes


# ========== - Create data in tidy format - ================

# Make copy for 
Origin_VerAtt <- VerAtt_LR
Destination_VerAtt <- VerAtt_LR

# Rename columns for origin
OriginColnames <- colnames(Origin_VerAtt[1])
for (i in 2:length(Origin_VerAtt)) {
  OriginColnames <- cbind(OriginColnames, (paste(c("Origin", colnames(Origin_VerAtt[i])), collapse = "")))
  
}
colnames(Origin_VerAtt) <- OriginColnames
rm(OriginColnames)

# Rename columns for destination
DestinationColnames <- colnames(Destination_VerAtt[1])
for (i in 2:length(Destination_VerAtt)) {
  DestinationColnames <- cbind(DestinationColnames, (paste(c("Destination", colnames(Destination_VerAtt[i])), collapse = "")))
  
}
colnames(Destination_VerAtt) <- DestinationColnames
rm(DestinationColnames)

# Turn matrix into dataframe
TidyData <- data.frame(as.table(AdjMat))
colnames(TidyData) <- c("Origin", "Destination", "Visafree")

# Clean Origin == Destination
TidyData <- TidyData[!(TidyData$Origin==TidyData$Destination),]



# Merge
TidyData <- merge(TidyData, DyaAtt_LR, by = c("Origin", "Destination"), all.x = TRUE)
TidyData <- merge(TidyData, Origin_VerAtt, by.x = "Origin", by.y = "VertexName", all.x = TRUE)
TidyData <- merge(TidyData, Destination_VerAtt, by.x = "Destination", by.y = "VertexName", all.x = TRUE)
TidyData <- TidyData %>% relocate(Origin)

# View(TidyData)
# class_labels(TidyData)


# ============== - Output Variables - =======================

TidyData <- TidyData

# # Variables cleaning - Comment/Uncomment at will
rm(DyaAtt_LR, VerAtt_LR, Origin_VerAtt, Destination_VerAtt)




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Will we compare e.g. Area? Do we need to re-transform first in that case? 
#       YES (but could easily be done in formula, right?)
# - Comparison between 
#       - GDP
#       - Area
#       - "SameLanguage" (till exempel?)
# - Also Language_Grouped (perhaps from earlier?)







