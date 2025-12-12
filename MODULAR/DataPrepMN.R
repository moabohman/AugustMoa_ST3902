# getwd()
# setwd("~/GitHub/BachelorThesis")
# setwd("~/GitHub/BachelorThesis/MoasLabbar")
# setwd("~/GitHub/BachelorThesis/MODULAR")

# Libraries

library(tidyverse)
# names(TidyData)


# ============== - Input Variables - =======================

TidyData <- TidyData


# =============== - Make data multinomial - =======================
# With 4 possible outcomes: {(0,0),(0,1),(1,0),(1,1)}
# 

# Create new dataframe with only relevant columns
TidyData_opposite <- TidyData[,names(TidyData) %in% c("Origin","Destination","Visafree")]


# --- Extend data with information on the reciprocal ---
TidyData_MN <- merge(TidyData, TidyData_opposite,
                       by.x = c("Origin","Destination"), 
                       by.y = c("Destination","Origin"), all.x = TRUE)

# Clean out duplicates by creating key independent of direction
key.creator <- function(columns) {
  key <- paste(sort(columns), collapse = "")
  return(key)
}

TidyData_MN$Key = apply(TidyData_MN[, c("Origin", "Destination")], 
                          1, FUN = key.creator)
TidyData_MN <- TidyData_MN %>% distinct(Key, .keep_all = TRUE)
# TidyData_MN <- TidyData_MN %>% relocate(Key)


# Create new id with a key that is _dependent_ of direction
dyad.creator <- function(columns) {
  dyad <- paste(columns, collapse = "")
  return(dyad)
}

TidyData_MN$CountryDyad = apply(TidyData_MN[, c("Origin", "Destination")], 
                        1, FUN = dyad.creator)
TidyData_MN <- TidyData_MN %>% relocate(CountryDyad)


# Find multinomial outcomes
dyad_outcome.creator <- function(columns) {
  dyad_outcome <- paste(columns, collapse = "")
  return(dyad_outcome)
}

TidyData_MN$VisafreeDyadic = apply(TidyData_MN[, c("Visafree.x", "Visafree.y")], 
                                1, FUN = dyad_outcome.creator)
TidyData_MN <- TidyData_MN %>% relocate(VisafreeDyadic, .after = CountryDyad)
TidyData_MN$VisafreeDyadic <- as.factor(TidyData_MN$VisafreeDyadic)
# table(TidyData_MN$VisafreeDyadic)


# Clean out redundant columns
TidyData_MN <- TidyData_MN[,!names(TidyData_MN) %in% c("Key","Origin","Destination",
                                                       "Visafree.x","Visafree.y")]





# ============== - Output Variables - =======================

TidyData_MN <- TidyData_MN

# # Variables cleaning - Comment/Uncomment at will
rm(TidyData_opposite, key.creator, dyad.creator, dyad_outcome.creator)




# ~=~=~=~=~=~=~=~=~ Unfinished in this script: ~=~=~=~=~=~=~=~=~

# - Future changes in LR-version (in case this one actually survives)
# And also, as for LR:
# - The data is unbalanced! Do wee need do handle that?
# - Re-do analysis with comparison between [more important for MN?!]
#       - GDP
#       - Area
#       - "SameLanguage" (till exempel?)
#     - Also Language_Grouped (perhaps from earlier?)






