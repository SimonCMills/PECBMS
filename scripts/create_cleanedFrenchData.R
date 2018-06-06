## Tidy up French datasets and save to ../counts/baseDatasets/
## Overview:
##    - firstly read in the file and "zero fill" to create 0 counts in years at
##    sites in which species have been observed, but a record not been created. 
##    - merge Euring information into this, using the French code. 
##  Notes:
##      (1) Minor issue with species codes and lack of correspondence- see line 45 
##      onwards. 

# housekeeping
require(data.table); require(xlsx); require(dplyr)

# read in dataset (provided as single file with all species, but with 0 counts 
# removed). As per advice from coordinators "zero fill" sites.
frenchDat <- fread("recievedFiles/France/stoc_1989_2015.csv")
frenchDat[,c("Ordre", "Famille", "NomF", "NomS", "XclimLamII", "YclimLamII") := NULL]

# zero filling sites. First create skeleton of all site:year combinations for all 
# species that have been recorded at a given site
combn_siteSpecies <- unique(frenchDat[,list(SITE, ESPECE)])
combn_siteYear <- unique(frenchDat[,list(SITE, ANNEE)])

# merge these together to create skeleton
frenchDat_full <- merge(combn_siteYear, combn_siteSpecies,
                        by="SITE", allow.cartesian=TRUE)

# now merge actual data into this, leaving NAs where species counts haven't been 
# recorded in a given year
frenchDat_full <- merge(frenchDat_full, frenchDat[,list(SITE,ANNEE, ESPECE, N)],
                        by=c("SITE", "ANNEE", "ESPECE"), all.x=T)
# set these to 0
frenchDat_full[is.na(N), N := 0]

## now add Euring information to this full dataframe
# First, read in key translating french codes to Euring number, and tidy up
frenchSpecies <- read.xlsx2("recievedFiles/France/FrenchNames_EUcodes.xlsx", 1)
frenchSpecies <- data.table(frenchSpecies[complete.cases(frenchSpecies),])
frenchSpecies <- frenchSpecies[Euring.Code != "",]
frenchSpecies <- frenchSpecies[,list(ESPECE = as.character(French.Code), Euring = Euring.Code)]

# Urgh, this results in loss of a minority of rows. 
finalDF <- merge(frenchDat_full, frenchSpecies, by="ESPECE") %>%
    .[,list(Euring, site = SITE, year = ANNEE, count_t = N)]

# Clearly some species present in datasets that aren't present in the species key. 
# Typically, these seem to be species with a very limited number of records. For example, 
# black-headed bunting, "EMBMEL" or pine bunting, "EMBLEU". These make up the vast majority
# of records (see lostRecords_n), but there are some notable exceptions, e.g. "SPESPE", 
# which makes up half the lost datapoints (n=997), but doesn't obviously correspond
# to any genus or species.
#
# Best guess is that SPESPE translates to SPECIES SPECIES and means unidentified, 
# but I can't understand why this would be recorded. Logic being that PASSSPE (n=303) doesn't 
# correspond to any European PAS- genus and PARSPE (n=241) doesn't correspond to any
# European PAR- genus. 
missingSpecies <- frenchDat_full[!ESPECE %in% unique(frenchSpecies$ESPECE),unique(ESPECE)]
lostRecords <- frenchDat_full[ESPECE %in% missingSpecies & N != 0, ]
setkey(lostRecords, ESPECE)
lostRecords_n <- lostRecords[,.N, by=ESPECE]
setkey(lostRecords_n, N)

# regardless of root cause, this affects a tiny minority of datapoints (~.5%), and 
# am just going to remove. 
nrow(lostRecords)/nrow(frenchDat)

# format to correct type
finalDF[,Euring := as.character(Euring)]
finalDF[,site := as.character(site)]

saveRDS(finalDF, "../counts/dataset_France.rds")