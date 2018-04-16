## get species lists for each country and merge to get a complete euring:species id key
##
## Notes:
##  (1) Multiple Euring codes can point to the same species- species LB is the unique
##  identifier, and these should in almost all cases (bar Anas platyrhynchos, 
##  Anser anser, and Columba livia) correspond to a unique common name also. 
##
## Overview:
##      (1) first looks at PECBMS datasets, and then separately at the UK key and then 
##      the French Key, before resolving conflicts and merging both into single df
##      (2) There are some issues with mismatches in species LB between PECBMS and
##      UK data- UK species LB appears more up to date so have used these in the 10
##      instances this occurs
##      (3) Following merge with UK dataset, 36 species don't have a common name. Typically, 
##      though not entirely
##      these are species that don't occur in UK scheme. I faffed about with using
##      a separate species latin:common key to add these but it was taking too long
##      so just added entries manually. 
##      (4) Following merge with French dataset, 31 species don't have a common name. Again
##      faffed about with using
##      a separate species latin:common key to add these but it was taking too long
##      so just added entries manually. 
##      (4) finally, ran some checks that the dataframe looked as it should- where
##      issues were flagged have edited accordingly
##      (5) saved to "../cleanedFiles/dataKey_Euring-species.rds"
##

## housekeeping
rm(list=ls()); graphics.off()
require(data.table); require(xlsx); require(dplyr)

## Read in PECBMS species lists and format to create a base species ID key 
species_id <- list.files("../recievedFiles/PECBMS_originalFiles/speciesLists/", 
                         full.names=T)
species_idList <- lapply(species_id, read.xlsx2, sheetIndex=1)

# what names crop up here?
# lapply(species_idList, names)

# get the various variants of Euring code and species names that crop up in PECBMS
# datasets and place single dataframe
generalCols <- c("Code","Euring", "Euring.Code", "Species")
species_cleanList <- vector("list", length(species_idList))

for(i in seq(species_idList)) {
    print(i)
    nCols_matched <- sum(names(species_idList[[i]]) %in% generalCols)
    species_cleanList[[i]] <- if(nCols_matched == 2) {
        species_idList[[i]][,names(species_idList[[i]]) %in% generalCols]
    }
}
speciesKey <- unique(rbindlist(species_cleanList))
speciesKey[,Euring := as.character(Euring)]
speciesKey[,Species := as.character(Species)]

# diagnostic check: are there any duplicated Eurings here (i.e. single ring pointing 
# to multiple species)
any(duplicated(speciesKey$Euring)) # No


################################################################################
## Now look at UK species List
## UK datasets were provided with separate key; read this in, and merge into full 
## dataframe. Check for congruence of species names etc.
speciesList_UK <- fread("../recievedFiles/UK/btolist.csv")
setnames(speciesList_UK, "Euring_code", "Euring")
speciesList_UK[,Euring := as.character(Euring)]
# merge to full df
speciesKey_incUK <- merge(speciesKey, speciesList_UK, "Euring", all=TRUE)
# hmm, UK dataset overall has more species, but some species are not present that 
# are only in Europe

# check for congruence of names
speciesKey_incUK[, list(Species[Species != Scientific_name],Scientific_name[Species != Scientific_name])] %>% 
    .[complete.cases(.),]
# Disagreement over latin binomial for 10 species. UK version appears to be more up 
# to date, so will use these, apart from where they don't exist in UK scheme
speciesKey_incUK[,species_new := Scientific_name]
speciesKey_incUK[is.na(Scientific_name), species_new := Species]
# speciesKey_incUK[is.na(species_new),]  ## all Eurings now have a corresponding latin binomial

# 36 species are missing a common name
speciesKey_incUK[is.na(English_name),]

## right, so this is further complicated by the fact that at least some of the Euring codes 
## in the UK dataset don't match with those in the PECBMS (i.e. single species links 
## to multiple Euring)
##
## case in point: mealy redpoll which is 16633 in UK dataset and 16630 in PECBMS. 
## On the website (http://blx1.bto.org/euringcodes/species.jsp) is listed as 16630
## Implication is that the Euring merge won't be correct
speciesList_UK[, Species := Scientific_name]
speciesKey_checkEURING <- merge(speciesKey, speciesList_UK, "Species", all=TRUE)
speciesKey_checkEURING[Euring.x != Euring.y,]
# okay, so actually mealy redpoll is the only species this applies to. Mallard has 
# a separate entry for 'domesticated' in UK dataset, but exists also under Mallard 

## I'm just going to do add these manually- has taken too much faffing about trying to 
## find a further species list and merging these in 
newCommonNames <- c("Tawny Pipit", "Thrush Nightingale", "Bluethroat", "Black-eared Wheatear", 
                    "River Warbler", "Great Reed Warbler", "Icterine Warbler", "Melodius Warbler",
                    "Subalpine Warbler", "Sardinian Warbler", "Orphean Warbler", "Barred Warbler",
                    "Western Bonelli's Warbler", "Collared Flycatcher", "Azure-winged Magpie",
                    "Spotted Nutcracker", "Spotless Starling", "Rock Sparrow", "Citril Finch",
                    "Common Redpoll", "Rock Bunting", "Ortolan Bunting", "Rustic Bunting", "Hazel Grouse",
                    "Little Bustard", "Great Spotted Cuckoo", "Bee Eater", "Grey-headed Woodpecker", 
                    "Black Woodpecker", "Middle Spotted Woodpecker", "Calandra Lark", "Greater Short-toed Lark",
                    "Crested Lark", "Thekla Lark", "Crag Martin", "Red-rumped Swallow")


speciesKey_incUK[is.na(English_name), english_new := newCommonNames]
speciesKey_incUK[!is.na(English_name), english_new := English_name]

## diagnostic check 1: manually added species look good to me
speciesKey_incUK[is.na(English_name),]

## diagnostic check 2: check for no duplicates in common name 
speciesKey_incUK[duplicated(english_new) | duplicated(english_new, fromLast=T),]
# in UK scheme Carduelis flammea is given a different Euring (as noted above)- 
# this is fine though, because will still associate with correct species (LB & English)
# info. 
# 
# Bluethroat species_new needs altering, so the Euring numbers will correspond to the
# same species (..that they both designate)
speciesKey_incUK[Euring == 11061, species_new := "Luscinia svecica"]

## diagnostic check 3: check for no duplicates in latin name 
speciesKey_incUK[duplicated(species_new) | duplicated(species_new, fromLast=T),]
# It doesn't matter if multiple Euring codes point to the same species, so this seems
# fine. Note that both Greylag and Mallard have 'domestic' versions, inherited from 
# UK data though.. 
speciesKey_updated <- speciesKey_incUK[,list(Euring, species_LB = species_new, species = english_new)]

################################################################################
## Now need to repeat the process, but for French data.  
speciesList_France <- read.xlsx2("../recievedFiles/France/FrenchNames_EUcodes.xlsx", 1) %>%
    data.table(.) %>%
    .[!Euring.Code =="",] 
setnames(speciesList_France, "Euring.Code", "Euring")
setnames(speciesList_France, "Species", "species_LB")
speciesList_France[, species_LB := gsub(" \\(.*\\)| L\\.", "", species_LB),]

# first check that all Eurings are unique -- explain this more.. 
all <- merge(speciesList_France, speciesKey_updated, by="Euring", all = TRUE) 
# there are no euring duplicates so all added species are either novel or referred
# to with a separate Euring in the other datasets
any(duplicated(all$Euring))

# first merge in finalKey to add common names to as many species as possible (i.e. those that
# exist already in dataset, but are referred to by different Euring)
newDF <- merge(speciesList_France, speciesKey_updated[,list(species, species_LB)], by="species_LB", all.x=TRUE)

# now merge in to finalKey
speciesKey_full <- merge(newDF, speciesKey_updated, by="Euring", all=TRUE, suffixes = c("_French", "_Full"))

# these are the actual new species (n=30)
speciesKey_full[is.na(species_Full),]
frenchAdditions <- speciesKey_full[is.na(species_Full),]

# after further screwing about, just going to enter manually.. 
# Note: checked as added them and they look good. 
newFrenchSp <- c("Water Pipit", "Yellow-crowned Wagtail", "Black-crowned Night Heron", 
                 "Alpine Accentor", "Common Rock Thrush", "Blue Rock Thrush", "Great Egret", 
                 "Aquatic Warbler", "Spectacled Warbler", "Dartford Warbler", "Black Stork",
                 "African Sacred Ibis", "American Flamingo", "Alpine Chough", 
                 "Carrion x Hooded Crow", "Egyptian Vulture", "Griffon Vulture", 
                 "Short-toed Eagle", "Lesser Kestrel", "Red-footed Falcon", "Southern Grey Shrike",
                 "Rock Partridge", "Numenius sp.", "Gull-billed Tern", "Whiskered Tern",
                 "Scops Owl", "Pallid Swift", "Alpine Swift", "Roller", "Little Bittern")

# finalKey[grepl("swift", ignore.case = T, species),]
# speciesList_UK[grepl("swift", ignore.case = T, English_name),]
# speciesKey[grepl("apus", ignore.case = T, Species),]

# update the species info
speciesKey_full[is.na(species_Full), species_LB_Full := species_LB_French]
speciesKey_full[is.na(species_Full), species_Full := newFrenchSp]


## diagnostic check 2: check for no duplicates in common name 
speciesKey_full[duplicated(species_Full) | duplicated(species_Full, fromLast=T),]
# in UK scheme Carduelis flammea is given a different Euring (as noted above)- 
# this is fine though, because will still associate with correct species (LB & English)
# info. 
# 
# Bluethroat species_new needs altering, so the Euring numbers will correspond to the
# same species (..that they both designate)

## diagnostic check 3: check for no duplicates in latin name 
dp <- speciesKey_full[duplicated(species_LB_Full) | duplicated(species_LB_Full, fromLast=T),]
setkey(dp, species_LB_Full)
dp

# format to final key and recheck it makes sense
finalKey <- unique(speciesKey_full[,list(Euring, species=species_Full, species_LB = species_LB_Full)])
finalKey[duplicated(Euring),]
finalKey[duplicated(species) | duplicated(species, fromLast=T),]
finalKey[duplicated(species_LB) | duplicated(species_LB, fromLast=T),]
# note that Greylag, Mallard, and rock Dove/Feral Pigeon show variable common names 

# now create final key and save
saveRDS(finalKey,"../cleanedFiles/dataKey_Euring-species.rds")
