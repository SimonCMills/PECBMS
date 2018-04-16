## minor reformatting of PECBMS files and saving to "../counts/baseDatasets/", 
## separate file for each country
##
## Note: first section is for the original datasets, and then I subsequently added
## the second section to do the same thing, but for the extra data

# housekeeping
require(data.table)

files <- list.files("../recievedFiles/PECBMS_originalFiles/counts/csv/", full.names=TRUE)
countryNames <- gsub(".*counts_(.*).csv", "\\1", files)
dat <- lapply(files, fread)
lapply(dat, head)
# read in and format. Throwing away "NA" (-1) rows
getDat <- function(x) {
    x[ObservedCount != -1, list(Euring, site=Site, year = CountYear, count_t = ObservedCount)]
}
datasets <- lapply(dat,getDat)
lapply(datasets, str)

# minor formatting of cols prior to save
lapply(datasets, function(x) x[,Euring := as.character(Euring)])
lapply(datasets, function(x) x[,site := as.character(site)])

# write out
saveNames <- paste0("../counts/PECBMS_originalFiles/dataset_", countryNames, ".rds")
for(i in seq(datasets)) {
    saveRDS(datasets[[i]], saveNames[i])
}


## Extra datasets
files <- list.files("../recievedFiles/PECBMS_extraFiles/counts/csv/", full.names=TRUE)
countryNames <- gsub(".*counts_(.*).csv", "\\1", files)
dat <- lapply(files, fread)
lapply(dat, head)

datasets <- lapply(dat,getDat)
# minor formatting of cols prior to save
lapply(datasets, function(x) x[,Euring := as.character(Euring)])
lapply(datasets, function(x) x[,site := as.character(site)])

# write out
saveNames <- paste0("../counts/PECBMS_extraFiles/dataset_", countryNames, ".rds")
for(i in seq(datasets)) {
    saveRDS(datasets[[i]], saveNames[i])
}
