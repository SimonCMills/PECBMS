## format UK files which were all sent as separate .csv files to single dataframe

# housekeeping
require(data.table)

files <- list.files("../recievedFiles/UK/", 
                    pattern = ".csv", full.names=TRUE, recursive=TRUE)
# extract the id file from the list and remove
speciesKey <- fread(files[grepl("btolist", files)])
files <- files[!grepl("btolist", files)]

getDF <- function(filename) {
    file_i <- fread(filename)
    Euring <- speciesKey[Two.letter.code == unique(file_i$sp),Euring_code]
    file_i[!is.na(count), list(Euring, site=square, year, count_t = count)]
}

out <- lapply(files, getDF)

toSave <- rbindlist(out)
toSave[,Euring := as.character(Euring)]

# write
saveRDS(toSave, "../counts/dataset_UK.rds")
