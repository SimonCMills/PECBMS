## create final full dataframe of counts for all schemes, with unique site and 
## scheme identifiers. 
#
# Note: now creates 2 sets of cleaned final files, one for extra datasets, one for original

# housekeeping
rm(list=ls()); graphics.off()
require(data.table)

# read in count data (note the double list files to ensure France and UK get added)
files_count_original <- c(list.files("counts/PECBMS_originalFiles/", full.names=TRUE), 
                 list.files("counts/", full.names=TRUE, pattern = ".rds", recursive = FALSE))

files_count_extra <- c(list.files("counts/PECBMS_extraFiles/", full.names=TRUE), 
                 list.files("counts/", full.names=TRUE, pattern = ".rds", recursive = FALSE))
# extract countries
countries_count_original <- gsub(".*dataset_(.*).rds", "\\1", files_count_original)
countries_count_extra <- gsub(".*dataset_(.*).rds", "\\1", files_count_extra)

# Spain didn't have extra files sent through, so add this to the extra files list
countries_count_original[!countries_count_original %in% countries_count_extra]
files_count_extra <- c(files_count_extra, 
                       files_count_original[!countries_count_original %in% countries_count_extra])
countries_count_extra <- c(countries_count_extra, 
                           countries_count_original[!countries_count_original %in% countries_count_extra])


# Create a schemeID key, that is shorthand for the full scheme name and can be 
# used to create a unique siteID. 
# note I am merging E and W germany- the coordinator has confirmed that there are 
# no methodological differences between the two. 
(schemeKey <- data.frame(country = sort(countries_count_original), 
                         schemeID = c("BEL.B", "BEL.W", "CZE.0", "DEN.0", "EST.0", 
                                      "FIN.0", "FRA.0", "GER.0", "GER.0", 
                                      "NET.0", "SPA.0", "SPA.C", "SWE.N", "SWE.O", "UKI.0")))


# create full dataframe for all datasets (using original PECBMS files)
data_count <- lapply(files_count_original, readRDS)
for(i in seq(data_count)) {
    data_count[[i]]$country <- countries_count_original[i]
    data_count[[i]] <- merge(data_count[[i]], schemeKey, by="country")
}
data_full <- rbindlist(data_count)
# make siteID unique by pasting together with the scheme identifier
data_full[,siteID:=paste0(schemeID, ":", site)]

# save (note that you now save as dplyr object)
saveRDS(dplyr::as_data_frame(data_full), "cleanedFiles/countData_allSchemes_originalPECBMSfiles.rds")

## Repeat process but using additional files ----
# create full dataframe for all datasets (using *extra* PECBMS files)
data_count <- lapply(files_count_extra, readRDS)
for(i in seq(data_count)) {
    data_count[[i]]$country <- countries_count_extra[i]
    data_count[[i]] <- merge(data_count[[i]], schemeKey, by="country")
}
data_full <- rbindlist(data_count)
# make siteID unique by pasting together with the scheme identifier
data_full[,siteID:=paste0(schemeID, ":", site)]

# save
saveRDS(dplyr::as_data_frame(data_full), "cleanedFiles/countData_allSchemes_extraPECBMSfiles.rds")