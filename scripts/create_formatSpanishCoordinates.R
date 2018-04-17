# ArcMap used to convert KML file to shapefile. This script extracts the shapefile
# coordinates and joins them to their corresponding "siteID" in the datasets 
# originally sent via PECBMS

library(maptools); library(dplyr); library(xlsx)

shpfile <- readShapePoints("recievedFiles/Spain_coordinates/Spain_UTM_convertedFromKML.shp")

# get coordinates and append the corresponding grid ID and 'symbolID'. 
# symbolIDs of 1 correspond to 10x10km centroids, so filter for these
coord_df <- coordinates(shpfile) %>% 
    as_data_frame %>%
    select(lon_WGS84 = 1, lat_WGS84 = 2) %>%
    mutate(gridID = as.character(shpfile$Name), SymbolID = as.character(shpfile$SymbolID)) %>%
    filter(SymbolID=="1") %>%
    select(-SymbolID)

coord_dataKey <- read.xlsx("recievedFiles/PECBMS_originalFiles/coords/sites_Spain.xlsx", 1) %>%
    as_data_frame %>%
    select(siteID = 1, gridID = 2)


coord_full <- left_join(coord_dataKey, coord_df) %>%
    mutate(siteID = as.integer(siteID)) %>%
    unique
# check no siteIDs dropped
nrow(coord_full) == nrow(coord_dataKey)

saveRDS(coord_full, "recievedFiles/Spain_coordinates/coordinates_WGS84.rds")