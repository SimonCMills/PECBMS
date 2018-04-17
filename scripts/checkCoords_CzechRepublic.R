## checking Czech coordinates. How different are 'site' coords and 'municipality' coords. 
require(dplyr); require(ggplot2); require(sp)
coords <- xlsx::read.xlsx("recievedFiles/PECBMS_originalFiles/coords/sites_Czech.xls", 1)
totalCoords <- nrow(coords)
# remove NA values as no comparison to be made
coords <- coords %>% 
    filter(!is.na(first_point_LON))
nrow(coords)/totalCoords


municipality <- coords %>% 
    select(long = municipality_LON,
           lat = municipality_LAT)
municipality <- SpatialPoints(municipality,  proj4string=CRS("+proj=longlat +datum=WGS84"))

site <- coords %>%
    select(long = first_point_LON, lat = first_point_LAT)
site <- SpatialPoints(site,  proj4string=CRS("+proj=longlat +datum=WGS84"))

# dists constructs a pairwise comparison between all points, returning a matrix
dists<- spDists(municipality, site, longlat = TRUE)
# diagonals of these are the distances between a municipality point and a site point

ggplot() + geom_histogram(aes(x=diag(dists)), fill="white", col="black", binwidth=.25) + theme_classic() +
    labs(title="Distances between 'municipality' and 'site' coordinates, where both exist",
         x="Distance (km)") +
    theme(plot.title = element_text(hjust=1))

ggsave("figures/CzechRepublic_municipalitySiteDiffs.png", dpi=200)
