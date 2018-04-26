# setup ------------------------------------------------------------------------
libs <- c("sf", "tidyverse", "extRemes", "viridis", "evmix", "ggpubr")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

# paths ------------------------------------------------------------------------
states_file <- "/home/computer/DATA/BACKGROUND/CUS/CUS.shp"
ecoregions_file <- "/home/computer/DATA/BACKGROUND/ecoregions/us_eco_l3.shp"
mtbs_file <- "/home/computer/DATA/FIRE/mtbs_pnts/mtbs_fod_pts_20170501.shp"
fpa_file <- "/home/computer/DATA/FIRE/Short/RDS-2013-0009.4_GPKG/Data/FPA_FOD_20170508.gpkg"
mtbs_joined <- "data/mtbs_w_ecoregions.gpkg"
fpa_joined <- "data/fpa_w_ecoregions.gpkg"
# data import ------------------------------------------------------------------
states <- st_read(states_file) 

ecoregions <- st_read(ecoregions_file) %>%
  st_transform(crs = st_crs(states))

if(!file.exists(mtbs_joined)){
  mtbs <- st_read(mtbs_file) %>%
    st_transform(crs = st_crs(states)) 
  joined <- st_intersection(mtbs,ecoregions) #takes long time
  st_write(joined, mtbs_joined)
}
if(file.exists(mtbs_joined)){joined <- st_read(mtbs_joined)}

# if(!file.exists(fpa_joined)){
# fpa <- st_read(fpa_file) %>%
#   st_transform(crs=st_crs(ecoregions)) %>%
#   st_intersection(ecoregions)
# st_write(fpa,fpa_joined)
# }
# if(file.exists(fpa_joined)){fpa_j <- st_read(fpa_joined)}