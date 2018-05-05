# setup ------------------------------------------------------------------------
install.packages("prism") #package had a corrupted rdb and was really freaking annoying
libs <- c("sf", "tidyverse", "extRemes", "viridis", "evmix", "ggpubr")
for(i in libs){if(!require(i, character.only = TRUE)){install.packages(i)}}
lapply(libs, library, character.only = TRUE, verbose = FALSE)

x <- c("raster", "ncdf4", "tidyverse", "sf", "rasterVis", "gridExtra", "data.table", "assertthat", "rvest", 'parallel', 'doParallel', 'lwgeom',
       'parallel', 'foreach', "httr", "purrr", "rgdal", "maptools", "foreign", "purrr", "zoo", "lubridate", "magrittr", "snowfall")
lapply(x, library, character.only = TRUE, verbose = FALSE)

source("R/functions.R")
# paths ------------------------------------------------------------------------
dir.create("data")
dir.create("data/prism/")
options(prism.path = "data/prism")

ecol3_shp <- file.path("data", 'us_eco_l3.shp')
ecol3_remote <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
ecol3_zip <- file.path("data", "us_eco_l3.zip")

# data download ------------------------------------------------------------------

prism::get_prism_annual("ppt", years = 1895:2017)

# 
# if (!file.exists(fpa_gdb)) {
#   pg <- read_html("https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/")
#   fils <- html_nodes(pg, xpath=".//dd[@class='product']//li/a[contains(., 'zip') and contains(., 'GPKG')]")
#   dest <- paste0("fpa_fod.zip")
#   walk2(html_attr(fils, 'href'),  html_text(fils),
#         ~GET(sprintf("https:%s", .x), write_disk(dest), progress()))
#   unzip(dest, exdir = "data/")
#   unlink(dest)
#   #assert_that(file.exists(fpa_gdb))
#   # system(paste0('aws s3 sync ', #command
#   #               fpa_gdb, "/ ", #source file
#   #               s3_raw_prefix, "fpa-fod/",  "FPA_FOD_20170508.gdb/")) #destination
# }

if(!file.exists(ecol3_shp)){
download.file(ecol3_remote,
              'data/us_eco_l3.zip')
unzip(ecol3_zip, exdir = "data/")
}

# data import -------------------------------------------------------------
# system("aws s3 sync s3://earthlab-amahood/extremes/fpa_w_ecoregions.gpkg data/fpa_w_ecoregions.gpkg")
# 
# if(!file.exists(fpa_joined)){
#   fpa <- st_read(fpa_gdb)
#   
#   ecoregions <- st_read(ecol3_shp) %>%
#     st_transform(crs = st_crs(fpa))
# 
#   # joined <- st_intersection(fpa,ecoregions) #takes long time
#   joined <- st_par(fpa, st_intersection, detectCores(), y=ecoregions)
#   st_write(joined, fpa_joined)
#   system("aws s3 cp data/fpa_w_ecoregions.gpkg s3://earthlab-amahood/extremes/fpa_w_ecoregions.gpkg")
# }
# if(file.exists(fpa_joined)){joined <- st_read(fpa_joined)}


