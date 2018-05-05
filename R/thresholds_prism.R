source("R/a2_data_prep_ec2.R")

# import data ------------------------------------

ecoregions <- st_read(ecol3_shp) %>%
  st_simplify()

prism_bils <- prism::prism_stack(prism::ls_prism_data()[,1])

eco_cols <- c("NA_L1NAME","NA_L2NAME","NA_L3NAME")

ecos <- list()
ecos[[1]] <- unique(as.character(ecoregions$NA_L1NAME))
ecos[[2]] <- unique(as.character(ecoregions$NA_L2NAME))
ecos[[3]] <- unique(as.character(ecoregions$NA_L3NAME))

eco_res <- list()
eco_1 <- ecoregions %>%
  dplyr::group_by(NA_L1NAME) %>%
  dplyr::summarise(NA_L1CODE = mean(as.numeric(NA_L1CODE)))

eco_res[[1]] <- raster::extract(prism_bils, eco_1, df = T)
for(i in 1:length(ecos[[1]])){
  eco_res[[1]]$ID <- ifelse(eco_res[[1]]$ID == i, ecos[[1]][i], eco_res[[1]]$ID)
}

eco_2 <- ecoregions %>%
  dplyr::group_by(NA_L2NAME) %>%
  dplyr::summarise(NA_L2CODE = mean(as.numeric(NA_L3CODE)))

eco_res[[2]] <- raster::extract(prism_bils, eco_2, df = T)
for(i in 1:length(ecos[[2]])){
  eco_res[[2]]$ID <- ifelse(eco_res[[2]]$ID  == i, ecos[[2]][i], eco_res[[2]]$ID )
}

eco_3 <- ecoregions %>%
  dplyr::group_by(NA_L3NAME) %>%
  dplyr::summarise(NA_L3CODE = mean(as.numeric(NA_L3CODE)))

eco_res[[3]] <- raster::extract(prism_bils, eco_3, df = T)
for(i in 1:length(ecos[[3]])){
  eco_res[[3]]$ID  <- ifelse(eco_res[[3]]$ID == i, ecos[[3]][i], eco_res[[3]]$ID)
}



# looping the models ---------------------------------------------------
result <- list()
for(i in 1:length(ecos)){
  result[[i]] <- data.frame(Th = NA, eco_name = NA, n = NA)
  
  for(j in 1:length(ecos[[i]])){
    subset <- eco_res[[i]][eco_res[[i]]$ID == ecos[[i]][j],]
    long <- gather(subset, key = "bil_file", value = "ppt",-ID)
    

    model <- fgammagpd(long$ppt, 
                         phiu = FALSE, 
                         std.err = FALSE, 
                         control = list(maxit = 100000))
    
    result[[i]][j,1] <- round(model$u)
    result[[i]][j,2] <- as.character(ecos[[i]][j])
    result[[i]][j,3] <- length(subset$ppt)
    
  }
}


