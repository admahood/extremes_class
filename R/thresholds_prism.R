source("R/a2_data_prep_ec2.R")

# import data ------------------------------------

ecoregions <- st_read(ecol3_shp) %>%
  st_simplify()

eco_2 <- ecoregions %>%
  dplyr::group_by(NA_L2NAME) %>%
  dplyr::summarise(NA_L2CODE = mean(as.numeric(NA_L3CODE)))

ecos <- unique(as.character(ecoregions$NA_L2NAME))

eco_res <- list()
vars <- c("ppt", "tmax", "tmin","vpdmax")
varnames <- c("Precipitation", "Maximum Temperature", "Minimum Temperature", "Maximum Vapor Pressure Deficit")
units <- c("(mm)", "(Degrees C)", "(Degrees C)", "(hPa)")
labels <- c("A.", "B.", "C.", "D.")
# years <- 1895:2017
result <- list()
plots <- list()
plots95 <- list()
plots_diff <- list()
set.seed(938475239)

for(v in 1:length(vars)){
  # prism::get_prism_annual(vars[v], years = years)
  # prism_bils <- prism::prism_stack(prism::ls_prism_data()[,1])
  system(paste0("aws s3 sync s3://earthlab-amahood/climate/",
                vars[v],
                " data/prism/", vars[v]
               ))
  prism_files <- Sys.glob(paste0("data/prism/",vars[v],"/*bil.bil"))
  prism_stk <- raster::stack(prism_files, quick=T)
  eco_res[[v]] <- raster::extract(prism_stk, st_point_on_surface(eco_2), df = T)
  
  #system(paste0("rm -r data/prism/",vars[v]))
  
  for(i in 1:length(ecos)){
    eco_res[[v]]$ID <- ifelse(eco_res[[v]]$ID  == i, ecos[i], eco_res[[v]]$ID )
  }

  result[[v]] <- data.frame(Th = NA, p_95 = NA, eco_name = NA)
  
  for(j in 1:length(ecos)){
    subset <- eco_res[[v]][eco_res[[v]]$ID == ecos[j],]
    long <- tidyr::gather(subset, key = "bil_file", value = "value",-ID)
    
  
    model <- fgammagpd(long$value, 
                         phiu = FALSE, 
                         std.err = FALSE, 
                         control = list(maxit = 100000))
    
    result[[v]][j,1] <- round(model$u)
    result[[v]][j,2] <- as.numeric(quantile(long$value, 0.95))
    result[[v]][j,3] <- as.character(ecos[j])
  }
  system(paste0("rm -r data/prism/",vars[v]))
  
  eco_2_ <- dplyr::left_join(eco_2, result[[v]], by= c("NA_L2NAME" = "eco_name"))
  # plot(eco_2_[3])
  
  dir.create("results_tosave")
  st_write(eco_2_, paste0("results_tosave/",vars[v],"_L2.gpkg"), delete_layer = TRUE)
  
  plots[[v]] <- ggplot2::ggplot(eco_2_, aes(fill = Th)) +
    geom_sf(lwd=0.25) +
    theme_void() +
    ggtitle(paste(varnames[v], units[v])) +
    scale_fill_viridis()

  plots95[[v]] <- ggplot2::ggplot(eco_2_, aes(fill = p_95)) +
    geom_sf(lwd=0.25) +
    theme_void() +
    ggtitle(paste(varnames[v], units[v])) +
    scale_fill_viridis()
  
  plots_diff[[v]] <- ggplot2::ggplot(eco_2_, aes(fill = (abs(p_95-Th)/mean(p_95+Th)*100))) +
    geom_sf(lwd=0.25) +
    theme_void() +
    ggtitle(paste(varnames[v], units[v])) +
    scale_fill_viridis(name = "% Difference")
  
  gc()
}

# plotting ---------------------------------------------------------------------

mp <- ggarrange(plots[[1]], plots[[2]],plots[[3]],plots[[4]],
                ncol = 2,
                nrow = 2,
                #labels = labels,
                #common.legend = TRUE,
                legend = "right")
mp <- annotate_figure(mp,
                      top = text_grob("Monthly Thresholds from Extreme Value Mixed Models, Level 2 Ecoregions", 
                                      color = "black", 
                                      face = "bold", 
                                      size = 14))

ggsave(plot = mp,
       filename = "data/th_clim.png",
       limitsize = FALSE)

system("aws s3 cp data/th_clim.png s3://earthlab-amahood/extremes/th_clim.png")

#95th percentile

mp95 <- ggarrange(plots95[[1]], plots95[[2]],plots95[[3]],plots95[[4]],
                ncol = 2,
                nrow = 2,
                #labels = labels,
                #common.legend = TRUE,
                legend = "right")
mp95 <- annotate_figure(mp95,
                      top = text_grob("Monthly 95th percentile, Level 2 Ecoregions", 
                                      color = "black", 
                                      face = "bold", 
                                      size = 14))

ggsave(plot = mp95,
       filename = "data/th_clim95.png",
       limitsize = FALSE)

system("aws s3 cp data/th_clim95.png s3://earthlab-amahood/extremes/th_clim95.png")

# difference

mpdf <- ggarrange(plots_diff[[1]], plots_diff[[2]],plots_diff[[3]],plots_diff[[4]],
                  ncol = 2,
                  nrow = 2,
                  #labels = labels,
                  # common.legend = TRUE,
                  legend = "right")
mpdf <- annotate_figure(mpdf,
                        top = text_grob("Difference Between Threshold Estimation Methods, Level 2 Ecoregions", 
                                        color = "black", 
                                        face = "bold", 
                                        size = 14))

ggsave(plot = mpdf,
       filename = "data/th_climdf.png",
       limitsize = FALSE)

system("aws s3 cp data/th_climdf.png s3://earthlab-amahood/extremes/th_climdf.png")

#densityplots

mp_d <- ggarrange(
  ggplot(gather(eco_res[[1]], key = "bil_file", value = "value",-ID), aes(value)) +
    geom_density() + theme_pubr() + ggtitle(varnames[v]),
  ggplot(gather(eco_res[[2]], key = "bil_file", value = "value",-ID), aes(value)) +
    geom_density() + theme_pubr() + ggtitle(varnames[v]),
  ggplot(gather(eco_res[[3]], key = "bil_file", value = "value",-ID), aes(value)) + 
    geom_density() + theme_pubr() + ggtitle(varnames[v]),
  ggplot(gather(eco_res[[4]], key = "bil_file", value = "value",-ID), aes(value)) +
  geom_density() + theme_pubr() + ggtitle(varnames[v]),
  ncol = 2,
  nrow = 2)

mp_d <- annotate_figure(mp_d,
                        top = text_grob("Aggregated Distributions for Each Variable", 
                                        color = "black", 
                                        face = "bold", 
                                        size = 14))

ggsave(plot = mp_d,
       filename = "data/d_clim.png",
       limitsize = FALSE)

system("aws s3 cp data/d_clim.png s3://earthlab-amahood/extremes/d_clim.png")

