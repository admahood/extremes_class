
# calculate return intervals ---------------------------------------------------
# need to run a_data_prep first
# whole US ---------------------------------------------------------------------
annual_maxima <- joined %>%
  dplyr::group_by(FIRE_YEAR) %>%
  dplyr::summarise(max_size = max(R_ACRES))

model <- fevd(x = annual_maxima$max_size, type = 'GEV', method = "MLE")
estimates <- model$results$par
loc <- estimates['location']
sca <- estimates['scale']
sha <- estimates['shape']

cdf <- pevd(1000000, loc = loc, scale = sca, shape = sha, type = 'GEV')
ef <- 1-cdf
ri <- 1/ef
result0 <- c(ri, length(annual_maxima$FIRE_YEAR))



# level 1 ecoregions -----------------------------------------------------------
l1e <- unique(joined$NA_L1NAME)
result1 <- data.frame(ri_l1 = NA, NA_L1NAME = NA, n = NA, NA_L1CODE = NA)
for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L1NAME == l1e[i],]
  
  annual_maxima <- joined[joined$NA_L1NAME == l1e[i],] %>%
    dplyr::group_by(FIRE_YEAR) %>%
    dplyr::summarise(max_size = max(R_ACRES))
  
  if(dim(annual_maxima)[1]>10){
    model <- fevd(x = annual_maxima$max_size, type = 'GEV', method = "MLE")
    estimates <- model$results$par
    loc <- estimates['location']
    sca <- estimates['scale']
    sha <- estimates['shape']
    
    cdf <- pevd(1000000, loc = loc, scale = sca, shape = sha, type = 'GEV')
    ef <- 1-cdf
    ri <- 1/ef
    result1[i,1] <- round(ri)
    result1[i,2] <- as.character(l1e[i])
    result1[i,3] <- length(annual_maxima$FIRE_YEAR)
    result1[i,4] <- subset[1,]$NA_L1CODE
  }
  else{
    result1[i,1] <- NA
    result1[i,2] <- as.character(l1e[i])
    result1[i,3] <- length(annual_maxima$FIRE_YEAR)
    result1[i,4] <- subset[1,]$NA_L1CODE
    
  }
}

# level 2 ecoregions -----------------------------------------------------------
l1e <- unique(joined$NA_L2NAME)
result2 <- data.frame(ri_l2 = NA, NA_L2NAME = NA, n = NA, NA_L2CODE = NA)
for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L2NAME == l1e[i],]
  
  annual_maxima <- joined[joined$NA_L2NAME == l1e[i],] %>%
    dplyr::group_by(FIRE_YEAR) %>%
    dplyr::summarise(max_size = max(R_ACRES))
  
  if(dim(annual_maxima)[1]>10){
    model <- fevd(x = annual_maxima$max_size, type = 'GEV', method = "MLE")
    estimates <- model$results$par
    loc <- estimates['location']
    sca <- estimates['scale']
    sha <- estimates['shape']
    
    cdf <- pevd(1000000, loc = loc, scale = sca, shape = sha, type = 'GEV')
    ef <- 1-cdf
    ri <- 1/ef
    result2[i,1] <- round(ri)
    result2[i,2] <- as.character(l1e[i])
    result2[i,3] <- length(annual_maxima$FIRE_YEAR)
    result2[i,4] <- subset[1,]$NA_L2CODE
    
  }
  else{
    result2[i,1] <- NA
    result2[i,2] <- as.character(l1e[i])
    result2[i,3] <- length(annual_maxima$FIRE_YEAR)
    result2[i,4] <- subset[1,]$NA_L2CODE
    
  }
}

# level 3 ecoregions -----------------------------------------------------------
l1e <- unique(joined$NA_L3NAME)
result3 <- data.frame(ri_l3 = NA, NA_L3NAME = NA, n = NA, NA_L3CODE = NA)

for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L3NAME == l1e[i],]
  
  annual_maxima <- joined[joined$NA_L3NAME == l1e[i],] %>%
    dplyr::group_by(FIRE_YEAR) %>%
    dplyr::summarise(max_size = max(R_ACRES))
  
  if(dim(annual_maxima)[1]>10){
    model <- fevd(x = annual_maxima$max_size, type = 'GEV', method = "MLE")
    estimates <- model$results$par
    loc <- estimates['location']
    sca <- estimates['scale']
    sha <- estimates['shape']
    
    cdf <- pevd(1000000, loc = loc, scale = sca, shape = sha, type = 'GEV')
    ef <- 1-cdf
    ri <- 1/ef
    result3[i,1] <- round(ri)
    result3[i,2] <- as.character(l1e[i])
    result3[i,3] <- length(annual_maxima$FIRE_YEAR)
    result3[i,4] <- subset[1,]$NA_L3CODE
    
  }
  else{
    result3[i,1] <- NA
    result3[i,2] <- as.character(l1e[i])
    result3[i,3] <- length(annual_maxima$FIRE_YEAR)
    result3[i,4] <- subset[1,]$NA_L3CODE
    
  }
}

# plot prep---------------------------------------------------------------------
eco_1 <- ecoregions %>%
  dplyr::group_by(NA_L1NAME) %>%
  dplyr::summarise(NA_L1CODE = mean(as.numeric(NA_L1CODE))) %>%
  left_join(result1, by = "NA_L1CODE")

eco_2 <- ecoregions %>%
  dplyr::group_by(NA_L2NAME) %>%
  dplyr::summarise(NA_L2CODE = mean(as.numeric(NA_L2CODE), na.rm = TRUE)) %>%
  left_join(result2, by = "NA_L2CODE")

eco_3 <- ecoregions %>%
  dplyr::group_by(NA_L3NAME) %>%
  dplyr::summarise(NA_L3CODE = mean(as.numeric(NA_L3CODE), na.rm = TRUE)) %>%
  left_join(result3, by = "NA_L3CODE")

# reclassifying ----------------------------------------------------------------

eco_1$Interval <- cut(eco_1$ri_l1,
                   breaks=c(-Inf, 20, 50, 100, 200, 1000, Inf),
                   labels=c("Under 20","20-50","50-100","100-200","200-1000","1000+"))

eco_2$Interval <- cut(eco_2$ri_l2,
                   breaks=c(-Inf, 20, 50, 100, 200, 1000, Inf),
                   labels=c("Under 20","20-50","50-100","100-200","200-1000","1000+"))

eco_3$Interval <- cut(eco_3$ri_l3,
                   breaks=c(-Inf, 20, 50, 100, 200, 1000, Inf),
                   labels=c("Under 20","20-50","50-100","100-200","200-1000","1000+"))


mycols <- viridis(6, alpha = 1, begin = 0, end = 1, option = "D")

colours <- c("Under 20" = mycols[6],
             "20-50" = mycols[5],
             "50-100" = mycols[4],
             "100-200" = mycols[3],
             "200-1000" = mycols[2],
             "1000+" = mycols[1])

# ggplots ----------------------------------------------------------------------

p1 <- ggplot(eco_1, aes(fill = Interval)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 1 Ecoregions")  + 
  scale_fill_manual(values = colours, na.value='grey',
                    name = "Interval \n (Years)") +
  theme(panel.grid.major = element_line(colour = 'transparent'))

p2 <- ggplot(eco_2, aes(fill = Interval)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 2 Ecoregions") +
  scale_fill_manual(values = colours, na.value='grey',
                    name = NULL) +
  theme(panel.grid.major = element_line(colour = 'transparent'))

p3 <- ggplot(eco_3, aes(fill = Interval)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 3 Ecoregions")  +
  scale_fill_manual(values = colours, na.value='grey',
                    name = NULL) +
  theme(panel.grid.major = element_line(colour = 'transparent'))


mp <- ggarrange(p1,p2,p3, 
                ncol = 1,
                nrow = 3,
                labels = c("A","B","C"),
                #common.legend = TRUE,
                legend = "right")
mp <- annotate_figure(mp,
                      top = text_grob("Million Acre Return Invervals", 
                                      color = "black", 
                                      face = "bold", 
                                      size = 14))


ggsave(plot = mp,
       filename = "ri.png",
       limitsize = FALSE,
       width = 4.5,
       height = 6.5)
