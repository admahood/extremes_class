# whole US ---------------------------------------------------------------------
fit <- fweibullgpd(joined$P_ACRES, 
                    phiu = F, 
                    std.err = F, 
                    control = list(maxit = 100000))


# thresholds by ecoregion ------------------------------------------------------

# level 1 ----------------------------------------------------------------------
l1e <- unique(joined$NA_L1NAME)
result1 <- data.frame(Th = NA, NA_L1NAME = NA, n = NA, NA_L1CODE = NA)
counter = 1
for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L1NAME == l1e[i],]
  
  if(dim(subset)[1]>10){
    model <- fweibullgpd(subset$P_ACRES, 
                         phiu = TRUE, 
                         std.err = FALSE, 
                         control = list(maxit = 100000))
    
    result1[i,1] <- round(model$u)
    result1[i,2] <- as.character(l1e[i])
    result1[i,3] <- length(subset$FIRE_ID)
    result1[i,4] <- subset[1,]$NA_L1CODE
  }
  else{
    result1[i,1] <- NA
    result1[i,2] <- as.character(l1e[i])
    result1[i,3] <- length(subset$FIRE_ID)
    result1[i,4] <- subset[1,]$NA_L1CODE
    print(counter)
    counter <- counter +1
  }
}

# level 2 ----------------------------------------------------------------------
l1e <- unique(joined$NA_L2NAME)
result2 <- data.frame(Th = NA, NA_L2NAME = NA, n = NA, NA_L2CODE = NA)
counter = 1
for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L2NAME == l1e[i],]
  
  if(dim(subset)[1]>10){
    model <- fweibullgpd(subset$P_ACRES, 
                         phiu = TRUE, 
                         std.err = FALSE, 
                         control = list(maxit = 100000))
    
    result2[i,1] <- round(model$u)
    result2[i,2] <- as.character(l1e[i])
    result2[i,3] <- length(subset$FIRE_ID)
    result2[i,4] <- subset[1,]$NA_L2CODE
  }
  else{
    result2[i,1] <- NA
    result2[i,2] <- as.character(l1e[i])
    result2[i,3] <- length(subset$FIRE_ID)
    result2[i,4] <- subset[1,]$NA_L2CODE
    print(counter)
    counter <- counter +1
  }
}

# level 3 ----------------------------------------------------------------------
l1e <- unique(joined$NA_L3NAME)
result3 <- data.frame(Th = NA, NA_L3NAME = NA, n = NA, NA_L3CODE = NA)
counter = 1

for(i in 1:length(l1e)){
  subset <- joined[joined$NA_L3NAME == l1e[i],]
  
  if(dim(subset)[1]>20){
    model <- fweibullgpd(subset$P_ACRES, 
                         phiu = FALSE, 
                         std.err = FALSE, 
                         control = list(maxit = 100000))
    
    result3[i,1] <- round(model$u)
    result3[i,2] <- as.character(l1e[i])
    result3[i,3] <- length(subset$FIRE_ID)
    result3[i,4] <- subset[1,]$NA_L3CODE
  }
  else{
    result3[i,1] <- NA
    result3[i,2] <- as.character(l1e[i])
    result3[i,3] <- length(subset$FIRE_ID)
    result3[i,4] <- subset[1,]$NA_L3CODE
    print(counter)
    counter <- counter +1
  }
}

# grouping polygons ------------------------------------------------------------

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

eco_1$Threshold <- cut(eco_1$Th,
                      breaks=c(-Inf, 5000, 10000, 15000, 25000, 30000, Inf),
                      labels=c("2K-5K","5K-10K","10K-15K",
                               "15K-25K","25K-30K","30K-54K"))

eco_2$Threshold <- cut(eco_2$Th,
                       breaks=c(-Inf, 5000, 10000, 15000, 25000, 30000, Inf),
                       labels=c("2K-5K","5K-10K","10K-15K",
                                "15K-25K","25K-30K","30K-54K"))

eco_3$Threshold <- cut(eco_3$Th,
                       breaks=c(-Inf, 5000, 10000, 15000, 25000, 30000, Inf),
                       labels=c("2K-5K","5K-10K","10K-15K",
                                "15K-25K","25K-30K","30K-54K"))


mycols <- viridis(6, alpha = 1, begin = 0, end = 1, option = "D")

colours <- c("2K-5K" = mycols[1],
             "5K-10K" = mycols[2],
             "10K-15K" = mycols[3],
             "15K-25K" = mycols[4],
             "25K-30K" = mycols[5],
             "30K-54K" = mycols[6])

# plotting ---------------------------------------------------------------------

p1 <- ggplot(eco_1, aes(fill = Threshold)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 1 Ecoregions")  + 
  scale_fill_manual(values = colours, 
                    na.value='grey',
                    name = "Threshold \n (Acres)") +
  theme(panel.grid.major = element_line(colour = 'transparent'))

p2 <- ggplot(eco_2, aes(fill = Threshold)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 2 Ecoregions")  + 
  scale_fill_manual(values = colours, 
                    na.value='grey',
                    name = NULL) +
  theme(panel.grid.major = element_line(colour = 'transparent'))

p3 <- ggplot(eco_3, aes(fill = Threshold)) +
  geom_sf(lwd=0.25) +
  theme_void() +
  ggtitle("      Level 3 Ecoregions")  + 
  scale_fill_manual(values = colours, 
                    na.value='grey',
                    name = NULL) +
  theme(panel.grid.major = element_line(colour = 'transparent'))

mp <- ggarrange(p1,p2,p3, 
                ncol = 1,
                nrow = 3,
                labels = c("A","B","C"),
                #common.legend = TRUE,
                legend = "right")
mp <- annotate_figure(mp,
                top = text_grob("Extreme Fire Thresholds", 
                                color = "black", 
                                face = "bold", 
                                size = 14))
                

ggsave(plot = mp,
       filename = "th.png",
       limitsize = FALSE,
       width = 4.5,
       height = 6.5)
