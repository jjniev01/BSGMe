require(data.table)
require(ggplot2)
require(sf)

root <- "E:/Research/"
iso3 <- c("CHE","PAN","UGA","VNM")


##  BSGMi Modeled Input  ----


for(i in iso3){
  ##  Bring in the data from the model:
  xx <- readRDS(paste0(root,
                       "BSGMe/Modeled Input Validation/prj_2010-2015_",i,"/tmp/",
                       "forecast_table_prj_2010-2015_",i,"_unadjusted.RDS"))
  
  ##  Fill in the model tag for the URR on years not predicted for based upon GID 
  ##  in order to plot properly:
  uni_urr_mod <- copy(xx[!is.na(URR.MOD.TAG) ,.(GID,URR.MOD.TAG)])
  for(g in uni_urr_mod$GID){
    xx[GID == g, URR.MOD.TAG := uni_urr_mod[GID == g]$URR.MOD.TAG[1]]
  }
  ##  Do the same for BSPDBAR:
  unibspdbar_mod <- copy(xx[!is.na(BSPDBAR.MOD.TAG) ,.(GID,BSPDBAR.MOD.TAG)])
  for(g in uni_urr_mod$GID){
    xx[GID == g, BSPDBAR.MOD.TAG := unibspdbar_mod[GID == g]$BSPDBAR.MOD.TAG[1]]
  }
  
  
  ##  Determine which ones experienced "negative growth" and remove them from the 
  ##  dataset as they are not used for growth in the model and are assumed to 
  ##  remain stationary:
  net.change <- xx[YEAR==2015,.(BSCNT)] - xx[YEAR==2010,.(BSCNT)]
  net.change$GID <- unique(xx$GID)
  names(net.change)[1] <- "NET.SHIFT"
  xx <- copy(merge(xx,net.change, by = "GID"))
  
  ##  Create reduced copies of the dataset for only units that we predicted growth
  ##  for:
  xx_urr <- copy(xx[!is.na(URR.MOD.TAG) & URR.MOD.TAG != "" & 
                      GID %in% unique(xx[NET.SHIFT>=0]$GID),])
  xx_bspdbar <- copy(xx[!is.na(BSPDBAR.MOD.TAG) & BSPDBAR.MOD.TAG != "" &
                          GID %in% unique(xx[NET.SHIFT>=0]$GID),])
  
  
  ##  Read in the shapefile:
  shp <- st_read(dsn = paste0(root,"BSGMe/data/",i,"/",tolower(i),"_grid_100m_ccidadminl1.shp"),
                 layer = paste0(tolower(i),"_grid_100m_ccidadminl1"),
                 stringsAsFactors = F)
  names(shp)[1] <- "GID"
  
  ##  Merge the URR model tag info and save a copy of the shapefile in the 
  ##  derived folder:
  unique_xx_urr <- unique(xx_urr[,.(GID,URR.MOD.TAG)])
  unique_xx_bspdbar <- unique(xx_bspdbar[,.(GID,BSPDBAR.MOD.TAG)])
  modin_shp <- merge(shp,unique_xx_urr, by = "GID")
  modin_shp <- merge(modin_shp, unique_xx_bspdbar, by = "GID")
  st_write(modin_shp, 
           dsn = paste0(root,
                        "BSGMe/Modeled Input Validation/prj_2010-2015_",
                        i,"/derived/",i,"_Modeled_URR_BSPDBAR_Model_Class.shp"),
           layer = paste0(i,"_Modeled_URR_BSPDBAR_Model_Class"),
           driver = "ESRI Shapefile",
           delete_dsn = T)
  
  
  
  pdf(file = paste0(root,"BSGMe/figures/",i,"_BSGMi_Input_Typology_Exploration_BSGMe.pdf"),
      width = 7.5,height = 5,
      onefile = T,paper = "a4")
  print(ggplot(xx_urr[YEAR <=2010], aes(x=YEAR, y = BSPOP, group = GID))+
    geom_line(alpha = 0.5, size =0.5, color = "grey50")+
    geom_line(data = xx_urr[YEAR >=2011], aes(x= YEAR, y = BSPOP, group=GID),
              alpha = 0.25, size = 0.5, color = "red", inherit.aes = F)+
    facet_wrap(~URR.MOD.TAG)+
    theme_bw()+
    ggtitle("BSGMi Time Series BSPOP by URR MOD TAG"))
  
  print(ggplot(xx_bspdbar,
         aes(x=YEAR, y = BSPDBAR, group = GID))+
    geom_line(alpha = 0.5, size =0.5, color = "grey50")+
    geom_line(data = xx_bspdbar[YEAR >=2011], aes(x= YEAR, y = BSPDBAR, group=GID),
              alpha = 0.25, size = 0.5, color = "red", inherit.aes = F)+
    facet_wrap(~BSPDBAR.MOD.TAG)+
    theme_bw()+
    ggtitle("BSGMi Time Series BSPOP by BSPDBAR MOD TAG")
  )
  
  
  
  ##  Scatterplots by model fit:
  ##  Initital scatter of BS pop v pop density in 2000:
  print(ggplot(xx_urr[YEAR == 2000],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("BSGMi Scatter BSPOP v BSPDBAR 2000 by URR MOD TAG"))
  
  ##  Scatter of BS pop v pop density prior to projections in 2010:
  print(ggplot(xx_urr[YEAR == 2010],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("BSGMi Scatter BSPOP v BSPDBAR 2010 by URR MOD TAG")
  )
  
  print(ggplot(xx_bspdbar[YEAR == 2000],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("BSGMi Scatter BSPOP v BSPDBAR 2000 by BSPDBAR MOD TAG"))
  
  ##  Scatter of BS pop v pop density prior to projections in 2010:
  print(ggplot(xx_bspdbar[YEAR == 2010],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("BSGMi Scatter BSPOP v BSPDBAR 2010 by BSPDBAR MOD TAG"))
  
  
  
  ##  Trajectory of bs pop between 2000 and 2010:
  print(ggplot(xx_urr[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    facet_wrap(~URR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("BSGMi Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  print(ggplot(xx_urr[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    #coord_cartesian(ylim=c(0))+
    facet_wrap(~URR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("BSGMi Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  
  print(ggplot(xx_bspdbar[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPDBAR, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    facet_wrap(~BSPDBAR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("BSGMi Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  print(ggplot(xx_bspdbar[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPDBAR, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    #coord_cartesian(ylim=c(0))+
    facet_wrap(~BSPDBAR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("BSGMi Trajectory BSPOP 2000-2010 by BSPDBAR MOD TAG"))
  
  dev.off()
  
  
  
  ##  OBSERVED INPUT DATA  ----
  yy <- readRDS(paste0(root,
                       "BSGMe/Observed Input Validation/prj_2010-2015_",i,"/tmp/",
                       "forecast_table_prj_2010-2015_",i,"_unadjusted.RDS"))
  
  ##  Fill in the model tag for the URR on years not predicted for based upon GID 
  ##  in order to plot properly:
  uni_urr_mod <- copy(yy[!is.na(URR.MOD.TAG) ,.(GID,URR.MOD.TAG)])
  for(g in uni_urr_mod$GID){
    yy[GID == g, URR.MOD.TAG := uni_urr_mod[GID == g]$URR.MOD.TAG[1]]
  }
  ##  Do the same for BSPDBAR:
  unibspdbar_mod <- copy(yy[!is.na(BSPDBAR.MOD.TAG) ,.(GID,BSPDBAR.MOD.TAG)])
  for(g in uni_urr_mod$GID){
    yy[GID == g, BSPDBAR.MOD.TAG := unibspdbar_mod[GID == g]$BSPDBAR.MOD.TAG[1]]
  }
  
  
  ##  Determine which ones experienced "negative growth" and remove them from the 
  ##  dataset as they are not used for growth in the model and are assumed to 
  ##  remain stationary:
  net.change <- yy[YEAR==2015,.(BSCNT)] - yy[YEAR==2010,.(BSCNT)]
  net.change$GID <- unique(yy$GID)
  names(net.change)[1] <- "NET.SHIFT"
  yy <- copy(merge(yy,net.change, by = "GID"))
  
  ##  Create reduced copies of the dataset for only units that we predicted growth
  ##  for:
  yy_urr <- copy(yy[!is.na(URR.MOD.TAG) & URR.MOD.TAG != "" & 
                      GID %in% unique(yy[NET.SHIFT>=0]$GID),])
  yy_bspdbar <- copy(yy[!is.na(BSPDBAR.MOD.TAG) & BSPDBAR.MOD.TAG != "" &
                          GID %in% unique(yy[NET.SHIFT>=0]$GID),])
  
  
  ##  Merge the URR model tag info and save a copy of the shapefile in the 
  ##  derived folder:
  unique_yy_urr <- unique(yy_urr[,.(GID,URR.MOD.TAG)])
  unique_yy_bspdbar <- unique(yy_bspdbar[,.(GID,BSPDBAR.MOD.TAG)])
  modin_shp <- merge(shp,unique_yy_urr, by = "GID")
  modin_shp <- merge(modin_shp, unique_yy_bspdbar, by = "GID")
  st_write(modin_shp, 
           dsn = paste0(root,
                        "BSGMe/Observed Input Validation/prj_2010-2015_",
                        i,"/derived/",i,"_Observed_URR_BSPDBAR_Model_Class.shp"),
           layer = paste0(i,"_Observed_URR_BSPDBAR_Model_Class"),
           driver = "ESRI Shapefile",
           delete_dsn = T)
  
  
  
  pdf(file = paste0(root,"BSGMe/figures/",i,"_OBS_Input_Typology_Exploration_BSGMe.pdf"),
      width = 7.5,height = 5,
      onefile = T,paper = "a4")
  print(ggplot(yy_urr[YEAR %in% {2000:2010}],
         aes(x=YEAR, y = BSPOP, group = GID))+
    geom_line(alpha = 0.5, size =0.5, color = "grey50")+
    geom_line(data = yy_urr[YEAR >=2011],
              aes(x=YEAR, y = BSPOP, group = GID),
              alpha = 0.25, size =0.5, color = "red",
              inherit.aes = F)+
    theme_bw()+
    facet_wrap(~URR.MOD.TAG,scales="free_y")+
    ggtitle("Obs. Time Series BSPOP by URR MOD TAG"))
  
  print(ggplot(yy_bspdbar[YEAR %in% {2000:2010}],
         aes(x=YEAR, y = BSPDBAR, group = GID))+
    geom_line(alpha = 0.5, size =0.5, color = "grey50")+
    geom_line(data = yy_bspdbar[YEAR >=2011],
              aes(x=YEAR, y = BSPDBAR, group = GID),
              alpha = 0.25, size =0.5, color = "red",
              inherit.aes = F)+
    theme_bw()+
    facet_wrap(~BSPDBAR.MOD.TAG,scales="free_y")+
    ggtitle("Obs. Time Series BSPDBAR by BSPDBAR MOD TAG"))
  
  ##  Zoomed versions  ----
  # print(ggplot(yy_urr[YEAR %in% {2000:2010}],
  #        aes(x=YEAR, y = BSPOP, group = GID))+
  #   geom_line(alpha = 0.5, size =0.5, color = "grey50")+
  #   geom_line(data = yy_urr[YEAR >=2011],
  #             aes(x=YEAR, y = BSPOP, group = GID),
  #             alpha = 0.25, size =0.5, color = "red",
  #             inherit.aes = F)+
  #   coord_cartesian(ylim=c(0,50000))+
  #   facet_wrap(~URR.MOD.TAG))
  # 
  # print(ggplot(yy_bspdbar[YEAR %in% {2000:2010}],
  #        aes(x=YEAR, y = BSPDBAR, group = GID))+
  #   geom_line(alpha = 0.5, size =0.5, color = "grey50")+
  #   geom_line(data = yy_bspdbar[YEAR >=2011],
  #             aes(x=YEAR, y = BSPDBAR, group = GID),
  #             alpha = 0.25, size =0.5, color = "red",
  #             inherit.aes = F)+
  #   coord_cartesian(ylim=c(0,150))+
  #   facet_wrap(~BSPDBAR.MOD.TAG, scales = "free_y"))
  
  
  ##  Scatterplots by model fit:
  ##  Initital scatter of BS pop v pop density in 2000:
  print(ggplot(xx_urr[YEAR == 2000],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("OBS. Scatter BSPOP v BSPDBAR 2000 by URR MOD TAG"))
  
  ##  Scatter of BS pop v pop density prior to projections in 2010:
  print(ggplot(xx_urr[YEAR == 2010],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("OBS. Scatter BSPOP v BSPDBAR 2010 by URR MOD TAG"))
  
  
  print(ggplot(xx_bspdbar[YEAR == 2000],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("OBS. Scatter BSPOP v BSPDBAR 2000 by BSPDBAR MOD TAG"))
  
  ##  Scatter of BS pop v pop density prior to projections in 2010:
  print(ggplot(xx_bspdbar[YEAR == 2010],
         aes(x=BSPDBAR, y = BSPOP, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_point(alpha = 0.5)+
    ggtitle("OBS. Scatter BSPOP v BSPDBAR 2010 by BSPDBAR MOD TAG"))
  
  
  
  ##  Trajectory of bs pop between 2000 and 2010:
  print(ggplot(xx_urr[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    facet_wrap(~URR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("OBS. Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  print(ggplot(xx_urr[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPOP, group = GID, color = URR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    #coord_cartesian(ylim=c(0))+
    facet_wrap(~URR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("OBS. Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  
  print(ggplot(xx_bspdbar[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPDBAR, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    facet_wrap(~BSPDBAR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("OBS. Trajectory BSPOP 2000-2010 by URR MOD TAG"))
  
  
  print(ggplot(xx_bspdbar[YEAR %in% c(2000,2010)],
         aes(x=YEAR, y = BSPDBAR, group = GID, color = BSPDBAR.MOD.TAG))+
    geom_line(alpha = 0.5, size = 0.5)+
    #coord_cartesian(ylim=c(0))+
    facet_wrap(~BSPDBAR.MOD.TAG)+
    theme_bw()+
    scale_x_continuous(breaks = c(2000,2010))+
    ggtitle("OBS. Trajectory BSPOP 2000-2010 by BSPDBAR MOD TAG"))
  dev.off()
  
  
  bspopper_diff <- copy({{xx[,.(BSPOP)]-yy[,.(BSPOP)]}/yy[,.(BSPOP)]}*100)
  bspopper_diff$YEAR <- xx$YEAR
  bspopper_diff$GID <- xx$GID
  bspopper_diff$BSCNT <- yy$BSCNT
  names(bspopper_diff)[1] <- "BSPOP.PER.DIFF"
  
  ggplot(bspopper_diff[BSCNT>=10],
         aes(x=YEAR, y= BSPOP.PER.DIFF,
             color=log(BSCNT)))+
    geom_point(alpha=0.5)+
    theme_bw()+
    coord_cartesian(ylim=c(-100,150))
  
  ggplot(bspopper_diff,aes( y = BSPOP.PER.DIFF, x = YEAR,group = YEAR))+
    geom_boxplot(outlier.shape = NA)+
    theme_bw()+
    coord_cartesian(ylim=c(-100,150))
  
  bspop_diff <- copy(xx[,.(BSPOP)]-yy[,.(BSPOP)])
  bspop_diff$YEAR <- xx$YEAR
  bspop_diff$GID <- xx$GID
  bspop_diff$BSCNT <- yy$BSCNT
  names(bspop_diff)[1] <- "BSPOP.DIFF"
  
  ggplot(bspop_diff,aes( y = BSPOP.DIFF, x = YEAR,group = YEAR))+
    geom_boxplot(outlier.shape = NA)+
    theme_bw()+
    coord_cartesian(ylim=c(-5000,7500))
  
  ggplot(bspop_diff[BSCNT>=10],
         aes(x=YEAR, y= BSPOP.DIFF,
             color=log(BSCNT)))+
    geom_point(alpha=0.5)+
    theme_bw()
  
  
  
  avgbspopdiff <- copy(bspop_diff[, AVG.BSPOP.DIFF := mean(BSPOP.DIFF,na.rm=T),
                             by=GID])
  avgbspopdiff[,BSPOP.DIFF:=NULL]
  avgbspopdiff[,YEAR := NULL]
  avgbspopdiff<- unique(avgbspopdiff)
  medbspopdiff <- copy(bspop_diff[, MED.BSPOP.DIFF := median(BSPOP.DIFF,na.rm=T),
                             by=GID])
  medbspopdiff[,BSPOP.DIFF:=NULL]
  medbspopdiff[,YEAR := NULL]
  medbspopdiff<- unique(medbspopdiff)
  bspopdiff_shp <- merge(shp, avgbspopdiff, by = "GID")
  bspopdiff_shp <- merge(shp, medbspopdiff, by = "GID")
  # st_write(modin_shp, 
  #          dsn = paste0(root,
  #                       "BSGMe/Observed Input Validation/prj_2010-2015_",
  #                       i,"/derived/",i,"_Observed_URR_BSPDBAR_Model_Class.shp"),
  #          layer = paste0(i,"_Observed_URR_BSPDBAR_Model_Class"),
  #          driver = "ESRI Shapefile",
  #          delete_dsn = T)
  
  
}



##  SPATIAL MODEL FIT MAPS BY TYPE  ----

