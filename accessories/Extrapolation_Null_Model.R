require(raster)
require(data.table)
require(sf)
source("E:/Research/BSGMe/accessories/Geoprocessing_Functions.R")


ensure_dir <- function(d){
  ##  Function for ensuring that a directory exists and creating it if it does 
  ##  not; returns the path of the input path
  if(!dir.exists(d)){
    dir.create(d)
  }
  return(d)
}


####  COMPARISON RASTER CREATION  ----
##  Takes observed binary rasters at time one and time two and creates a 
##  contingency matrix under the null model of no land cover change.
root <- "E:/Research/BSGMe/"
iso <- c("CHE","PAN","UGA","VNM")
model_tag <- "2000-2005-2010"
val_tag <- "Observed Input Validation"
data_tag <- "esa_cls190"
t0_year <- 2010
year_tag <- "2010-2015"
comp_years <-c(2011,2012,2013,2014,2015)

for(i in iso){
  ##  Directory where observed extents should be:
  obsdir <- paste0(root, "data/",i,"/")
  preddir <- paste0(root,val_tag,"/prj_",year_tag,"_",i,"/")
  ##  Directory for output:
  outdir <- ensure_dir(paste0(preddir,"derived/"))
  
  ##  The GID raster path:
  regfile_path <- paste0(obsdir, tolower(i),"_grid_100m_ccidadminl1.tif")
  ##  GID raster:
  region_ras <- raster(regfile_path)
  ##  Get the number of cells for every GIE:
  tot_cells_dt <- as.data.table(freq(region_ras))
  ##  Format the table and remove water (GID == 0) and NA areas:
  names(tot_cells_dt) <-c("GID","N.TOT")
  tot_cells_dt <- copy(tot_cells_dt[!is.na(GID) & GID != 0,])
  ##  Determine which cell indices are valiE:
  valid_cid <- intersect(which(values(region_ras) != 0),
                         which(!is.na(values(region_ras))))
  
  ##  Bring in the null "predicted" raster:
  pred_ras <- raster(paste0(root,"data/",i,"/",tolower(i),"_grid_100m_",
                data_tag,"_",t0_year,".tif"))
  ##  Retrieve the indices of the predicted raster whose value is equal to 1:
  pred_1_ind <- which(values(pred_ras)==1) 
  pred_1_ind <- pred_1_ind[pred_1_ind %in% valid_cid]
  
  
  ##  For every year:
  for(y in comp_years){
    print(paste0("Working on ",i," ",y, " ", Sys.time()))
    ##  If the shapefile doesn't already exist:
    if(!file.exists(paste0(obsdir, tolower(i),
                           "_grid_100m_ccidadminl1.shp"))){
      ##  Create it:
      gdal_polygonizeR(paste0(obsdir,
                              tolower(i),"_grid_100m_ccidadminl1.tif"),
                       outshape = paste0(obsdir,
                                         tolower(i),"_grid_100m_ccidadminl1.shp"))
      ##  Kludge to remove supposed improper geometries:
      foo_shp <- st_buffer(st_read(dsn=paste0(obsdir,
                                              tolower(i),"_grid_100m_ccidadminl1.shp"),
                                   layer=paste0(tolower(i),"_grid_100m_ccidadminl1")),
                           0.0)
      
      ##  Select all shapes that don't have the GID (DN) of zero which is water,
      ##  group the records by their GID, and then dissolve the boundaries by 
      ##  GID so there is one feature per GIE:
      foo_shp<- foo_shp[foo_shp$DN!=0,] %>% 
        group_by(DN) %>%
        summarise(geometry = sf::st_union(geometry)) %>%
        ungroup()
      ##  Rename "DN" to GIE:
      names(foo_shp)[1] <- "GID"
      
      ##  If all the geometries are valid, write it out to disk:
      if(all(st_is_valid(foo_shp))){
        st_write(foo_shp, 
                 dsn = paste0(obsdir,tolower(i),
                              "_grid_100m_ccidadminl1.shp"),
                 layer = paste0(tolower(i),
                                "_grid_100m_ccidadminl1"),
                 driver = "ESRI Shapefile",
                 delete_dsn = TRUE)
      }else{
        stop("All geometries are not valid")
      }
    }
    
    ##  Load in shapefile:
    shp <- read_sf(paste0(obsdir,tolower(i),
                          "_grid_100m_ccidadminl1.shp"),
                   layer = paste0(tolower(i),
                                  "_grid_100m_ccidadminl1"),
                   stringsAsFactors = F)
    ##  Ensure that the identifying field name is GID
    names(shp)[1] <- "GID"
    ##  For some reason, when the shapefile is read in, it says there are 
    ##  various invalid geometries even with our check prior to writing to disk;
    ##  however, adding a buffer of zero then makes all of them valid.
    shp <- st_buffer(shp, 0.0)
    
    ##  Get the binary files of interest both predicted and observeE:
    obsfile_path <- paste0(obsdir, tolower(i), "_grid_100m_",data_tag,"_",y,".tif")
    
    
    ##  Read in the raster data:
    obs_ras <- raster(obsfile_path)
    
    ##  Determine total number of cells that are not NA (should all be the same,
    ##  but just in case I'll grab the minimum):
    tot_valid_cells <- min(length(valid_cid),
                           length(!is.na(values(obs_ras))),
                           length(!is.na(values(pred_ras))))
    
    ##  Retrieve the indices of the observed raster whose value is equal to 1:
    obs_1_ind <- which(values(obs_ras)==1)
    obs_1_ind <- obs_1_ind[obs_1_ind %in% valid_cid]
    ##  Retain only the indices that are not in the pred_1_ind so we don't 
    ##  double count:
    obs_1_ind <- obs_1_ind[!(obs_1_ind %in% pred_1_ind)]
    
    
    ##    Extract the GID of the Region Raster, the Predicted Value, and the 
    ##    Observed Value and create a data.table:
    foo_table <- data.table("CID" = pred_1_ind,
                            "GID" = region_ras[pred_1_ind],
                            "PRED.NULL" = pred_ras[pred_1_ind],
                            "OBS.NULL" = obs_ras[pred_1_ind])
    
    foo_2 <- data.table("CID" = obs_1_ind,
                        "GID" = region_ras[obs_1_ind],
                        "PRED.NULL" = pred_ras[obs_1_ind],
                        "OBS.NULL" = obs_ras[obs_1_ind])
    
    ##  Combine the two:
    foo_table <-rbind(copy(foo_table), copy(foo_2))
    
    ##  Make sure no GID == 0 is not in the table:
    foo_table <- copy(foo_table[GID != 0,])
    
    ##  Calculate the number of TP, FP, TN, FN:
    foo_table[,c("TP.NULL","FP.NULL","TN.NULL","FN.NULL") := 0]
    foo_table[OBS.NULL == 1 & PRED.NULL == 1, TP.NULL := 1]
    foo_table[OBS.NULL == 1 & PRED.NULL == 0, FN.NULL := 1]
    foo_table[OBS.NULL == 0 & PRED.NULL == 1, FP.NULL := 1]
    # foo_table[OBS == 0 & PRED == 0, TN := 1]
    ##  Add a special row with the inferred total number of TN in the country:
    foo_table <- rbind(copy(foo_table),
                       list(99999999,99999999,0,0,0,0,
                            {tot_valid_cells - sum(foo_table$TP.NULL,
                                                   foo_table$FP.NULL,
                                                   foo_table$FN.NULL,
                                                   na.rm = T)},
                            0))
    
    ##  Add an identifying year column:
    foo_table[,YEAR := y]
    
    
    ##  Save the pixel level contingency data:
    saveRDS(foo_table, 
            file = paste0(outdir, i,"_",data_tag,
                          "NULLMODEL_pixel_level_contingency_data_",y,".RDS"))
    
    ##  Copy the pixel level data table to then aggregate it:
    aggtab <- copy(foo_table)
    ##  Drop the CID and GID and YEAR columns from the aggregate table prior to
    ##  aggregation:
    aggtab[,c("CID","GID","YEAR") := NULL]
    
    ##  Aggregate to the subnational unit level and join to the shapefile we 
    ##  brought in earlier:
    aggtab <- as.data.table(aggregate.data.frame(aggtab,
                                                 by = list(foo_table$GID),
                                                 simplify = F,
                                                 FUN = sum, 
                                                 na.rm = T))
    names(aggtab)[1] <- "GID"
    
    ##  Drop the special row which has the TN:
    aggtab <- copy(aggtab[GID != 99999999])
    
    ##  Convert the column types to numeric as they are coming out as class list 
    ##  from aggregate:
    aggtab$PRED.NULL <- as.numeric(aggtab$PRED.NULL)
    aggtab$OBS.NULL <- as.numeric(aggtab$OBS.NULL)
    aggtab$TP.NULL <- as.numeric(aggtab$TP.NULL)
    aggtab$FP.NULL <- as.numeric(aggtab$FP.NULL)
    aggtab$TN.NULL <- as.numeric(aggtab$TN.NULL)
    aggtab$FN.NULL <- as.numeric(aggtab$FN.NULL)
    ##  Merge the Total cell data frame with the aggregate data:
    aggtab <- merge(copy(aggtab),copy(tot_cells_dt), by = "GID")
    ##  Derive the TN for each GIE:
    aggtab[,TN.NULL := {N.TOT-TP.NULL-FP.NULL-FN.NULL},by=GID]
    
    ##  Add a year column:
    aggtab$YEAR <- y
    ##  Return to a data.table format:
    aggtab <- as.data.table(aggtab)
    ##  Calculate derived metrics from the TP,FP,FN,TN:
    aggtab[, RECALL.NULL := TP.NULL/{TP.NULL+FN.NULL}, 
           by = list(GID)]
    aggtab[, PRECISION.NULL := TP.NULL/{TP.NULL+FP.NULL}, 
           by = list(GID)]
    aggtab[, F1.NULL := {2*(PRECISION.NULL*RECALL.NULL)/(PRECISION.NULL+RECALL.NULL)}, 
           by = list(GID)]
    aggtab[, SPECIFICITY.NULL := {TN.NULL/(FP.NULL+TN.NULL)}, 
           by = list(GID)]
    aggtab[, OVERACC.NULL := {{TN.NULL+TP.NULL}/(TP.NULL+FN.NULL+FP.NULL+TN.NULL)}, 
           by = list(GID)]
    aggtab[, QUANTDIS.0.NULL := {abs({FN.NULL-FP.NULL}/(TP.NULL+FP.NULL+FN.NULL+TN.NULL))}, 
           by = list(GID)]
    aggtab[, QUANTDIS.1.NULL := {abs({FP.NULL-FN.NULL}/(TP.NULL+FP.NULL+FN.NULL+TN.NULL))}, 
           by = list(GID)]
    aggtab[, QUANTDIS.NULL := {{QUANTDIS.0.NULL+QUANTDIS.1.NULL}/2}, 
           by = list(GID)]
    aggtab[, ALLOCDIS.0.NULL := {2*min({FP.NULL/{TP.NULL+TN.NULL+FP.NULL+FN.NULL}},
                                       {FN.NULL/{TP.NULL+TN.NULL+FP.NULL+FN.NULL}})},
           by = list(GID)]
    aggtab[, ALLOCDIS.1.NULL := {2*min({FN.NULL/{TP.NULL+TN.NULL+FP.NULL+FN.NULL}}, 
                                       {FP.NULL/{TP.NULL+TN.NULL+FP.NULL+FN.NULL}})},
           by = list(GID)]
    aggtab[, ALLOCDIS.NULL := {ALLOCDIS.0.NULL+ALLOCDIS.1.NULL}/2, 
           by = list(GID)]
    aggtab[, PROP.CORRECT.NULL := {{TN.NULL+TP.NULL}/{TP.NULL+TN.NULL+FP.NULL+FN.NULL}}, 
           by = list(GID)]
    
    ##  Save the aggregate table as an RDS object:
    saveRDS(data.table(aggtab), 
            file = paste0(outdir, i,"_",data_tag,
                          "NULLMODEL_admin_level_contingency_data_",y,".RDS"))
    
    ##  Merge the aggegate table with the shapefile:
    aggshp <- merge(shp, as.data.frame(aggtab), by = "GID")
    
    ##  Pull in the null model for the country and all years:
    # null_data <- readRDS(Sys.glob(paste0(outdir,
    #                                      "*_NullBootbyGIDandYear_500_",year_tag,".RDS")))
    
    ##  Join the null data to the contingency data:
    # aggshp<- merge(aggshp, as.data.frame(null_data[YEAR==y,]),
    #                by = "GID", all.x=T)
    
    ##  Write the shapefile with the data:
    if(all(st_is_valid(aggshp))){
      st_write(aggshp,
               dsn = paste0(outdir,i,"_",data_tag,"NULLMODEL_contingency_data_",y,".shp"),
               layer = paste0(i,"_",data_tag,"NULLMODEL_contingency_data_",y),
               driver = "ESRI Shapefile",
               delete_dsn = TRUE,delete_layer=TRUE)
    }else{stop("Aggshp geometries are not all valid.")}
  }
  ##  Combine all annual datasets into a single dataset for each country:
  outdir <- ensure_dir(paste0(preddir,"derived/"))
  
  ##  Aggregate all the annual datasets into a single data.table
  dts <- Sys.glob(paste0(outdir, i,"_",data_tag,
                         "NULLMODEL_admin_level_contingency_data_*.RDS"))
  dts <- grep(".*data_[0-9]{4}[.]RDS",dts,value = T)
  for(d in 1:length(dts)){
    if(d ==1){
      dt <- readRDS(file = dts[d])
    }
    if(d!=1){
      dt <- rbind(dt, readRDS(file=dts[d]))
    }
  }
  saveRDS(dt,file=paste0(outdir, i,"_",data_tag,
                         "NULLMODEL_admin_level_contingency_data_",year_tag,".RDS"))
}
