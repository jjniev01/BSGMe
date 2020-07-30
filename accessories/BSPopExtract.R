##  Author: Jeremiah J. Nieves
##  Last Update: 2019-01-31
##  Title:  Built-Settlement Population Extract
##  License: MIT
##  Notes:
##  
####


require(data.table)
require(raster)

root <- "E:/Research/BSGMe/"


iso3 <- "UGA"
years <- c(2000,2005,2010)

for(year in years){
  bs_file_name <- paste0(tolower(iso3),"_grid_100m_esa_cls190_", year)
  
  outfilename <- paste0(iso3,"_BSPOPExtract_",year,".csv")
  
  zoneras <- raster(paste0("E:/Research/BSGMi1a/data/",iso3,
                           "/",tolower(iso3),"_grid_100m_ccidadminl1.tif"))
  inpopras <- raster(paste0("E:/Research/BSGMi1a/data/",iso3,
                            "/Pop/ppp_prj_",year,"_",iso3,".tif"))
  bsras <- raster(paste0("E:/Research/BSGMi1a/data/",iso3,
                         "/",bs_file_name,".tif"))
  
  
  
  
  ##  Get non NA indices of the zonal ID:
  nonna_ind <- which(!is.na(values(zoneras)))
  ##  Retrieve the GIDs
  ind_gids <- extract(zoneras,nonna_ind)
  ##  Initiate our data.table:
  bsdt <- data.table("IND" = nonna_ind,"GID"=ind_gids)
  
  rm(ind_gids,nonna_ind)
  gc()
  
  ##  Remove water areas:
  bsdt <- bsdt[GID !=0,]
  
  ##  Extract the extent condition of the cells:
  bsdt[,BS := extract(bsras,IND)]
  
  ##  Retain only cells that are BS:
  #bsdt <- bsdt[BS == 1,]
  
  ##  Extract the corresponding population counts of those cells:
  bsdt[,POP := 0]
  bsdt[BS == 1, POP := extract(inpopras,IND)]
  
  ##  Aggregate to the sum of BSCNT and extracted BSPOP by GID:
  findt <- copy(bsdt[,.(BSCNT = sum(BS), BSPOP = sum(POP)), by = GID])
  
  ##  Bring in the population table:
  load(paste0("E:/Research/BSGMi1a/output/prj_2000-2005-2010_",iso3,
              "/tmp/population_data_prj_2000-2005-2010_",iso3,".Rdata"))
  foodt <- as.data.table(pop_df)
  ## Remove all records that are not our record of interest:
  foodt <- foodt[YEAR == year,]
  setkey(foodt,GID)
  ##  Merge with the BS information prior to saving:
  foo <- merge(findt,foodt,by="GID",all.x=T)
  
  
  write.csv(foo,file = paste0(root,outfilename), row.names = F)
}
