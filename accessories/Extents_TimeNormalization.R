require(raster)

root <- "C:/Users/Wheesky/Desktop/"
outroot <- paste0(root,"VNM_tmp/")
data_tag <- "ESA"
yearseq <- 2000:2014
iso3 <- c("VNM")

for(i in iso3){
  print(paste0("Working on: ", i))
  ##  Get the initial extents:
  init_ext_path <- Sys.glob(paste0(outroot, tolower(i),
                                   "_grid_100m_esa_cls190_",min(yearseq),
                                   ".tif"))[[1]]
  ##  Get the list of extents for the given country and years:
  # fpath_list <- Sys.glob(paste0(root, data_tag," Input Validation/prj_",
  #                               min(yearseq),"-",max(yearseq),"_",i,"/BSGM_*.tif"))
  fpath_list <- Sys.glob(paste0(outroot,tolower(i),
                                "_grid_100m_esa_cls190_",
                                "*.tif"))
  fpath_list <- grep(paste0(".*_(?:",
                            paste(yearseq[yearseq!= min(yearseq)],
                            collapse="|"),
                     ")[.]tif"),
                     fpath_list,
                     value = T)
  ##  Get the BSGMe modeled extants in there too
  foo_bsgme <- Sys.glob(paste0(outroot,
                               "BSGM_Extentsprj_2010-2015_*.tif"))
  fpath_list <- c(fpath_list,foo_bsgme)
  ##  Start with a copy of the initial extent raster and change its urban area 
  ##  values to the year it represents:
  norm_ras <- raster(init_ext_path)
  init_urb_inx <- which(norm_ras[]==1)
  norm_ras[init_urb_inx] <- min(yearseq)
  
  ##  For the rest of the years:
  for(y in 1:length(yearseq[yearseq!= min(yearseq)])){
    year <- yearseq[yearseq!= min(yearseq)][y]
    
    print(paste0("     Normalizing for year: ", year))
    ##  Get the indices of the urban areas for that year:
    foo_inx <- which(values(raster(fpath_list[y]))==1)
    
    ##  Only retain those indices which are not in the original indices or prior
    ##  iterations:
    foo_inx <- setdiff(foo_inx,init_urb_inx)
    ##  Add them to the initial urb index holder to carry to the next iteration:
    init_urb_inx <- c(init_urb_inx,foo_inx)
    
    ##  Modify the rastr values for those indices:
    norm_ras[foo_inx] <- year
  }
  
  print(paste0("     Writing to: ", outroot))
  ##  Save the raster to file:
  writeRaster(norm_ras,
              filename = paste0(outroot, i, "_BSGMe_TimeNormalized_", data_tag,"_Input_",
                                min(yearseq),"-",max(yearseq),".tif"),
              format = "GTiff",
              datatype = "INT4U",
              options = c("COMPRESS = LZW"),
              overwrite = T)
}
