require(raster)
require(sf)
require(rgdal)

root <- "E:/Research/BSGMe/"
iso3 <- c("UGA")

ensure_dir <- function(d){
  ##  Function for ensuring that a directory exists and creating it if it does 
  ##  not; returns the path of the input path
  if(!dir.exists(d)){
    dir.create(d)
  }
  return(d)
}

gdal_polygonizeR <- function(x, 
                             outshape=NULL, 
                             gdalformat = 'ESRI Shapefile',
                             pypath=NULL, 
                             readpoly=TRUE, 
                             quiet=TRUE){
  
  ## gdal_polygonizeR
  ## Getting rasters into shape from R
  ## John Baumgartner's Research
  ## https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
  ##
  ##  This function turns a raster into a polygon via gdal
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}





outdir <- ensure_dir(paste0(root,"PolyExtents/"))
years <- 2011:2015
#years <- 2010
##  Modeled Observed True
#inseries <- "True"
inseries <- "Observed"
for(i in iso3){
  for(y in years){
    if(inseries != "True"){
      ##  Get the specific layer:
      foo_ras <- paste0(root,inseries,
                               " Input Validation/prj_2010-2015_",i,
                               "/BSGM_Extentsprj_2010-2015_",i,"_",y,
                               ".tif")
    }else{
      foo_ras<-paste0(root,"data/",i,"/",tolower(i),
                      "_grid_100m_esa_cls190_",y,".tif")
      }
    
    ##  Polygonize:
    gdal_polygonizeR(foo_ras,
                     outshape=paste0(outdir,
                                     strsplit(basename(foo_ras),
                                              ".tif")[[1]],"_",
                                     inseries,"_.shp"))
  }
}

##  Get a list of all shapefiles in the output folder:
shplist <- Sys.glob(paste0(outdir,"*.shp"))

for(s in shplist){
  lyr <- strsplit(basename(s),".shp")[[1]]
  
  foo <- st_read(s,layer = lyr,stringsAsFactors = F)
  
  ## Remove all features with DN of zero
  foo1 <- foo[foo$DN==1,]
  st_write(foo1,dsn=s,layer=lyr,driver = "ESRI Shapefile",delete_dsn = T,delete_layer = T)
  
}
