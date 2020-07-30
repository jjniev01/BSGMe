##  The credentials used to access the FTP repository:
bsgm.ftp.username <-"wpftp"
bsgm.ftp.password <-"rt45te3gZQ"

##  Create log statements?:
bsgm.DologDEBUG <- TRUE

## If zonal stats was calculated before then overwrite it if TRUE
bsgm.overwrite.compiled.covariates <- FALSE

## Variable name of the inland water mask
bsgm.water.mask <- "level1_water"

## var name of the region mask
bsgm.ccidadminl1 <- "ccidadminl1"

##  var name of the pixel area raster:
bsgm.pxarea <- "px_area"

##  Declare if we want the RF quant output:
bsgm.input.quant.output <- FALSE


##	Set the path to the system variable which points to the python.exe file:
bsgm.python_path <- "python"

##  Calculate Zonal stats using R default if FALSE or Python if kPythonZonal 
##  is TRUE:
bsgm.kPythonZonal <- FALSE

##  Save the zonal stats as a table?:
bsgm.saveZonalStats <- TRUE

##  Specify a path to gdal_merge.py
##  EXAMPLE: On some system you should do 
##           gdal_merge_path <- "python  /local/software/gdal/1.10.1/gcc/bin/gdal_merge.py "
##  NOTE:  These paths hsould only need to be changed for first time setup on a
##         machine.
##  Note: PErsonal laptop has GDAL in \\Program Files\\ while work comp has it in \\Program Files (x86)\\
bsgm.gdal_gdalwarp_path <- paste0("/local/software/gdal/1.10.1/gcc/bin/gdalwarp\"")
bsgm.gdal_merge_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_merge.py\"",spr="")
bsgm.gdal_calc_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_calc.py\"",spr="")
bsgm.gdal_polygonize_path <- paste("/local/software/gdal/1.10.1/gcc/bin/gdal_polygonize.py\"",spr="")

##  Declare the packages needed so they can be loaded using the LoadPackages() 
##  script:
bsgm.pkgs <- c("rgdal",
               "plyr",
               "dplyr",
               "raster", 
               "randomForest", 
               "quantregForest", 
               "foreign",
               "MASS",
               "snow",
               "doParallel",
               "gdalUtils",
               "logging",
               "doSNOW",
               "RCurl",
               "data.table",
               "tseries",
               "forecast")




##	RF CONFIGURATION  ----------------------------------------------------------
##
##	Configuration options for RandomForest modeling and prediction.
##  If we are using a set of covariates from another country set the 
##		fixed_set variable to specify which will then be used to fix the 
##		covariate list to an existing randomForest object, otherwise, 
##		use the full set from the covariate metadata if it is NULL.
##
##    Note that the fixed_set flag also changes the way that the 
##		randomForest object is created below by eliminating the variable 
##		elimination routine:
if (!exists("bsgm.fixed.set")) {
  bsgm.fixed.set <- NULL
}

##  Set the expected folder location where old RFs should be found and or 
##  downloaded to for combination and use in creating the extrapolation 
##  probability surface:
bsgm.data.old.popfits.final <- paste0(root_path, 
                                      "/data/old_bsgmfits/bsgmfits_final/")
bsgm.data.old.popfits.quant <- paste0(root_path, 
                                      "/data/old_bsgmfits/bsgmfits_quant/")


##	You can control whether you want covariates re-aggregated/summarized
##		by setting this flag to TRUE or FALSE:
bsgm.overwrite_compiled_covariates <- FALSE

##	You can control whether or not we should estimate or combine Random
##		Forest models (init_popfit, popfit, popfit_combined, popfit_final, and
##		the quantile output) by setting this flag to TRUE or FALSE.  This
##		is useful when we don't want to run the RF code at all for new
##		countries, but instead just want to use an existing popfit_final.RData
##		and popfit_quant.RData that we copied into the /output/XXX/tmp folder
##		for the current country:
##  Warning:  For the extrapolation model, we will never be estimating the RF
##            because we lack future BS extents to determine transitions upon 
##            which we can train the RF model.
bsgm.estimate_RF <- FALSE




##	FIXED PARAMETERS AND DEFAULTS  ---------------------------------------------
bsgm.proj4str_gcs_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

bsgm.cores = parallel:::detectCores()
##  Set the number of cores to use in parallel operations:
#bsgm.cluster_workers <- bsgm.cores
bsgm.cluster_workers <- bsgm.cores[1]-1
#bsgm.cluster_workers <- bsgm.cores[1]-5

# if bsgm.minblocks  <- NULL then minblocks for cluster prediction parallesation 
# will be calculated based on available memory 
# see function get_minblocks_rf_prd in internal_function.R file
bsgm.minblocks  <- NULL


####
##  NOTICE:  IN PRACTICE NOTHING BELOW THIS LINE SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
####
bsgm.gdal_merge <- paste(bsgm.python_path, bsgm.gdal_merge_path,spr=" ")
bsgm.gdal_calc <- paste(bsgm.python_path, bsgm.gdal_calc_path,spr=" ")
bsgm.gdal_polygonize <- paste(bsgm.python_path, bsgm.gdal_polygonize_path,
                              spr=" ")

logindicator = TRUE