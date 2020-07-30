##  TITLE:  BUILT-SETTLEMENT GROWTH MODEL (BSGM) V1e - EXTRAPOLATION
##  AUTHORS:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  LICENSE: MIT
##  DATE:  2018-04-18
##  NOTES:
##
##
##
####

##  BEGIN:  GENERAL SETTINGS AND CONFIGURATIONS  -------------------------------
##  Remove all objects from memory to have a clean run:
rm(list=ls(all=TRUE)) 

##  Define the root path; this should be set to the folder containing the 
##  primary directory tree of the model folder hierarchy:
root_path <- "E:\\Research\\BSGMe"


####
##  NOTICE:  IN PRACTICE NOTHING BELOW THIS LINE SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
####


##  Load the model input script and get an idea about what countries we are 
##  working with:
source(paste0(root_path,"/inpute.R"))
bsgm.nb.countries <- as.integer(length(bsgm.input.countries))

##  Assign the proper covariate name for retrieval in primary script similar 
##  to how we download the watermask and the level 1 rasters:
# bsgm.input.cvr <- c(bsgm.input.cvr, bsgm.t0.extents)
#print(bsgm.input.cvr)

##  Set the random seed:
set.seed(1964)

##  TODO:  ENSURE ALL NECESSARY MODULES ARE SOURCED
##  Load all our other "modules" which do the heavy lifting for this script:
source(paste0(root_path,"/confige.R"))
source(paste0(root_path,"/modules/load_Packages.R"))
source(paste0(root_path,"/modules/check_config_input_extp.R"))
source(paste0(root_path,"/modules/create_dirs_for_prj_extp.R"))
source(paste0(root_path,"/modules/internal.R"))
source(paste0(root_path,"/modules/download_covariates_extp.R"))
source(paste0(root_path,"/modules/mrg_rst_cvr_countries.R"))
source(paste0(root_path,"/modules/rf_functions.R"))
source(paste0(root_path,"/modules/cluster_predict.R"))
source(paste0(root_path,"/modules/temporal_functions.R"))
source(paste0(root_path,"/modules/transitionTools.R"))

if (!load.Packages()){
  stop("There was an error when loading R packages")
}

##  Check the configuration options:
##    Function is sourced from check_config_input.R
if (!check_config_input()){
  stop("There was an error in your input or configuration!")
}


##  Determine our sequence of years we are modeling with (in numeric form):
year_seq <- seq(as.numeric(t0), as.numeric(t1), by = 1)

##  Time frame the model will represent (in string form):
bsgm.timeframe <- paste0(t0,'-',t1)


##  Start the setup of the project directories for this model run.
##    Function is sourced from create_dirs_for_prj.R
glPaths <- create_dirs_for_prj()  

##  Get the paths to the countries' data:
bsgm.data.path.countries <- glPaths$data

##  Declare where we are outputting things:
bsgm.output.path.countries <- glPaths$output

##  Declare where our temporary path is:
bsgm.output.path.countries.tmp <- paste0(bsgm.output.path.countries, "/tmp/")

##  Retrieve the country tag:
##  EXAMPLE:  "prj_2000-2012_ARM_AZE_GEO"
bsgm.countries.tag <- glPaths$countries_tag

if(bsgm.fixed.set){
  bsgm.data.old.bsgmfits.final <- glPaths$data_old_bsgmfits_final
  bsgm.data.old.bsgmfits.quant <- glPaths$data_old_bsgmfits_quant
}

##  Load the module for dealing with set variable name patterns:
source(paste0(root_path,"/modules/variable_names_extp.R")) 

##  Remove unnecessary items:
rm(glPaths)


##
##  END:  GENERAL SETTINGS AND CONFIGURATIONS  ---------------------------------




##  BEGIN:  DATA IMPORTATION AND PREPROCESSING  --------------------------------
##  If a custom poptable is declared, source it:
if(!is.null(bsgm.input.poptables)){
  hist_pop<- read.csv(bsgm.input.poptables,
                      header = TRUE,
                      stringsAsFactors = FALSE)
}

##  If the historical data does not exist, and there are no declared custom BS 
##  pop tables try to download it:
if(!file.exists(paste0(root_path,"/data/",bsgm.input.countries,"/",
                       bsgm.input.countries,"_POP_BSCNT_BSPOP_",t_train,"_",
                       {t0-1},".csv")) &
   is.null(bsgm.input.poptables)){
  
  DownloadFileFromWPFTP(file_path = paste0("Covariates/",bsgm.input.countries,
                               "/BSGM/BS_Pop/",bsgm.input.countries,
                               "_POP_BSCNT_BSPOP_2000_14.csv"),
                        dest_file = paste0(root_path,"/data/",
                                           bsgm.input.countries,"/",
                                           bsgm.input.countries,
                                           "_POP_BSCNT_BSPOP_2000_14.csv"),
                        quiet = TRUE,
                        method = "auto")
}


##  Check if the required population table containing the extracted annually 
##  modeled BSPOP, BSPDBAR, BSCNT, as well as the total POP exists in the 
##  specified country's data folder:
if(file.exists(paste0(root_path,"/data/",bsgm.input.countries,"/",
                      bsgm.input.countries,"_POP_BSCNT_BSPOP_",t_train,"_",
                      {t0-1},".csv")) &
   is.null(bsgm.input.poptables)){
  hist_pop<- read.csv(paste0(root_path,"/data/",
                             bsgm.input.countries,"/",
                             bsgm.input.countries,
                             "_POP_BSCNT_BSPOP_",t_train,"_",
                             {t0-1},".csv"),
                      header = TRUE,
                      stringsAsFactors = FALSE)
}


##  Retrieving the 'future' population table data:
##  Add the population data for the predictive years:
##  If a file path to a custom poptable exists:
if(!is.null(bsgm.input.poptables)){
  ##TODO:  Define function for handling the potential intricaies of this.
  ##  See internal.R
  ##  NOTE:  The custom file must have the following information and field names 
  ##         (given in brackets)in LONG format:
  ##         [GID] - unique identifier of the spatial areas. Needs to correspond 
  ##                 to the unique identifiers in the historical data
  ##         [YEAR] - years the rows of data correspond to; must span from t0+1
  ##                  to t1 even if there are no population values, i.e. 0, 
  ##                  observed for the year in a given unit.
  ##         [POP] -  the total population count for the given unit and year
  ####
  future_pop <- loadCustomPopTable()
  
}else if(is.null(bsgm.input.poptables)){
  ##  If the file does not exist:
  if(!file.exists(paste0(root_path,"/output/",
                         bsgm.countries.tag,"/tmp/",
                         tolower(bsgm.input.countries),
                         "_population_2000_2020.csv"))){
    ##  Download it from the FTP:
    ##  Note we want the entire set of years from 2000 to 2020, but the function
    ##  requires a year as an argument for what gets returned by the function; 
    ##  the full file is still written to the specified destination directory.
    wpgpGetPOPTable(ISO3 = bsgm.input.countries, 
                    year = 2000, 
                    destDir = paste0(root_path,"/output/",
                                     bsgm.countries.tag,"/tmp/"),
                    username = bsgm.ftp.username,
                    password = bsgm.ftp.password)
  }
  ##  Read in the file and format it:
  future_pop <- read.csv(file = paste0(root_path,"/output/",
                                       bsgm.countries.tag,"/tmp/", 
                                       tolower(bsgm.input.countries),
                                       "_population_2000_2020.csv"),
                         header = T,
                         stringsAsFactors = F)
  ##  Format and subset to only the prediction years:
  future_pop <- tidyCIESIN(future_pop, times = seq({t0+1}, t1, by = 1))
  future_pop$BSCNT <- NA
  future_pop$BSPOP <- NA
}

##  Add the rows to the hist_popdata frameand make sure its sorted by GID and 
##  year:
hist_pop <- rbind(hist_pop, future_pop)
hist_pop <- hist_pop %>% dplyr::arrange(GID, YEAR)

##  Calculate the URR based upon the POP and BSPOP values and also calculate 
##  the BSPDBAR:
hist_pop <- as.data.table(hist_pop)
hist_pop[, URR:=BSPOP/(POP-BSPOP)]
# hist_pop[, BSPDBAR:=BSPOP/BSCNT]
# hist_pop[BSCNT == 0, BSPDBAR := 0]
hist_pop[, BSPDBAR:=ifelse(BSCNT==0, 0, BSPOP/BSCNT)]

##  If the last year of the input timeseries have zero BSCNT, then we will 
##  assume no growth occurs for the short term future. Perhaps future will allow
##  us to identify some average reasonable threshold that we would expect to see 
##  built settlement, but it is so data source dependent that this is 
##  realisticaly the most generalizable solution. 
hist_pop$EMPTYFLAG <- 0
hist_pop[YEAR == t0 & BSCNT == 0, EMPTYFLAG := 1]
##  JJN: 2019-10-28: Found that if BS only showed up in the last year, you would
##       get uncontrolled BSCNT growth, leading to "measles" speckeled 
##       predictions that filled up an entire admin unit unrealistically.
#foogid <- hist_pop[{YEAR == t0 & BSCNT !=0}, GID][{hist_pop[{YEAR == t0 & BSCNT !=0}, GID] %in% hist_pop[{YEAR == {t0-1} & BSCNT == 0}, GID]}]
##  For now, put it under "EMPTYFLAG" to avoid processing it later:
#hist_pop[GID %in% foogid & YEAR == t0, EMPTYFLAG := 1]

##  JJN 2019-10-30:  Also applies to those that only have tow final observations
##      particularly if the values are equal in value.
#foogid <- hist_pop[{YEAR == t0 & BSCNT !=0}, GID][hist_pop[{YEAR == t0 & BSCNT !=0}, GID][{hist_pop[{YEAR == t0 & BSCNT !=0}, GID] %in% 
#    hist_pop[{YEAR == {t0-1} & BSCNT != 0}, GID]}] %in% 
#      hist_pop[{YEAR=={t0-2} & BSCNT == 0}, GID]]
##  For now, put it under "EMPTYFLAG" to avoid processing it later:
#hist_pop[GID %in% foogid & YEAR == t0, EMPTYFLAG := 1]

##    Retrieve the GIDS of those types of areas:
empty_gid <- hist_pop[YEAR == t0  & EMPTYFLAG == 1, ]$GID

hist_pop[GID %in% empty_gid & YEAR >= t0, EMPTYFLAG := 1]
hist_pop[EMPTYFLAG == 1, BSCNT := 0]
hist_pop[EMPTYFLAG == 1, URR := 0]
hist_pop[EMPTYFLAG == 1, BSPOP := 0]

##  If the BSPOP value is equal to the POP value then the URR will be Inf which 
##  causes issues down stream. Also, in the extraction tables that David created
##  there are issues with the BSPOP values being larger than the POP values at 
##  the fourth decimal place or smaller leading to negative URR values of a 
##  large magnitude. Issue now with setting them to zero is that this causes an 
##  issue with the temporal fitting and will require later logic to handle. To 
##  aid in handling, we will introduce a flag column to identify the records 
##  with "special" zeros.
hist_pop[BSPOP >=POP, URR := NA]
hist_pop[round(BSPOP,digits = 2) == round(POP,digits = 2), URR := NA]

hist_pop$SATURATIONFLAG <- 0

hist_pop[BSPOP >=POP & YEAR <= t0, SATURATIONFLAG := 1]
hist_pop[round(BSPOP,digits = 2) == round(POP,digits = 2) & YEAR <= t0,
         SATURATIONFLAG := 1]

satthresh <-1
hist_pop[YEAR <= t0, NONNA :={1+(t0-t_train)-sum(SATURATIONFLAG)}, by=GID]
hist_pop[NONNA==satthresh & YEAR <= t0, URR := NA]
hist_pop[NONNA == satthresh & YEAR <= t0, SATURATIONFLAG :=1]

##  If the last observed date has a URR value of 0 and a SATURATIONFLAG marker,
##  We will assume that the population remains totally in built settlement into 
##  the future regardless of the changing total population (both increase and 
##  decrease). If total pop is increasing, we assume that no more expansionary 
##  development can occur, i.e. the area is saturated, and any development is 
##  related to the densification of or land use change of the established built 
##  settlement area. If total population decreases, we assume that the built-
##  settlement structures remain and remaining populaton is located entirely in 
##  built-settlement.

##    Retrieve the GIDS of those types of areas:
sat_gid <- hist_pop[YEAR == t0 & is.na(URR) & SATURATIONFLAG == 1, ]$GID
##    Set their 'future' records to 'saturated' and go ahead and prepopulate 
##    their URR and BSPOP values
hist_pop[GID %in% sat_gid & YEAR > t0, URR := NA]
hist_pop[GID %in% sat_gid & YEAR > t0, SATURATIONFLAG := 1]
hist_pop[GID %in% sat_gid & YEAR > t0, BSPOP := POP]


##  Download the covariates, which will be used for the RF prediction, to a 
##  local folder.
##  Pre allocate a list to hold all possible covariate names we will be dealing 
##  with:
covariates <- list()
covariates.var.names <- list()

##  If the census_data_<countries_tag>.Rdata file already exist locally:
if(file.exists(paste0(bsgm.output.path.countries.tmp, 
                       bsgm.countries.fln.Rdata))){
  ##  Load them:
  load(file=paste0(bsgm.output.path.countries.tmp, bsgm.countries.fln.Rdata))
  
}else{
  ##  Begin the download and check of covariates from database to /data/ folder 
  ##  using a function called from internal_functions.R module:
  covariates <- Download_Covariates()
  
  ##  Nonstandard insertion:
  for(v in bsgm.nonstand.cvr){
    covariates[[bsgm.input.countries[[1]]]][[v]] <- list("dataset_folder" = paste0(root_path, "/data/",bsgm.input.countries[[1]],"/"), 
                                                         "dataset_filename" = paste0(tolower(bsgm.input.countries[[1]]),
                                                                                     "_grid_100m_",v,".tif"),
                                                         "dataset_description" = v,
                                                         "dataset_summary" = "mean",
                                                         "dataset_country" = bsgm.input.countries[[1]],
                                                         "dataset_class" = v,
                                                         "path" = paste0(root_path, "/data/",bsgm.input.countries[[1]],"/",
                                                                         tolower(bsgm.input.countries[[1]]),"_grid_100m_",v,".tif"))
  }
}
for(v in bsgm.nonstand.cvr){
  bsgm.input.cvr[[ {length(bsgm.input.cvr) + 1} ]] <- v}


##  Download the corresponding LAN data if we are using it in reweighting:
##    Sourced from the downloadLAN.R file.
if(bsgm.LAN.weighting){
  DownloadLANRasterExtrp()
}


##  Create a list of covariates for main RF subrouting function called 
##  from mrg_rst_cvr_countries.R file:
covariates <- create_covariates_list_for_RF()

loginfo("Saving covariates for RF...")
##  Save the covariates as an RData file:
save(covariates, 
     file=paste(bsgm.output.path.countries.tmp, 
                bsgm.covariates.RF.Rdata, 
                sep="")) 

##  Make sure the correct covariates object is loaded:
load(paste0(bsgm.output.path.countries.tmp, 
            bsgm.covariates.RF.Rdata))

##  Retrieve the paths for the watermask, the census mask, the t0 extents, and
##  the t1 extents (if applicable):
watermaskPathFileName <- covariates[[bsgm.water.mask]]$path
censusmaskPathFileName <- covariates[[bsgm.ccidadminl1]]$path
t0extentsPathFileName <- paste0(root_path,"/data/",bsgm.input.countries,"/",
                                tolower(bsgm.input.countries), 
                                "_grid_100m_", bsgm.t0.extents,
                                ".tif")#covariates[[bsgm.t0.extents]]$path
pxareaPathFileName <- covariates[["px_area"]]$path


##  Remove AdminId, Watermask, t0 extents, t1 extents, and px_area info from 
##  prepared covariates list:
if(bsgm.water.mask %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.water.mask)]
}
if(bsgm.ccidadminl1 %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.ccidadminl1)]
}
if(bsgm.t0.extents %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == bsgm.t0.extents)]
}
if("px_area" %in% names(covariates)){
  covariates <- covariates[ - which(names(covariates) == "px_area")]
}


##  Prepare a single 'region_mask' which is an subtraction of the watermask from
##  the census mask so we only have to worry about applying one mask moving 
##  forward.
##  Bring in the region raster mask which is aligned with all our covariates:
region_mask <- raster(censusmaskPathFileName)

##  Check if we can carry out the entire processing and modelling in memory or 
##  if we need to break this down in parallel:
bsgm.prl <- ! canProcessInMemory(region_mask, 3)

##  If the region raster is not already all NA values or 1s, make it so:
if (bsgm.prl==FALSE){
  region_mask[!is.na(region_mask)] <- 1
}else{
  region_mask <- wpSetAllValuesTo(x=region_mask, v=1,
                                  cores=bsgm.cluster_workers, silent=F)
}


##	Load the watermask raster:
water_raster <- raster(watermaskPathFileName)

##  Go ahead and turn inland water areas within the region mask to NA so we 
##  can just have one mask we work with:
##    If we are running in memory:
if (bsgm.prl==FALSE){
  region_mask[which(getValues(water_raster)==1)] <- NA
}else{
  ##    If we are running it in parallel, source the functions to make it happen
  ##    more efficiently and change those values to NA:
  source(paste0(root_path,"/modules/check_region_mask_to_NA.R"))
  region_mask <- check_region_mask_to_NA(region_mask, water_raster)
}


##  Set up a template raster for prediction:
prediction_raster <- region_mask


##  Check if the fixed set is being used and if so, check if there are RFs to 
##  be used in the prespecified folder:
if(length(Sys.glob(paste0(bsgm.data.old.bsgmfits.final, "*.Rdata"))) < 1){
  ##  List the files held in the RF object directory of the FTP:
  rf_ftp_list <- strsplit(getURL(paste0("ftp://",
                                        bsgm.ftp.username,":",
                                        bsgm.ftp.password,
                                        "@ftp.worldpop.org.uk/WP515640_Global/Covariates/",
                                        bsgm.input.countries,"/BSGM/RF_Objects/"),
                                 dirlistonly = T),
                          "\r*\n")[[1]]
  
  for(r in rf_ftp_list){
    print(paste0("Downloading RF for ", r, " from FTP..."))
    ##  Download the RF files from the FTP location if none exist already:
    DownloadFileFromWPFTP(file_path = paste0("Covariates/",bsgm.input.countries,
                                             "/BSGM/RF_Objects/",r),
                          dest_file = paste0(bsgm.data.old.bsgmfits.final, r),
                          quiet = TRUE,
                          method = "auto")
  }
  bsgm.data.old.bsgmfits.final
}

##  Load the old bsgmfit:
#if(sum(names(covariates) != names(bsgmfit_final$forest$xlevels)) != length(names(covariates)))

##  END:  DATA IMPORTATION AND PREPROCESSING  ----------------------------------




##  BEGIN:  RANDOM FOREST PREDICTION  ------------------------------------------
##  If the prediction of transition probabilities already exists and we are not 
##  overwriting:
if(file.exists(paste0(bsgm.output.path.countries,
                      "predict_transition_base_rf_pred_",
                      bsgm.countries.tag, ".tif")) & 
   overwrite == FALSE){
  
  ##  Load in the raster:
  prob_ras <- raster(paste0(bsgm.output.path.countries,
                            "predict_transition_base_rf_pred_",
                            bsgm.countries.tag, ".tif"))
}else{
  ##  If the transition probability raster does not exist or we are overwriting,
  ##  Find and retrieve the fixed set declared RF objects that we will combine 
  ##  to then predict the future transition probabilities of the period we are 
  ##  modeling and combine them if there is more than one:
  ##  NOTE:  In the below function call we are standardizing the predictor 
  ##         variable names to not have time-data origin-specific or time-specific 
  ##         tags in the predictor names, e.g. 'ghsl_esa_dst' becomes 'bs_dst'
  ##         and 'wdpa_cat1_dst_2000' becomes 'wdpa_cat1_dst'. 
  ##          
  ##         This necesitates the changing of the variable names in the 
  ##         covariates object to match these standardized names for the 
  ##         covariate stack creation to work. 
  ##         See internal.R::alignCovariateNames() for the covariates object 
  ##         name standardization.
  ##
  ##
  ##  See in rf_functions.R.
  ##  Loads in the bsgmfit_final
  set_fixed_set_to_existing_countries()
  
  ##  Ensure proximity is nullified:
  bsgmfit_final$proximity <- NULL
  
  covariates <- alignCovariateNames()
  ##  TODO:  Put in a proper flexible name alignment function that handles both 
  ##  the covariates object and the bsgm object. May be better to have proper 
  ##  naming conventions coming out of the BSGMi functions; this will suffice 
  ##  for now as long as the order declared in the inpute.R function is the same
  ##  order as in the BSGMfit object:
  names(covariates) <- names(bsgmfit_final$forest$xlevels)
  
  ##  Get a rasterstack of covariates:
  ##  Note: Function in 
  covariate_stack <- creat_raster_stack()
  
    ##  Start the parallel prediction of the transition probability surface 
  ##  using the trained BSGM RF:
  beginCluster(n = bsgm.cluster_workers)
  
  ##  Create the probability of transition layer:
  prob_ras <- cluster_predict(prediction_raster, 
                              quant_output=FALSE, 
                              nmb=30)
  endCluster()
  
  ##  Clean up memory:
  gcQuiet(quiet = F)  
}
##  Remove unneeded objects from memory:
rm()
gcQuiet()


##
##  END:  RANDOM FOREST PREDICTION  --------------------------------------------




##  BEGIN:  FITTING OF ARIMA/ETS/logGLM BY SUBUNIT  -----------------------------------
##  Load up the table containing the previous time period data originating from
##  provided or modeled and extracted data:


##  For every subnational unit, fit a series of ARIMA/EXS models to the observed 
##  data selecting the final model based upon the Bayesian Information Criterion 
##  (BIC) value. The parameters needed to replicate the model are saved in a 
##  data.table linked to the unique GID so we do not have to save numerous model 
##  objects, but can instead just save a single .RDS. Further, once the final 
##  model has been selected, the initial annual projections into the future and 
##  their prediction interval are computed, written/appended to the intial 
##  population table that the models were fit on, and then saved in the 
##  ../output/tmp/ folder for future reference. 
##
##  This is all done in parallel using a task farm, where every task is the 
##  fitting and finding of a final model and predicting for the next 't' 
##  timesteps. The object returned from each list is a list where the 
##  first item is a numeric vector containing an ordered sequence of the final 
##  model parameters and the second item in the list is an ordered numeric 
##  vector containing the projected values and the projected prediction 
##  interval bounds. Both of these items are then added to the two, respective, 
##  and desired data.table objects for record keeping and further use.
####
##  Get a list of all the GIDs we will be processing through except those that 
##  we previously identified as being saturated or empty for the future.
gid_list <- unique(hist_pop[YEAR == t1 & SATURATIONFLAG != 1 & EMPTYFLAG !=1,]$GID)

##  We will do this twice, once for URR, once for BSPDBAR
##  Preallocate an empty data.frame for the cluster results:
rows <- length(gid_list) * bsgm.predict.steps
forecast_df <- data.frame("GID"= numeric(length = rows),
                          "YEAR" = numeric(length = rows),
                          "FORECAST" = numeric(length = rows),
                          "H95" = numeric(length = rows),
                          "L95" = numeric(length = rows),
                          "MOD.TAG" = character(length = rows),
                          "MOD.DESC" = character(length = rows),
                          stringsAsFactors = FALSE)

##  Set the run mode for the URR first:
run_mode <- "URR"


##  Carry out the parallel estimation for URR:
beginCluster(n = bsgm.cluster_workers)
urr_forecasts <- cluster_Timewarp(forecast_df)
endCluster()
rm(forecast_df)
gcQuiet()


##  Merge them with the historical population table
##  WARNING:  AS OF 2018-04-24  I AM ASSUMING THAT  THE HIST TABLE WILL NOT HAVE 
##            MANY OF THE COLUMNS FROM THE OUTPUT, E.G. H95, L95, MOD.TAG, 
##            MOD.DESC, FORECAST. THEREFORE MODIFYING PRIOR 
##            TO MERGE. HOWEVER, IT WIL HAVE THE YEAR AND TOTAL POP VALUE AND 
##            THE CORRESPONDING GIDS SO ADDING IN THE VALUES WILL BE TRICKY
hist_pop$URR.H95 <- numeric(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$URR.H95 <- NA
hist_pop$URR.L95 <- numeric(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$URR.L95 <- NA
hist_pop$URR.MOD.TAG <- character(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$URR.MOD.TAG <- NA
hist_pop$URR.MOD.DESC <- character(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$URR.MOD.DESC <- NA


##  Change names of the dataframe prior to rbinding:
names(urr_forecasts) <- c("GID","YEAR","URR","URR.H95",
                          "URR.L95","URR.MOD.TAG","URR.MOD.DESC")
urr_forecasts <- as.data.table(urr_forecasts)

##  Convert any linear and exponetial typoe model H95 and L95 valeus to NA
urr_forecasts[URR.MOD.TAG == "LIN" | URR.MOD.TAG == "SQR", c("URR.H95", "URR.L95") := NA] 

##  For every GID and year of the prediction dataframe:
for(g in gid_list){
  for(y in {t0+1}:{t0+bsgm.predict.steps}){
    ##  Transfer the values
    hist_pop[GID == g & YEAR == y,
             c("URR","URR.H95","URR.L95",
               "URR.MOD.TAG","URR.MOD.DESC")] <- urr_forecasts[GID == g & YEAR == y,
                                                               c("URR","URR.H95","URR.L95",
                                                                 "URR.MOD.TAG","URR.MOD.DESC")]
  }
}

rm(urr_forecasts)
gcQuiet()



##  Preallocate an empty data.frame for the cluster results, again:
forecast_df <- data.frame("GID"= numeric(length = rows),
                          "YEAR" = numeric(length = rows),
                          "FORECAST" = numeric(length = rows),
                          "H95" = numeric(length = rows),
                          "L95" = numeric(length = rows),
                          "MOD.TAG" = character(length = rows),
                          "MOD.DESC" = character(length = rows),
                          stringsAsFactors = FALSE)

##  Set the run mode for the BSPDBAR:
run_mode <- "BSPDBAR"


##  Carry out the parallel estimation for BSPDBAR:
beginCluster(n = bsgm.cluster_workers)
bspdbar_forecasts <- cluster_Timewarp(forecast_df)
endCluster()
rm(forecast_df)
gcQuiet()


##  Merge them with the historical population table
##  WARNING:  AS OF 2018-04-24  I AM ASSUMING THAT  THE HIST TABLE WILL NOT HAVE 
##            MANY OF THE COLUMNS FROM THE OUTPUT, E.G. H95, L95, MOD.TAG, 
##            MOD.DESC, FORECAST. THEREFORE MODIFYING PRIOR 
##            TO MERGE. HOWEVER, IT WIL HAVE THE YEAR AND TOTAL POP VALUE AND 
##            THE CORRESPONDING GIDS SO ADDING IN THE VALUES WILL BE TRICKY


hist_pop$BSPDBAR.H95 <- numeric(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$BSPDBAR.H95 <- NA
hist_pop$BSPDBAR.L95 <- numeric(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$BSPDBAR.L95 <- NA
hist_pop$BSPDBAR.MOD.TAG <- character(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$BSPDBAR.MOD.TAG <- NA
hist_pop$BSPDBAR.MOD.DESC <- character(length = nrow(hist_pop))
hist_pop[YEAR <= t0]$BSPDBAR.MOD.DESC <- NA

##  Change names of the dataframe prior to rbinding:
names(bspdbar_forecasts) <- c("GID","YEAR","BSPDBAR","BSPDBAR.H95",
                          "BSPDBAR.L95","BSPDBAR.MOD.TAG","BSPDBAR.MOD.DESC")
bspdbar_forecasts <- as.data.table(bspdbar_forecasts)

##  Convert any linear and logGLM type model H95 and L95 valeus to NA as we are 
##  not inferring anything:
bspdbar_forecasts[BSPDBAR.MOD.TAG == "LIN" | BSPDBAR.MOD.TAG == "SQR", c("BSPDBAR.H95", "BSPDBAR.L95") := NA] 

##  For every GID and year of the prediction dataframe:
for(g in gid_list){
  for(y in {t0+1}:{t0+bsgm.predict.steps}){
    ##  Transfer the values
    hist_pop[GID == g & YEAR == y,
             c("BSPDBAR","BSPDBAR.H95","BSPDBAR.L95",
               "BSPDBAR.MOD.TAG","BSPDBAR.MOD.DESC")] <- bspdbar_forecasts[GID == g & YEAR == y,
                                                                           c("BSPDBAR","BSPDBAR.H95","BSPDBAR.L95",
                                                                             "BSPDBAR.MOD.TAG","BSPDBAR.MOD.DESC")]
  }
}

rm(bspdbar_forecasts)
gcQuiet()

##  Carryout initial BSPOP estimation from the projections:
hist_pop[YEAR %in% {t0+1}:{t0+bsgm.predict.steps}, BSPOP := POP*{URR/(1+URR)}]
##  Carry out the initial BSCNT estimation from the projections, using regular 
##  rounding rules to the nearest whole number:
hist_pop[YEAR %in% {t0+1}:{t0+bsgm.predict.steps},
         BSCNT := round(BSPOP/BSPDBAR)]

##  Calculate the annual change in BS cells:
hist_pop[,("EST.CHANGE") := lapply(.SD, function(x){c(0,diff(x))}),
         by = GID, .SDcols = "BSCNT"]

##  If we have any negative values

##  Save the unadjusted predictions as an RDS in the temporary output:
saveRDS(hist_pop, file = paste0(bsgm.output.path.countries.tmp,
                                bsgm.forec.table))


##
##  END:  FITTING OF ARIMA/EXS BY SUBUNIT  -------------------------------------




##  BEGIN:  PROJECTED TRANSITION MAPPING  --------------------------------------
##  ----  Pre Task List Creation:  One-time Processes  ----
##
##  Retrieve the cell indices of cells which are not built settlement at time t0
##  i.e. transitionable:
print("Retrieving Transitionable indices...")
transition_raster <- raster(t0extentsPathFileName)
##  If we are running things locally:
if (!bsgm.prl) { 
  ##  Retrieve those cell indices:
  trans_ind <- which(values(transition_raster)==0)
}else{##  If we are running in parallel:
  ##  Retrieve those indices in parallel:
  trans_ind <- wpGetindexesWhichValues(x=transition_raster, 
                                       v=0, 
                                       cores=bsgm.cluster_workers)  
}  
##  Set up a processing loop which calls upon the parallelized function to 
##  determine which cells transition
##  For every year in the sequence which needs to be predicted:
for(t in 2:(length(year_seq))){
  start_time <- proc.time()[3]
  ##  Retrieve the proper year:
  y <- year_seq[t]
  
  ##  If we are using the Weighted LAN values to adjust the probabilities:
  if(bsgm.LAN.weighting){
    ##  Retrieve the corresponding pattern we'll be using for retrieving the 
    ##  year specific LAN derived data:
    if(y <= "2012"){lan_pattern <- "_dmsp_"}else{lan_pattern <- "_viirs_"}
  }
  ##  Determine output name for this year:
  out_name <- paste0("BSGM_Extents",bsgm.countries.tag,"_",y,".tif")
  
  ##  Retrieve the year prior's built settlement extent raster path:
  if(y == {t0+1}){
    ##  If prior year is base year, get the initial extent path:
    bsip <- t0extentsPathFileName
  }else{
    ##  Retrieve the output from the year prior's predictions:
    bsip <- paste0(bsgm.output.path.countries,"BSGM_Extents",
                   bsgm.countries.tag,"_",(y-1),".tif")
  }
  
  ##  ----  Pre Task List Creation:  Annual Processes  ----
  ##  Retrieve the unique admin ids, store those IDs as the name in a list,
  ##  and store the corresponding cell indices as a numeric vector under that
  ##  the corresponding name in the list as long as the index is listed as 
  ##  having transitioned and was not already built:
  ##
  ##  Retrieve the indices of the cells which are built settlement, i.e. have a
  ##  value of '1'
  start_time <- proc.time()[3]
  print("Retrieving initial BS inidices...")
  init_built_ras <- raster(bsip)
  ##  If we are running locally:
  if (!bsgm.prl) { 
    ##  Determine the indices of the BS extents at the initial time points:
    init_built_ind <- which(values(init_built_ras)==1)
  }else{##  If we are running in parallel:
    ##  Determine the indices of the BS extents at the intiial time point in
    ##  parallel:
    init_built_ind <- wpGetindexesWhichValues(x=init_built_ras, 
                                              v=1, 
                                              cores=bsgm.cluster_workers)  
  } 
  
  ##  Remove the intial built settlement raster from memory:
  rm(init_built_ras)
  gc()
  
  ##  From those transitionable indices, remove cells which have already been 
  ##  transitioned in previous steps:
  print("     Differencing those indices...")
  trans_ind_diff <- setdiff(trans_ind, init_built_ind)
  admin_ind <- trans_ind_diff
  
  ##  Filter our growth dataframe to the records for a specific year and only 
  ##  the records for non-zero growth admin units
  print("Filtering data specific to year...")
  #grow_info <- pop_df %>% filter(YEAR == y, EST.CHANGE != 0)
  grow_info <- hist_pop[YEAR == y & EST.CHANGE > 0,]
  
  if(nrow(grow_info)>0){
    print("     Pulling GIDs...")
    admin_ras <- raster(censusmaskPathFileName)
    ##  If processing locally
    if (!bsgm.prl) { 
      ##  Retrieve the GIDs of those identified cells:
      admin_gid <- getValues(admin_ras)[admin_ind]
    }else{##  If we are running in parallel:
      ##  Retrieve the GIDs of those identified cells in parallel:
      admin_gid <- wpGetValuesbyInds(x=admin_ras,
                                     v=admin_ind,
                                     cores=bsgm.cluster_workers)  
    }      
    print("     Creating dataframe and filtering...")
    admin_df <- data.frame("IND" = admin_ind, "GID" = admin_gid)
    
    ##  Make sure the only records we have are those which have growth observed
    ##  for the given year:
    admin_sub_df <- admin_df[admin_df$GID %in% unique(grow_info$GID),]
    
    if(y == 2012){
      lan_path <- Sys.glob(paste0(bsgm.data.path.countries, "/LAN/derived/*",lan_pattern,"2010_normlag_2011-2010.tif"))
    }
    if(y > 2016){
      lan_path <- Sys.glob(paste0(bsgm.data.path.countries, "/LAN/derived/*",lan_pattern,"2015_normlag_2016-2015.tif"))
    }
    if(y != 2012 & y <= 2016){
      lan_path <- Sys.glob(paste0(bsgm.data.path.countries, "/LAN/derived/*",lan_pattern,{y-1},"*.tif"))[1]
    }
    if(bsgm.LAN.weighting & !is.na(lan_path)){
      
      print(paste0("     Weighting probabilities with ", basename(lan_path)))
      ##  Get the LAN weights from the corresponding processed LAN raster band:
      ##  If we are running locally:
      if (!bsgm.prl) { 
        ##  Weight the probabilities by the year's LAN data:
        admin_sub_df$WEIGHT <- getValues(raster(lan_path))[admin_sub_df$IND]
      }else{##  If we are running in parallel:
        ##  Weight the probabilities by the year's LAN data in parallel:
        admin_sub_df$WEIGHT <- wpGetValuesbyInds(x=raster(lan_path), 
                                                 v=admin_sub_df$IND, 
                                                 cores=bsgm.cluster_workers)  
      }       
      
      
      ##  Get the base prob values so we don't have to get them again:
      ##  If we are running locally:
      if (!bsgm.prl) { 
        ##  Retrieve the probability values:
        admin_sub_df$BASEPROB <- getValues(prob_ras)[admin_sub_df$IND]
      }else{##  If we are running in parallel:
        ##  Retrieve the probability values in parallel:
        admin_sub_df$BASEPROB <- wpGetValuesbyInds(x=prob_ras,
                                                   v=admin_sub_df$IND,
                                                   cores=bsgm.cluster_workers)  
      }        
      ##  Calculate the weighted probability of transition:
      admin_sub_df$PROB <- admin_sub_df$WEIGHT * admin_sub_df$BASEPROB
    }
    ##  If we are not using LAN weighting of transition probabilities:
    if(!bsgm.LAN.weighting | is.na(lan_path)){
      ##  Get the prob values so we don't have to get them again:
      if (!bsgm.prl) { 
        admin_sub_df$PROB <- getValues(prob_ras)[admin_sub_df$IND]
      }else{
        admin_sub_df$PROB <- wpGetValuesbyInds(x=prob_ras,
                                               v=admin_sub_df$IND,
                                               cores=bsgm.cluster_workers)  
      }       
    }
    
    ##  There is potential for the probability surface to have NA values so 
    ##  we'll remove the 'incomplete' rows of the dataframe:
    admin_sub_df <- admin_sub_df[complete.cases(admin_sub_df),]
    
    ##  For every admin unit which has a non-zero amount of growth for the year:
    gid_list <- unique(admin_sub_df$GID)
    print(paste0("Creating task list of ", 
                 as.character(length(gid_list)),
                 ":"))
    
    gc()
    
    ##  ----  Task List Creation  ----
    ##  Create an empty preallocated_list of the correct size:
    task_list <- vector(mode="list", length = length(gid_list))
    
    ##  Start up the cluster and carry out the transition index extraction:
    ##  NOTE:  This is a modified clusterPredict is from the transitionTools.R
    beginCluster(n = bsgm.cluster_workers)
    master_task <- clusterTasker(task_list)
    endCluster()
    rm(task_list)
    
    ##  Create the cell vector to transition cells:
    ##  NOTE:  bsInterpolate is from transitionTools.R
    transitions_ty_list <- sapply(master_task, bsInterpolate, simplify = TRUE)
    ##  Unlist and compile into a single vector of cell indices:
    transitions_ty <- unlist(transitions_ty_list, use.names = FALSE)
    transitions <- unique(transitions_ty[!is.na(transitions_ty)])
    
    ##  Carry out the transition mapping.
    ##  If we are running locally:
    if (bsgm.prl==FALSE){
      ##  Carry out the transition mapping:
      bsTransitionMap(bsip, 
                      transitions, 
                      out_name)
    }else{##  If we are running in parallel:
      ##  Carry out the transition mapping in parallel:
      wpSetValueWhichindexes(x=raster(bsip),
                             y=transitions,
                             v=1,
                             filename = paste0(bsgm.output.path.countries, out_name),
                             cores=bsgm.cluster_workers)
    }
    
    rm(master_task, transitions_ty)
    gc()
  }
  
  
  ##  If there are actually no transitions in for the year:
  if(nrow(grow_info) < 1){
    ##  Take the previous year's extents and write them out as the next years 
    ##  extents.
    file.copy(from = bsip, 
              to = paste0(bsgm.output.path.countries, out_name), 
              overwrite = overwrite)
  } 
  
}
##
##  END:  PROJECTED TRANSITION MAPPING  ----------------------------------------
