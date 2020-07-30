##  TITLE:  INPUTe.R
##  AUTHORS:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  NOTES:  THIS SCRIPT SERVES AS A VEHICLE FOR USER INPUT PARAMETERS USED IN 
##          THE BSGMe SCRIPT. ALL USER INPUT PARAMETERS SHOULD BE DECLARED HERE
##          OR IN THE CONFIGe.R FILE, WITH THE EXCEPTION OF THE 'ROOT' PATH 
##           VARIABLE WHICH MUST BE SET WITHIN THE BSGMe.R SCRIPT.
####


##  STATIC SETUP  --------------------------------------------------------------
##
##  NOTICE:  IN PRACTICE NOTHING IN THIS SECTION SHOULD NEED TO BE REGULARLY 
##           EDITED UNLESS VERY SPECIFIC DETAILS NEED TO BE CHANGED ABOUT THE 
##           MODELING PROCESS.
######
##  Configuration options for the BSGM modeling and predictions.
##  Indicate the BSGM version used to produce the mapping products:
bsgm.version <- "1e_local"


##  Create a look up list for the type of LAN data (i.e. DMSP or VIIRS) based 
##  upon the year:
bsgm.LAN.cvr.names <- data.frame("NAME" = c("dmsp_2000",
                                            "dmsp_2001",
                                            "dmsp_2002",
                                            "dmsp_2003",
                                            "dmsp_2004",
                                            "dmsp_2005",
                                            "dmsp_2006",
                                            "dmsp_2007",
                                            "dmsp_2008",
                                            "dmsp_2009",
                                            "dmsp_2010",
                                            "dmsp_2011",
                                            "viirs_2012",
                                            "viirs_2013",
                                            "viirs_2014",
                                            "viirs_2015",
                                            "viirs_2016",
                                            NA,
                                            NA,
                                            NA,
                                            NA),
                                 "YEAR" = seq(2000, 2020, by = 1))
##
##  END:  STATIC SETUP  --------------------------------------------------------


##  BEGIN:  USER INPUTS  -------------------------------------------------------
##
##  Declare the 3-letter ISO code of the country you are interested in 
##  modeling.
##  EXAMPLE:
##    bsgm.input.countries <- c("NPL")
#bsgm.input.countries <- c("VNM")


##  Declare the year at which to start the training of the ARIMA/EXS models from
##  and, if necessary, download previous years observed/modeled data (pop. and 
##  bs extents):
t_train <- 2000

##  Declare starting year of the modelling period, for which we have observed 
##  built-settlement extents:
#t0 <- 2014

##  Declare end year of the modelling period that we wish to project out to:
##    Warning:  After six time steps, i.e. t0 +6, the projections based on 
##              ARIMA/EXS are considered to destabilize and performance may 
##              vary significantly more per Hyndman et al. How these perform 
##              using an iCAR model to limit the amount of transition in any 
##              year is currently unknown and will likely vary both between and 
##              within countries. 
#t1 <- 2020


##  Determine the number of steps we will be forecasting for:
bsgm.predict.steps <- t1-t0

##  Determine what type of test to use to determine which model will be used in 
##  forecasting. Values can be "rolling" or "multistep":
bsgm.model.test.mode <- "multistep"


##  Determine how many observations will be retained in a time series for 
##  initial model fit testing.The value should be 1 less than the desired amount  
##  NOTE:  For example, if you want 10 of you observations for fitting then you 
##         would put k <- 9
bsgm.min.time.series <- 9

##  Overwrite the outputs?:
overwrite <- TRUE


##  If you are using specific Population tables, i.e. non-standard, stored 
##  locally, declare their paths here. Otherwise, the script will source the 
##  ones from the database when bsgm.input.poptables <- NULL.
##  EXAMPLE:
##    bsgm.input.poptables <- list("D:/WorldPop_Data/BTN_BSPOP_TABLE_FINAL.csv"
##                               )
bsgm.input.poptables <- NULL


##  Declare a factor by which to break up parallel tasks into manageable chunks:
##    NOTE:  Used in cluster_predict()
#nmb <- 50

##  Declare a factor by which to break up the tasks to be completed in the 
##  parallel version of the interpolation and weighting functions of the BSGM:
##    NOTE:  Used in - interpolateURR_prl(), interpolateBSPDBAR_prl(), 
##           wpDFcalcBS_POP(), wpDFcalcBS_CNT(), and weightChanges_prl()
cblk <- 1


##  Declare a list of the character representations of the covaraites with which
##  we intend to do modeling with:
##  NOTE:  You can use the function wpgpListCountryCovariates() from the 
##         wpgpCovariates library to see what all covariates are available, but 
##         most will remain the same between covariate runs excluding the year 
##         specific part of their name.
##         EXAMPLE:  
##           wpgpListCountryCovariates(ISO3="NPL",
##                                     username = "wpftp",
##                                     password = "qw12wq1sZQ")
##  
##  NOTE:  If you are interpolating from, e.g., 2000 to 2012 you should use the 
##         category 1 protected areas corresponding to the last year of
##         modeling, e.g. 2012. For extrapolation, use the most recently data 
##         available.
##
##  WARNING:  Ensure that the covariates declared match the year declared for 
##            the bsgm.input.year variable.
##
##  WARNING:  WHEN RUNNING MODELS AND CHANGING FROM INTERPOLATION TO 
##            EXTRAPOLATION ENSURE THAT THE COVARIATES, WHILE DIFFERRING IN 
##            YEAR, ARE LISTED IN THE SAME ORDER. THAT IS, IF SLOPE IS DECLARED 
##            FIRST AND THEN THE URBAN BINARY LAYER SECOND, ETC., IN THE 
##            INTERPOLATION MODEL TO BE USED IN THE EXTRAPOLATION PHASE FOR THE 
##            SAME COUNTRY, THEN THE SAME ORDER MUST BE DECLARED WITH THE YEAR 
##            SPECIFIC COVARIATES TO BE USED IN THE CORRESPONDING EXTRAPOLATION 
##            PHASE FOR THAT COUNTRY. IF YOU DO NOT, YOU WILL STILL GET OUTPUT,
##            BUT IT WILL BE MEANINGLESS AS THE MODEL WILL HAVE PREDICTED USING 
##            THE WRONG RELATIONSHIPS FOR EACH COVARIATE.
bsgm.input.cvr <- list("slope",
                       "topo",
                       "tt50k_2000",
                       "urbpx_prp_1_2014",
                       "urbpx_prp_5_2014",
                       "urbpx_prp_10_2014",
                       "urbpx_prp_15_2014",
                       "guf_ghsl_dst_2014",
                       "wdpa_cat1_dst_2014")

##  Declare the intial time (t0) built-settlement extents covariate name, as 
##  stored in the WP FTP:
bsgm.t0.extents <- "guf_ghsl_2014"


##  Declare if we want to use the Lights at Night (LAN) data to relatively 
##  weight the transition probabilities for each year that it is available
##  (obviously not available for future dates):
bsgm.LAN.weighting <- TRUE


##  Declare if we are using a fixed set in this modeling, i.e. are we 
##  parameterizing, in part or in full, this RF model run upon another 
##  country's(ies') RF model object. 
##  EXAMPLE:
##    bsgm.fixed.set <- TRUE   | bsgm.fixed.set <- FALSE
##
bsgm.fixed.set <- TRUE

##  Boolean parameter indicating if we should create a hybrid RF, asumming 
##  bsgm.fixed.set  and estimate.RF is TRUE:
bsgm.fixed.set.incl.input.countries <- FALSE


##  ------  NOTHING SHOULD BE MODIFIED WITHIN THESE BRACKETS BELOW THIS LINE
##  ------  IN REGULAR PRACTICE
##
##  END:  USER INPUTS  --------------------------------------------------