##  THIS FILE CONTAINS INTERNAL FUNCTIONS WHICH ARE USED IN THE BSGM SCRIPT
##  TITLE:  INTERNAL .R
##  AUTHOR:  JEREMIAH J. NIEVES & MAKSYM BONDARENKO
##  VERSION: 1.0
##  LAST UPDATE: 2017-05-27
##  NOTES:  SOME OF THE FUNCTIONS WITHIN HERE ARE BSGM SPECIFIC FUNCTIONS, 
##          OTHERS ARE GENERAL, AND OTHERS ARE RELATED TO THE INTERACTION WITH
##          THE WORLDPOP DATABASE. FUNCTIONS RELATED TO THE LATTER TWO WHICH 
##          WERE WRITTEN IN PART OR IN ENTIRITY BY MAKSYM BONDARENKO ARE NOTED.

##  -----------------------------------------------------------------------  ##
ensure_dir <- function(d){
  ##  Function for ensuring that a directory exists and creating it if it does 
  ##  not; returns the path of the input path
  if(!dir.exists(d)){
    dir.create(d)
  }
  return(d)
}

pkgLoad <- function(x){
  #  This function loads a package and installs it if it is not already present
  #  Written by Maksym Bondarenko
  if(!require(x, character.only = TRUE)){
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE))stop("Package not found")
  }
}

##  -----------------------------------------------------------------------  ##
logConscole <- function(x){
  ##  This is a logging function which not only logs output messages in a text 
  ##  file, but also prints the message to the console if kDologConscole 
  ##  is TRUE.
  ##  Written by Maksym Bondarenko
  if(kDologConscole){
    cat("\n")
    cat("\n")
    cat(sprintf("%s", "+", 1:50))
    cat("\n")
    print(x)
    cat(sprintf("%s", "+", 1:50))
    cat("\n")
  }
}

##  -----------------------------------------------------------------------  ##
logr <- function(log_inidicator, 
                 log_file_path, 
                 message_string,
                 separator = "\n",
                 ...){
  ## "Boy, the Brits sure do love their LOGR."
  ##  Logging function I made which operates similarly to the logging function
  ##  Maksym made. Allows greater flexibility of output, inputs, and 
  ##  formatting.
  if(logindicator){
    cat(message_string,
        file = log_file_path,
        sep = separator,
        append = TRUE)
  }else{
    print(message_string)
    flush.console()
  }
}

##  -----------------------------------------------------------------------  ##
checkNoDataValueInRaster <- function(fileNamePath = NULL,
                                     NoDataValue = 9999){
  ##  This function checks for the existence of NO DATA in the raster's values.
  ##  Originally written by Maksym Bondarenko.
  ##  -------------------------
  
  ##  If the file path was not defined:
  if(is.null(fileNamePath)){
    stop(paste0("ERROR: Please specify a file path for the raster of interest:"))
  }
  
  ##  If the defined file does not exist:
  if(!file.exists(fileNamePath)){
    stop(paste0("ERROR: File ", fileNamePath, " does not exist!"))
  }
  
  ##  Get the raster info:
  rasterGdalInfo <- gdalinfo(fileNamePath,
                             "mm",
                             verbose = FALSE,
                             raw_output = TRUE)
  ##  Retrieve the No Data value as defined in the information:
  nindex <- grep('\\bNoData\\b', rasterGdalInfo)
  ##  Retrieve and split the relevant information:
  OutputStrSplitS <- strsplit( rasterGdalInfo[ nindex[1] ], "=" )
  
  ##  Create message indicating the NoData value retrieved:
  message(paste0("Raster ", 
                 fileNamePath, 
                 " has a NoData value of ",
                 OutputStrSplitS[[1]][2]))
  
  ##  If we are getting an obscene number of hits back for cells which have 
  ##  NoData values, give a warning message:
  # if(as.numeric(OutputStrSplitS[[1]][2] > -10000000)){
  #   stop(paste0("WARNING: Raster ",
  #               fileNamePath,
  #               " likely has the incorrect NoData value specified.",
  #               "Currently specified as: ",
  #               OutputStrSplitS[[1]][2]))  
  # }
}

##  -----------------------------------------------------------------------  ##
specify_decimal <- function(x,k){
  ##  This function formats a numeric to a specific decimal precision.
  ##  Written by Maksym Bondarenko
  format(round(x,k), nsmall = k)}

##  -----------------------------------------------------------------------  ##
checkExtentRaster <- function(r1, r2){
  ##  This function checks the extents of any two given rasters and if their 
  ##  extents do not match, the function rectifies them to match.
  ##  Written by Maksym Bondarenko.
  xmin.r1 <- specify_decimal( bbox(r1)[1,1], 5 )
  xmax.r1 <- specify_decimal( bbox(r1)[1,2], 5 )
  ymin.r1 <- specify_decimal( bbox(r1)[2,1], 5 )
  ymax.r1 <- specify_decimal( bbox(r1)[2,2], 5 )
  
  xmin.r2 <- specify_decimal( bbox(r2)[1,1], 5 )
  xmax.r2 <- specify_decimal( bbox(r2)[1,2], 5 )
  ymin.r2 <- specify_decimal( bbox(r2)[2,1], 5 )
  ymax.r2 <- specify_decimal( bbox(r2)[2,2], 5 )
  
  if( (xmin.r1) != (xmin.r2) | (xmax.r1) != (xmax.r2) | (ymin.r1) != (ymin.r2) | (ymax.r1) != (ymax.r2)){
    return(FALSE)
  }else{return(TRUE)}
}

##  -----------------------------------------------------------------------  ##
changeExtentRaster <- function(rFrom, rToPath){
  ##  This function changes the extent of two rasters.
  ##  Written by Maksym Bondarenko.
  ##  Retrieve the initial extents:
  xmin <- bbox(rFrom)[1,1]
  xmax <- bbox(rFrom)[1,2]
  ymin <- bbox(rFrom)[2,1]
  ymax <- bbox(rFrom)[2,2]
  
  ##  Retrieve the output file path information:
  rFileName <- basename(rToPath)
  rPath <- dirname(rToPath)
  
  ##  Use gdal warp to change the extents:
  system(paste0("gdalwarp -te ",
                ' ',xmin,
                ' ',ymin,
                ' ',xmax,
                ' ',ymax,
                ' ',rToPath,' ',
                paste0(rPath,"/","tmp_",rFileName)),
         ignore.stdout = FALSE,
         ignore.stderr = FALSE)
  
  ##  If the file already exists:
  if(file.exists(rToPath)){unlink(rToPath, recursive = TRUE, force = FALSE)}
  file.rename(from = paste0(rPath,"/","tmp_",rFileName), to = rToPath)
}

##  -----------------------------------------------------------------------  ##
CreateSubFolderForPrj <- function(){
  ##  This function creates subfolders within "/Data/" for each country and 
  ##  within "/output/" for each country and the combined countries. Returned 
  ##  is a list of the output and data subfolders which were created.
  ##  Orignally written by Maksym Bondarenko; modified for BSGM.
  ##  Define the ouput tag:
  countries_tag_output <- paste0("prj_",input_rf_data$data$year0,"_",input_rf_data$data$year1)
  
  ##  For every covariate given:
  for( i in levels(rd.cvr.factors.iso3) ){
    ##  Data SubDirectory
    ##    Define the sub directory path:
    subDir.country <- paste0(root_path,"/data/",i)
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Temporary Output Subdirectory
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/tmp/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Built Settlement (BS) Est. Output Subdirectory (.CSV):
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/bsest/csv/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    
    ##  Built Settlement (BS) Est. Output Subdirectory (.RData):
    ##    Define the sub directory path:
    subDir.country.output.tmp <- paste0(root_path,"/output/",i,"/bsest/RData/")
    
    ##    If that folder does not exist already, create it:
    if(!file.exists(subDir.country)){
      dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE)
    }
    ##  Set the next iterations tag output:
    countries_tag_output <- paste0(countries_tag_output, i)
  }
  output.path.countries <- paste0(root_path,"/output",countries_tag_output,"/")
  output.path.countries.tmp <- paste0(root_path, "/output/",countries_tag_output,"/tmp/")
  
  ##  If the temporary path does not exist create it:
  if(!file.exists(output.path.countries.tmp)){
    dir.create(output.path.countries.tmp, recursive = TRUE, showWarnings = FALSE)
  }
  
  ##  If only one country was defined in the input parameters, i.e. we are 
  ##  working with only one country, then the path to "/data/" will be the ISO
  ##  of the country, otherwise that path will be a new folder created upon the
  ##  ISOs of the multiple countries which were defined in the input parameters
  ##  e.g. "prj_2000_2012_PER_COL", and that folder will contian the merged 
  ##  rasters of the countries to be modeled.
  ##  If only one country is specified:
  if(length(rd.cvr.factors.iso3) == 1){
    data.path.countries <- paste0(root_path, 
                                  "/data/", 
                                  as.character(rd.cvr.factors.iso3[[1]]),
                                  "/")
    
    ##  If that folder does not already exist, create it:
    if(!file.exists(data.path.countries)){
      dir.create(data.path.countries,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
  }else{
    data.path.countries <- paste0(root_path,
                                  "/data/",
                                  countries_tag_output,
                                  "/")
  }
  if(!file.exists(data.path.countries)){
    dir.create(data.path.countries,
               recursive = TRUE,
               showWarnings = FALSE)
  }
  ##  Return a list of the output paths:
  return(list(output = output.path.countries, 
              data = data.path.countries))
}

##  -----------------------------------------------------------------------  ##
DownloadCovariates <- function(){
  ##  This function loads covariates from the WorldPop DataBase (DB) to the 
  ##  "/Data/" folder.
  ##  Written by Maksym Bondarenko
  ##  For every covariate given in the input parameters:
  for( i in 1:nrow(rd.cvr)){
    ##  Retrieve the current row information:
    current.row <- rd.cvr[i,]
    current.row.iso3 <- as.character( current.row$iso3 )
    current.row.folder <- as.character( current.row$folder )
    current.row.dst_name <- as.character( current.row$dst_name )
    current.row.description <- as.character( current.row$description )
    current.row.summary <- as.character( current.row$summary )
    current.row.class <- as.character( current.row$class )
    
    ##  Define the folder where the covariates should be:
    subDir.covariates <- paste0(root_path,
                                "/data/",
                                current.row.iso3,
                                "/",
                                current.row.folder)
    
    ##  If it doesn't exist, create it:
    if(file.exists(subDir.covariates)){
      dir.create(subDir.covariates,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    
    ##  Define path of the covariate rasters within the folder:
    subFile.covariates <- paste0(subDir.covariates,"/",
                                 current.row.dst_name,".tif")
    
    ##  If those covariate files are not present locally, download them:
    if(!file.exists(subFile.covariates)){
      message(paste0("     Downloading ", 
                     current.row.dst_name,
                     " to ", 
                     current.row.iso3, 
                     " ", 
                     subDir.covariates))
      
      ##  Actully perform the download:
      getRasterWPDB(schema = paste0(wpdb.prefix.schema,
                                    tolower(current.row.iso3)),
                    table = current.row.dst_name,
                    fileName = subFile.covariates,
                    user = wpdb.user,
                    password = wpdb.password)
    }else{
      message(paste0("     Covariate ",
                     current.row.dst_name,
                     " already exists in ",
                     subDir.covariates))
    }
    ##  Record the current rows meta information:
    covariates[[current.row.iso3]][[current.row.dst_name]] <- list(
      dataset_folder = current.row.folder,
      dataset_name = paste0(current.row.dst_name, ".tif"),
      dataset_description = current.row.description,
      dataset_summary = current.row.summary,
      dataset_country = current.row.iso3,
      dataset_class = current.row.class
    )
    ##  Define the path:
    covariates[[current.row.iso3]][[current.row.dst_name]][["path"]] <- subFile.covariates
  }
  ##  Save the covariates object:
  save(covariates, file = paste0(output.path.countries.tmp,
                                 "covariates.RData"))
  ##  Convert it to JSON too while we're at it:
  covariates_json <- toJSON(covariates, pretty = TRUE)
  cat(covariates_json, file = paste0(output.path.countries.tmp,
                                     "covariates.json"))
  
  ##  Retrieve the admin ID raster
  ##    For every country given in the input parameters:
  for( i in levels(rd.cvr.factors.iso3)){
    ##  Define the 'census' directory:
    Dir.Census <- paste0(root_path,"/data/",i,"/CensusDerived/")
    
    ##  If the file does not exist:
    if(!file.exist(Dir.Census)){
      ##  Create it:
      dir.create(Dir.Census,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    zonal_raster_path <- paste0(Dir.Census, "census_zones.tif")
    
    if(!file.exists(zonal_raster_path)){
      logConscole("Downloading zonal raster from database. census_zones.tif has ADMINID.")
      
      getRasterWPDB(schema = paste0(wpdb.prefix.schema,
                                    tolower(i)),
                    table = paste0(tolower(i),"adminid"),
                    fileName = zonal_raster_path,
                    user = wpdb.user,
                    password = wpdb.password)
    }
  }
  ##  Return the covariates object:
  return(covariates)
}

##  -----------------------------------------------------------------------  ##
get.covariates.var.names <- function(){
  ##  This function gets the covariate names.
  ##  Written by MAksym Bondarenko.
  ##  Create an empty vector to store things in:
  covariates.var.names <- c()
  
  ##  For every covariate item:
  for( icvritm in 1:length( covariates[[1]] ) ){
    ##  Retrieve the name of the item:
    covariate <- covariates[[1]][[icvritm]]
    var_name_class <- covariate$dataset_class
    covariates.var.names <- c(covariates.var.names, var_name_class)
  }
  
  ##  Sort the names:
  sort(covariates.var.names)
  
  ##  Return the sorted names:
  return(covariates.var.names)
}

##  -----------------------------------------------------------------------  ##
get.df.for.merging.rst.cvr <- function(country_factor, covariates_cntrys_obj){
  ##  This function assists in handling multiple countries by returning a
  ##  dataframe containing the covariates and their corresponding raster paths
  ##  for every covariate of every input country. Sepcifically, the dataframe
  ##  produced by this function assists in merging the rasters.
  ##  Written by Maksym Bondarenko.
  ##  Declare an empty dataframe and adminid path:
  emptyDfclassPath <- data.frame(class = character(), path = character())
  adminIdPath <- ""
  
  
  ##  Begin: Class - Path Retrieval  --------------------
  ##    This section of the function takes the covariates, of all countries 
  ##    being processed, one-by-one and retrieves the meta/path info which is 
  ##    then placed in one of two dataframes; 
  ##      1) holding the class of covariate and the path
  ##      2) holding the class of covariate, the folder it is in, and the 
  ##         summary information
  ##
  ##  Establish a counter:
  icount <- 0
  ##  For every country given:
  for( icountry in levels(country_factor)){
    ##  Increase the counter by 1:
    icount <- {icount+1}
    ##  Create an temporary dataframe:
    emptyDfclassPathTmp <- data.frame(class = character(), path = character())
    ##  Create an empty data frame which will contain the folders:
    emptyDfclassFolder <- data.frame(class = character(),
                                     folder = character(),
                                     summary = character())
    
    ##  For every covariate in that country's repetoire, as given in the 
    ##  covariate objects holding country specific covariate records:
    for( i in 1:length(covariates_cntrys_obj[[icountry]])){
      ##  Retrieve information about that covariate specifically:
      covariate <- covariates_cntrys_obj[[icountry]][[i]]
      dataset_name <- covariate$dataset_name
      dataset_path <- covariate$path
      dataset_folder <- covariate$dataset_folder
      dataset_class <- covariate$dataset_class
      dataset_summary <- covariate$dataset_summary
      
      ##  Add the info to relevant temporary dataframes:
      emptyDfclassPathTmp <- rbind(emptyDfclassPathTmp,
                                   data.frame(class=dataset_class,
                                              path=dataset_path))
      emptyDfclassFolder <- rbind(emptyDfclassFolder, data.frame(class=dataset_class,
                                                                 folder=dataset_folder,
                                                                 summary=dataset_summary))
    }
    ##  If this was the first iteration:
    if(icount==1){
      ##  Assign the df:
      emptyDfclassPath <- emptyDfclassPathTmp
    }else{
      ##  Otherwise merge with the previous info by class:
      emptyDfclassPath <- merge(as.data.frame(emptyDfclassPath),
                                as.data.frame(emptyDfclassPathTmp),
                                by="class",
                                sort = FALSE)
    }
    
    ##  Save the path to the admin zone raster within a single character string
    ##  with paths to different country's raster separated by a single space:
    adminIdPath <- paste(adminIdPath,
                         paste0(root_path,"/data/",icountry,"Census/Derived/census_zones.tif"),
                         spr = " ")
  }
  ##  Begin:  Merging Covariate Paths Into Single Path for Merging  -----
  ##    This section of the function takes the two previous created 
  ##    dataframes and merges the paths of the individual, country specific
  ##    covariates into a single character string of multiple paths, much like
  ##    was done for the admin raster paths above, representing a single class
  ##    of covariate.
  ##
  Dfclass <- data.frame(class = character(), folder = character(),
                        summary = character(), path = character())
  
  ##  For every row, i.e. country specific covariate, we put in the previous df:
  for(i in 1:nrow(emptyDfclassFolder)){
    ##  Start empty string for path:
    pat <- ""
    ##  Retrieve a df subset for a single class:
    ds <- emptyDfclassPath[emptyDfclassPath$class == as.character(emptyDfclassFolder$class[[i]]),]
    
    ##  For every one of the columns in that subset:
    for(ids in names(ds)){
      ##  If the name isn't "class":
      if(ids!="class"){
        ##  Create a single character holding the paths of each separated by a
        ##  space:
        pat <- paste(pat,
                     as.character(ds[[ids]]),
                     spr = " ")
      }
    }
    
    ##  Add to the df as a new row with pertinent info:
    Dfclass <- rbind(Dfclass,
                     data.frame(class=as.character(emptyDfclassFolder$class[[i]]),
                                folder=as.character(emptyDfclassFolder$folder[[i]]),
                                summary=as.character(emptyDfclassFolder$summary[[i]]),
                                path=pat))
  }
  ##  Add the admin raster paths as an entry to that df as well:
  Dfclass <- rbind(Dfclass,
                   data.frame(class = "AdminId", 
                              folder = "Census/Derived",
                              summary = "AdminId",
                              path = adminIdPath))
  
  ##  Return the final dataframe:
  return(Dfclass)
}

##  -----------------------------------------------------------------------  ##
merging.rst.cvr.multy.country <- function(country_factor, 
                                          covariates_cntrys_obj,
                                          mode="single"){
  ##  This function actually carries out the merging of the covariates for 
  ##  multiple countries. By setting mode to "cluster", the function will carry
  ##  out the merging in parallel to speed up processing.
  if( mode == "cluster"){
    pkgLoad('doSNOW')
    
    ##  Create the dataframe needed to assist with merging:
    df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                                 covariates_cntrys_obj)
    
    ##  Initialize the cluster:
    cl <- makeSOCKcluster(cluster_workers)
    registerDoSNOW(cl)
    
    ##  Load the required packages, functions, and variables on the cores:
    #clusterExport( cl, library(randomForest) )
    clusterExport( cl, "data.path.countries" )
    clusterExport( cl, "gdal_merge_path" )
    #clusterExport( cl, df.for.merging)
    
    logConscole("Start merging rasters in a stack for RF...")
    
    ##  Start a progress bar:
    pb <- txtProgressBar(max = nrow(df.for.merging), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    ##  Carry out the parallel processing:
    r <- foreach(i = 1:nrow(df.for.merging), .options.snow = opts) %dopar%
    {##  Declare needed paths:
      fileOutput <- paste0( as.character( df.for.merging[i, "class"] ), ".tif" )
      filePathOutput <- paste0( data.path.countries, as.character(df.for.merging[i, "folder"]), "/")
      
      ##  If file does not exist create it:
      if(!file.exists(filePathOutput)){dir.create(filePathOutput, 
                                                  recursive = TRUE,
                                                  showWarnings = FALSE)}
      ##  Carryout the merging:
      print(
        system(
          paste0(gdal_merge_path,
                 ' -a_nodata -99999 -of GTiff ',
                 paste0('-co COMPRESS=LZW '),
                 paste0('-co BIGTIFF=YES '),
                 paste0('-o ', filePathOutput, fileOutput, ' '),
                 as.character( df.for.merging[i,"path"] ) ),
          ignore.stdout = FALSE,
          ignore.stderr = FALSE
        )
      )
    }
    ##  Close the progress bar and stop the cluster:
    close(pb)
    stopCluster(cl)
  }else{
    ##  If the mode is 'single':
    df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                                 covariates_cntrys_obj)
    logConscole("Start merging rasters in a stack for RF...")
    
    for(i in 1:nrow(df.for.merging)){
      fileOutput <- paste0(as.character(df.for.merging[i,"class"]),".tif")
      filePathOutput <- paste0(data.path.countries, 
                               as.character(df.for.merging[i,"folder"]),"/")
      
      if(!file.exists(filePathOutput)){dir.create(filePathOutput,
                                                  recursive = TRUE,
                                                  showWarnings = FALSE)}
      
      print(paste0("Merging ", as.character(df.for.merging[i,"path"])))
      print(paste0("To ", filePathOutput, fileOutput))
      print("-------------------------------------------------------------")
      
      ##  Carryout the merging:
      system(
        paste0(gdal_merge_path,
               '-ot Float64 -a_nodata -99999 -of GTiff ',
               paste0('-co COMPRESS=LZW '),
               paste0('-co BIGTIFF=YES '),
               paste0('-o ', filePathOutput, fileOutput, ' '),
               as.character( df.for.merging[i,"path"] ) ),
        ignore.stdout = FALSE,
        ignore.stderr = FALSE
      )
    }
  }
}

##  -----------------------------------------------------------------------  ##
create.covariates.list.for.RF <- function(country_factor,
                                          covariates_cntrys_obj){
  ##  This function creates a list of the covariates used in the RF script
  ##  which matches the expected 'one name for one covariate' schema in the RF,
  ##  i.e. this removes any country specific prefix.
  ##  Written by Maksym Bondarenko.
  ##  Create an empty list to hold the covariates:
  covariates.new <- list()
  
  ##  Retrieve the merging dataframe:
  df.for.merging <- get.df.for.merging.rst.cvr(country_factor,
                                               covariates_cntrys_obj)
  
  logConscole("Creating a new covariate list for the RF...")
  
  for(i in 1:nrow(df.for.merging)){
    class.t <- as.character(df.for.merging[i,"class"])
    folder.t <- as.character(df.for.merging[i,"folder"])
    summary.t <- as.character(df.for.merging[i,"summary"])
    
    ##  Remove AdminID and Watermask from the covariates list:
    if((class.t != 'AdminId') | (class.t!='Watermask')){
      if(length(country_factor > 1)){
        filePathOutput <- paste0(data.path.countries, folder.t,"/")
        fileOutput <- paste0(as.character(df.for.merging[i,"class"]),".tif")
        fileNamePath <- paste0(filePathOutput,fileOutput)
      }else{
        fileNamePath <- as.character(df.for.merging[i,"path"])
      }
      
      covariates.new[[class.t]] <- list(
        dataset_folder = folder.t,
        dataset_name = class.t,
        dataset_description = "",
        dataset_summary = summary.t,
        path = fileNamePath
      )
    }
  }
  return(covariates.new)
}

################################################################################

.rasterInfoGDType <- function(filename){
  ##  This function utilizes GDAL based operations to retrieve raster 
  ##  information and return it in a data.frame.
  ####
  ##
  ##  Retrieve info from the input file using a call to gdalinfo:
  gdalinfo <- try(rgdal::GDALinfo(filename,  
                                  returnCategoryNames=TRUE,
                                  returnStats=TRUE ,
                                  silent=TRUE))
  
  ##  Extract info which came from gdalinfo and format it:
  nc <- as.integer(gdalinfo[["columns"]])
  nr <- as.integer(gdalinfo[["rows"]])
  
  xn <- gdalinfo[["ll.x"]]
  xn <- round(xn, digits=9)
  xx <- xn + gdalinfo[["res.x"]] * nc
  xx <- round(xx, digits=9)
  
  yn <- gdalinfo[["ll.y"]]
  yn <- round(yn, digits=9)
  yx <- yn + gdalinfo[["res.y"]] * nr
  yx <- round(yx, digits=9)  
  
  bi <- attr(gdalinfo, 'df')
  GDType <- as.character(bi[['GDType']])
  hasNoDataValues <- bi[['hasNoDataValue']]
  NoDataValue <- bi[['NoDataValue']]
  Bmin <- bi[['Bmin']]
  Bmax <- bi[['Bmax']]
  Bmean <- bi[['Bmean']]  
  
  driver <- attr(gdalinfo, 'driver')
  projection <- attr(gdalinfo, 'projection')
  resX <- gdalinfo[["res.x"]]
  resY <- gdalinfo[["res.y"]]
  lower_left_originX <- gdalinfo[[4]]
  lower_left_originY <- gdalinfo[[5]]
  
  ##  Place that extracted information into a dataframe:
  df <- data.frame( "nrow" = nr,
                    "ncol" = nc,
                    "xmin" = xn,
                    "xmax" = xx,
                    "ymin" = yn,
                    "ymax" = yx,
                    "Bmin" = Bmin,
                    "Bmax" = Bmax,
                    "Bmean" = Bmean,
                    "NoDataValue" = NoDataValue,
                    "hasNoDataValues" = hasNoDataValues,
                    "driver" = driver,
                    "GDType" = GDType,
                    "projection" = projection,
                    "resX" = resX,
                    "resY" =resY,
                    "lower_left_originX" = lower_left_originX,
                    "lower_left_originY" = lower_left_originY)
  
  ##  Return that dataframe:
  return(df)
}

###############################################################################



.rasterInfo<- function(filename){
  ##  This function retrieves raster information based upon GDAL information
  ##  and returns it in a data.frame. This function is similar to the function
  ##  '.rasterInfoGDType,' also defined in this module, but this is limited to 
  ##  information on extent, raster dimensions, and NA values.
  ####
  ##
  ##  Retrieve info from the input file using a call to gdalinfo:
  gdalinfo <- try ( rgdal::GDALinfo(filename,
                                    returnCategoryNames=TRUE,
                                    returnStats=TRUE,
                                    silent=TRUE) 
  )
  
  ##  Extract info which came from gdalinfo and format it:
  nc <- as.integer(gdalinfo[["columns"]])
  nr <- as.integer(gdalinfo[["rows"]])
  
  xn <- gdalinfo[["ll.x"]]
  xn <- round(xn, digits=9)
  xx <- xn + gdalinfo[["res.x"]] * nc
  xx <- round(xx, digits=9)
  
  yn <- gdalinfo[["ll.y"]]
  yn <- round(yn, digits=9)
  yx <- yn + gdalinfo[["res.y"]] * nr
  yx <- round(yx, digits=9)  
  
  bi <- attr(gdalinfo, 'df')
  GDType <- as.character(bi[['GDType']])
  hasNoDataValues <- bi[['hasNoDataValue']]
  NoDataValue <- bi[['NoDataValue']]
  Bmin <- bi[['Bmin']]
  Bmax <- bi[['Bmax']]
  Bmean <- bi[['Bmean']]  
  
  ##  Place that extracted information into a dataframe:
  df <- data.frame(nr, nc, xn,
                   xx, yn, yx,
                   Bmin, Bmax, Bmean,
                   NoDataValue, hasNoDataValues)
  colnames(df) <- c("nrow","ncol","xmin",
                    "xmax","ymin","ymax",
                    "Bmin","Bmax","Bmean",
                    "NoDataValue","hasNoDataValues")
  
  ##  Return that dataframe:
  return(df)
}

###############################################################################



tmDiff <- function(start, end, frm="hms") {
  ##  Function which takes time objects and calculates the difference between 
  ##  the start and end time point. Returned is the formatted time.
  dsec <- as.numeric(difftime(end, start, units = c("secs")))
  hours <- floor(dsec / 3600)
  
  if (frm == "hms" ){
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    
    out=paste0(
      sapply(c(hours, minutes, seconds), function(x) {
        formatC(x, width = 2, format = "d", flag = "0")
      }), collapse = ":")
    
    return(out)
  }else{
    return(hours)
  }
}

###############################################################################



check.df.has.na<- function(df){
  ##  Check if there are NAs in the input dataframe and return a TRUE or FALSE
  ##  value:
  M <- colSums(is.na(df))
  
  if (length(M[M == 1]) > 0)
    return(TRUE)
  else
    return(FALSE)
}

###############################################################################



get.covariates.var.names <- function(){
  ##  This function retrieves variable names from the given covariates.
  ##  Create a vector to hold the covariate variable names:
  covariates.var.names <- c()
  
  ##  For every covariate in the covariates object:
  for ( icvritm in 1:length(covariates) ) { 
    ##  Retrieve that covariate object:
    covariate <- covariates[[icvritm]]
    ##  Retrieve the dataset_name attribute:
    var_name_class <- covariate[['dataset_name']]
    ##  Append that variable name to the covariates.var.names vector:
    covariates.var.names <- c(covariates.var.names, var_name_class)
  }    
  ##  Sort those names:
  sort(covariates.var.names)
  ##  Return the names vector:
  return(covariates.var.names)
}


###############################################################################



transY <- function(x, inverse=FALSE) {
  ##	The default is to log transform the x argument:
  if (!inverse) {
    return( log(x) )
  } else {
    ##  Otherwise we backtransform it by exponentiating it:
    return( exp(x) )
  }
}


###############################################################################



get_minblocks_rf_prd_need <- function() {
  ##  Determine how many blocks woud be optimal for the RF prediction phase.
  ##  If there is a minimum of blocks specified, return that minimum:
  if (!is.null(rfg.minblocks)) {
    return(rfg.minblocks)
  }else{
    ##  Return the number of blocks determined to be optimal as decided by the 
    ##  function wpGetBlocksNeed() which is housed in the wpUtilities package:
    return(wpGetBlocksNeed(covariate_stack, rfg.cluster_workers, n=1))
    
    # mem.avail <- (0.5 * (memory.size(NA)-memory.size(FALSE)))/rfg.cluster_workers
    # stack.cells <- ncell(census_mask) +2
    # stack.nlayers <- length(popfit_final$forest$xlevels)
    # 
    # maxmemory.need <- ( round( 10 * 8 * stack.cells * stack.nlayers) )/ (1024 * 1024)
    # rf.minblocks <- 1
    # 
    # while ((maxmemory.need > mem.avail))
    # {
    #   stack.cells <- stack.cells/rf.minblocks
    #   maxmemory.need <- ( round( 10 * 8 * stack.cells * stack.nlayers) )/ (1024 * 1024)
    #   rf.minblocks <- rf.minblocks + 1
    # }   
    # 
    # if (rf.minblocks < rfg.cluster_workers){
    #   rf.minblocks <- rfg.cluster_workers
    # }
    ##  And return the minimum number of blocks:
    return(rf.minblocks)
  }  
  
}

###############################################################################



##  Create a raster stack from all covariates from popfit and census_mask 
##  and water_raster:
##  Create a raster stack from all covariates from popfit and census_mask 
##  and water_raster:
creat_raster_stack <- function() {
  ##  Create an empty list to hold the rasters:
  list_ras <- list()
  
  ##  For every raster name in the list of names used in the RF:
  for (i in 1:length(names(bsgmfit_final$forest$xlevels))){
    ##  Retrieve the name:
    var_name <- names(bsgmfit_final$forest$xlevels)[i]  
    ##  Assign that raster path to the varname:
    assign(var_name, raster( covariates[[var_name]]$path ), envir = .GlobalEnv)
    ##  Put the raster path in the list:
    list_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
  }  
  ##  Append the regions mask and the water mask to that list:
  list_ras[[length(list_ras)+1]] <- get("region_mask")
  list_ras[[length(list_ras)+1]] <- get("water_raster")
  
  ##  Stack all the rasters we just retrieved:
  ras_stack <- stack(list_ras)
  
  ##  Return the raster stack object:
  return(ras_stack)
}


###############################################################################



gcQuiet <- function(quiet = TRUE, ...) {
  ##  Use gc (i.e. garbage collection) to clear memory and report about the 
  ##  usage.
  if(quiet){ 
    invisible(gc())
  }else{
    print(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB'))
    gc(...)
    print(paste('R is using', memory.size(), 'after using garbige collection'))
  }  
}

###############################################################################


checkRetrieveExtents <- function(){
  ##  This function is typically called under the extrapolation mode to ensure 
  ##  that a folder called "Extents" exists in the country's /Data/ folder,
  ##  that said folder has the correct number of rasters in it, that those 
  ##  rasters have the proper file name structure which is retrievable, and then
  ##  returns a named list of those raster paths.
  ##  If the directory exists:
  if(dir.exists(paste0(bsgm.data.path.countries,"Extents/"))){
    ##  Preallocate an empty list to hold paths:
    bsgm.bs.extents.path.list <- list()
    
    ##  For every declared year:
    for( yy in 1:length(bsgm.interp.year.seq)){
      ##  Retrieve what should be a valid file:
      foo_path <- Sys.glob(paste0(bsgm.data.path.countries,
                                  "Extents/BSGM_Extents*",
                                  bsgm.interp.year.seq[yy],".tif"))
      ##  As long as it is not an empty file path:
      if(foo_path != ""){
        ##  For every year, place the corresponding  year-specific extent tif:
        bsgm.bs.extents.path.list[[as.character(bsgm.interp.year.seq[yy])]] <- foo_path
      }
    }
    
    ##  If the number of those retrieved file names is equal to the sequence 
    ##  declared by the user:
    if(length(bsgm.bs.extents.path.list)==length(bsgm.interp.year.seq)){
      ##  Return the named list of file paths retrieved:
      return(bsgm.bs.extents.path.list)
    }else{
      stop(paste0("ERROR: The number of files found in the Extents Folder does not match the number declared by the user in input.R.",
                  " Please check the contents of the folder for the correct number of files, that the files match the expected file name pattern, and that the user specified number of files is correct."))
    }
  }else{stop(paste0("ERROR: Manually create the missing folder ",
                    bsgm.data.path.countries,
                    "Extents/ and populate it with the ",
                    length(bsgm.interp.year.seq), 
                    " corresponding extents rasters."))
  }
}




alignCovariateNames <- function(){
  ##  Function align the variable names in the covariates object if we had to 
  ##  standardize the covariate names in the input RF objects which used the 
  ##  covariates and confgurations from the WPGlobal project. If this is being 
  ##  used outside of the wpGlobal configuration, best practice is to not even 
  ##  need this function and use standard covariate names the whole way. Reason
  ##  this function exists is that production forgot to remove time specific 
  ##  tags on the covariate names so rectification must occur. Going to try to 
  ##  make this flexible to allow for stuff that matches up to just pass through
  ##  no hassle, but care should be taken or this function should be modified or
  ##  commented out in the main script should trouble occur. 
  ##  Can't plan for everything.
  ####
  ##  If there are inconsistencies in the occurrence of covariates seen in the 
  ##  combined or final RF object we plan on using to predict with:
  for(i in names(covariates)){
    ##  If that covariate name is not in the RF object:
    if(!(i %in% names(bsgmfit_final$forest$xlevels))){
      ##  If that covariate begins with 'ghsl' or 'guf' we're going to assume it
      ##  is one of our binary BS distance covariates and rename it 
      ##  appropriately:
      if(startsWith(i,"ghsl")|startsWith(i,"guf")){
        ##  Replace the name of that item properly:
        names(covariates)[which(names(covariates)==i)] <- "bs_dst"
      }
      ##  Otherwise, if the variable does not match one in the RF object and 
      ##  happens to end in four digits following an underscore and the rest of 
      ##  the covariate name matches besides the underscore_digits
      if(grepl(".*_[0-9]{4}", i, perl = T) &
      {gsub("(.*)_[0-9]{4}", "\\1", i, perl = T) %in%
          names(bsgmfit_final$forest$xlevels)}){
        names(covariates)[which(names(covariates)==i)] <- gsub("(.*)_[0-9]{4}",
                                                               "\\1", i,
                                                               perl = T)
      }
      ##  If none of the above conditions are true:
      if(startsWith(i,"ghsl")|startsWith(i,"guf") & grepl(".*_[0-9]{4}", i, perl = T) &
      {gsub("(.*)_[0-9]{4}", "\\1", i, perl = T) %in% names(bsgmfit_final$forest$xlevels)}){
        stop(paste0("ERROR:  Covariate name found in covariate object which does not match any existing covariate name or expected pattern in the bsgmfit_final object.",
                    " Reexamine the input covariates, their names, the RF object, and adjust naming conventions accordingly."))
      }
    }
  }
  return(covariates)
}




##  Local copy of the function wpGetOS from the wpUtilities package written
##  by M. Bondarenko. For some reason this function is not accessible directly 
##  from the package so I have included a local version here.
wpGetOS <- function(){
  
  sysinf <- Sys.info()
  
  if (!is.null(sysinf)){
    
    OS <- tolower(sysinf['sysname'])
    
    if(OS == 'windows'){
      
      return('windows')
      
    } else if (OS == 'darwin') {
      
      return('osx')
      
    } else if (OS == 'linux') {
      
      return('linux')
      
    }
    
  } else { ## other OS
    OS <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      return('osx')
    if (grepl("linux-gnu", R.version$os))
      return('linux')
  }
}




##  Local copy of the function wpGetAvalMem from the wpUtilities package written
##  by M. Bondarenko. For some reason this function is not accessible directly 
##  from the package so I have included a local version here.

wpGetAvalMem <- function(){
  
  OS <- wpGetOS()
  
  if(OS == 'windows'){
    memavail = shell('wmic OS get FreePhysicalMemory /Value',intern=T)
    memavail = memavail[grep('FreePhysicalMemory', memavail)]
    memavail = as.numeric(gsub('FreePhysicalMemory=','',memavail))
  }else if (OS == 'osx'){
    memavail = as.numeric(unlist(strsplit(system("sysctl hw.memsize", intern = T), split = ' '))[2])/1e3
  }else if (OS == "linux"){
    memavail = as.numeric(system(" awk '/MemFree/ {print $2}' /proc/meminfo", intern=T))
  }
  
  return(memavail/ (1024 * 1024))
}




##  Local version of the wpUtilities function wpGetBlocksNeed written by 
##  M. Bondarenko; while usually identical, I have this version locally and have 
##  replaced the call to wpGetBlocksNeed with wpGetBlocksNeeded in order to 
##  allow for ready modifications of the calculation used for estimating how may
##  blocks are needed, as opposed to being stuck with the algorithm in the 
##  wpUtilities package should that be unsatisfactory:
wpGetBlocksNeeded <- function(x, cores, n=1, number_type = "numeric"){
  
  #stopifnot(hasValues(x))
  
  n <- n + nlayers(x) - 1
  cells <- round( 1.1 * ncell(x) ) * n
  #memneed <- cells * 8 * n / (1024 * 1024)
  
  if(number_type == "integer") {
    
    byte_per_number = 4
    
  } else if(number_type == "numeric") {
    
    byte_per_number = 8
    
  } else {
    
    #byte_per_number = .Machine$sizeof.pointer
    stop(sprintf("Unknown number_type: %s", number_type))
  }
  
  blocks <- 1
  
  memneed <- (cells * byte_per_number * n / (1024 * 1024 * 1024))/blocks
  
  memavail <- wpGetAvalMem()/cores
  
  while ((memneed > memavail)) {
    
    memneed <- (cells * byte_per_number * n / (1024 * 1024 * 1024))/blocks
    blocks <- blocks + 1
  }
  
  if ( blocks < cores) blocks <- cores
  
  return(blocks)
  
}




##  Fucntion for formatting the population input data originally from the BSGMi 
##  modules. Ported over here for similar functionality.
tidyCIESIN <- function(orig_df, times = seq(2000,2020, by=1)){
  ##  Function to transform data from CIESIN into a more palatable format 
  ##  (i.e. tidy data).
  ##  Description:  Takes the non-tidy data from CIESIN and makes is tidy per 
  ##                Wickham 2013 (doi: 10.18637/jss.v059.i10) and then subsets 
  ##                it by years given as a numeric vector argument. This will 
  ##                let us take advantage of plyr later on in this script.
  ##
  ##  Parameters: 
  ##  indata -  Input data from CIESIN containing the unique geoID of admin 
  ##            units and associated time specific populations; give as a 
  ##            character path
  ##  times  -  vector of years of interest for which we will subset the 
  ##            CIESIN origin data in order to keep our output dataframes tidy 
  ##            and readable (As much as possible)
  ##
  ##  Values:
  ##  Returns a dataframe object in long format (i.e. tidy data)
  ##
  ##  ---------------------------------------------------------------------  ##
  #  Bring in the population dataframe which will form the base of our data 
  ##  work and storage:
  print("Bringing in original population data...")
  start_time <- proc.time()[3]
  
  ##  Let's get tidy.
  print("Rearranging data to tidy format...")
  ##  For every time point we specified earlier:
  for(y in 1:length(times)){
    if(y == 1){
      ##  If it is the first iteration, populate a new dataframe with the first
      ##  year's records:
      pop_df <- data.frame("GID" = orig_df$GID, 
                           "YEAR" = rep(times[y],length(orig_df$GID)), 
                           "POP" = orig_df[, paste0("P_",times[y])])
    }else{
      ##  Otherwise, just rbind the new rows on the end:
      pop_df <- rbind(pop_df,
                      data.frame("GID"  = orig_df$GID,
                                 "YEAR" = times[y],
                                 "POP"  = orig_df[, paste0("P_",times[y])])
      )
    }
  }
  print("Data tidying complete!")
  print(paste0("Elapsed time: ", proc.time()[3]-start_time, " seconds"))
  
  ##  Return the new dataframe object:
  return(pop_df)
}
