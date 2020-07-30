##  Written by Maksym Bondarenko and Jeremiah J. Nieves
Download_Covariates <- function(){
  ##  If we have logging turned on:
  if (bsgm.DologDEBUG) 
    d.quiet=FALSE
  else
    d.quiet=TRUE
  
  loginfo("Download Covariates files from WP FTP if they dont exist already ")
  ##  For every country declared:
  for ( i in bsgm.input.countries ) {
    ##  Retrieve a list of the available covariates:
    df.ListCountryCovariates <- wpgpListCountryCovariates(ISO3=i, 
                                                          username = bsgm.ftp.username, 
                                                          password = bsgm.ftp.password, 
                                                          detailed = T
                                                          )
    
    ##  For every covariate the user declared to be of interest in the input.R 
    ##  file:
    for ( j in bsgm.input.cvr ) {
      ##  Retrieve only the covariate which name matches the current covariate 
      ##  of iteration:
      df.filtered <- df.ListCountryCovariates[df.ListCountryCovariates$CvtName == j,]
      
      ##  Declare the folder where we will store it
      cvr.folder <-  paste0(root_path,"/","data/", i)
      ##  Declare the name of the covariate raster we will save it as:
      cvr.rst.name <-  paste0(df.filtered$RstName,".tif")
      ##  Retrieve the covariate name:
      cvr.name <-  df.filtered$CvtName
      ##  Retrieve the covariate description:
      cvr.description <-  df.filtered$Description
      
      ##  If that file does not already exist locally:
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        logdebug( paste0('Start downloading Covariate: ',j," for ",i))
        ##  Download it:
        df.download <- wpgpGetCountryCovariate(df.user=NULL,
                                               ISO3=i,
                                               covariate=j,
                                               destDir=cvr.folder,
                                               username=bsgm.ftp.username,
                                               password=bsgm.ftp.password,
                                               quiet=d.quiet)    
      }else{
        ##  Otherwise note it and pass:
        logdebug( paste0(' Covariate: ',j," for ",i,
                         " already exists in ",cvr.folder))
      }
      
      ##  Assemble a covariates list object which will store the associated 
      ##  metadata and path for each covariate:
      covariates[[i]][[j]] <- list(
        dataset_folder      =  cvr.folder,
        dataset_filename    =  cvr.rst.name,
        dataset_description =  cvr.description,
        dataset_summary     =  "mean",
        dataset_country     = i,
        dataset_class       = j
      )
      covariates[[i]][[j]][["path"]] <- paste0(cvr.folder,"/",cvr.rst.name)
    }

    ##  Download water mask and zonal (L1) and the built-settlement extents at 
    ##  t0:
    for ( k in c(bsgm.water.mask, bsgm.ccidadminl1, bsgm.t0.extents) ) {
      cvr.folder <-  paste0(root_path,"/","data/", as.character(i))
      cvr.rst.name <-  paste0(tolower(i),"_grid_100m_",as.character(k),".tif")
      cvr.name <-  as.character(k)
      cvr.description <-  as.character(k)
      
      if(!file.exists(paste0(cvr.folder,"/",cvr.rst.name))){ 
        
        logdebug( paste0('Start downloading Covariate: ',k," for ",i))
        
        wpgpGetCountryCovariate(df.user=NULL,
                                ISO3=i,
                                covariate=as.character(k),
                                destDir=cvr.folder,
                                username=bsgm.ftp.username,
                                password=bsgm.ftp.password,
                                quiet=d.quiet)
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_summary     =  as.character(k),
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )     
        
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,
                                                 "/",paste0(tolower(i),
                                                            "_grid_100m_",
                                                            as.character(k),
                                                            ".tif"))        
        
      } else{
        
        logdebug( paste0(' Covariate: ',k," for ",i,
                         " alsready exists in ",cvr.folder))
        
        covariates[[i]][[k]] <- list(
          dataset_folder      =  cvr.folder,
          dataset_filename    =  cvr.rst.name,
          dataset_description =  as.character(k),
          dataset_country     =  as.character(i),
          dataset_class       =  as.character(k)
        )            
        covariates[[i]][[k]][["path"]] <- paste0(root_path,"/","data/", i,"/",
                                                 paste0(tolower(i),
                                                        "_grid_100m_",
                                                        as.character(k),".tif"))        
      } 
    }  # End of downloading water mask, L1, and t0 extents
  }  
  
  ##  Save the covariates object as a RData and a JSON file:
  save(covariates, file=paste(bsgm.output.path.countries.tmp,
                              bsgm.countries.fln.Rdata, sep=""))
  
  ##  Return the covariates object:
  return(covariates) 
}







DownloadFileFromWPFTP <- function(file_path, dest_file, quiet, method="auto") {
  
  wpgpFTP <- "ftp.worldpop.org.uk"
  credentials <- paste(bsgm.ftp.username, bsgm.ftp.password, sep = ":")
  file_remote <- paste0('ftp://',credentials,'@',wpgpFTP,'/WP515640_Global/',
                        file_path)
  
  tmStartDw  <- Sys.time()
  
  checkStatus <- tryCatch(
    {
      utils::download.file(file_remote, destfile=dest_file,mode="wb",
                           quiet=quiet, method=method)
    },
    error=function(cond){
      message(paste("URL does not seem to exist:", file_remote))
      message("Here's the original error message:")
      message(cond)
    },
    warning=function(cond){
      message(paste("URL caused a warning:", file_remote))
      message("Here's the original warning message:")
      message(cond)
    },
    finally={
      if (!quiet){
        tmEndDw  <- Sys.time()
        #message(paste("Processed URL:", file_remote))
        message(paste("It took ", tmDiff(tmStartDw ,tmEndDw,frm="hms"),
                      "to download" ))
      }
    }
  )
  
  if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
    return(NULL)
  } else{
    return(1)
  }
}

DownloadPopRasterExtrp <- function(){
  ##  Downloading ppp populations from FTP from 
  r_fn <- list()

  ##  For every input country:
  for(i in bsgm.input.countries){
    ##  For every year in the sequence
    for(t in t_train:t0){
      foo_tif <- paste0(root_path,"/data/",i,"/Pop/ppp_prj_",t,i,".tif")
      foo_tif_ftp <- paste0("/ppp_datasets/",t,"/",i,"/", tolower(i),"_ppp_wpgp_",
                            t, ".tif")
      
      if(!file.exists(foo_tif)){ 
        print(paste0("Copying ppp_prj_",t,"_",i, ".tif from WP FTP"))
        DownloadFileFromWPFTP(foo_tif_ftp, foo_tif, TRUE, method="auto") 
      }
    }
  }
}



DownloadLANRasterExtrp <- function(){
  ##  Downloading LAN rasters from FTP.
  ##  For every input country:
  for(i in bsgm.input.countries){
    for(t in 1:(length(year_seq))){
      if(year_seq[t] == 2011){next}
      if(year_seq[t] < 2011){
        lan_pattern <- "_dmsp_"
      }else{
          lan_pattern <- "_viirs_"
          }
      y <- year_seq[t]
      ##  If we have LAN data for that year:
      if(any(!is.na(bsgm.LAN.cvr.names[bsgm.LAN.cvr.names$YEAR == {y+1},]$NAME)) & 
         length(bsgm.LAN.cvr.names[bsgm.LAN.cvr.names$YEAR == {y+1},]$NAME) != 0){
        tif_lan <- paste0(root_path,"/data/",i,
                          "/LAN/derived/"
                          ,i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif")
        
        tif_ftp <- paste0("LAN/",i,
                          "/LAN/derived/"
                          ,i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif")
        
        if(!file.exists(tif_lan)){ 
          print(paste0("Copying ",i,lan_pattern,y,"_normlag_",y+1,"-",y,".tif"))
          DownloadFileFromWPFTP(tif_ftp, tif_lan, TRUE, method="auto") 
        }else{
          print(paste0("We have already download the file before ",i,
                       lan_pattern,y,"_normlag_",y+1,"-",y,".tif"))
        }
      }      
    }
  }
}




##  DIRECT FTP DOWNLOADS WITH REGEX QUERYING  ----------------------------------
directListingFTP <- function(url = "ftp.worldpop.org.uk", 
                             subdir ="", 
                             query = "",
                             returnsub = TRUE,
                             returnfull = FALSE,
                             username = "", 
                             password = ""){
  ####
  ##  url - location of the desired FTP
  ##  subdir - path of any desired subdirectory within the main url (no leading 
  ##           slash required)
  ##  query - regular expression based query to be used to subset results of 
  ##          files found at given url and subdirectory; default is to return 
  ##          all
  ##  returnsub - return the basenames with the sub directory prepended?
  ##  returnfull - return the basenames with the full url and subdirectory, if 
  ##               any?
  ##  Note: requires RCurl package
  ####
  ##  Add any subdirectories to the url:
  url <- paste0(url, subdir)
  
  ##  Define full FTP remote connection:
  remote <- paste0("ftp://",username,":",password,"@",url)
  
  ##  Retrieve list of things at the defined path as a vector of file names:
  filelist <- unlist(strsplit(getURL(remote,verbose=F,dirlistonly=T),"\r\n"))
  
  ##  Query the results:
  if(query == ""){
    query = ".*"
  }
  qfiles <- grep(query, filelist, perl = T, value = T)
  
  ##  If we have optons on how we want the files returned:
  if(returnsub){
    ##  Paste the sub directory to the file names:
    filevector <- paste0(subdir,qfiles)
  }
  if(returnfull){
    ##  Paste the full url with subdirectory to the filename:
    filevector <- paste0(url,qfiles)
  }
  ##  Return the vector of filenames with or without prepended stuff:
  return(filevector)
}


##  Define the direct download of extents function and the function for age 
##  normalizing the rasters for output:
directDownloadFTP <- function(url = "ftp.worldpop.org", 
                              subdir ="/WP515640_Global/", 
                              query = "", 
                              outdir = "",
                              filelist = NULL,
                              username = "",
                              password = "",
                              quiet = FALSE){
  remote <- paste0("ftp://",username,":",password,"@")
  ##  If not given a vector of filenames:
  if(is.null(filelist)){
    ##  Perform the querying function to return a vector of desired file 
    ##  basenames with the full directory attached for download later within the 
    ##  function:
    filelist = directListingFTP(url=url,
                                subdir = subdir, 
                                query = query,
                                returnfull = T,
                                returnsub = F,
                                username = username,
                                password = password)
    
  }
  ##  For every file in the filelist:
  for(f in filelist){
    ## Note this is expecting the full file path.
    file_remote <- paste0(remote,f)
    ##  Download the file:
    checkStatus <- tryCatch(
      {
        utils::download.file(file_remote, destfile=outdir,
                             mode="wb",quiet=quiet, method="auto")
      },
      error=function(cond){
        message(paste("URL does not seem to exist:", file_remote))
        message("Here's the original error message:")
        message(cond)
      },
      warning=function(cond){
        message(paste("URL caused a warning:", file_remote))
        message("Here's the original warning message:")
        message(cond)
      }
    )
  }
  
  if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
    return(NULL)
  } else{
    return(1)
  }
}
