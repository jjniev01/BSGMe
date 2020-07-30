##  ATUHORS:  MAKSYM BONDARENKO & JEREMIAH J. NIEVES


get_bsgmfit_final_old <- function(only.names=FALSE){
  ##  Function which retrieves previously constructed bsgmfit.RData objects and 
  ##  then combines them into a single RF.
  err_mess <- ""
  err_bool <- FALSE
  
  ##  Retrieve a list of .Rdata objects in the folder where the old RF objects 
  ##  (and only them) should be stored:
  list.of.old.bsgmfits.final <- list.files(paste0(bsgm.data.old.bsgmfits.final),
                                           pattern=paste0("\\.Rdata$"),
                                           full.names=TRUE) 
  
  
  logdebug(paste0('Loading old bsgmfit final from: ',
                  bsgm.data.old.bsgmfits.final))
  
  if ( length(list.of.old.bsgmfits.final) == 0 ){
    err_mess <- paste0('There is no old bsgmfit Please check the folder : ',
                       bsgm.data.old.bsgmfits.final)
    stop(err_mess)
  }
  
  ##  Load it:
  logdebug(paste0( 'Load old bsgmfit final with  ',
                   list.of.old.bsgmfits.final[[1]] ))
  local_env.bsgmfit_final <- local({load(file=list.of.old.bsgmfits.final[[1]]);environment()})
  
  bsgmfit.final.old <- local_env.bsgmfit_final$bsgmfit_final
  ##  Make sure the names are stripped of any year ending:
  strip_names <- gsub("((?:urb|ghsl|wdpa).*)_[0-9]{4}",
                      "\\1",
                      names(bsgmfit.final.old$forest$xlevels),
                      perl = TRUE)
  ##  Make sure our binary BS layer has the same standardized name:
  strip_names[which(startsWith(strip_names,"ghsl"))] <- "bs_dst"
  
  names(bsgmfit.final.old$forest$xlevels) <- strip_names
  row.names(bsgmfit.final.old$importance) <- strip_names
  row.names(bsgmfit.final.old$importanceSD) <- strip_names
  names(bsgmfit.final.old$forest$ncat) <- strip_names
  
  ##  Set objects to NA or NULL that will cause an issue when combining:
  bsgmfit.final.old$proximity <- NULL
  bsgmfit.final.old$predicted <- 0  
  bsgmfit.final.old$oob.times <- NA
  bsgmfit.final.old$votes <- NA
  
  if (only.names){
    fixed.predictors <- gsub("((?:urb|ghsl|wdpa).*)_[0-9]{4}", 
                             "\\1",
                             row.names(importance(bsgmfit.final.old)),
                             perl = TRUE)
    return(fixed.predictors)  
  }
  
  ##  For every RF object found:
  for(i in 1:length(list.of.old.bsgmfits.final)){
    if(i==1){
      
      next()
    }
    ##  Load it in:
    local_env.bsgmfit_final <- local({load(file=list.of.old.bsgmfits.final[[i]]);environment()})
    
    ##  Remove the proximity and remove the predictions:
    local_env.bsgmfit_final$bsgmfit_final$proximity <- NULL
    local_env.bsgmfit_final$bsgmfit_final$predicted <- 0
    local_env.bsgmfit_final$bsgmfit_final$votes <-NA
    local_env.bsgmfit_final$bsgmfit_final$oob.times <-NA
    ##  Make sure the names are stripped of any year ending:
    strip_names <- gsub("((?:urb|ghsl|wdpa).*)_[0-9]{4}",
                        "\\1",
                        names(local_env.bsgmfit_final$bsgmfit_final$forest$xlevels),
                        perl = TRUE)
    strip_names[which(startsWith(strip_names,"ghsl"))] <- "bs_dst"
    
    names(local_env.bsgmfit_final$bsgmfit_final$forest$xlevels) <- strip_names
    row.names(local_env.bsgmfit_final$bsgmfit_final$importance) <- strip_names
    row.names(local_env.bsgmfit_final$bsgmfit_final$importanceSD) <- strip_names
    names(local_env.bsgmfit_final$bsgmfit_final$forest$ncat) <- strip_names
    #str(bsgmfit.final.old)
    #str(local_env.bsgmfit_final$bsgmfit_final)
    
    ##  Combine it with the other bsgmfit finals:
    logdebug(paste0( 'Combine old bsgmfit final with  ',
                     list.of.old.bsgmfits.final[[i]] ))
    bsgmfit.final.old <- combine(bsgmfit.final.old,
                                 local_env.bsgmfit_final$bsgmfit_final)    
  } 
  
  ##  Return the final old bsgmfit (combined or solo):
  return(bsgmfit.final.old)  
  
}




################################################################################
# get_bsgmfit_quant_old <- function(only.names=FALSE) {
#   ##  Function which retrieves previously constructed bsgmfit_quant.RData
#   ##  objects.
#   err_mess <- ""
#   
#   list.of.old.bsgmfits.quant <- list.files(paste0(bsgm.data.old.bsgmfits.quant),
#                                           pattern=paste0("\\.Rdata$"),
#                                           full.names=TRUE) 
#   
#   
#   logdebug(paste0('Loading old bsgmfit final from: ',
#                   bsgm.data.old.bsgmfits.quant))
#   
#   if ( length(list.of.old.bsgmfits.quant) == 0 ){
#     err_mess <- paste0('There is no old bsgmfit Please check the folder : ',
#                        bsgm.data.old.bsgmfits.quant)
#     stop(err_mess)
#   }
#   
#   ##  Load it:
#   logdebug(paste0( 'Load old bsgmfit quant  with  ',
#                    list.of.old.bsgmfits.quant[[1]] ))  
#   local_env.bsgmfit_quant <- local({load(file=list.of.old.bsgmfits.quant[[1]]);environment()})
#   
#   bsgmfit.quant.old <- local_env.bsgmfit_quant$bsgmfit_quant
#   bsgmfit.quant.old$proximity <- NULL
#   bsgmfit.quant.old$predicted <- 0  
#   
#   if (only.names){
#     
#     fixed.predictors <- row.names(importance(bsgmfit.quant.old))    
#     return(bsgmfit.quant.old)  
#   }  
#   
#   
#   for ( i in 1:length(list.of.old.bsgmfits.quant) ) {
#     
#     if (i==1) next()
#     
#     local_env.bsgmfit_quant <- local({load(file=list.of.old.bsgmfits.quant[[i]]);environment()})
#     
#     local_env.bsgmfit_quant$bsgmfit_quant$proximity <- NULL
#     local_env.bsgmfit_quant$bsgmfit_quant$predicted <- 0
#     
#     ##  Combine it with the other bsgmfit finals:
#     logdebug(paste0( 'Combine old bsgmfit final with  ',
#                      list.of.old.bsgmfits.quant[[i]] ))
#     bsgmfit.quant.old <- combine( bsgmfit.quant.old,
#                                   local_env.bsgmfit_quant$bsgmfit_quant )    
#   } 
#   
#   ##  Return it:
#   return(bsgmfit.quant.old)  
#   
# }


################################################################################




set_fixed_set_to_existing_countries <- function() {
  ## Set the fixed_set to existing countries if you are using an existing
  ##    set of randomForest objects to predict from:
  if(bsgm.fixed.set){  
    ##  Retrieve the old bsgmfit(s) and bsgmfit_quant(s):
    bsgmfit.final.old <- get_bsgmfit_final_old()
    ##  We are not using the quant objects in the model currently. 
    #bsgmfit.quant.old <- get_bsgmfit_quant_old()
    
    ##  If creating a hybrid RF and we have more than the minimum observations 
    ##  for creating a new RF :
    if(bsgm.fixed.set.incl.input.countries==TRUE){
      ##  Put the objects into another variable:
      bsgmfit.final.tmp <- bsgmfit_final
      #bsgmfit.quant.tmp <- bsgmfit_quant
      
      ##  Get rid of any memory hogging proximity and wipe the predictions:
      bsgmfit.final.tmp$proximity <- NULL
      bsgmfit.final.tmp$predicted <- 0
      
      #bsgmfit.quant.tmp$proximity <- NULL
      #bsgmfit.quant.tmp$predicted <- 0
      ##  Combine the  old and tmp
      bsgmfit_final <<- combine(bsgmfit.final.tmp, bsgmfit.final.old)
      #bsgmfit_quant <<- combine(bsgmfit.quant.tmp, bsgmfit.quant.old)  
      
    }else{
      ##  Just load the old ones:
      bsgmfit_final <<- bsgmfit.final.old
      #bsgmfit_quant <<- bsgmfit.quant.old
    }
  }  
}


################################################################################




get_fixed_predictors <- function() {
  ##  Get a list of the covariate names to be used in the prediction phase:  
  if(bsgm.fixed.set) {
    
    fixed.predictors <- get_bsgmfit_final_old(only.names=TRUE)
    
  }else{
    
    fixed.predictors <- get.covariates.var.names() 
  }
  
  return(fixed.predictors)
}


################################################################################




save_rf_pred <- function(rf_pred,mask) {
  
  logdebug(paste0('Saving : ',bsgm.predict.density.rf.pred))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density.rf.pred), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_pred),start =1 )
  sd <- writeStop(sd)
  
}


################################################################################




save_rf_sd <- function(rf_sd,mask) {
  
  logdebug(paste0('Saving : ',bsgm.predict.density.rf.sd))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density.rf.sd), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
}


################################################################################




save_all_pred <- function(rf_sd,mask) {
  
  save_rf_pred (rf_sd$result_rf_pred,mask)
  save_rf_sd(rf_sd$result_rf_sd,mask)
  
  colnames(oper) <-c('result_rf_pred','result_rf_sd','result_rf_05',
                     'result_rf_50','result_rf_95')
  
  bsgm.predict.density_rf_05 <- paste0("predict_density_rf_05_",
                                       bsgm.countries.tag, ".tif")
  bsgm.predict.density_rf_50 <- paste0("predict_density_rf_50_",
                                       bsgm.countries.tag, ".tif")
  bsgm.predict.density_rf_95 <- paste0("predict_density_rf_95_",
                                       bsgm.countries.tag, ".tif")
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_05))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_05), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_50))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_50), 
                   format="GTiff", datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)
  
  logdebug(paste0('Saving : ',bsgm.predict.density_rf_95))
  
  sd <- writeStart(mask, 
                   filename=paste0(bsgm.output.path.countries,
                                   bsgm.predict.density_rf_95), 
                   format="GTiff", 
                   datatype="FLT4S", 
                   overwrite=TRUE, 
                   options=c("COMPRESS=LZW"))
  
  sd <- writeValues(sd, as.matrix(rf_sd),start =1 )
  sd <- writeStop(sd)   
}


################################################################################




get_bsgmfit <- function() {
  ##  If the bsgmfit object already exists:
  if (file.exists(paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData))) {
    ##  Load it and toss up the Variable Importance Plot:
    loginfo(paste0("Loading bsgmfit object from ",bsgm.bsgmfit.RData))
    load(file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData))
    
    varImpPlot(bsgmfit) 
    ##  Return that object:
    return(bsgmfit)
  }
  ##  Set the seed:
  set.seed(1964)
  
  ##  If the fixed set is FALSE:
  if (!bsgm.fixed.set) {
    logdebug("fixed_set is FALSE. Will optimize the model... ") 	
    start_time <- Sys.time()
    init_bsgmfit <- get_init_bsgmfit()
    
    ##	Now we will optimize the model by iteratively removing any 
    ##		covariates with negative increases in node purity:
    
    ##	Get list of covariates that have an importance score greater than 0:
    importance_scores <- importance(init_bsgmfit)[order(importance(init_bsgmfit)[,1],
                                                        decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
    
    if (length(pos_importance) == length(importance_scores[,1])) {
      
      x_data <- x_data[pos_importance]
      
      bsgmfit = tuneRF(x=x_data, 
                       y=y_data, 
                       plot=TRUE, 
                       #mtryStart=length(x_data)/3, 
                       #ntreeTry=length(y_data)/20, 
                       improve=0.0001, 
                       stepFactor=1.20, 
                       trace=TRUE, 
                       doBest=TRUE, 
                       nodesize=length(y_data)/1000, 
                       na.action=na.omit, 
                       importance=TRUE, 
                       proximity=bsgm.proximity, 
                       sampsize=min(c(length(y_data), 1000)), 
                       replace=TRUE) 
      
    }else{
      while (length(pos_importance) < length(importance_scores[,1])) {
        
        logdebug(" Jumping into the [while (length(pos_importance) < length(importance_scores[,1])) ] ... ")
        ##	Subset our x_data to just those columns having positive scores:
        x_data <- x_data[pos_importance]
        
        bsgmfit = tuneRF(x=x_data, 
                         y=y_data, 
                         plot=TRUE, 
                         mtryStart=floor(sqrt(ncol(x_data))), 
                         ntreeTry=500, 
                         improve=0.0001, 
                         stepFactor=1.20, 
                         trace=TRUE, 
                         doBest=TRUE, 
                         nodesize=length(y_data)/1000, 
                         na.action=na.omit, 
                         importance=TRUE, 
                         proximity=bsgm.proximity, 
                         sampsize=min(c(length(y_data), 1000)), 
                         replace=TRUE) 
        
        ##	Re-check importance scores:
        importance_scores <- importance(bsgmfit)[order(importance(bsgmfit)[,1], 
                                                       decreasing=TRUE),]
        pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
        print(bsgmfit)
        
      } ## End of while loop
    }
    
    end_time <- Sys.time()
    loginfo(paste("Elapsed Fitting Time:", tmDiff(start_time,end_time)))
    
  }else{
    ##  If the fixed set is TRUE, retrieve the old bsgmfit(s):   
    bsgmfit_final_old <- get_bsgmfit_final_old()
    
    bsgmfit = randomForest(x=x_data, 
                           y=y_data, 
                           mtry=bsgmfit_final_old$mtry, 
                           ntree=bsgmfit_final_old$ntree, 
                           nodesize=length(y_data)/1000, 
                           importance=TRUE, 
                           proximity=bsgm.proximity)
    print(bsgmfit)
    
  }  
  
  loginfo(paste0("Saving ",bsgm.bsgmfit.RData))
  ##	Save off our bsgmfit object for this set of data:
  save(bsgmfit, file=paste0(bsgm.output.path.countries.tmp, bsgm.bsgmfit.RData)) 
  
  ##  Don't toss up the varimplot as it will not work for combined models
  #varImpPlot(bsgmfit)  
  
  return(bsgmfit)
}


################################################################################




rasterize_data  <- function(df,val_col_name,flname) {
  ##  Declare the column names:
  colnames(df) <- c("ADMINID", val_col_name)
  
  ##  Use the L1 admin areas as the template raster:
  rst <- raster(censusmaskPathFileName)
  
  ##  Rasterize the values:
  tmpRaster <- wpRasterize(rst,
                           df,
                           cores=bsgm.cluster_workers,
                           filename=paste0(bsgm.output.path.countries.tmp, flname),
                           overwrite=TRUE,
                           silent=TRUE, 
                           minblk = wpGetBlocksNeeded(rst, 
                                                      cores=bsgm.cluster_workers,
                                                      n=nmb))
  
  
  return(tmpRaster)
  
}