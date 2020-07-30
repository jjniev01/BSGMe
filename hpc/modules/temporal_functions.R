##  AUTHOR:  JEREMIAH J. NIEVES
##  CREATED:  2018-04-23
##  NOTES:  These functions are meant to be utilized within a parallel task 
##          farm. The witholding is necessary because the ARIMA and ETS models 
##          have different starting assumptions and therefore need to be 
##          compared to hold out samples and compare the model errors based on 
##          MAPE for mean absolute percent error.
##
##          Additionally, we are comparing the models by witholding 'h' time
##          steps corresponding to the number of time steps we are projecting. 
##          This is being done as opposed to a one-step comparison or cross-
##          validation given the extremely short nature of the time series at 
##          this point (15 observations with <=20 being already considered quite
##          short). Also because of the nature of our data, and past findings 
##          that the historical mean or simple linear or exponential functions
##          can often outperform more sophisticated time series methods, we also
##          compare a linear growth and exponential growth model with the ARIMA 
##          and ETS models.
##          Should 'h' be larger than the length of the input time series then 
##          this code will need some reworking to allow for that within the 
##          model selection process.
####


##  ARRANGE TEMPORAL DATA FUNCTION  --------------------------------------------
arrange.temporal <- function(indata, retain_column){
  ##  indata should be a data.frame or data.table containing the YEAR, a single 
  ##  GID, BSPOP, and BSPDBAR. retain_column is the column for which we want to 
  ##  retain when creating the time series object, e.g. BSPOP, or BSPDBAR.
  ##  Note:  This function requires the package 'tseries'.
  
  ##  Turn the YEAR column into a time series-able date:
  indata <- indata[YEAR <= t0,]
  indata$DATE <- paste0(indata$YEAR,"-12-31")
  indata$DATE <- as.Date(indata$DATE)
  
  ##  Create the singlular time series:
  foo_ts <- ts(indata[,retain_column,with=F])
  
  return(foo_ts)
}




##  HISTORICAL MEAN FITTING  ---------------------------------------------------
# fitter.HIST.init <- function(ints, k_value, h_value = 1, testmode = "rolling"){
#   ##  ints is an input timeseries for a single GID.
#   ##  k_value is the size of the training set desired minus 1
#   ##  h_value is the size of the predictive step if not using the rolling test (optional)
#   ##  Mode can be either 'rolling' for a one-step test or 'multistep' for a 
#   ##  single multistep test (optional)
#   ##  WARNING IF testmode == 'rolling' then h_value must equal 1
#   if(testmode == "rolling"){
#     ##  Create a holder for all the k-cross validations we'll need:
#     mape_holder <- vector(mode = "numeric",length = {length(ints)-k_value})
#     for(i in 1:{length(ints)-k_value-1}){
#       ##  Calculate the mean value for the observed data:
#       fooHIST <- mean(ints[1:{k_value+i}])
#       holdout_ts <- ints[{k_value+i+1}]
#       mape_holder[i] <- 100 *
#         sum(abs({as.numeric(holdout_ts)-fooHIST}/as.numeric(holdout_ts))) 
#     }
#     return(median(mape_holder))
#   }
#   if(testmode == "multistep"){
#     fooHIST <- mean(ints[1:{k_value+1}])
#     holdout_ts <- ints[{k_value+1+h_value}]
#     mape <- 100 *
#       sum(abs({as.numeric(holdout_ts)-fooHIST}/as.numeric(holdout_ts)))
#     return(mape)
#   }
# }




##  ARIMA FITTING FUNCTION  ----------------------------------------------------
fitter.ARIMA.init <- function(ints, k_value, h_value = 1, testmode = "rolling"){
  ##  ints is an input timeseries for a single GID.
  ##  k_value is the size of the training set desired minus 1
  ##  h_value is the size of the predictive step if not using the rolling test (optional)
  ##  Mode can be either 'rolling' for a one-step test or 'multistep' for a 
  ##  single multistep test (optional)
  ##  WARNING IF testmode == 'rolling' then h_value must equal 1
  if(testmode == "rolling"){
    mape_holder <- vector(mode = "numeric", length = {length(ints)-k_value})
    for(i in 1:{length(ints)-k_value-1}){
      ##  Automatically fit the best non-seasonal ARIMA class model:
      fooRIMA <- auto.arima(ints[1:{k_value+i}],
                            seasonal = FALSE, ic = "bic",parallel = FALSE, 
                            allowdrift = TRUE, allowmean = TRUE)
      holdout_ts <- ints[{k_value+i+1}]
      foo_fore <- forecast(fooRIMA,h=1)
      mape_holder[i] <- 100 *
        sum(abs({as.numeric(holdout_ts)-foo_fore$mean[1]}/as.numeric(holdout_ts)))
    }
    return(median(mape_holder))
  }
  if(testmode == "multistep"){
    fooRIMA <- auto.arima(ints[1:{k_value+1}],
                          seasonal = FALSE, ic = "bic",parallel = FALSE, 
                          allowdrift = TRUE, allowmean = TRUE)
    holdout_ts <- ints[{k_value+1}:{k_value+h_value}]
    foo_fore <- forecast(fooRIMA,h=h_value)$mean[h_value]
    mape <- 100 *
      sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    return(mape)
  }
}




##  ETS FITTING FUNCTION  ------------------------------------------------------
fitter.ETS.init <- function(ints, k_value, h_value = 1, testmode = "rolling"){
  ##  ints is an input timeseries for a single GID.
  ##  k_value is the size of the training set desired minus 1
  ##  h_value is the size of the predictive step if not using the rolling test (optional)
  ##  Mode can be either 'rolling' for a one-step test or 'multistep' for a 
  ##  single multistep test (optional)
  ##  WARNING IF testmode == 'rolling' then h_value must equal 1
  if(testmode == "rolling"){
    mape_holder <- vector(mode = "numeric", length = {length(ints)-k_value})
    for(i in 1:{length(ints)-k_value-1}){
      fooETS <- ets(ints[1:{k_value+i}],
                    ic = "bic",
                    allow.multiplicative.trend = TRUE)
      holdout_ts <- ints[k_value+i+1]
      foo_fore <- forecast(fooETS,h=h_value)$mean[h_value]
      mape_holder[i] <- 100 *
        sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
      ##  Return the MAPE value for comparison:
    }
    return(mape_holder)
  }
  if(testmode == "multistep"){
    fooETS <- ets(ints[1:{k_value+1}],
                  ic = "bic",
                  allow.multiplicative.trend = TRUE)
    holdout_ts <- ints[{k_value+1}:{k_value+h_value}]
    foo_fore <- forecast(fooETS,h=h_value)$mean[h_value]
    mape <- 100 *
      sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    return(mape)
  }
}




##  HISTORICAL LS LINEAR GROWTH/DECAY FITTING FUNCTION  -----------------------
fitter.LIN.init <- function(ints, k_value, h_value = 1, testmode = "rolling"){
  ##  ints is an input timeseries for a single GID.
  ##  k_value is the size of the training set desired minus 1
  ##  h_value is the size of the predictive step if not using the rolling test 
  ##  (optional).
  ##  Mode can be either 'rolling' for a multiple one-step test or 'multistep' 
  ##  for a single multistep test (optional)
  ##  WARNING IF testmode == 'rolling' then h_value must equal 1
  if(testmode == "rolling"){
    mape_holder <- vector(mode = "numeric", length = {length(ints)-k_value})
    for(i in 1:{length(ints)-k_value-1}){
      ##  This function provides an initial fit on a 2/3 sample of time points and 
      ##  calculating the MAPE of the estimate 6 steps ahead:
      holdout_ts <- ints[k_value+i+1]
      ##  Put the test data into a data.frame for the glm function:
      testdf <- data.frame("VAL" = as.numeric(ints[1:{k_value+i}]),
                           "TIME" = 1:{k_value+i})
      ##  Least Squares Linear best Fit:
      fooLIN <- glm(VAL~TIME, data = testdf)
      #avg_delta <- mean({ints[2:{k_value+i}]-ints[1:{k_value+i-1}]},
      #                  na.rm = TRUE)
      ##  Predict next h steps, i.e. 1, using the average rate of change:
      foo_fore <- predict(fooLIN,data.frame("TIME"={k_value+i+1}))
      mape_holder[i] <- 100 *
        sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    }
      return(mape_holder)
  }
  if(testmode == "multistep"){
    ##  This function provides an initial fit on a 2/3 sample of time points and 
    ##  calculating the MAPE of the estimate 6 steps ahead:
    holdout_ts <- ints[{k_value+1}:{k_value+h_value}]
    ##  Put the test data into a data.frame for the glm function:
    testdf <- data.frame("VAL" = as.numeric(ints[1:{k_value+1}]),
                         "TIME" = 1:{k_value+1})
    ##  Least Squares Linear best Fit:
    fooLIN <- glm(VAL~TIME, data = testdf)
    ##  Historical change per timestep:
    # avg_delta <- mean({ints[2:{k_value+1}]-ints[1:{k_value}]},
    #                   na.rm = TRUE)
    ##  Predict next h steps using the average rate of change:
    foo_fore <- predict(fooLIN, data.frame("TIME"={{k_value+1}:{k_value+h_value}}))
    mape <- 100 *
      sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    return(mape)
  }
}




##  INITIAL HIST. AVG. EXP GROWTH/DECAY FITTING FUNCTION  ----------------------
fitter.EXP.init <- function(ints, k_value, h_value=1, testmode = "rolling"){
  #  ints is an input timeseries for a single GID.
  ##  k_value is the size of the training set desired minus 1
  ##  h_value is the size of the predictive step if not using the rolling test (optional)
  ##  Mode can be either 'rolling' for a one-step test or 'multistep' for a 
  ##  single multistep test (optional)
  ##  WARNING IF testmode == 'rolling' then h_value must equal 1
  ####
  ##  Adding a miniscule amount to prevent NA/Inf from appearring and kicking 
  ##  off errors in the fit. May eventually look at a zero inflated or hurdle 
  ##  type handling.
  ints <- ints+0.000000001
  if(testmode == "rolling"){
    mape_holder <- vector(mode = "numeric", length = {length(ints)-k_value})
    for(i in 1:{length(ints)-k_value-1}){
      holdout_ts <- ints[{k_value+i+1}]
      ##  Put the test data into a data.frame for the glm function:
      testdf <- data.frame("VAL" = as.numeric(ints[1:{k_value+i}]),
                           "TIME" = 1:{k_value+i})
      ##  Least Squares LOG NORMAL best Fit:
      fooEXP <- glm(log(VAL)~TIME, data = testdf)
      #avg_delta <- mean({ints[2:{k_value+i}]-ints[1:{k_value+i-1}]},
      #                  na.rm = TRUE)
      ##  Predict next h steps, i.e. 1, using the average rate of change:
      foo_fore <- exp(predict(fooEXP,data.frame("TIME"={k_value+i+1})))
      mape_holder[i] <- 100 *
        sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    }
    ##  Return the rate:
    return(mape_holder)
  }
  if(testmode == "multistep"){
    holdout_ts <- ints[{k_value+1}:{k_value+h_value}]
    ##  Put the test data into a data.frame for the glm function:
    testdf <- data.frame("VAL" = as.numeric(ints[1:{k_value+1}]),
                         "TIME" = 1:{k_value+1})
    ##  Least Squares LOG NORMAL best Fit:
    fooEXP <- glm(log(VAL)~TIME, data = testdf)
    
    ##  Predict next h steps using the average rate of change:
    foo_fore <- exp(predict(fooEXP, data.frame("TIME"={{k_value+1}:{k_value+h_value}})))
    mape <- 100 *
      sum(abs({as.numeric(holdout_ts)-foo_fore}/as.numeric(holdout_ts)))
    return(mape)
  }
}




##  FINAL HIST FITTING FUNCTION  -----------------------------------------------
# fitter.HIST.final <- function(ints){
#   fooHIST <- mean(ints)
#   
#   return(fooHIST)
# }




##  FINAL ARIMA FITTING FUNCTION  ----------------------------------------------
fitter.ARIMA.final <- function(ints){
  fooRIMA <- auto.arima(ints,
                        seasonal = FALSE, ic = "bic",parallel = FALSE, 
                        allowdrift = TRUE, allowmean = TRUE)#, stepwise = TRUE, 
  #approximation = TRUE)
  return(fooRIMA)
}




##  FINAL ETS FITTING FUNCTION  ------------------------------------------------
fitter.ETS.final <- function(ints){
  fooETS <- ets(ints,
                ic = "bic",
                allow.multiplicative.trend = TRUE)
  
  return(fooETS)
}




##  FINAL HIST. AVG LINEAR GROWTH/DECAY FITTING FUNCTION  ----------------------
fitter.LIN.final <- function(ints){
  ##  Put into data.frame for the glm function:
  foodf <- data.frame("VAL" = as.numeric(ints), "TIME" = 1:length(ints))
  ##  Fit the LS Linear line of best fit through the observed data:
  fooLIN <- glm(VAL~TIME, data = foodf)
  
  ##  Return the model:
  return(fooLIN)
}




##  FINAL HIST. AVG. EXP GROWTH/DECAY FITTING FUNCTION  ------------------------
fitter.EXP.final <- function(ints){
  ##  Put into data.frame for the glm function:
  ##  Offsetting all values by 0.000000001 to eliminate zeros:
  foodf <- data.frame("VAL" = {as.numeric(ints)+0.000000001}, "TIME" = 1:length(ints))
  ##  Fit the LS Linear line of best fit through the observed data:
  fooEXP <- glm(log(VAL)~TIME, data = foodf)
  ##  Return the rate:
  return(fooEXP)
}




##  ARIMA REDUCTION FUNCTION  --------------------------------------------------
reduce.ARIMA <- function(object){
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  arimastring <- paste0("ARIMA(", order[1], ",", order[2], ",", order[3], 
                  ") non-seasonal ", sep = "")
  if (is.element("constant", names(object$coef)) |
      is.element("intercept", names(object$coef))){ 
    arimastring <- paste(arimastring, "with non-zero mean ")}
  else if (is.element("drift", names(object$coef))){ 
    arimastring <- paste0(arimastring, "with drift ", 
                          round(object$coef[[1]],
                          digits = 3))}
  else if (order[2] == 0 & order[5] == 0){
    arimastring <- paste0(arimastring, "with zero mean ")}
  return(arimastring)
}




##  LIN REDUCTION FUNCTION  ----------------------------------------------------
reduce.LIN <- function(object){
  ##  Retrieve the intercept value to 3 decimal places:
  intercept_str <- as.character(round(coefficients(object)[1],digits = 3))
  ##  Retrieve the time coefficient value to 3 decimal places:
  timecov_str <- as.character(round(coefficients(object)[2],digits = 3))
  
  linstring <- paste0("Linear regression with intercept of ", intercept_str,
                      " and time coefficient of ", timecov_str)
  return(linstring)
}




##  EXP REDUCTION FUNCTION  ----------------------------------------------------
reduce.EXP <- function(object){
  ##  Retrieve the log-scale intercept value to 3 decimal places:
  intercept_str <- as.character(round(coefficients(object)[1],digits = 3))
  ##  Retrieve the log-scale time coefficient value to 3 decimal places:
  timecov_str <- as.character(round(coefficients(object)[2],digits = 3))
  
  expstring <- paste0("Log normal regression with log-scale intercept of ", intercept_str,
                      " and log-scale time coefficient of ", timecov_str)
  return(expstring)
}




##  ETS REDUCTION FUNCTION  ----------------------------------------------------
reduce.ETS <- function(object){
  components <- object$components
  etsstring <- object$method
  if(components[4]){
    etsstring <- paste0(etsstring, " with damped trend")
  }
  return(etsstring)  
}




##  FINAL MODEL SELECTION FUNCTION  --------------------------------------------
final.time.model.select <- function(arimamape = 10000, 
                                    etsmape = 10000,
                                    linmape = 10000,
                                    expmape = 10000){
  ##  Take care of any passed mape values which may be NULL or NA or anything 
  ##  else by setting them absurdly high so they are not chosen as the final 
  ##  model somehow:
  if(!is.numeric(arimamape) | is.na(arimamape) | is.null(arimamape)){
    arimamape <- 10000}
  if(!is.numeric(etsmape) | is.na(etsmape) | is.null(etsmape)){etsmape <- 10000}
  if(!is.numeric(linmape) | is.na(linmape) | is.null(linmape)){linmape <- 10000}
  if(!is.numeric(expmape) | is.na(expmape) | is.null(expmape)){expmape <- 10000}
  # if(!is.numeric(histmape) | is.na(histmape) | is.null(histmape)){histmape <- 10000}
  ##  List final model tags in order:
  finmods <- c("ETS","ARIMA","EXP","LIN")
  ##  Compile them all in a vector going from "most complicated" to 
  ##  "least complicated"; this will be used to determine preference should any 
  ##  ties occur:
  allmapes <- c(etsmape,arimamape,expmape,linmape)
  ##  Find the minimum MAPE value index:
  minmape <- which.min(allmapes)
  ## If there are duplicates, we need to check against all:
  if(length(which(allmapes == allmapes[minmape])) > 1){
    ##  Retrieve the indices of all duplicates:
    dupes <- which(allmapes == allmapes[minmape])
    ##  Of those duplicates, pick the one with the lowest index, i.e. the 
    ##  "simplest" one:
    minmape <- which.max(dupes)
  }
  
  ##  Get the corresponding model type based upon the index of the selected 
  ##  model:
  finmod <- finmods[minmape]
  ##  Return the final model:
  return(finmod)
}




##  MAIN PARALLEL FITTING FUNCTION  --------------------------------------------
time.model.fit.prl <- function(i,
                               k = bsgm.min.time.series,
                               runmode = run_mode,
                               test_mode = bsgm.model.test.mode){
  ##  Note:  i is the node tag. mode can be "BSPOP" or "BSPDBAR", which doesn't 
  ##         change the functionality, but will change its output tag.
  ##
  ####
  if(test_mode == "multistep"){
    h <- bsgm.predict.steps
  }else{h <- 1}
  
  ##  Retrieve the gid:
  g <- gid_list[i]
  
  ##  Retrieve the time series object for this iteration:
  ts_full <- arrange.temporal(hist_pop[hist_pop$GID == g,], run_mode)
  
  ##  Get the initial historical mean model object:
  #best.hist <- fitter.HIST.init(ts_full, k_value = k, h_value = h,
  #                              testmode = test_mode)
  ##  Get the best initial ARIMA model object:
  best.arima <- fitter.ARIMA.init(ts_full, k_value = k, h_value = h,
                                  testmode = test_mode)
  ##  Get the best initial exponetial smooth (ets) model object:
  best.ets <- fitter.ETS.init(ts_full, k_value = k, h_value = h,
                              testmode = test_mode)
  # if(runmode != "BSPDBAR"){
  #   ##  Get the best initial linear model:
  #   best.lin <- fitter.LIN.init(ts_full, k_value = k, h_value = h, 
  #                               testmode = test_mode)}
  
  ##  Get the best initial exponential model:
  best.exp <- fitter.EXP.init(ts_full, k_value = k, h_value = h, 
                              testmode = test_mode)
  
  ##  Compare the two models to determine which is best and then reduce it to 
  ##  table type data that minimizes the memory space, but allows for its 
  ##  reconstruction on demand:

  final_model_type <- final.time.model.select(arimamape = best.arima, 
                                              etsmape = best.ets,
                                              #linmape = best.lin,
                                              expmape = best.exp)
  
  
  ##  Depending on which model had less error:
  # if(final_model_type == "HIST"){
  #   ##  Get the historical average:
  #   hist <- fitter.HIST.final(ts_full)
  #   ##  Get the model descriptive string:
  #   final_model_desc <- paste0("Historical average of ", 
  #                              as.character(round(hist, digits = 4)))
  #   ##  Produce a prediction for the number of desired/indicated time steps and 
  #   ##  place it in the object which is being returned from this function:
  #   final_model_predict <- rep(hist,bsgm.predict.steps)
  #   ##  Construct the output format of all the data we want:
  #   outdf <- data.frame("GID" = rep(gid_list[i],bsgm.predict.steps),
  #                       "YEAR" = t0:{t0+bsgm.predict.steps},
  #                       "FORECAST" = as.numeric(final_model_predict),
  #                       "H95" = rep(NA,length(final_model_predict)),
  #                       "L95" = rep(NA,length(final_model_predict)),
  #                       "MOD.TAG" = rep(final_model_type,
  #                                       length(final_model_predict)),
  #                       "MOD.DESC" = rep(final_model_desc,
  #                                        length(final_model_predict)))
  # }
  if(final_model_type == "ARIMA"){
    ##  Fit the final ARIMA model based upon the entire dataset:
    final_model <- fitter.ARIMA.final(ts_full)
    ##  Get the model descriptive string:
    final_model_desc <- reduce.ARIMA(final_model)
    ##  Produce a prediction for the number of desired/indicated time steps and 
    ##  place it in the object which is being returned from this function:
    final_model_predict <- forecast(final_model,bsgm.predict.steps, level = c(95))
    ##  Construct the output format of all the data we want:
    outdf <- data.frame("GID" = rep(gid_list[i],length(final_model_predict$mean)),
                        "YEAR" = {t0+1}:{t0+bsgm.predict.steps},
                        "FORECAST" = as.numeric(final_model_predict$mean),
                        "H95" = as.numeric(final_model_predict$upper),
                        "L95" = as.numeric(final_model_predict$lower),
                        "MOD.TAG" = rep(final_model_type,
                                        length(final_model_predict$mean)),
                        "MOD.DESC" = rep(final_model_desc,
                                         length(final_model_predict$mean)),
                        stringsAsFactors = F)
  }
  if(final_model_type == "ETS"){
    ##  Fit the final ETS model based upon the entire dataset:
    final_model <- fitter.ETS.final(ts_full)
    ##  Get the model descriptive string:
    final_model_desc <- reduce.ETS(final_model)
    ##  Produce a prediction for the number of desired/indicated time steps and 
    ##  place it in the object which is being returned from this function
    final_model_predict <- forecast(final_model,bsgm.predict.steps, level = c(95))
    ##  Construct the output format of all the data we want:
    outdf <- data.frame("GID" = rep(gid_list[i],length(final_model_predict$mean)),
                        "YEAR" = {t0+1}:{t0+bsgm.predict.steps},
                        "FORECAST" = as.numeric(final_model_predict$mean),
                        "H95" = as.numeric(final_model_predict$upper),
                        "L95" = as.numeric(final_model_predict$lower),
                        "MOD.TAG" = rep(final_model_type,
                                        length(final_model_predict$mean)),
                        "MOD.DESC" = rep(final_model_desc,
                                         length(final_model_predict$mean)),
                        stringsAsFactors = F)
  }
  # if(final_model_type == "LIN"){
  #   ##  Get the linear model rate:
  #   final_model <- fitter.LIN.final(ts_full)
  #   ##  Get the model descriptive string:
  #   final_model_desc <- reduce.LIN(final_model)
  #   ##  Produce a prediction for the number of desired/indicated time steps and 
  #   ##  place it in the object which is being returned from this function:
  #   predictframe <- data.frame("TIME" = {length(ts_full)+1}:{length(ts_full)+bsgm.predict.steps})
  #   final_model_predict <- predict(final_model, predictframe)
  #   ##  Construct the output format of all the data we want:
  #   outdf <- data.frame("GID" = rep(gid_list[i],length(final_model_predict)),
  #                       "YEAR" = {t0+1}:{t0+bsgm.predict.steps},
  #                       "FORECAST" = as.numeric(final_model_predict),
  #                       "H95" = numeric(length = length(final_model_predict)),
  #                       "L95" = numeric(length = length(final_model_predict)),
  #                       "MOD.TAG" = rep(final_model_type,
  #                                       length(final_model_predict)),
  #                       "MOD.DESC" = rep(final_model_desc,
  #                                        length(final_model_predict)),
  #                       stringsAsFactors = F)
  # }
  if(final_model_type == "EXP"){
    ##  Get the exponential (log normal) model rate:
    final_model <- fitter.EXP.final(ts_full)
    ##  Get the model descriptive string:
    final_model_desc <- reduce.EXP(final_model)
    ##  Produce a prediction for the number of desired/indicated time steps and 
    ##  place it in the object which is being returned from this function:
    predictframe <- data.frame("TIME" = {length(ts_full)+1}:{length(ts_full)+bsgm.predict.steps})
    final_model_predict <- exp(predict(final_model, predictframe))
    ##  Construct the output format of all the data we want:
    outdf <- data.frame("GID" = rep(gid_list[i],length(final_model_predict)),
                        "YEAR" = {t0+1}:{t0+bsgm.predict.steps},
                        "FORECAST" = as.numeric(final_model_predict),
                        "H95" = numeric(length = length(final_model_predict)),
                        "L95" = numeric(length = length(final_model_predict)),
                        "MOD.TAG" = rep(final_model_type,
                                        length(final_model_predict)),
                        "MOD.DESC" = rep(final_model_desc,
                                         length(final_model_predict)),
                        stringsAsFactors = F)
  }
  
  return(outdf)
}




cluster_Timewarp <- function(forecast_df,...){
  ##  Description:
  ##  "Let's do the time warp aga-" seriously though,
  ##    Cluster task farm where each task is the model selection, fitting, 
  ##    of a series of model classes and the prediction of the mean, with 95%
  ##    intervals if applicable, and returning that to the main hadler as a 
  ##    dataframe where it is stored in a single dataframe before being output 
  ##    as a single dataframe for all years and given GIDs.
  ##
  ##  Parameters:
  ##
  ##  Values:
  ##
  ##
  ##  ---------------------------------------------------------------------  ##
  ##	Start the timer:
  tStart <- Sys.time()
  
  ##	Pull the cluster:
  cl <- getCluster()
  on.exit( returnCluster() )
  
  ##	Determine the number of cores we're working with:
  nodes <- length(cl)
  
  ##	Pass off required libraries and data to the cluster workers:
  clusterEvalQ(cl, {
    require(ts)
    require(forecast)
  })
  ##  Pass off the required data and functions to the nodes in the cluster
  ##   - this includes the lists used for informing predictions, and the
  ##     task functions that create the predictions/info for each subnational 
  ##     unit:
  clusterExport(cl, c("gid_list", 
                      "hist_pop",
                      "t0",
                      "t1",
                      "bsgm.model.test.mode",
                      "bsgm.min.time.series",
                      "bsgm.predict.steps",
                      "run_mode",
                      "arrange.temporal",
                      # "fitter.HIST.init",
                      "fitter.ARIMA.init",
                      "fitter.ETS.init",
                      "fitter.LIN.init",
                      "fitter.EXP.init",
                      # "fitter.HIST.final",
                      "fitter.ARIMA.final",
                      "fitter.ETS.final",
                      "fitter.LIN.final",
                      "fitter.EXP.final",
                      "reduce.ARIMA",
                      "reduce.ETS",
                      "reduce.EXP",
                      "reduce.LIN",
                      "final.time.model.select",
                      "time.model.fit.prl"))
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    ##  Send the taskMaker function call to the ith node with ith task from
    ##  the prediction_list as an argument and tag it with the value i
    sendCall(cl[[i]], time.model.fit.prl, i, tag=i)
  }
  
  
  ##	Create our primary cluster processing loop, recalling that we already
  ##		have clusters running:
  cat("Total tasks to process: ", length(gid_list), "\n")
  for (i in 1:length(gid_list)) {
    ##	Receive results from a node:
    predictions <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!predictions$value$success) {
      stop("ERROR: Cluster barfed...\n\n", predictions)
    }
    
    ##	Which block are we processing:
    block <- predictions$value$tag
    # cat("Received gid: ", block, "\n")
    # flush.console()
    
    ##	Now store our predictions in their proper rows of the data.frame:
    forecast_df[{i+(i-1)*(bsgm.predict.steps-1)}:
                  {i+i*{bsgm.predict.steps-1}},] <- predictions$value$value
    
    # forecast_df <- merge.data.frame(forecast_df, 
    #                                 predictions$value$value, 
    #                                 by.x = c("GID","YEAR"), all.x = T)
    
    ##	Check to see if we are at the end of our tasklist:
    ni <- nodes + i
    if (ni <= length(gid_list)) {
      ##	And if not, send it to the cluster node that just gave us
      ##		our last result...
      sendCall(cl[[predictions$node]], time.model.fit.prl, ni, tag=ni)
    }
    tEnd <- Sys.time()
    wpProgressMessage(i,
                      max = length(gid_list),
                      label = paste0("Received chunk ", ni,
                                     " Processing Time: ",
                                     wpTimeDiff(tStart,tEnd)))
  }
  
  
  ##  Return the full df of results:
  return(forecast_df)
}
