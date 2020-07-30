##  AUTHOR:  JEREMIAH J. NIEVES
##  TITLE:  HIERARCHICAL TIME SERIES POST-PROCESSING
##  INITIAL DATE: 2018-05-14
##  NOTES:  This module contains functions for transforming the bottom-level, 
##          i.e. subnational unit level, forecasts into a format acceptable for 
##          implementing the optimal combination process from the hts package
##          for hierarchical time series. The idea is that given we are using
##          extremely short time series to forecast for individual units, that 
##          we can smooth these out based upon information , i.e. an OLS 
##          regression, of the larger study area. Should be important for longer
##          term forecasts and may help for areas that have particulalry noisy 
##          data. Best smoothing option until we can implement a spatially 
##          informed smoothing based upon a Bayesian iCAR or something.
####

formatHTS <- function(input_ts_forecasts, h = 0){
  ##  Function to take the base level forecasts from their initial dataframe and 
  ##  put them in a format acceptable for the hts package. Returns an hts object
  ####
  if(h==0){
    stop("Failed to declare the time step of the forecasts to retrieve.")
  }
  ##  Pull the bottom level forecasts for the time step as forecasts and sum one
  ##  for the total forecast. Each row represents one forecast horizon and each 
  ##  column represents on time series from the hierarchy (i.e. node). Item 
  ##  stored is a matrix:
  fore <- 
  ##  Make it an hts object
  htsobj <- hts(  bnames =, nodes = list(nrow(fore)))
  return(htsobj)
}