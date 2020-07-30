##  AUTHORS:  MAKSYM BONDARENKO & JEREMIAH J. NIEVES
load.Packages <- function(){
  
  ##  This function takes a given package name and attempts to load it. If it is
  ##  not installed the function will return a warning.
  ####
  ##
  ##  If there is a discrepancy between the packages listed and the packages 
  ##  installed:
  if (length(setdiff(bsgm.pkgs, rownames(installed.packages()))) > 0) {
    warning(paste0("The follwing R packages are not installed: ",
                   setdiff(bsgm.pkgs, rownames(installed.packages()))))
    return(FALSE)
  }else{
    ##  Load up the packages:
    sapply(bsgm.pkgs, require, character.only = TRUE)
    if (bsgm.DologDEBUG) 
      basicConfig(level='DEBUG')
  }
  
  
  ##  LOAD PACKAGES TO SUPPORT WP FTP ACCESS  ----------------------------------
  ##  If the custom package is installed:
  if( is.element("wpgpCovariates", installed.packages()[,1])){
    ##  Load the package:
    library("wpgpCovariates", quietly = TRUE)
  }else{
    ##  If the devtools package is installed:
    if ( is.element("devtools", installed.packages()[,1])){
      ##  Load it:
      library("devtools", quietly = TRUE)
      ##  Install the custom package from the GitHub repository:
      devtools::install_github('wpgp/wpgpCovariates')
    }else{
      ##  Install the devtools pacakge:
      install.packages('devtools')
      ##  Load it:
      library("devtools", quietly = TRUE)
      ##  Install the custom package from the GitHub repository:
      devtools::install_github('wpgp/wpgpCovariates')
    }  
    ##  Now load the custom package:
    if ( is.element("wpgpCovariates", installed.packages()[,1])){
      library("wpgpCovariates", quietly = TRUE)
      return(TRUE)
    }  
    ##  Should none of the above work, produce a warning and direct user to 
    ##  manually attempt:
    warning("Error: package wpgpCovariates is not installed. ")
    print("wpgpCovariates isn't available from CRAN yet, but you can get it from github with:")
    print("install.packages('devtools')")
    print("devtools::install_github('wpgp/wpgpCovariates')")
    return(FALSE)
  }
  
  
  
  ##  LOAD wpUtilities PACKAGE TO SUPPORT RF SCRIPT  ---------------------------
  ##  If the package is already installed:
  if(is.element("wpUtilities", installed.packages()[,1])){
    ##  Load it:
    library("wpUtilities", quietly = TRUE)
  }else{
    ##  If the devtools package is already installed:
    if(is.element("devtools", installed.packages()[,1])){
      ##  Load it:
      library("devtools", quietly = TRUE)
      ##  Install the custom package from the GitHub repository:
      devtools::install_github('wpgp/wpUtilities')
    }else{
      ##  Install the devtools package:
      install.packages('devtools')
      ##  Load it:
      library("devtools", quietly = TRUE)
      ##  Install the custom package from the GitHub repository:
      devtools::install_github('wpgp/wpUtilities')
    }  
    ##  If the package is installed now:
    if ( is.element("wpUtilities", installed.packages()[,1])){
      ##  Load it:
      library("wpUtilities", quietly = TRUE)
      return(TRUE)
    }  
    ##  Should none of the above work, produce a warning and direct user to 
    ##  manually attempt:
    warning("Error: package wpUtilities is not installed. ")
    print("wpUtilities isn't available from CRAN yet, but you can get it from github with:")
    print("install.packages('devtools')")
    print("devtools::install_github('wpgp/wpUtilities')")
    return(FALSE)
  }
  return(TRUE)
} 