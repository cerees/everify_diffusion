#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#########################################################
###SET OPTIONS, INSTALL PACKAGES, AND CREATE FUNCTIONS###
#########################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#################
###SET OPTIONS###
#################
options(stringsAsFactors = F)
options(scipen=999)

###################
###LOAD PACKAGES###
###################

###PACKAGES###
if(!require(pacman)){
  install.packages('pacman')
}
pacman::p_load(plyr, dplyr, haven, tidyr, 
               plyr, reshape2, ggplot2, 
               foreign, tools, rgdal,
               ggmap, car, Hmisc, 
               maptools, PBSmapping, acs,
               stringr, gtools, blsAPI,
               ggvis, zoo, survival,
               spdep, sp, rgdal, maptools,
               igraph, McSpatial, glmulti) 

###FUNCTIONS###
#clear labels from imported stata data
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

#clean blsapi data
apiDF <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  
  i <- 0
  for(d in data){
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}
#######################################