library(RCurl)
library(rjson)

fetchclimate <- function(
                         data=NULL,
                         lat,
                         lon,
                         variable,
                         firstYear=1961,
                         lastYear=1990,
                         firstDay=1,
                         lastDay=365,
                         startHour=0,
                         stopHour=24,
                         url="http://fetchclimate2.cloudapp.net/",
                         ) {
  requestDataList <- list();
  
  namesToCheck <- c("lat","lon","firstYear","lastYear","firstDay","lastDay","startHour","stopHour")        
  
  requestData <- as.data.frame(requestDataList)
  
  for(name in namesToCheck) {
    v <- get(name)
    
    if(length(v)>1) {requestDataList[[v]] <-v} #explicit vector is set
    
    if(is.null(data)) {      
    }
    else
    {
      if((v %in% names(data)) && (length(data[[v]])>1)) {requestDataList[[name]] <- data[[v]]}
    }
    if(!(name %in% names(requestData)) && (length(get(name))==1) && (is.numeric(get(name)))) {
      requestData[[name]] <- rep(get(name),nrow(requestData))
    }
  }
  
  #now 
  requestData
}