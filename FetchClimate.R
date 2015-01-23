#git repo at ssh://home.dgrechka.net/data/git/Rfc.git

library(RCurl)
library(RJSONIO)

internal_fc.formPointsRequestBody <- function(envVar, # must be private
                                  lat,lon,
                                  years,
                                  days,
                                  hours) {
  ## JSON request object. 
  ## The text may be downloaded from http://fetchclimate2.cloudapp.net/form
  
  timeRegion <- list(
    Years=years,
    Days=days,
    Hours=hours,
    IsIntervalsGridYears=T,
    IsIntervalsGridDays=T,
    IsIntervalsGridHours=T
  )
  
  if(length(lat) == 1)
  {
    lat <- I(lat)
    lon <- I(lon)
  }
  
  domain <- list(
    Lats=lat,
    Lats2=NULL,
    Lons=lon,
    Lons2=NULL,
    TimeRegion=timeRegion,
    Mask=NULL,
    SpatialRegionType="Points"
  )    
  
  request <- list(
    EnvironmentVariableName=envVar,
    ParticularDataSources="", # any availalbe
    Domain=domain
  )
  j <- toJSON(request)
  #print(j)
  return(j)
}

internal_fc.reformPointsTimeseries <- function(resultList) { #must be private. accepts the decoded JSON recieved from FC result proxy. converts it into matrix
  N <- length(resultList$values)
  res <- matrix(ncol=length(resultList$values[[1]]),nrow=N)
  for(i in 1:N) {
    res[i,] = resultList$values[[i]]
  }
  return(res)
}

internal_fc.TimeSeries <-function(envVar, #must be private
                        lat,lon,
                        years,
                        days,
                        hours,
                        url) {
  N <- length(lat)
  if(length(lon) != N) {
    stop("lon and lat must be the same length");
  }  
  json <- internal_fc.formPointsRequestBody(envVar,
                                lat,lon,
                                years,
                                days,
                                hours);
  result <- internal_fc.fetchCore(json,url)
  resultMatrix <- internal_fc.reformPointsTimeseries(result)
  return(resultMatrix)
}

internal_fc.fetchCore <- function(jsonRequest,url) {
  #print("requesting JSON")
  #print(jsonRequest)
  h = basicTextGatherer()
  h$reset()
  curlPerform(url = paste(url,"/api/compute",sep=''),
              httpheader=c(Accept="text/plain", 'Content-Type' = "application/json"),
              postfields=jsonRequest,
              writefunction = h$update)
  reply = h$value()
  print(reply)
  
  ## wait while processing in progress
  while (substr(reply,1,7)=='pending' || substr(reply,1,8)=='progress') {
    Sys.sleep(1)
    hash=strsplit(reply,"hash=")[[1]][2]
    h$reset()
    curlPerform(url = paste(url,"/api/status?hash=",curlEscape(hash),sep=""),
                httpheader=c(Accept="text/plain"),
                writefunction = h$update)
    reply = h$value()
    print(reply)
  }
  
  result <- c()
  
  ## get result data
  if (substr(reply,1,9)=='completed') {
    msds = substr(reply,11,nchar(reply))
    h$reset()
    curlPerform(url = paste(url,"/jsproxy/data?uri=",curlEscape(msds),"&variables=values",sep=""),
                httpheader=c(Accept="application/json"),
                writefunction = h$update)
    #print(h$value())
    result=fromJSON(h$value())
  }
  return(result);
}


fcTimeSeriesYearly<-function(
  envVar,
  lat,lon,
  firstYear,lastYear,
  firstDay=1,lastDay=365,
  startHour=0,stopHour=24,
  url="http://fetchclimate2.cloudapp.net/") {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).
  #firstYear,lastYear are scalars
  
  #return matrix NxM. N = length(lat), M = timeseries length
  
  
  years <- seq(from=firstYear,to=lastYear+1,by=1)
  days <- c(firstDay,lastDay+1)
  hours <- c(startHour,stopHour)
  
  resultMatrix <-internal_fc.TimeSeries(envVar,lat,lon,years,days,hours,url)
  
  colnames(resultMatrix) <- years[1:(length(years)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesDaily<-function(
  envVar,
  lat,lon,
  firstDay=1,lastDay=365,
  firstYear=1961,lastYear=1990,
  startHour=0,stopHour=24,
  url="http://fetchclimate2.cloudapp.net/") {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).  
  #firstDay,lastDay are scalars
  
  #return matrix NxM. N = length(lat), M = timeseries length
  
  
  years <- c(firstYear,lastYear+1)
  days <- seq(from=firstDay,to=lastDay+1,by=1)
  hours <- c(startHour,stopHour)
  
  resultMatrix <-internal_fc.TimeSeries(envVar,lat,lon,years,days,hours,url)
  
  colnames(resultMatrix) <- days[1:(length(days)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesHourly<-function(
  envVar,
  lat,lon,
  startHour,stopHour,
  firstYear=1961,lastYear=1990,
  firstDay=1,lastDay=365,
  url="http://fetchclimate2.cloudapp.net/") {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).
  #startHour,stopHour are scalars
  
  #return matrix NxM. N = length(lat), M = timeseries length
  
  
  years <- c(firstYear,lastYear+1)
  days <- c(firstDay,lastDay+1)
  hours <- seq(from=startHour,to=stopHour+1)
  
  resultMatrix <-internal_fc.TimeSeries(envVar,lat,lon,years,days,hours,url)
  
  colnames(resultMatrix) <- hours[1:(length(hours))]
  
  return(resultMatrix)
}


fetchclimate <- function( #returns a column to be put to the dataframe  
  lat, #a column name (string), single number or  a vector of a numbers()
  lon,
  envVariable,
  data=NULL, #the dataframe to look for the column names
  firstYear=1961,
  lastYear=1990,
  firstDay=1,
  lastDay=365,
  startHour=0,
  stopHour=24,
  url="http://fetchclimate2.cloudapp.net/"
) {
  
  formTimeSliceRequestBody <- function(envVar,
                                       lat,lon,
                                       firstYear,lastYear,
                                       firstDay,lastDay,
                                       startHour,stopHour) {
    ## JSON request object. 
    ## The text may be downloaded from http://fetchclimate2.cloudapp.net/form
    
    timeRegion <- list(
      Years=c(firstYear,lastYear),
      Days=c(firstDay,lastDay),
      Hours=c(startHour,stopHour),
      IsIntervalsGridYears=T,
      IsIntervalsGridDays=T,
      IsIntervalsGridHours=T
    )
    
    
    domain <- list(
      Lats=lat,
      Lats2=NULL,
      Lons=lon,
      Lons2=NULL,
      TimeRegion=timeRegion,
      Mask=NULL,
      SpatialRegionType="Points"
    )    
    
    request <- list(
      EnvironmentVariableName=envVariable,
      ParticularDataSources="", # any availalbe
      Domain=domain
    )
    
    return(toJSON(request))
  }
  
  requestDataList <- list();
  
  vectorOrConstNames <- c("lat","lon","firstYear","lastYear","firstDay","lastDay","startHour","stopHour")        
  
  #first pass: setting vector values
  for(name in vectorOrConstNames) { 
    v <- get(name)
    
    if(length(v)>1) {
      if(is.numeric(v)) {requestDataList[[name]] <-v} #explicit vector is set
      else {
        stop(paste("You've set non-numeric vector as a argument for ",name,". please set the numeric one"));
      }
    }
    else {
      if(is.character(v)) { # the length is 1 here
        if(is.null(data)) {
          stop(paste("You specified ",v," as a data frame name for ",name," but data parameter is set to NULL. Please set data parameter as well"))
        }
        else
        {
          if((v %in% names(data)) && (length(data[[v]])>1)) {
            requestDataList[[name]] <- data[[v]]
          }
          else {
            stop(paste("You spacified ",v," as a column name in data.frame. But this column is not found"))
          }
        }
      }      
    }    
  }
  
  requestData <- as.data.frame(requestDataList)
  
  for(name in vectorOrConstNames) { # second pass: setting single values
    if(!(name %in% names(requestData)) && (length(get(name))==1) && (is.numeric(get(name)))) {
      requestData[[name]] <- rep(get(name),nrow(requestData))
    }
  }
  
  if(ncol(requestData) != length(vectorOrConstNames)) {
    stop("Not all parameters are set")
  }
  
  uniqueTimes <- unique(subset(requestData,select=c("firstYear","lastYear","firstDay","lastDay","startHour","stopHour")))
  result <- vector(length=nrow(requestData))
  for(i in 1:nrow(uniqueTimes)) {
    r <- uniqueTimes[1,]
    rFirstYear <- r$firstYear
    rLastYear <- r$lastYear
    rFirstDay <- r$firstDay
    rLastDay <- r$lastDay
    rStartHour <- r$startHour
    rStopHour <- r$stopHour
    
    print(paste("Fetching data for time slice ",as.character(i)," out of ",nrow(uniqueTimes)))
    
    timeMatchMap <- requestData$firstYear==rFirstYear &
      requestData$lastYear==rLastYear &
      requestData$firstDay==rFirstDay &
      requestData$lastDay==rLastDay &
      requestData$startHour==rStartHour &
      requestData$stopHour==rStopHour
    
    currentTimeSliceData <- requestData[timeMatchMap,]
    
    lats <- currentTimeSliceData$lat
    lons <- currentTimeSliceData$lon
    
    requestJson <- formTimeSliceRequestBody(envVariable,
                                            lats,lons,
                                            rFirstYear,rLastYear,
                                            rFirstDay,rLastDay,
                                            rStartHour,rStopHour)
    
    subresult <- fetchCore(requestJson,url)$values
    result[timeMatchMap] <- subresult
  }
  
  return(result)
}