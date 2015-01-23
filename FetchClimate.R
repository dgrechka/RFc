#git repo at ssh://home.dgrechka.net/data/git/Rfc.git

library(RCurl)
library(RJSONIO)
library(sp)

internal_fc.formRequestBody <- function(envVar, # must be private
                                              lat,lon,
                                              years,
                                              days,
                                              hours,
                                              spatialRegionType) {
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
    SpatialRegionType=spatialRegionType
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

internal_fc.reformGridResponse <- function(resultList,lats,lons) {#must be private. accepts the decoded JSON recieved from FC result proxy. converts it into matrix
  lonN <- length(lons)
  latN <- length(lats)  
  stratched_lats <- c()
  stratched_lons <- c()
  stratched_values <- c()
  for(i in 1:lonN) {
    stratched_lons <- c(stratched_lons,rep(lons[i],times=latN))
    stratched_lats <- c(stratched_lats,lats)
    stratched_values <- c(stratched_values,resultList$values[[i]])
  }
  resultdf <- data.frame(lon=stratched_lons,lat=stratched_lats,value=stratched_values)
  coordinates(resultdf) <- c("lon","lat") #promote to SpatialPointsDataFrame
  proj4string(resultdf) <- CRS("+proj=longlat")
  gridded(resultdf) <- TRUE # promote to SpatialPixelsDataFrame
  resultdf <- as(resultdf, "SpatialGridDataFrame") # promote to SpatialGridDataFrame. creates the full grid
  return(resultdf)
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
  json <- internal_fc.formRequestBody(envVar,
                                            lat,lon,
                                            years,
                                            days,
                                            hours,
                                            spatialRegionType="Points");
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
  variable,
  latitude,longitude,
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
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url)
  
  colnames(resultMatrix) <- years[1:(length(years)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesDaily<-function(
  variable,
  latitude,longitude,
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
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url)
  
  colnames(resultMatrix) <- days[1:(length(days)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesHourly<-function(
  variable,
  latitude,longitude,
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
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url)
  
  colnames(resultMatrix) <- hours[1:(length(hours))]
  
  return(resultMatrix)
}

fcGrid <- function(
  variable,
  latitudeFrom,latitudeTo,latitudeBy,
  longitudeFrom,longitudeTo,longitudeBy,  
  firstYear=1961,lastYear=1990,
  firstDay=1,lastDay=365,
  startHour=0,stopHour=24,
  url="http://fetchclimate2.cloudapp.net/") {
  
  lats <- seq(from=latitudeFrom,to=latitudeTo,by=latitudeBy)
  lons <- seq(from=longitudeFrom,to=longitudeTo,by=longitudeBy)
  
  years <- c(firstYear,lastYear+1)
  days <- c(firstDay,lastDay+1)
  hours <- c(startHour,stopHour)
  
  requestBody <- internal_fc.formRequestBody(variable,
      lats,lons,
      years,days,hours,
      "PointGrid")
  response <- internal_fc.fetchCore(requestBody,url)
  resMatrix <- internal_fc.reformGridResponse(response,lats,lons)
  return(resMatrix);
}

test.fc <-function() { #run automatic tests
  #declaring tests
  tests <- c()
  tests <- c(tests,function()
  {# test fcTimeSeriesYearly ,single point
    fcTimeSeriesYearly(variable="airt",latitude=75.0, longitude=57.7,firstYear=1950,lastYear=2000)
  })
  tests <- c(tests,function()
  {# test fcTimeSeriesDaily , lots of points
    data(quakes) #the only built-in dataset with locations which I’ve found. The Fiji earthquakes
    ts <- fcTimeSeriesDaily( #fetching day-to-day temperature variations at earthquake locations
      "airt", 
      quakes$lat, quakes$long,
      firstYear=1981, lastYear=2000 #averaging across 20 years
    )  
  })  
  
  #running tests
  print("Running automatic tests")
  for(i in 1:length(tests)){
    print(paste("Running test",i," out of",length(tests)))
    tests[[i]]();
  }
  print("All tests finished. Ready to use")
}

test.fc()