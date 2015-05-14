#git repo at https://github.com/dgrechka/Rfc

require(RCurl)
require(RJSONIO)
require(sp)

internal_fc.formRequestBody <- function(envVar, # must be private
                                              lat,lon,
                                              years,
                                              days,
                                              hours,
                                              spatialRegionType,dataSources,timestampStr) {
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
  
  if(length(dataSources)==1) {
    if(dataSources=="ANY") {
      dataSources <- "";# any availalbe
    }
    else {
      dataSources <- I(dataSources)
    }
  }
  
  
  timestamp<-c()
  if(timestampStr=="NOW") {
    timestamp <- 253404979199999; #max value
  }
  else
  {#"/Date(1317303882420+0500)\/" format
    timestampObj <- as.POSIXct(timestampStr, tz = "UTC", origin="1970-01-01")
    timestamp<-paste("/Date(",as.numeric(timestampObj,digits=20),"000+0000)/",sep='');
  }
  
  
  request <- list(
    EnvironmentVariableName=envVar,
    ParticularDataSources=dataSources, 
    Domain=domain,
    ReproducibilityTimestamp=timestamp
  )
  j <- toJSON(request,digits=20)
  #print(j)
  return(j)
}

internal_fc.getConfiguration<-function(url,timestempStr) {
  h = basicTextGatherer()
  h$reset()
  timestampPar <- c()
  if(timestempStr=="NOW") {
    timestampPar<-""
  }
  else {
    timestamp <- as.POSIXct(timestempStr, tz = "UTC", origin="1970-01-01")
    timestampPar<-paste("?timestamp=",format(timestamp,"%Y-%m-%dT%H:%MZ"),sep='')
  }
  curlPerform(url = paste(url,"/api/configuration",timestampPar,sep=''),
              httpheader=c(Accept="text/plain", 'Content-Type' = "application/json"),              
              writefunction = h$update)
  reply = h$value()
  result=fromJSON(h$value())
  return(result)
}

internal_fc.getProvID <- function(configuration,dataSourceName) {
  for(i in 1:length(configuration$DataSources)) {
    if(configuration$DataSources[[i]]$Name==dataSourceName){
       return(configuration$DataSources[[i]]$ID);
    }    
  }
}

internal_fc.nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

internal_fc.replaceProvIDwithNames <- function(values,configuration) {
  replaced <- values;
  for(i in 1:length(configuration$DataSources)) {
    current <- configuration$DataSources[[i]]    
    replaced[values==current$ID]<-current$Name
  }
  return(factor(replaced))
}

internal_fc.reformGridResponse <- function(resultList,lats,lons,explicitProvenance=NULL) {#must be private. accepts the decoded JSON recieved from FC result proxy. converts it into matrix
  lonN <- length(lons)
  latN <- length(lats)  
  stratched_lats <- c()
  stratched_lons <- c()
  stratched_values <- c()
  stratched_sd <- c()
  stratched_provenance<-c()    
  
  for(i in 1:lonN) {
    stratched_lons <- c(stratched_lons,rep(lons[i],times=latN))
    stratched_lats <- c(stratched_lats,lats)
    stratched_values <- c(stratched_values,internal_fc.nullToNA(resultList$values[[i]])) #null is MV. replacew with NA
    stratched_sd <- c(stratched_sd,internal_fc.nullToNA(resultList$sd[[i]])) #null is MV. replacew with NA
    if(is.null(explicitProvenance)) {
      stratched_provenance <- c(stratched_provenance,internal_fc.nullToNA(resultList$provenance[[i]])) #null is MV. replacew with NA
    }
  }
  if(!is.null(explicitProvenance)) {
    stratched_provenance <- rep(explicitProvenance,times=lonN*latN)
  }
  resultdf <- data.frame(lon=stratched_lons,lat=stratched_lats,
                         values=unlist(stratched_values),
                         sd=unlist(stratched_sd),
                         provenance=unlist(stratched_provenance))
  coordinates(resultdf) <- c("lon","lat") #promote to SpatialPointsDataFrame
  proj4string(resultdf) <- CRS("+proj=longlat") #unprojected data
  gridded(resultdf) <- TRUE # promote to SpatialPixelsDataFrame
  
  #resultdf <- as(resultdf, "SpatialGridDataFrame") # promote to SpatialGridDataFrame. creates the full grid
  return(resultdf)
}

internal_fc.reformPointsTimeseries <- function(resultList,explicitProvenance) { #must be private. accepts the decoded JSON recieved from FC result proxy. converts it into matrix
  N <- length(resultList$values)
  M <- length(resultList$values[[1]])
  resV <- matrix(ncol=M,nrow=N)
  resU <- matrix(ncol=M,nrow=N)
  resP <- matrix(ncol=M,nrow=N)
  if(!is.null(explicitProvenance)) {
    resP <- matrix(rep(explicitProvenance,N*M),ncol=M,nrow=N)  
  }  
  
  for(i in 1:N) {
    resV[i,] = internal_fc.nullToNA(resultList$values[[i]])
    resU[i,] = internal_fc.nullToNA(resultList$sd[[i]])
    if(is.null(explicitProvenance))
      resP[i,] = resultList$provenance[[i]]
  }
  resV <- matrix(resV,ncol=M,nrow=N)
  resU <- matrix(resU,ncol=M,nrow=N)
  resP <- matrix(resP,ncol=M,nrow=N)
  return(list(values=resV,sd=resU,provenance=resP))
}

internal_fc.TimeSeries <-function(envVar, #must be private
                                  lat,lon,
                                  years,
                                  days,
                                  hours,
                                  url,dataSources,timestampStr) {
  N <- length(lat)
  if(length(lon) != N) {
    stop("lon and lat must be the same length");
  }  
  json <- internal_fc.formRequestBody(envVar,
                                            lat,lon,
                                            years,
                                            days,
                                            hours,
                                            spatialRegionType="Points",
                                            dataSources=dataSources,
                                            timestampStr=timestampStr);
  requestProvenance <- length(dataSources)>1 || dataSources=="ANY"  
  
  result <- internal_fc.fetchCore(json,url,requestProvenance)    
  conf <- internal_fc.getConfiguration(url,timestampStr)
  explicitDs <- c()
  if(requestProvenance){
    explicitDs <- NULL
  }
  else {
    explicitDs<-internal_fc.getProvID(conf,dataSources)
  }
  resultMatrix <- internal_fc.reformPointsTimeseries(result,explicitDs)
  resultMatrix$provenance <- internal_fc.replaceProvIDwithNames(resultMatrix$provenance,conf)
  
  return(resultMatrix)
}

internal_fc.fetchCore <- function(jsonRequest,url,requestProvenance) {
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
  print("Receiving data...")
  if (substr(reply,1,9)=='completed') {
    msds = substr(reply,11,nchar(reply))
    h$reset()
    end_str <- c()
    if(requestProvenance) {
      end_str <- "&variables=values,provenance,sd"
    }
    else {
      end_str <- "&variables=values,sd"
    }
    curlPerform(url = paste(url,"/jsproxy/data?uri=",curlEscape(msds),end_str,sep=""),
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
  url="http://fetchclimate2.cloudapp.net/",
  dataSources="ANY",
  reproduceFor="NOW"
  ) {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).
  #firstYear,lastYear are scalars
  
  #return a list with NxM matrices (values,sd,provenance). N = length(lat), M = timeseries length
  
  
  years <- seq(from=firstYear,to=lastYear+1,by=1)
  days <- c(firstDay,lastDay+1)
  hours <- c(startHour,stopHour)
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url,dataSources,reproduceFor)
  
  resultMatrix$years <- years[1:(length(years)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesDaily<-function(
  variable,
  latitude,longitude,
  firstDay=1,lastDay=365,
  firstYear=1961,lastYear=1990,
  startHour=0,stopHour=24,
  url="http://fetchclimate2.cloudapp.net/",
  dataSources="ANY",
  reproduceFor="NOW") {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).  
  #firstDay,lastDay are scalars
  
  #return a list with NxM matrices (values,sd,provenance). N = length(lat), M = timeseries length
  
  
  years <- c(firstYear,lastYear+1)
  days <- seq(from=firstDay,to=lastDay+1,by=1)
  hours <- c(startHour,stopHour)
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url,dataSources,reproduceFor)
  
  resultMatrix$days <- days[1:(length(days)-1)]
  
  return(resultMatrix)
}

fcTimeSeriesHourly<-function(
  variable,
  latitude,longitude,
  startHour,stopHour,
  firstYear=1961,lastYear=1990,
  firstDay=1,lastDay=365,
  url="http://fetchclimate2.cloudapp.net/",
  dataSources="ANY",
  reproduceFor="NOW") {
  #envVar is string
  #lat,lon are vectors with the same length (can be length of 1).
  #startHour,stopHour are scalars
  
  #return a list with NxM matrices (values,sd,provenance). N = length(lat), M = timeseries length
  
  
  years <- c(firstYear,lastYear+1)
  days <- c(firstDay,lastDay+1)
  hours <- seq(from=startHour,to=stopHour)
  
  resultMatrix <-internal_fc.TimeSeries(variable,latitude,longitude,years,days,hours,url,dataSources,reproduceFor)
  
  resultMatrix$hours <- hours[1:(length(hours))]
  
  return(resultMatrix)
}

fcGrid <- function(
  variable,
  latitudeFrom,latitudeTo,latitudeBy,
  longitudeFrom,longitudeTo,longitudeBy,  
  firstYear=1961,lastYear=1990,
  firstDay=1,lastDay=365,
  startHour=0,stopHour=24,
  url="http://fetchclimate2.cloudapp.net/",
  dataSources="ANY",
  reproduceFor="NOW") {
  
  lats <- seq(from=latitudeFrom,to=latitudeTo,by=latitudeBy)
  lons <- seq(from=longitudeFrom,to=longitudeTo,by=longitudeBy)
  
  years <- c(firstYear,lastYear+1)
  days <- c(firstDay,lastDay+1)
  hours <- c(startHour,stopHour)
  
  requestBody <- internal_fc.formRequestBody(variable,
      lats,lons,
      years,days,hours,
      "PointGrid",dataSources,reproduceFor)
  requestProvenance <- length(dataSources)>1 || dataSources=="ANY"  
  response <- internal_fc.fetchCore(requestBody,url,requestProvenance)
  conf <- internal_fc.getConfiguration(url,reproduceFor)
  explicitDs <- c()
  if(requestProvenance){
    explicitDs <- NULL
  }
  else {
    explicitDs<-internal_fc.getProvID(conf,dataSources)
  }
  spObj <- internal_fc.reformGridResponse(response,lats,lons,explicitDs)  
  
  
  spObj$provenance <- internal_fc.replaceProvIDwithNames(spObj$provenance,conf)
  
  return(spObj);
}

test.fc <-function() { #run automatic tests
  #declaring tests
  tests <- c()
  tests <- c(tests,function()
  {# test fcTimeSeriesYearly ,single point
    a <- fcTimeSeriesYearly(variable="airt",latitude=75.0, longitude=57.7,firstYear=1950,lastYear=2000)
    if(ncol(a$values)!=51) stop(paste("wrong time series length. expected 51 but got",ncol(a$values)))
  })
  tests <- c(tests,function()
  {# test fcTimeSeriesDayly ,single point
    a<-fcTimeSeriesDaily(variable="airt",latitude=75.0, longitude=57.7,firstYear=1950,lastYear=2000)
    if(ncol(a$values)!=365) stop(paste("wrong time series length. expected 365 but got",ncol(a$values)))
  })
  tests <- c(tests,function()
  {# test fcTimeSeriesHourly ,single point, timestamp set
    a<-fcTimeSeriesHourly(variable="airt",latitude=75.0, longitude=57.7,firstYear=1950,lastYear=2000,startHour=0,stopHour=24,reproduceFor="2015-05-01")
    if(ncol(a$values)!=24) stop(paste("wrong time series length. expected 24 but got",ncol(a$values)))
  })
  tests <- c(tests,function()
  {# test fcTimeSeriesHourly ,single point
    a<-fcTimeSeriesHourly(variable="airt",latitude=75.0, longitude=57.7,firstYear=1950,lastYear=2000,startHour=0,stopHour=24)
    if(ncol(a$values)!=24) stop(paste("wrong time series length. expected 24 but got",ncol(a$values)))
  })
  tests <- c(tests,function()
  {# test fcTimeSeriesDaily , lots of points
    data(quakes) #the only built-in dataset with locations which I’ve found. The Fiji earthquakes
    a<-fcTimeSeriesDaily( #fetching day-to-day temperature variations at earthquake locations
      "airt", 
      quakes$lat, quakes$long,
      firstYear=1981, lastYear=2000 #averaging across 20 years
    )
    if(ncol(a$values)!=365) stop(paste("wrong time series length. expected 365 but got",ncol(a$values)))
    if(nrow(a$values)!=length(quakes$lat)) stop(paste("wrong time series count expected",length(quakes$lat),"but got",ncol(a$values)))
    
  })
  tests <- c(tests,function()
  {# test fcGrid 
    a<- fcGrid("airt",40,80,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31)
  })
  tests <- c(tests,function()
  {# test fcGrid with old reproduce timestamp
    a<- fcGrid("airt",40,80,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,reproduceFor="2015-05-01 05:00:00")
  })
  tests <- c(tests,function()
  {# test fcGrid with old reproduce timestamp
    a<- fcGrid("airt",40,80,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,reproduceFor="2015-05-01")
  })
  tests <- c(tests,function()
  {# test fcGrid  with datasource override
    a<- fcGrid("airt",40,80,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,dataSources=c("NCEP/NCAR Reanalysis 1 (regular grid)"))
  })
  tests <- c(tests,function()
  {# test fcGrid  with datasource override 2
    a<- fcGrid("airt",40,80,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,dataSources=c("NCEP/NCAR Reanalysis 1 (regular grid)","CRU CL 2.0"))
  })
  tests <- c(tests,function()
  {# test fcGrid  with datasource override 2
  ts <- fcTimeSeriesYearly(
    variable="airt",
    latitude=8.0, longitude=10.0,
    firstDay=152,lastDay=243,
    firstYear=1950,lastYear=2050,
    url='http://eafb05330fec4e289d897904b3b6c2b3.cloudapp.net/',
    dataSource="GHCNv2")  
  })
  
  #running tests
  print("Running automatic tests")
  for(i in 1:length(tests)){
    print(paste("Running test",i,"out of",length(tests)))
    tests[[i]]();
  }
  print("All tests have passed. Ready to use")
}

test.fc()