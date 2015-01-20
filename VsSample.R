library(RCurl)
library(rjson)

## JSON request object. 
## The text may be downloaded from http://fetchclimate2.cloudapp.net/form
## ('Download request' button)
## Alternatively, one may use R list constructor 
## following with a call to 'toJSON' function

body = '{"EnvironmentVariableName": "airt", "ParticularDataSources": ["GFDLAvTemp","GHCNv2"],\
"Domain": {\
"Lats": [52, 52.5, 53, 53.5, 54, 54.5, 55, 55.5, 56, 56.5, 57, 57.5, 58, 58.5, 59],\
"Lons": [35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45], "TimeRegion": \
{"Years": [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020],\
"Days": [1,32,60,91,121,152,182,213,244,274,305,335,366],\
"Hours": [0,24], "IsIntervalsGridYears": true, "IsIntervalsGridDays": true, "IsIntervalsGridHours": true}, "SpatialRegionType": "CellGrid"}}'

## perform request
h = basicTextGatherer()
h$reset()
curlPerform(url = "http://fetchclimate2.cloudapp.net/api/compute",
            httpheader=c(Accept="text/plain", 'Content-Type' = "application/json"),
            postfields=body,
            writefunction = h$update)
reply = h$value()
print(reply)

## wait while processing in progress
while (substr(reply,1,7)=='pending' || substr(reply,1,8)=='progress') {
  Sys.sleep(1)
  hash=strsplit(reply,"hash=")[[1]][2]
  h$reset()
  curlPerform(url = paste("http://fetchclimate2.cloudapp.net/api/status?hash=",curlEscape(hash),sep=""),
              httpheader=c(Accept="text/plain"),
              writefunction = h$update)
  reply = h$value()
  print(reply)
}

## get result data
if (substr(reply,1,9)=='completed') {
  msds = substr(reply,11,nchar(reply))
  h$reset()
  curlPerform(url = paste("http://fetchclimate2.cloudapp.net/jsproxy/data?uri=",curlEscape(msds),"&variables=values",sep=""),
              httpheader=c(Accept="application/json"),
              writefunction = h$update)
  result=fromJSON(h$value())
}

### result is a multidimentional R list
