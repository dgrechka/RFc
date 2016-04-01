context("TimeSeries")

seed <- 1.0

set.seed(seed)

offset <- runif(1) #cache braking offset. used in all tests

print(cat("Random seed is ",seed,"; Cache braking offset is",offset))

test_that("fcTimeSeriesYearly timeseries correct length for single point", {
        a <- fcTimeSeriesYearly(variable="airt",latitude=76.0+offset, longitude=57.7,firstYear=1950,lastYear=2000)
        expect_equal(ncol(a$values), 51,label=paste("wrong time series length. expected 51 but got",ncol(a$values)))
})

test_that("fcTimeSeriesDaily timeseries correct length for single point", {
        a<-fcTimeSeriesDaily(variable="airt",latitude=76.1+offset, longitude=57.7,firstYear=1950,lastYear=2000)
        expect_equal(ncol(a$values), 365,label=paste("wrong time series length. expected 365 but got",ncol(a$values)))
})

test_that("fcTimeSeriesHourly timeseries correct length for single point.", {
        a<-fcTimeSeriesHourly(variable="airt",latitude=75.1+offset, longitude=57.7,firstYear=1950,lastYear=2000,startHour=0,stopHour=24)
        expect_equal(ncol(a$values), 24,label=paste("wrong time series length. expected 24 but got",ncol(a$values)))
})

test_that("fcTimeSeriesHourly timeseries correct length for single point. timestamp set", {
        a<-fcTimeSeriesHourly(variable="airt",latitude=75.1+offset, longitude=57.7,firstYear=1950,lastYear=2000,startHour=0,stopHour=24,reproduceFor="2015-10-01")
        expect_equal(ncol(a$values), 24,label=paste("wrong time series length. expected 24 but got",ncol(a$values)))
})

test_that("fcTimeSeriesDaily timeseries correct length for a point set. timestamp set", {
        data(quakes) #the only built-in dataset with locations which I've found. The Fiji earthquakes
        a<-fcTimeSeriesDaily( #fetching day-to-day temperature variations at earthquake locations
                "airt", 
                quakes$lat+offset, quakes$long,
                firstYear=1981, lastYear=2000 #averaging across 20 years
        )
        expect_equal(ncol(a$values), 365,label=paste("wrong time series length. expected 365 but got",ncol(a$values)))
        expect_equal(nrow(a$values), length(quakes$lat),label=paste("wrong time series count expected",length(quakes$lat),"but got",ncol(a$values)))
})