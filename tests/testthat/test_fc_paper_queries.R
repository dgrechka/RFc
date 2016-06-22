context("Fetches used in Ecography paper")

#the tests in this context match exactly the fatched mentioned in the Ecography paper
#Note that url is omited

test_that("original Fc Paper figure 1 fetch succeeds", {
        ts <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                reproduceFor='2015-05-27')
        expect_equal(length(ts$values),101)
});


test_that("original Fc Paper figure 2 fetch succeeds", {
        africaJulyTemp <- fcGrid(variable="airt",
                                 latitudeFrom=-35, latitudeTo=35, latitudeBy=1,
                                 longitudeFrom=-20,longitudeTo=60,longitudeBy=1,
                                 firstDay=182,lastDay=212, #July
                                 firstYear=1950,lastYear=2000,
                                 reproduceFor='2015-05-27') 
        lonCells <- gridparameters(africaJulyTemp)["cells.dim"][rownames(gridparameters(africaJulyTemp)["cells.dim"])=='lon',]
        latCells <- gridparameters(africaJulyTemp)["cells.dim"][rownames(gridparameters(africaJulyTemp)["cells.dim"])=='lat',]
        expect_equal(latCells, 71)
        expect_equal(lonCells, 81)
});

test_that("original Fc Paper figure 3a fetch succeeds", {
        ts <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                dataSet="GHCNv2",
                reproduceFor='2015-05-27')
        expect_equal(length(ts$values),101)
});

test_that("original Fc Paper figure 3b fetch succeeds", {
        ts2 <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                dataSet ="NCEP/NCAR Reanalysis 1 (regular grid)",
                reproduceFor='2015-05-27')
        expect_equal(length(ts2$values),101)
});
test_that("original Fc Paper figure 3c fetch succeeds", {
        ts3 <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                dataSet ="CESM1-BGC airt",
                reproduceFor='2015-05-27')
        expect_equal(length(ts3$values),101)
});

context("Fetches similar to mentioned in Ecography paper")

test_that("Fc Paper figure 1 fetch succeeds (random shifted)", {
        ts <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0+offset, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                url = serviceURLtoTest,
                verbose=isVerbose)
        expect_equal(length(ts$values),101)
});


test_that("Fc Paper figure 2 fetch succeeds (random shifted)", {
        africaJulyTemp <- fcGrid(variable="airt",
                                 latitudeFrom=-35+offset, latitudeTo=35+offset, latitudeBy=1,
                                 longitudeFrom=-20,longitudeTo=60,longitudeBy=1,
                                 firstDay=182,lastDay=212, #July
                                 firstYear=1950,lastYear=2000,
                                 url = serviceURLtoTest,
                                 verbose=isVerbose)
        lonCells <- gridparameters(africaJulyTemp)["cells.dim"][rownames(gridparameters(africaJulyTemp)["cells.dim"])=='lon',]
        latCells <- gridparameters(africaJulyTemp)["cells.dim"][rownames(gridparameters(africaJulyTemp)["cells.dim"])=='lat',]
        expect_equal(latCells, 71)
        expect_equal(lonCells, 81)
});

#TEMPORARY DISABLES UNTIL GHCNv2 REENABLED
#test_that("Fc Paper figure 3a fetch succeeds (random shifted)", {
#        ts <- fcTimeSeriesYearly(
#                variable="airt",
#                latitude=8.0+offset, longitude=10.0,
#                firstDay=152,lastDay=243,
#                firstYear=1950,lastYear=2050,
#                url = serviceURLtoTest,
#                verbose=isVerbose,
#                dataSet="GHCNv2")
#        expect_equal(length(ts$values),101)
#});

test_that("Fc Paper figure 3b fetch succeeds (random shifted)", {
        ts2 <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0+offset, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                url = serviceURLtoTest,
                verbose=isVerbose,
                dataSet ="NCEP/NCAR Reanalysis 1 (regular grid)")
        expect_equal(length(ts2$values),101)
});
test_that("Fc Paper figure 3c fetch succeeds (random shifted)", {
        ts3 <- fcTimeSeriesYearly(
                variable="airt",
                latitude=8.0+offset, longitude=10.0,
                firstDay=152,lastDay=243,
                firstYear=1950,lastYear=2050,
                url = serviceURLtoTest,
                verbose=isVerbose,
                dataSet ="CESM1-BGC airt")
        expect_equal(length(ts3$values),101)
})