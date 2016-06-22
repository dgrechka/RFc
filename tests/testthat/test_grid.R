context("Grid fetches")

test_that("fcGrid succeeds", {
        a<- fcGrid("airt",40+offset,80+offset,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,
                   url = serviceURLtoTest, verbose=isVerbose)
        lonCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lon',]
        latCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lat',]
        expect_equal(latCells, 5)
        expect_equal(lonCells, 20)
})

test_that("fcGrid with long timestamp succeeds", {
        a<- fcGrid("airt",40+offset,80+offset,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,reproduceFor="2015-10-01 05:00:00",
                   url = serviceURLtoTest, verbose=isVerbose)
        lonCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lon',]
        latCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lat',]
        expect_equal(latCells, 5)
        expect_equal(lonCells, 20)
})

test_that("fcGrid with short timestamp succeeds", {
        a<- fcGrid("airt",40+offset,80+offset,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,reproduceFor="2015-10-01",
                   url = serviceURLtoTest, verbose=isVerbose)
        lonCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lon',]
        latCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lat',]
        expect_equal(latCells, 5)
        expect_equal(lonCells, 20)
})

test_that("fcGrid succeeds with dataset specified", {
        a<- fcGrid("airt",40+offset,80+offset,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,dataSets=c("NCEP/NCAR Reanalysis 1 (regular grid)"),
                   url = serviceURLtoTest, verbose=isVerbose)
        lonCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lon',]
        latCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lat',]
        expect_equal(latCells, 5)
        expect_equal(lonCells, 20)
})

test_that("fcGrid succeeds with two datasets specified", {
        a<- fcGrid("airt",40+offset,80+offset,10,10,200,10,firstYear=1950,lastYear=2000,firstDay=1,lastDay=31,dataSets=c("NCEP/NCAR Reanalysis 1 (regular grid)","CRU CL 2.0"),
                   url = serviceURLtoTest, verbose=isVerbose)
        lonCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lon',]
        latCells <- gridparameters(a)["cells.dim"][rownames(gridparameters(a)["cells.dim"])=='lat',]
        expect_equal(latCells, 5)
        expect_equal(lonCells, 20)
})