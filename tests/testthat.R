require(testthat)
require("RFc")

#Test configuration

seed <- as.numeric(as.POSIXct(Sys.Date())) #random seed is changed every day
#seed <- 1.0 #set this to particular value for debug once failed test run

#to carry out testing for staging not yet published instance of the service
#change this URL
serviceURLtoTest <- "http://fetchclimate2.cloudapp.net/"


isVerbose <- T


set.seed(seed)
offset <- runif(1) #cache braking offset. used in some of the tests
print(paste0("Current date (used for random seed generation) is ",Sys.Date()))
print(paste0("Random seed is ",seed,"; Cache braking offset is ",offset));



test_dir("testthat/",reporter = 'minimal')