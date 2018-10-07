library(testthat)
library(shinytest)

# Function: readInWeather -------------------------------------------------

test_that("readInWeather fails with non-numeric years - start", {
  expect_error(readInWeather(StationID = "RFRC2", County = "Larimer",Start = "ABC",End = 2015))
})

test_that("readInWeather fails with non-numeric years - end", {
  expect_error(readInWeather(StationID = "RFRC2", County = "Larimer",Start = 2015, End = "ABC"))
})

test_that("readInWeather fails if end is greater than start year", {
  expect_error(readInWeather(StationID = "RFRC2", County = "Larimer",Start = 2015, End = 2011),"Start year must be less than end year.")
})

test_that("readInWeather fails if StationID is invalid", {
  expect_error(readInWeather(StationID = "ABCD1", County = "Larimer", Start = 2010, End = 2011))
})

# Function: fxn_WeatherCleaner -------------------------------------------------

#test_that("fxn_weatherCleaner handles empty rainfall stations.",{
#  expect_warning()
#})