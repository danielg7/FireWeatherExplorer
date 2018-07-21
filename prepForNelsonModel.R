getwd()
# cmake .
# make

system(command = "Development/DeadFuelMoisture/compute_dfm --input_file Development/DeadFuelMoisture/testInput.txt --output_file testOutput.txt --verbose")

readWxForOutput <- function(DateTime,
                            AirTempF,
                            AirHumidity,
                            SolarRad,
                            HrRain,stickTemp,stickSurHumidity,stickMoisture){
  # Check if DateTime is truly a date time
  
  if(!inherits(DateTime, "POSIXct")){
    stop("DateTime is not POSIXct")
  }
  
  startYear <- lubridate::year(DateTime)
  startMonth <- lubridate::month(DateTime)
  startDay <- lubridate::day(DateTime)
  
}
