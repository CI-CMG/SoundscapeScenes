# Data for communication space project- MB01
rm(list=ls()) 
site = "mb01"
outputDir = paste0( "F:/ONMS/", site,"/")
# LOAD SPL-TOL DATA ####
# HOURLY TOLs with wind estimate (gps)
inFile = list.files(outputDir, pattern = paste0("data_",site,"_HourlySPL-gfs_"), full.names = T)
file_info = file.info(inFile)
load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
st = as.Date( min(gps$UTC) )
ed = as.Date( max(gps$UTC) )
udays = length( unique(as.Date(gps$UTC)) )
cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
gps$Day = as.Date(gps$UTC)

datesWeather = (c ( as.Date("01/14/2019", format = "%m/%d/%Y" ), as.Date("01/18/2019", format = "%m/%d/%Y" ) ))
dataWeather  = gps[ gps$Day > datesWeather[1] & gps$Day <=  datesWeather[2], ]

datesShip = (c ( as.Date("09/25/2019", format = "%m/%d/%Y" ), as.Date("09/27/2019", format = "%m/%d/%Y" ) ))
dataShip  = gps[ gps$Day > datesShip[1] & gps$Day <=  datesShip[2], ]

write.csv(dataWeather, file = paste0(outputDir, site, "WeatherPeriod.csv"))
write.csv(dataShip, file = paste0(outputDir, site, "ShipPeriod.csv"))

rm(list=ls()) 
site = "mb02"
outputDir = paste0( "F:/ONMS/", site,"/")
# LOAD SPL-TOL DATA ####
# HOURLY TOLs with wind estimate (gps)
inFile = list.files(outputDir, pattern = paste0("data_",site,"_HourlySPL-gfs_"), full.names = T)
file_info = file.info(inFile)
load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
st = as.Date( min(gps$UTC) )
ed = as.Date( max(gps$UTC) )
udays = length( unique(as.Date(gps$UTC)) )
cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
gps$Day = as.Date(gps$UTC)
datesShip = (c ( as.Date("09/25/2019", format = "%m/%d/%Y" ), as.Date("09/27/2019", format = "%m/%d/%Y" ) ))
dataShip  = gps[ gps$Day > datesShip[1] & gps$Day <=  datesShip[2], ]
write.csv(dataShip, file = paste0(outputDir, site, "ShipPeriod.csv"))
