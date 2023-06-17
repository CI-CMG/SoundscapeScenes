# troubleshoot dropped data MB02_02

as.Date("06-27-2019 22:47:47")
#audio file time stamps--- is there missing time between them
f1 = as.POSIXct(  "06-27-2019 22:47:47" , format = "%m-%d-%Y %H:%M:%S" , tz = "GMT" )
f2 = as.POSIXct(  "06-28-2019 10:47:42" , format = "%m-%d-%Y %H:%M:%S" , tz = "GMT" )

f1end = f1 + hms("05:59:59") # using duration in Raven... 
missingtime = f2 - f1end # is the end of this file the same as the beginning of the next?
missingtime


#audio file time stamps--- is there missing time between them
f1 = as.POSIXct(  "07-13-2019 04:45:36" , format = "%m-%d-%Y %H:%M:%S" , tz = "GMT" )
f2 = as.POSIXct(  "07-13-2019 13:23:34" , format = "%m-%d-%Y %H:%M:%S" , tz = "GMT" )

f1end = f1 + hms("02:30:28") # using duration in Raven... 
missingtime = f2 - f1end # is the end of this file the same as the beginning of the next?
missingtime
044536