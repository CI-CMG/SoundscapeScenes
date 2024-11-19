
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/mb01 F:/ONMS/mb01
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/sb03 F:/ONMS/sb03
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/oc02 F:/ONMS/oc02

#devtools::install_github('TaikiSan21/PAMscapes')

library(PAMscapes)
site = "mb01"
inDir = paste0( "F:/ONMS/",site) 
inFiles = list.files(inDir, pattern = "MinRes.nc", recursive = T, full.names = T)

cData = NULL
for (f in 1:length(inFiles) ){
  ncFile = inFiles[f]
  hmdData = loadSoundscapeData(ncFile)
  tolData = createOctaveLevel(hmdData, type='tol')
  tolData$site = site
  cData = rbind( cData, tolData )
}

hrmedData = binSoundscapeData(cData, bin = "1hour", method = c("median") )
# can you also output how many miuntes in each hour average, what about percentiles



#The function is matchSeascape, its currently on the GitHub version of PAMscapes (v 0.6.1). 
# So if you install from GitHub you should be able to use it. 
# You just need a dataframe with columns UTC, Latitude, and Longitude, and it will go get the matching class

# Load necessary library
library(dplyr)

# Create a sequence of dates between April 1, 2019 and May 31, 2019
date_seq <- seq(from = as.Date("2019-04-01"), to = as.Date("2019-05-31"), by = "day")

# Create columns for latitude and longitude with specified coordinates
lat <- rep(36.79, length(date_seq))
lon <- rep(-121.97, length(date_seq))

# Combine into a dataframe
df <- data.frame(latitude = lat, longitude = lon, date = date_seq)

# Display the dataframe
print(df)


data <- matchSeascape(df, type='8day')

# It's probably not quite straight forward to get at the answer you want though, 
# since you don't have a single location or a fixed point in time. 
# Basically you'd need to create a dataframe of lat/longs covering some representative area of Monterey Bay, 
# then you'd also need to create dates 8 days apart covering your time ranges you want. 
# Then you'd have the full distribution of seascape classes in that area over those times, and you could summarise them however you want.


baseurl <- 'https://cwcgom.aoml.noaa.gov/erddap/'
edi <- erddapToEdinfo(dataset=dataset,
                      baseurl=baseurl,
                      chooseVars=c('CLASS', 'P'))
