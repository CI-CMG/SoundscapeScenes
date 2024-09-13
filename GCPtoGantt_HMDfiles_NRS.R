rm(list=ls()) 

# install.packages("jsonlite")
# install.packages("curl")

library(stringr)    
library(jsonlite)
library(curl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)

# Method 3 - commmand line approach
# gsutil ls "gs://noaa-passive-bioacoustic/soundcoop/AU_CH01/*_2017100*_*"

gcpDir = "gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics"
gcpDir = "gs://noaa-passive-bioacoustic/nrs/audio"
command = "gsutil"
args =  c("ls", gcpDir)

sites = system2(command, args, stdout = TRUE, stderr = TRUE)  # Run the command using system2() #print(result)
split_sites = strsplit(basename( sites ), "/")
siteNames = sapply(split_sites, `[`, 1)

output = NULL
for (s in 1:length(sites) ) { #  s = 1
  args = c("ls", "-r", sites[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  #print(result)
  json_files = grep("\\.json$", sFiles, value = TRUE)
  # nc_files = grep("\\_netCDF.nc$", sFiles, value = TRUE)
  
  for (jf in 1:length( json_files) ) # jf = 1
  {
    if (grepl("-metadata", json_files[jf])) {
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      json_data <- fromJSON(paste(url, collapse = ""))
      instrument = json_data$DATASET_PARAMETERS$RAW_METADATA[[2]]$MODEL
      start = as.Date( json_data$START_DATE , format = "%d-%b-%Y")
      end = as.Date( json_data$END_DATE , format = "%d-%b-%Y")
      start = as.Date( json_data$START_DATE , format = "%d-%b-%Y")
      end = as.Date( json_data$END_DATE , format = "%d-%b-%Y")
      
      
      output = rbind(output, c(siteNames[s], jf, instrument, as.character(start), as.character(end)) )
    } else {
      
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      json_data <- fromJSON(paste(url, collapse = ""))
      # onms metdata
      instrument = json_data$INSTRUMENT_TYPE
      start = as.Date( json_data$DEPLOYMENT$AUDIO_START)
      end   = as.Date(json_data$DEPLOYMENT$AUDIO_START )
      
      
      #NRS - metadata
      instrument = json_data$instrument_id
      sponsor = json_data$sponsors$'0'$name
      start = as.Date( json_data$channels$'1'$channel_start )
      end   = as.Date( json_data$channels$'1'$channel_end )
      
      output = rbind(output, c(siteNames[s], jf, instrument, as.character(start), as.character(end)) )
    }
    
    
    
  }
  
}
colnames(output) = c("Site", "Deployment", "Instrument", "Start_Date", "End_Date")
output = as.data.frame(output)
output$Start_Date <- as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date <- as.Date(output$End_Date, format = "%Y-%m-%d")
output
x_min <- min(output$Start_Date)
x_max <- max(output$End_Date)

output_long <- output %>%
  pivot_longer(cols = c(Start_Date, End_Date), names_to = "state", values_to = "date")
unique(output$Instrument)
instrument_colors <- c(
  "SoundTrap 500" = "#1f77b4",  # Blue
  "SoundTrap 600" = "#ff7f0e",  # Orange
  "SoundTrap 300" = "#2ca02c"   # Green
)

p = ggplot(output_long, aes(date, Site, color = Instrument, group=Site)) +
  geom_line(size = 6,lineend = "round") +
  labs(x="Project year", y=NULL, title="Project timeline")+
  scale_color_manual(values = instrument_colors) + 
  scale_x_date(
    date_labels = "%b %Y", 
    date_breaks = "2 months",
    limits = c(x_min, x_max)
  ) +
  labs(x = "", y = "", title = "Ocean Sound Monitoring Summary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))
p

ggplot(output, aes(y = Site, x = Start_Date, xend = End_Date)) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date), fill = Instrument), 
            color = "black", height = 0.4) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "2 month", limits = c(x_min, x_max) ) +
  labs(x = "", y = "", title = "Ocean Sound Monitoring Summary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))



#local file approach
#local_file ="F:\\ONMS\\"
#system2("gsutil", args = c("cp", json_files, local_file))
#l_json_files = paste0( local_file, basename( json_files )) 
#json_data  = fromJSON(l_json_files[1])

# get information for gnatt chart
# date range of data
basename( nc_files )[1]
split_parts = strsplit(basename( nc_files ), "_")
#unique(sapply(split_parts, `[`, 2))

dys = as.Date(sapply(split_parts, `[`, 5),  format = "%Y%m%d")
dys <- sort(dys)
date_diff  = c(1, diff(dys))
start_indices  =  which(date_diff > 1)
start_dates = dys[ c(1,start_indices-1) ]
end_dates = dys[ c(start_indices, length(dys)) ]


fil = 1
str_split(basename(nc_files[fil]), "_")[[1]]


instrument
fil = 1

split_string = str_split(basename(result[fil]), "_")[[1]]
site =  split_string[1]



## READ in HMD netCDF files ####
nc <- nc_open(inFiles[fil])
f =  t( as.data.frame( ncvar_get(nc, "frequency")   ) )
myPSD <- ( ncvar_get(nc, "psd")   )
qualityMat <- ncvar_get(nc, "quality_flag")   
target_value <- target_valueIn  
indices <- which(qualityMat == target_value, arr.ind = TRUE)
row_indices <- indices[, 1]
col_indices <- indices[, 2]
myPSD[row_indices, col_indices] <- NA
myPSD <- as.data.frame( myPSD  )
time <- ncvar_get(nc, "time")
myTime <- as.data.frame( as.POSIXct(time, origin = "1970-01-01", tz = "UTC") )
myPSD <- t( as.data.frame( myPSD  ) )

inHMDdata = cbind(myTime,myPSD)
colnames(inHMDdata) = c("dateTime",f)
nc_close(nc)
tmpDate = unique( as.Date(inHMDdata$dateTime) )









# TESTING other methdods--- hit blocks

# METHOD 1- expects authorization step, so does not run
install.packages("googleCloudStorageR")
library(googleCloudStorageR)

bucket <- "noaa-passive-bioacoustic"
object_name <- "soundcoop/AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"
gcs_get_object(object_name, bucket = bucket, saveToDisk = "FILENAME", overwrite = TRUE)
# The message "No authorization yet in this session!" is likely coming from the googleCloudStorageR package, indicating that it expects authentication to access a resource. However, since the data you are accessing is open access, you shouldn't need authentication.


# METHOD 2 -  does not have funciton to list files, need to read in
install.packages("httr")
install.packages("curl")
library(httr)
library(curl)
library (ncdf4)
url <- "https://storage.googleapis.com/noaa-passive-bioacoustic/soundcoop/AEON5/AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"
destfile <- "AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"  # Specify the destination path and file name
GET(url, write_disk(destfile, overwrite = TRUE))
file_size <- file.info(destfile)$size
print(file_size)

nc_data <- nc_open(destfile)
print(nc_data)
nc_close(nc_data)


response <- GET(url)


if (http_status(response)$category == "Success") {
  # Parse the JSON response
  content <- content(response, "parsed", type = "application/x-netcdf")
  
  # Extract the list of files
  files <- content$items
  if (!is.null(files)) {
    file_names <- sapply(files, function(item) item$name)
    print(file_names)
  } else {
    print("No files found or error in response.")
  }
} else {
  print(paste("Request failed with status:", http_status(response)$message))
}
