rm(list=ls()) 
# PURPOSE -  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - gantt chart summarizing data in the archive

# FUTURE UPDATES - 
#- make all headers small caps so we can read in
#- read in project look-up tables to help with formatting- e.g. converts site codes to names
#- save graphic as html, interactive (p_plotly <- ggplotly(p), htmlwidgets::saveWidget(p_plotly, "plot.html"))

# LOAD LIBRARIES ####
library(stringr)    
library(jsonlite)
library(curl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(ncdf4)

# SET GCP DIRECTORY ####
#gcpDir = "gs://noaa-passive-bioacoustic/nrs/audio" # NRS
gcpDir = "gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics" #ONMS
projectN = "onms" #nrs # set this to deal with different metadata formats
command = "gsutil"
args =  c("ls", gcpDir)
outputDir = ""

# LIST SUB DIRECTORIES ####
# these should be the "monitoring sites" you want to gather information about
subdirs = system2(command, args, stdout = TRUE, stderr = TRUE)  
dirNames = sapply(strsplit(basename( subdirs ), "/"), `[`, 1)

# GET INFORMATION FROM METADATA FILES ####
output = NULL
for (s in 1:length(dirNames) ) { #testing -  s = 1
  
  args = c("ls", "-r", subdirs[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  #nc_files = grep("\\_netCDF.nc$", sFiles, value = TRUE) #data files- netcdf
  
  #accounts for multiple metadata formats 
  for (jf in 1:length( json_files) ) #testing - jf = 1
  {
    if (grepl("-metadata", json_files[jf])) {
      
      #read in one metadata file
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      json_data <- fromJSON(paste(url, collapse = ""))
      #tolower(names(json_data))-- might help with different naming in files?
      
      #extract variables
      if(projectN == "onms") {  
        instrument = json_data$DATASET_PARAMETERS$RAW_METADATA[[2]]$MODEL
        start = as.Date( json_data$START_DATE , format = "%d-%b-%Y")
        end = as.Date( json_data$END_DATE , format = "%d-%b-%Y")
        start = as.Date( json_data$START_DATE , format = "%d-%b-%Y")
        end = as.Date( json_data$END_DATE , format = "%d-%b-%Y")
      }
      
      #save to output data for plotting
      output = rbind(output, c(dirNames[s], jf, instrument, as.character(start), as.character(end)) )
      rm(json_data)
      
    } else {
      
      #read in one metadata file
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      json_data <- fromJSON(paste(url, collapse = ""))
      
      #extract variables
      if(projectN == "onms") { 
        instrument = json_data$INSTRUMENT_TYPE #variable you want to color bars by
        start = as.Date( json_data$DEPLOYMENT$AUDIO_START)
        end   = as.Date(json_data$DEPLOYMENT$AUDIO_START )
        
      } else if (projectN == "nrs") {
        instrument = json_data$sponsors$'0'$name #variable you want to color bars by
        start = as.Date( json_data$channels$'1'$channel_start )
        end   = as.Date( json_data$channels$'1'$channel_end )
      }
      
      #save to output data for plotting
      output = rbind(output, c(dirNames[s], jf, instrument, as.character(start), as.character(end)) )
    }
  }
}

# REFORMAT FOR PLOTTING ####
colnames(output) = c("Site", "Deployment", "Instrument", "Start_Date", "End_Date")
output = as.data.frame(output)
output$Start_Date <- as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date <- as.Date(output$End_Date, format = "%Y-%m-%d")
x_min = min(output$Start_Date)
x_max = max(output$End_Date)

output_long <- output %>%
  pivot_longer(cols = c(Start_Date, End_Date), names_to = "state", values_to = "date")

# SET COLORS ####
uColors = unique(output$Instrument)
if (projectN == "onms"){
  instrument_colors <- c(
    "SoundTrap 500" = "#1f77b4",  
    "SoundTrap 600" = "#ff7f0e",  
    "SoundTrap 300" = "#2ca02c" ) 
}

# GENERATE PLOT ####
# geom_line option 
p = ggplot(output_long, aes(date, Site, color = Instrument, group=Site)) +
  geom_line(linewidth = 6,lineend = "round") +
  scale_color_manual(values = instrument_colors) + 
  scale_x_date(
    date_labels = "%b %Y", 
    date_breaks = "2 months", limits = c(x_min, x_max)  ) +
  labs(x = "", y = "", title = paste0(" ONMS - Ocean Sound Monitoring Data Summary"),
       subtitle = paste0("NCEI cloud as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))
p

# geom_tile option 
pT = ggplot(output, aes(y = Site, x = Start_Date, xend = End_Date)) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date), fill = Instrument), 
            color = "black", height = 0.4) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = paste0(projectN, "- Ocean Sound Monitoring Data Summary")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))
pT


# GET INFORMATION FROM DATA FILES ####
outputData = NULL
for (s in 1:length(sites) ) { #testing -  s = 1
  
  args = c("ls", "-r", sites[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
  nc_files = grep("\\_netCDF.nc$", sFiles, value = TRUE) #data files- netcdf
  
  #information on files
  nFiles =  length(nc_files)
  split_parts = strsplit(basename( nc_files ), "_")
  dys = as.Date(sapply(split_parts, `[`, 5),  format = "%Y%m%d")
  dys = sort(dys)
  date_diff  = c(1, diff(dys))
  
  for (jf in 1:length( nc_files) ) {#testing - jf = 1
    
    #file details
    dyFile = as.Date(sapply(strsplit(basename( nc_files[jf] ), "_"), `[`, 5),  format = "%Y%m%d")
    
    #read in one data file
    url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(nc_files[jf], collapse = "") ) )
    # create a temporary file
    h = url(url, "rb")  
    temp_file = tempfile(fileext = ".nc")
    writeBin(readBin(h, what = "raw", n = 1e6), temp_file)
    close(h)
    nc = nc_open(temp_file)
    # not reading in the file, alternative method of downloading local copy and then deleting??
    # START HERE ####
    
    #download temp file, then delete
    destfile <- "local_nc_file.nc"
    download.file(url, destfile, mode = "wb")

    #extract variables, assumes above code works
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
    
    
    #save to output data for plotting
    outputData = rbind(output, c() )
  }
} 



# # TESTING other methdods ####
# # --- hit blocks do not work
# # METHOD 1- expects authorization step, so does not run
# install.packages("googleCloudStorageR")
# library(googleCloudStorageR)
# bucket <- "noaa-passive-bioacoustic"
# object_name <- "soundcoop/AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"
# gcs_get_object(object_name, bucket = bucket, saveToDisk = "FILENAME", overwrite = TRUE)
# # The message "No authorization yet in this session!" is likely coming from the googleCloudStorageR package, indicating that it expects authentication to access a resource. However, since the data you are accessing is open access, you shouldn't need authentication.
# 
# # METHOD 2 -  does not have funciton to list files, need to read in
# install.packages("httr")
# install.packages("curl")
# library(httr)
# library(curl)
# library (ncdf4)
# url <- "https://storage.googleapis.com/noaa-passive-bioacoustic/soundcoop/AEON5/AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"
# destfile <- "AEON5_AMAR378.1.16000_20210721_DAILY_MILLIDEC_MinRes_v2.nc"  # Specify the destination path and file name
# GET(url, write_disk(destfile, overwrite = TRUE))
# file_size <- file.info(destfile)$size
# print(file_size)
# nc_data <- nc_open(destfile)
# print(nc_data)
# nc_close(nc_data)


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
