rm(list=ls()) 
# PURPOSE -  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - gantt chart summarizing data in the archive

# FUTURE UPDATES - 
#- make a map of the sites with bubble size for number of days at each site
#- read in project look-up tables to help with formatting- e.g. converts site codes to names, separates by region (west, east, pacific, great lakes)
#- save graphic as html, interactive (p_plotly <- ggplotly(p), htmlwidgets::saveWidget(p_plotly, "plot.html"))
#- what do I do with the quality matrix- create a csv?
#- how do I update NCEI data product?? 

# LOAD LIBRARIES ####
library(stringr)    
library(jsonlite)
library(curl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(ncdf4)
#for map
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
#library(ggsn)
DC = Sys.Date()
        
# SET GCP DIRECTORY ####
# get directories from NCEI PAM map viewer
typ = "audio"
gcpDir  = "gs://noaa-passive-bioacoustic/onms/audio" #ONMS
gcpDir2 = "gs://noaa-passive-bioacoustic/sanctsound/audio" #SANCTSOUND
projectN = "onms" #nrs # set this to deal with different metadata formats
projectN2 = "sanctsound" #nrs # set this to deal with different metadata formats
outputDir = "F:\\ONMS" #"G:\\My Drive\\ActiveProjects\\SANCTSOUND_shared\\ONMS"

# gcpDir = "gs://noaa-passive-bioacoustic/nrs/audio" # NRS
# projectN = "nrs"
# outputDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND_shared\\NRS"

# LIST SUB DIRECTORIES ####
# these should be the "monitoring sites" you want to gather information about
command = "gsutil"
args =  c("ls", gcpDir)
subdirs = system2(command, args, stdout = TRUE, stderr = TRUE) 
command = "gsutil"
args =  c("ls", gcpDir2)
subdirs2 = system2(command, args, stdout = TRUE, stderr = TRUE)  
#paste onms + sanctsound
subdirsALL = c(subdirs, subdirs2)

dirNames  = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
cat("Processing... ", projectN, length(dirNames), "directories" )

# GET INFORMATION FROM METADATA FILES ####
output = NULL
for (s in 1:length(subdirsALL) ) { # s=21
  
  # read in files
  args = c("ls", "-r", subdirsALL[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  cat("Processing... ", dirNames[s], "[", s, " of ", length(dirNames),"]", "\n" )
  
  if ( length(grep(projectN, subdirsALL[s]) ) > 0 ) { # check for format - onms
    
    for (jf in 1:length( json_files) ) {
      
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      tmp = fromJSON(paste(url, collapse = ""))
      
      name  = tmp$DATA_COLLECTION_NAME 
      instr = tmp$INSTRUMENT_TYPE
      start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
      lat   = tmp$DEPLOYMENT$DEPLOY_LAT
      lon   = tmp $DEPLOYMENT$DEPLOY_LON
      
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
    }
    
    
  } else if ( length(grep(projectN2, subdirsALL[s]) ) > 0 ) {  # check for format - sanctsound
    
    for (jf in 1:length( json_files) ) {
      
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      h = curl(url, "r")
      json_content = readLines(url)
      close(h)
      tmp = fromJSON(paste(url, collapse = ""))
      
      
      name  = tmp$DEPLOYMENT_NAME  
      instr = tmp$INSTRUMENT_NAME
      if ( length(instr ) == 0 ){
        instr = tmp$INSTRUMENT_TYPE
        start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
        end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
        lat   = tmp$DEPLOYMENT$DEPLOY_LAT
        lon   = tmp $DEPLOYMENT$DEPLOY_LON
        
      } else {
        range = tmp$DATA_QUALITY$'1'$`Date Range`
        start = as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[1]
        end   = as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[2] 
        lon   = tmp$LOCATION$lon
        lat   = tmp$LOCATION$lat
        
      }
      
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
    }
  }
}

# REFORMAT FOR PLOTTING ####
output = as.data.frame(output)
colnames(output) = c("Path", "DeploymentNumber", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
output$Site = basename((output$Path))

output$Start_Date <- as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date <- as.Date(output$End_Date, format = "%Y-%m-%d")
x_min = min(output$Start_Date)
x_max = max(output$End_Date)

save(output, file = paste0(outputDir, "\\GCPaudio_ONMS_", DC, ".Rda") )

# SET COLORS ####
uColors = unique(output$Instrument)
if (projectN == "onms"){
  instrument_colors <- c(
    "SoundTrap 500" = "#88CCEE",  
    "SoundTrap 600" = "#CC6677",#88CCEE",  
    "SoundTrap 300" = "#44AA99", 
    "HARP" =          "#DDCC77") #DDCC77
}

# GENERATE PLOT ####
# geom_tile option 
pT = ggplot(output, aes(y = Site, x = Start_Date, xend = End_Date)) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date), fill = Instrument), 
            color = "black", height = 0.4) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = paste0(toupper(projectN),  " - Ocean Sound Monitoring Data Summary"),
       subtitle = paste0("NCEI google cloud platform (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))
pT

#PLOT MAP OF REGION ####
world = ne_countries(scale = "medium", returnclass = "sf")
WGS84proj = 4326
sites = st_as_sf(data.frame( latitude = output$Lat, longitude = output$Lon ), 
                  coords = c("longitude", "latitude"), crs = WGS84proj, 
                  agr = "constant")
ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  geom_sf(data = sites, size = 3, shape = 20, fill = "darkred") +
  coord_sf(crs = st_crs(2163)  , 
           xlim = c(-6000000, 2500000), 
           ylim = c(1000000, -2300000), expand = FALSE, datum = NA) +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA)) 

# UNDER CONSTRUCTION ....
# geom_line option - something seems off!!!
output_long <- output %>%
  pivot_longer(cols = c(Start_Date, End_Date), names_to = "state", values_to = "date")

p = ggplot(output_long, aes(date, Site, color = Instrument, group=DeploymentName)) +
  geom_line(linewidth = 6,lineend = "round") +
  scale_color_manual(values = instrument_colors) + 
  scale_x_date(
    date_labels = "%b %Y", 
    date_breaks = "2 months", limits = c(x_min, x_max)  ) +
  labs(x = "", y = "", title = paste0(" ONMS - Ocean Sound Monitoring Data Summary"),
       subtitle = paste0("NCEI google cloud platform as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16))
p



# TEST METADATA FILES ####
#onms
s = 1
args = c("ls", "-r", subdirsALL[s] )
sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
cat(s, ": ", ( json_files ),"\n")
url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[1], collapse = "") ) )
h = curl(url, "r")
json_content = readLines(url)
close(h)
json_dataONMS = fromJSON(paste(url, collapse = ""))

json_dataONMS$DATA_COLLECTION_NAME #deployment name
json_dataONMS$INSTRUMENT_TYPE #instrument type
json_dataONMS$DEPLOYMENT$DEPLOY_LAT
json_dataONMS$DEPLOYMENT$DEPLOY_LON
as.Date( gsub("T"," ", json_dataONMS$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d") 
as.Date( gsub("T"," ", json_dataONMS$DEPLOYMENT$AUDIO_END) ,  format = "%Y-%m-%d")
json_dataONMS$CALIBRATION_INFO
json_dataONMS$QUALITY_DETAILS$quality_details

#sanctsound
s = 21
subdirsALL[s]
args = c("ls", "-r", subdirsALL[s] )
sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
cat(s, ": ", ( json_files ),"\n")
url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[1], collapse = "") ) )
h = curl(url, "r")
json_content = readLines(url)
close(h)
json_dataSancSound = fromJSON(paste(url, collapse = ""))

json_dataSancSound$DEPLOYMENT_NAME
json_dataSancSound$INSTRUMENT_NAME
json_dataSancSound$LOCATION$lon
json_dataSancSound$LOCATION$lat
range = json_dataSancSound$DATA_QUALITY$'1'$`Date Range`
as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[1]
as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[2]
# no calibration saved !!
json_dataSancSound$DATA_QUALITY$'1'$`Frequency (Hz)`
json_dataSancSound$DATA_QUALITY$'1'$Quality


# GET INFORMATION FROM DATA FILES ####
## underconstruction ####
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

