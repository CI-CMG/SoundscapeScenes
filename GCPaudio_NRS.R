rm(list=ls()) 

# PURPOSE ####
#-  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - Rdat file to use in plotting

#!!!! NEEDS ATTENTION ####
# MAP is not working yet
# what do do about text files for early deployments -  
# LAT/LON are switched
# how do you want the labels on the gantt and map? include lookup table

# LOAD LIBRARIES ####
library(stringr)    
library(jsonlite)
library(curl)
library(tidyverse)
library(ncdf4)
library(googlesheets4)
library(openxlsx)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Program - level METADATA ####
inFile = paste0(outputDir, "//ONMSSound_IndicatorCategories_2024-10-29.xlsx")
lookup = as.data.frame ( read.xlsx(inFile) )
colnames(lookup) <- lookup[1, ]  # Set first row as column names
lookup <- as.data.frame( lookup[-1, ] )          # Remove the first row
colnames(lookup) 

# SET GCP DIRECTORY ####
# get directories from NCEI PAM map viewer
DC = Sys.Date()
typ = "audio"
gcpDir  = "gs://noaa-passive-bioacoustic/nrs/audio" #ONMS
outputDir = "F:\\ONMS//overview"   #UPDATE TO LOCAL COMPUTER
projectN = "NRS"
projectN2 = "NRS-early"


# LIST SUB DIRECTORIES ####
#these should be the "monitoring sites" you want to gather information about
command = "gsutil"
args =  c("ls", gcpDir)
subdirsALL = system2(command, args, stdout = TRUE, stderr = TRUE) 

dirNames  = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
cat("Processing... ", projectN, length(dirNames), "directories" )

#read one file-- to see what files are available?
siteIn = 1
args = c("ls", "-r", subdirsALL[siteIn])
sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
json_files

url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[1], collapse = "") ) )
h = curl(url, "r")
json_content = readLines(url)
close(h)
tmp = fromJSON(paste(url, collapse = ""))

# GET INFORMATION FROM METADATA FILES ####
output = NULL
for (s in 1:length(subdirsALL) ) { # s=8
  
  ## read in files ####
  args = c("ls", "-r", subdirsALL[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  cat("Processing... ", dirNames[s], "[", s, " of ", length(dirNames),"]", "\n" )
  
  for (jf in 1:length( json_files) ) { # jf = 1
    
    url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
    h = curl(url, "r")
    json_content = readLines(url)
    close(h)
    tmp = fromJSON(paste(url, collapse = ""))
    
    ## check which version ####
    if ( is.null( tmp$deployment$audio_start) ) { #not small caps
      
      name  = tmp$SITE 
      deploy = tmp$DEPLOYMENT_NAME
      instr = tmp$INSTRUMENT_TYPE
      start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
      lat   = tmp$DEPLOYMENT$DEPLOY_LAT
      lon   = tmp$DEPLOYMENT$DEPLOY_LON
      
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
      
    } else if ( !is.null( tmp$deployment$audio_start) ) { #small caps
      
      name  = tmp$site 
      names(tmp)[names(tmp) == "INSTRUMENT_TYPE"] =  "instrument_type"
      instr = tmp$instrument_type
      deploy =  tmp$deployment_id
      start = as.Date( gsub("T"," ", tmp$deployment$audio_start), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$recover$audio_end), format = "%Y-%m-%d")
      lat   = tmp$deployment$lat
      lon   = tmp$deployment$lon
       
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
    } else {
      cat(subdirsALL[s], "- Not correct format", "\n" )
      
      output = rbind(output, c(subdirsALL[s], jf, NA, NA, 
                               NA, NA, 
                               NA, NA) )
      
    } 
  }
}


# SAVE SUMMARY ####
output = as.data.frame(output)
colnames(output) = c("Path", "FileCount", "SiteName", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
output$Site = basename((output$Path))
output$Start_Date = as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date = as.Date(output$End_Date, format = "%Y-%m-%d")

#add site names from look-up colnames(lookup)
# load(file = paste0(outputDir, "\\GCPaudio_ONMS_gantt_", DC, ".Rda") )
# unique(lookup$`NCEI ID`)
#matched_data = merge(output, lookup, by.x = "Site", by.y = "NCEI ID", all.x = TRUE)
# matched_data = output %>%   left_join(lookup, by = c("Site" = "NCEI ID"))
#output$Region = matched_data$Region
#output$Identifer = matched_data$`Common Name/Identifers`
#output$Description = matched_data$`Site Description/Driver for Monitoring Location Choice`
output$Duration = difftime( output$End_Date, output$Start_Date,"days")
#colnames(output)
#remove non-monitoring sites
#outputT = output[!is.na(output$Region),] 
#outputTt = outputT[,c(1,10, 9, 3:6, 13, 7:8, 11:12) ]
outputT = output
outputTt = output

save(outputTt, file = paste0(outputDir, "\\data_gantt_", projectN, "_gantt_", DC, ".Rda") )
write.csv(outputTt, file = paste0(outputDir, "\\data_gantt_", projectN, "_gantt_", DC, ".csv") )
colnames(outputTt)

#reformat for per site- total recordings 
outputMap =  as.data.frame(
  outputT %>%
    group_by(Site) %>%
    summarise(
      total_days = sum(Duration, na.rm = TRUE),   # Summing total duration for each site
      min_start_date = min(Start_Date, na.rm = TRUE)  # Getting the minimum start date for each site
    )
)
#lookupT = lookup[!is.na(lookup$`NCEI ID`),] 
outputMap$Site = as.character(outputMap$Site)
#lookupT$`NCEI ID` <- as.character(lookupT$`NCEI ID`)
#outputMap2 <- merge(outputMap, lookupT, by.x = "Site", by.y = "NCEI ID", all.x = TRUE)
#colnames ( outputMap2)
#outputMap2a = outputMap2[,c(4,6, 1:3,8:9) ]
#colnames ( outputMap2a)
#colnames(outputMap2a) = c("Region","Sanctuary", "Site",
                         # "TotalDays","StartDate","Latitude", "Longitude") 
#outputMap2a = na.omit(outputMap2a)

outputMap2a = outputMap
save(outputMap2a, file = paste0(outputDir, "\\map_", projectN, "_map_", DC, ".Rda") )
write.csv( outputMap2a, file = paste0(outputDir, "\\map_", projectN, "_map_", DC, ".csv") )

# GANTT PLOT  ####
uColors = unique(output$Instrument)
if (projectN == "onms"){
  instrument_colors <- c(
    "SoundTrap 500" = "#88CCEE",  
    "SoundTrap 600" = "#CC6677",#88CCEE",  
    "SoundTrap 300" = "#44AA99", 
    "HARP" =          "#DDCC77") #DDCC77
} else if (projectN == "NRS") {
  instrument_colors <- c(
    "NRS" = "#88CCEE",  
    "AUH" = "#CC6677",#88CCEE",  
    "HARP" = "#44AA99") 
}


# geom_tile option 
pT = ggplot(output, aes(y = Site, x = Start_Date, xend = End_Date)) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date), fill = Instrument), 
            color = "black", height = 0.4) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = paste0(toupper(projectN),  " - Ocean Sound Monitoring Data Summary"),
       subtitle = paste0("NCEI google cloud platform (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 10, size = 12))
pT
ggsave(filename = paste0(outputDir, "/gantt_", projectN, "_", DC, ".jpg"), plot = pT, width = 8, height = 6, dpi = 300)

# Assuming your data frame is named 'data'
# Convert the data frame to an sf object for mapping
outputMap2a$Latitude = as.numeric(as.character( gsub("°", '', outputMap2a$Latitude )) )
outputMap2a$Longitude = as.numeric(as.character( gsub("°", '', outputMap2a$Longitude )) )
outputMap2a$TotalDays = as.numeric(as.character(outputMap2a$TotalDays))
data_sf <- st_as_sf(outputMap2a, coords = c("Longitude", "Latitude"), crs = 4326)

# Create the map
# Get world land data (in low resolution, change to 'medium' or 'large' if needed)
world <- ne_countries(scale = "medium", returnclass = "sf")
bbox <- st_bbox(data_sf)
# Create the map with land backgroun
p = ggplot() +
  geom_sf(data = world, fill = "gray80", color = "gray40") +   # Add land background
  geom_sf(data = data_sf, aes(color = Region, size = TotalDays)) +  # Color by Region, size by total days
  #facet_wrap(~Region) +
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]+5), 
           ylim = c(bbox["ymin"], bbox["ymax"]+5), 
           expand = FALSE) +  # Trim map to data points
  labs(title = "Map of Sites by Region and Total Days",
       size = "Total Days",
       color = "Region") +
  scale_size_continuous(range = c(.5, 10))  # Adjust point size range

ggsave(filename = paste0(outputDir, "/map_ONMS_", DC, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
