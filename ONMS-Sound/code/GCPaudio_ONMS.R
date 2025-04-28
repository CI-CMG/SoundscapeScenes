rm(list=ls()) 

# PURPOSE -  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - Rdat file to use in plotting


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

# SET GCP DIRECTORY ####
# get directories from NCEI PAM map viewer
DC = Sys.Date()
typ = "audio"
gcpDir  = "gs://noaa-passive-bioacoustic/onms/audio" #ONMS
gcpDir2 = "gs://noaa-passive-bioacoustic/sanctsound/audio" #SANCTSOUND
projectN = "onms" #nrs # set this to deal with different metadata formats
projectN2 = "sanctsound" #nrs # set this to deal with different metadata formats

outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outputDir = paste0( outDir,"products\\onms\\")   
outputR = paste0( outDir,"report")   

metadataDir = "F:\\CODE\\GitHub\\SoundscapesWebsite\\resources"
inFile = paste0(metadataDir, "//ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(inFile) )
colnames(lookup) <- lookup[1, ]  # Set first row as column names
lookup <- as.data.frame( lookup[-1, ] )          # Remove the first row
colnames(lookup) 
colnames(lookup)[5] = "NCEI"

# LIST SUB DIRECTORIES ####
#these should be the "monitoring sites" you want to gather information about
command = "gsutil"
args =  c("ls", gcpDir)
subdirs = system2(command, args, stdout = TRUE, stderr = TRUE) 
command = "gsutil"
args =  c("ls", gcpDir2)
subdirs2 = system2(command, args, stdout = TRUE, stderr = TRUE)  
subdirsALL = c(subdirs, subdirs2) #paste onms + sanctsound
dirNames  = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
cat("Processing... ", projectN, length(dirNames), "directories" )

#read one file
args = c("ls", "-r", subdirsALL[1])
sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[1], collapse = "") ) )
h = curl(url, "r")
json_content = readLines(url)
close(h)
tmp = fromJSON(paste(url, collapse = ""))

# GET INFORMATION FROM METADATA FILES ####
output = NULL
for (s in 1:length(subdirsALL) ) { # s = 1
  
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
      lon   = tmp$DEPLOYMENT$DEPLOY_LON
      
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
        lon   = tmp$DEPLOYMENT$DEPLOY_LON
        
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

# SAVE SUMMARY ####
output = as.data.frame(output)
colnames(output) = c("Path", "DeploymentNumber", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
output$Site = basename((output$Path))
output$Start_Date = as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date = as.Date(output$End_Date, format = "%Y-%m-%d")

# ADD INFO from lookup ####
lookup_selected <- lookup %>% select(NCEI, Region,`Common Name/Identifiers`,`Site Description/Driver for Monitoring Location Choice`)
output = left_join(output, lookup_selected, by = c("Site" = "NCEI"))
output$Duration = difftime( output$End_Date, output$Start_Date,"days")
colnames(output)
#remove non-monitoring sites
# fix- only once
# output <- output %>% select(-Region.x)
# output <- output %>% rename(Region = Region.y)

setdiff(unique(output$Site), unique(lookup$NCEI))
outputT = output[!is.na(output$Region),] 


save(outputT, file = paste0(outputDir, "\\data_gantt_ONMS_gantt_", DC, ".Rda") )
write.csv(outputT, file = paste0(outputDir, "\\data_gantt_ONMS_gantt_", DC, ".csv") )
colnames(outputT)

# GANTT CHART  ####
load(file = paste0(outputDir, "\\data_gantt_ONMS_gantt_2025-04-28.Rda") )
projectN = "onmsRegion"
uColors = unique(outputT$Instrument)
if (projectN == "onms"){
  instrument_colors <- c(
    "SoundTrap 500" = "#88CCEE",  
    "SoundTrap 600" = "#CC6677",#88CCEE",  
    "SoundTrap 300" = "#44AA99", 
    "HARP" =          "#DDCC77") #DDCC77
}
uColors = unique(outputT$Region)
if (projectN == "onmsRegion"){
  instrument_colors <- c(
    "Pacific Islands" = "#C6E6F0",  
    "West Coast"      = "#53B0D7",  
    "East Coast"      = "#004295", 
    "Gulf Coast"      = "#001743") 
}
nmfspalette::nmfs_palette("oceans")(10)
#[1] "#C6E6F0" "#8CCBE3" "#53B0D7" "#1F95CF" "#0072BB" "#004295" "#002B7B" "#002467" "#001D55" "#001743"

# geom_tile option 
colnames(outputT)
pT = ggplot(outputT, aes(y = Site, x = Start_Date, xend = End_Date, fill = Region ) ) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date) ) , 
            color = "gray", height = 0.6) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = instrument_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = "",
       subtitle = paste0("Data available on NCEI-GCP (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 10, size = 12))
pT
ggsave(filename = paste0(outputR, "\\gantt_ONMS.jpg"), plot = pT, width = 8, height = 6, dpi = 300)

outputT$Site
# MAP DATA ####
#reformat for per site- total recordings 
rm ( outputMap )
outputMap =  as.data.frame(
  outputT %>%
    group_by(Site) %>%
    summarise(
      total_days = sum(Duration, na.rm = TRUE),   # Summing total duration for each site
      min_start_date = min(Start_Date, na.rm = TRUE)  # Getting the minimum start date for each site
    )
)
head(outputMap)
outputMap$Site = as.character(outputMap$Site)

lookup_selected <- lookup %>% select(NCEI, Region, Latitude, Longitude)
outputMap = left_join(outputMap, lookup_selected, by = c("Site" = "NCEI"))
colnames(outputMap)
write.csv( outputMap, file = paste0(outputDir, "\\map_ONMS_map_", DC, ".csv") )
outputMap2a = outputMap
# Assuming your data frame is named 'data'
# Convert the data frame to an sf object for mapping
outputMap2a$Latitude = as.numeric(as.character( gsub("B0", '', outputMap2a$Latitude )) )
outputMap2a$Longitude = as.numeric(as.character( gsub("B0", '', outputMap2a$Longitude )) )
outputMap2a$TotalDays = as.numeric(as.character(outputMap2a$total_days))
data_sf <- st_as_sf(outputMap2a, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf
# Create the map
# Get world land data (in low resolution, change to 'medium' or 'large' if needed)
world <- ne_countries(scale = "medium", returnclass = "sf")
bbox <- st_bbox(data_sf)
# Create the map with land backgroun
p = ggplot() +
  geom_sf(data = world, fill = "gray80", color = "gray40") +   # Add land background
  geom_sf(data = data_sf, aes(color = Region, size = TotalDays)) +  # Color by Region, size by total days
  scale_color_manual(values = instrument_colors) +  # Use specific colors for instruments
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]+5), 
           ylim = c(bbox["ymin"], bbox["ymax"]+5), 
           expand = FALSE) +  # Trim map to data points
  labs(title = "",
       size =  "Total Days",
       color = "Region",
       caption = paste0("Data available on NCEI-GCP (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y") ) ) +
  scale_size_continuous(range = c(.5, 8))  # Adjust point size range
p

ggsave(filename = paste0(outputR, "\\map_ONMS.jpg"), plot = p, width = 8, height = 6, dpi = 300)
