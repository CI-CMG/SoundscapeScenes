
rm(list=ls()) 
# PURPOSE -  plot the outpts of query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - 
# INPUT - 
# OUTPUT - png, eps, files 


# FUTURE UPDATES - 
#- make a map of the sites with bubble size for number of days at each site
#- read in project look-up tables to help with formatting- e.g. converts site codes to names, separates by region (west, east, pacific, great lakes)
#- save graphic as html, interactive (p_plotly <- ggplotly(p), htmlwidgets::saveWidget(p_plotly, "plot.html"))
#- what do I do with the quality matrix- create a csv?
#- how do I update NCEI data product?? 


library(ggplot2)
library(dplyr)
library(plotly)
#for map
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)


DC = Sys.Date()

# INPUT - 
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context
outDirP = paste0( outDir,"products\\onms\\")#products
outDirG = paste0( outDir,"report\\" ) #graphics
setwd(outDirP)

#PLOT MAP OF REGION ####
load("map_ONMS_map.Rda" )
names(outputMap2a)
outputMap2a$Latitude  = as.numeric(as.character(gsub("B0","", outputMap2a$Latitude)) )
outputMap2a$Longitude = as.numeric(as.character(gsub("B0","", outputMap2a$Longitude)) )
outputMap2a$TotalDays = as.numeric( outputMap2a$TotalDays )
output = outputMap2a[outputMap2a$Sanctuary =="MB",]
data_sf = st_as_sf(output, coords = c("Longitude", "Latitude"), crs = 4326)
bbox=st_bbox(data_sf)
world = ne_countries(scale = "large", returnclass = "sf")
p = ggplot() +
  geom_sf(data = world, fill = "gray80", color = "gray40") +   # Add land background
  geom_sf(data = data_sf, aes(color = Region, size = as.numeric( TotalDays ) ) )  +  # Color by Region, size by total days
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"]-1, bbox["xmax"]+1), 
           ylim = c(bbox["ymin"]-1, bbox["ymax"]+1), 
           expand = FALSE) +  # Trim map to data points
  labs(title = "Map of Sites by Region and Total Days",
       size = "Total Days") +
  scale_size_continuous(range = c(1, 5))  # Adjust point size range
p

