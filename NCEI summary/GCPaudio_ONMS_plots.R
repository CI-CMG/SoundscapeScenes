
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
#library(ggsn)

DC = Sys.Date()

# INPUT - 
inDir = "F:\\ONMS" #"G:\\My Drive\\ActiveProjects\\SANCTSOUND_shared\\ONMS"



x_min = min(output$Start_Date)
x_max = max(output$End_Date)


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

