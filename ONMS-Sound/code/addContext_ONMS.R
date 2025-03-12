#ADD context metadata to hourly TOLs
#output of HrTOLs_ONMS.R, loads the most recent file

#ONMS metadata, wind Model, AIS (Jeff Adams, if available)

# works for each monitoring site

#LIBRARIES ####
rm(list=ls()) 
library(PAMscapes)
library(scales)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(reshape)
library(gtable)
library(grid)

#INPUT PARAMS ####
DC = Sys.Date()
project = "ONMS"
site = "sb03" # nrs11 mb02"
site1 =  "sb03" #cbnrs11 is weird...
fqIn = "TOL_125" 
ab = 70 # threshold for above frequency in
fqIn2 = "TOL_100" # no wind model for 125 Hz- ugh!!!
ab2 = 5
windUpp = 22.6 #which wind model result to show on plot
windLow = 1
windH = 10 #measured wind speeds
windL = 5

#DIRECTORIES ####
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context
if (site == "cb11"){
  outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" ) #products
  site = "nrs11"
}else (
  outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" )#products
)
outDirG = paste0( outDir,"report\\" ) #graphics
  
# LOAD ONMS Metadata ####
metaFile = paste0(outDirC,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
## frequency of interest ####
# tab 2 in lookup tables
FOI = as.data.frame ( read.xlsx(metaFile, sheetIndex = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOIs = FOI [ FOI$Sanctuary == substr(site1, 1,2), ]
## Seasonality ####
sidx = siteInfo$Seasonality
if ( is.na(sidx) ) {
  season = data.frame(
    Season = c("Winter", "Spring", "Summer", "Fall"),
    Months = c("1,2,3", "4,5,6", "7,8,9", "10,11,12") )
}else {
  season = data.frame(
    Season = c("Winter", "Spring", "Summer", "Fall"),
    Months = c("1,2,3", "4,5,6", "7,8,9", "10,11,12") )
}
# TOL conversion
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)
## times of interest ####
TOI = as.data.frame ( read.xlsx(metaFile, sheetIndex = "Time period of interest") )
TOI = TOI[!apply(TOI, 1, function(row) all(is.na(row))), ]
TOIs = TOI [ TOI$Site == (site1), ]
TOIs <- TOIs %>%
  mutate(
    Start_Julian = as.numeric(format(as.Date(start_date), "%j")),
    End_Julian = as.numeric(format(as.Date(end_date), "%j")),
    Mid_Julian = (Start_Julian + End_Julian) / 2  # Midpoint for annotation
  )
TOIs$yr = TOIs$Year

# LOAD SPL-TOL DATA ####
# HOURLY TOLs with wind estimate (gps)
inFile = list.files(outDirP, pattern = paste0("data_",site,"_HourlySPL-gfs_"), full.names = T)
file_info = file.info(inFile) 
load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
st = as.Date( min(gps$UTC) )
ed = as.Date( max(gps$UTC) )
udays = length( unique(as.Date(gps$UTC)) )
cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
#add seasonality
seas = unique(season$Season)
for( ss in 1:length(seas) ){
  moi = as.numeric(unlist(strsplit(as.character(season$Months[ss]), ",")))
  gps$Season[gps$mth %in% moi] = season$Season[ss]
}

### monitoring effort ####
summary <- gps %>%
  mutate(
    year = year(UTC),  # Extract Year
    month = format(UTC, "%m")  # Extract Month (numeric format)
  ) %>%
  count(year, month)  # Count occurrences (hours) in each year-month

p1 = ggplot(summary, aes(x = month, y = n, fill = as.factor(year))) +
  geom_col(position = "dodge") +  # Use dodge to separate bars for each year within the same month
  labs(
    caption = paste0(toupper(site), " has ", udays, 
                   " unique days: ", as.character(st), " to ", as.character(ed)),
    x = "",
    y = "Total Hours",
    fill = "Year"
  ) +
  scale_x_discrete(labels = month.name) +  # Show month names instead of numbers
  scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "top"  # Place the legend at the top
  )

p1
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_Effort.jpg"), plot = p1, width = 10, height = 6, dpi = 300)

## add wind category ####
hist( gps$windMag )
gps$wind_category = NA
gps <- gps %>%
  mutate(wind_category = case_when(
    is.na(windMag) ~ NA_character_,
    windMag < windL ~ "low",
    windMag >= windL & windV <= windH ~ "med",
    windMag > windH ~ "high"
  ))
# Calculate the counts for each category
category_counts <- gps %>%
  count(wind_category) %>%
  mutate(label = paste(wind_category, ":", n))
subtitle_text <- paste(category_counts$label, collapse = ", ")

# LOAD WIND Model ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!
# unique(windModel$si)
windInfo = windModel[tolower(windModel$si) == site1,]
widx = which( as.numeric(as.character( (colnames(windInfo)) ) )  == max(Fq) )
windInfo = windInfo[,1:widx]
#re-structure for ggplot
mwindInfo = melt(windInfo, id.vars = c("windSpeed"), measure.vars = colnames(windInfo)[4:ncol(windInfo)])
mwindInfo$variable = as.numeric( as.character(mwindInfo$variable ))
## evaluating the wind model ####
#ggplot(gps, aes(x = windMag , y = TOL_500), color = ( "mth" ))+
  #geom_point()+
  #scale_x_log10()

# LOAD AIS data ####
if (!is.na(siteInfo[24])) {
  load(paste0(outDirC, "Combine_ONMS_AIStransits_dataF.Rda") )
 
  ais = aisONMS[ tolower(aisONMS$loc_id) == site,]
  ais$Start = as.POSIXct( gsub("[+]00", "", ais$start_time_utc), tz = "GMT" ) 
  ais$End   = as.POSIXct( gsub("[+]00", "", ais$end_time_utc), tz = "GMT" ) 
  cat("AIS data for ", site, as.character( min(ais$Start) ), " to ", as.character( max(ais$Start) ))
  #min( ( as.numeric( as.character(ais$dist_nm) ) ) )
  #ais10 = ais[( as.numeric( as.character(ais$dist_nm) )) < 20,] #distance traveled in buffer
}

# PLOTS ####
## SPECTRA - seasonal quantiles ####
### wind bar ####
# Create the horizontal stacked bar plot with the counts of wind speed categories
gps$wind_category <- factor(gps$wind_category, levels = c("low", "med", "high"))
l = ggplot(gps, aes(x = "", fill = wind_category)) +
  geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +  # Stacked bar chart
  coord_flip() +  # Flip the coordinates to make it horizontal
  ggtitle("% time the soundscape is in different wind speed categories ") +  # Add the main title
  theme_minimal() +
  labs(x = NULL, y = NULL,
       subtitle = paste0("(low < ",windL, ", med ", windL,"-",windH, ", high > ",windH, " m/s)")) +  # Remove x-axis label
  theme(
    plot.title = element_text(hjust = 0),  # Align the title to the left
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels (now categories will appear below)
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Optional: remove legend title
    legend.text = element_text(size = 10)  # Optional: adjust legend text size
  ) +
  #scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
  scale_fill_manual(values = c("low" = "lightgray",  "med" = "gray", "high" = "darkgray") )

### quantiles ####
tol_columns = grep("TOL", colnames(gps))
allData = as.data.frame ( apply(gps[,tol_columns ], 2, quantile, na.rm = T) ) # all data
allData$Quantile = rownames((allData))
rownames(allData) <- NULL
### quantiles by season ####
tol_columns = grep("TOL", colnames(gps))
season_split = split(gps, gps$Season) # Calculate quantiles for each season
season_quantiles = lapply(season_split, function(season_data) {
  apply(season_data[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
})
tol_columns = grep("TOL", colnames(gps))
seasonAll = NULL
for (ii in 1: length(season_quantiles) ) {
  tmp = as.data.frame ( season_quantiles[ii] ) 
  colnames(tmp) = colnames(gps)[tol_columns]
  tmp$Quantile = rownames(tmp)
  tmp$Season = names(season_quantiles)[ii]
  rownames(tmp) = NULL
  seasonAll = rbind(seasonAll,tmp)
}
seasonAllbb = seasonAll #save the bb measurement
### convert 1 Hz ####
tol_columns = grep("TOL", colnames(seasonAll))
#divide unlog, by the bandwidth, and re-log
for( cc in 1:length(tol_columns)) {
  toltmp = colnames(seasonAll)[tol_columns[cc]]
  bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
  dtmp = seasonAll[,tol_columns[cc] ]
  seasonAll[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
}
### format for plot ####
mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
colnames(mallData) = c("Quantile", "Season", "Frequency" , "SoundLevel" )
max(gps$windMag, na.rm = T)
fqupper = max(as.numeric( as.character( mallData$Frequency) ))

### plot ####
p = ggplot() +
  
  # Add shaded area for 25%-75% range
  geom_ribbon(data = mallData %>% 
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
              alpha = 0.1) +  # Use alpha for transparency
  
  # Median (50%) TOL values
  geom_line(data = mallData[mallData$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel, color = Season), linewidth = 2) +
  
  # Set color and fill to match seasons
  scale_color_manual(values = c("Winter" = "#56B4E9", "Spring" = "#009E73", 
                                "Summer" = "#CC79A7", "Fall" = "#E69F00")) +
  scale_fill_manual(values = c("Winter" = "#56B4E9", "Spring" = "#009E73", 
                               "Summer" = "#CC79A7", "Fall" = "#E69F00")) + 
  #median TOL values
  #geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Season), linewidth = 2) +
  #scale_color_manual(values = c("Winter" = "#56B4E9", "Spring" = "#009E73", "Summer" = "#CC79A7", "Fall" = "#E69F00")) +
  #shade for 25/75 TOL values... coming soon
  #add wind model values
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper))) +  # Log scale for x-axis

  # Add vertical lines at FQstart
  geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    # Add labels at the bottom of each line
  geom_text(data = FOIs, aes(x = FQstart, y = 50, label = Label), angle = 90, vjust = 1, hjust = 0.5, size = 3) +
  #scale_color_manual(values = setNames(FOI$Color, FOI$Label)) +
  
  # Additional aesthetics
  
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0)) +  # This line removes the legend
  labs(
    title = paste0("Sounscape Contributions at ", toupper(site), ", a ", 
                   tolower(FOIs$Oceanographic.setting[1]), " monitoring site" ),
    subtitle = paste0( "data summarized from ", st, " to ", ed, "\n verticle lines indicate frequencies for sounds of interest in this soundscape" ),
    caption = paste0("black lines are expected wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s]"), 
    x = "Frequency Hz",
    y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
  )

separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
arranged_plot = grid.arrange(p,separator, l,heights =c(4, 0.05, 0.8))
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_SeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)


## SPECTRA - yearly quantiles ####
tol_columns = grep("TOL", colnames(gps))
season_split = split(gps, gps$yr) # Calculate quantiles for each season
season_quantiles = lapply(season_split, function(season_data) {
  apply(season_data[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
})
tol_columns = grep("TOL", colnames(gps))
yearAll = NULL
for (ii in 1: length(season_quantiles) ) {
  tmp = as.data.frame ( season_quantiles[ii] ) 
  colnames(tmp) = colnames(gps)[tol_columns]
  tmp$Quantile = rownames(tmp)
  tmp$Season = names(season_quantiles)[ii]
  rownames(tmp) = NULL
  yearAll = rbind(yearAll,tmp)
}
yearAllbb = yearAll #save the bb measurement

### convert 1 Hz ####
#divide unlog, by the bandwidth, and re-log
tol_columns = grep("TOL", colnames(yearAll))
for( cc in 1:length(tol_columns)) {
  toltmp = colnames(yearAll)[tol_columns[cc]]
  bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
  dtmp = yearAll[,tol_columns[cc] ]
  yearAll[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
}
### format for plot ####
mallData = melt(yearAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
colnames(mallData) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
max(gps$windMag, na.rm = T)
fqupper = max(as.numeric( as.character( mallData$Frequency) ))
p = ggplot() +
  
  geom_ribbon(data = mallData %>% 
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year),
              alpha = 0.1) +  # Use alpha for transparency
  
  #median TOL values
  geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year), linewidth = 2) +
  #scale_color_manual(values = c("Winter" = "#56B4E9", "Spring" = "#009E73", "Summer" = "#CC79A7", "Fall" = "#E69F00")) +
  #shade for 25/75 TOL values... coming soon
  #add wind model values
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper))) +  # Log scale for x-axis
  scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  scale_fill_manual(values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  # Add vertical lines at FQstart
  geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  # Add labels at the bottom of each line
  geom_text(data = FOIs, aes(x = FQstart, y = 50, label = Label), angle = 90, vjust = 1, hjust = 0.5, size = 3) +
  #scale_color_manual(values = setNames(FOI$Color, FOI$Label)) +
  
  # Additional aesthetics
  
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0)) +  # This line removes the legend
  labs(
    title =  paste0("Yearly Soundscape Comparison at ", toupper(site), ", a ", 
                    tolower(FOIs$Oceanographic.setting[1]), " monitoring site" ),
    subtitle = paste0( "data summarized from ", st, " to ", ed, "\n verticle lines indicate frequencies for sounds of interest in this soundscape" ),
    caption = paste0("black lines are modeled wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s]"), 
    x = "Frequency Hz",
    y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
  )
p
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_YearSPL.jpg"), plot = p, width = 10, height = 6, dpi = 300)

## CONVERT ALL DATA TO 1 Hz  ####
gpsBB = gps
tol_columns = grep("TOL", colnames(gps))
names(gps)
#divide unlog, by the bandwidth, and re-log
for( cc in 1:length(tol_columns)) {
  toltmp = colnames(gps)[tol_columns[cc]]
  bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
  dtmp = gps[, tol_columns[cc] ]
  gps[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
}
gpsBB$TOL_125[1] - gps$TOL_125[1]

## TIME SERIES - median 125 Hz TOL ####
cols_to_select = c("UTC", "windMag","wind_category",fqIn)
gpsFQ = gps %>% select(all_of(cols_to_select))
wspeeds = unique( (windModel$windSpeed) )
gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
### daily percentiles ####
dailyFQ = gpsFQ %>%
  mutate(Date = as.Date(UTC)) %>%
  group_by(Date) %>%
  summarise(
    TOL100_25 = quantile(.data[[fqIn]], 0.25, na.rm = TRUE),
    TOL100_50 = quantile(.data[[fqIn]], 0.50, na.rm = TRUE),
    TOL100_75 = quantile(.data[[fqIn]], 0.75, na.rm = TRUE),
    windspeed = quantile(windMag, 0.50, na.rm = TRUE)
  )
# names(dailyFQ)
dailyFQ$yr = year(dailyFQ$Date)
dailyFQ$Julian = yday(dailyFQ$Date)
### fill in days ####
dailyFQ_complete <- dailyFQ %>%
  group_by(yr) %>%
  complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>%  # Fill in missing days
  arrange(yr, Julian) 
monthly_sequence <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
month_names_seq <- format(monthly_sequence, "%b")  # Extracts full month names
days_of_year_for_months <- yday(monthly_sequence)
### calculate percentage above ####
# TOL100_50 > 60 for each year
percentage_above <- dailyFQ %>%
  mutate(year = year(Date)) %>%  # Create 'year' column from Date
  group_by(year) %>%  # Group by year
  summarise(
    total_count = n(),  # Total number of rows for each year
    count_above = sum(TOL100_50 > ab, na.rm = TRUE),  # Count of values > 60
    percentage_above = (count_above / total_count) * 100  # Percentage calculation
  )
# percentage_above
yrs = unique(dailyFQ_complete$yr)
dailyFQ_complete$facet_title = NA
for ( ii in 1:length(yrs ) ) {
 idx = which(dailyFQ_complete$yr == yrs[ii])
 idx2 =  which(percentage_above$year == yrs[ii])
 dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", round(percentage_above$percentage_above[idx2]),"% above" )
}

TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
for (ii in 1:nrow(TOIs) ) {
  TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
}


 ### plot ####
p = ggplot(dailyFQ_complete, aes(x = Julian, y = TOL100_50, group = yr, color = factor(yr))) +
  
  # Shaded area first (so it's in the background)
  geom_rect(data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
            inherit.aes = FALSE,
            aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
            alpha = 0.2) +
  
  scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  
  # Main data layers (drawn above the shaded area)
  geom_line(size = 1, na.rm = TRUE) +
  geom_ribbon(aes(ymin = TOL100_25, ymax = TOL100_75), fill = "gray", alpha = 0.5) +
  facet_wrap(~facet_title, nrow = length(unique(dailyFQ_complete$yr))) +
  theme_minimal() +
  scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq) +  
  geom_hline(aes(yintercept = ab), linetype = "dashed", color = "gray", size = 0.7) +
  
  theme(
    legend.position = "none",
        strip.text = element_text(size = 10, hjust =0, vjust = 0),  # Facet labels inside (centered)
        strip.background = element_blank(),  # Remove background behind facet labels
        panel.spacing = unit(.1, "lines") ) + # Adjust the spacing between facets) +
  labs(
    title = paste0("Contribution of ship noise to the soundscape at 125 Hz" ) ,
    subtitle =  paste0(toupper(site), ", a ", tolower(FOI$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ),
    caption = '',
    x = "",
    y = expression(paste("Sound Levels (125 Hz dB re 1", mu, " Pa/Hz)" ) ),
    color = "Year"  # Label for the color legend
  ) 
p
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_125Hz.jpg"), plot = p, width = 10, height = 10, dpi = 300)


 ## TIME SERIES - exceedence 100 Hz  ####
cols_to_select = c("UTC", "windMag","wind_category",fqIn2)
gpsFQ = gps %>% select(all_of(cols_to_select))
#ADD wind speed estimate for each hour of data in frequency of interest
wspeeds = unique( (windModel$windSpeed) )
gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
# what is the spl values for that windspeed?
fqIdx = which( colnames( windInfo) == substr( fqIn2, 5,8)) #'100'
wsIdx <- match(gpsFQ$closest_windMag, windInfo$windSpeed)
gpsFQ$WindModel100 <- windInfo[wsIdx, fqIdx]
names(gpsFQ)
gpsFQ$Exceed = gpsFQ$TOL_100 -  gpsFQ$WindModel100 #actual - model
#hist(gpsFQ$Exceed)

dailyFQ = gpsFQ %>%
  mutate(Date = as.Date(UTC)) %>%
  group_by(Date) %>%
  summarise(
    Exceed_25 = quantile(Exceed, 0.25, na.rm = TRUE),
    Exceed_50 = quantile(Exceed, 0.50, na.rm = TRUE), # Median
    Exceed_75 = quantile(Exceed, 0.75, na.rm = TRUE),
    TOL100_25 = quantile(.data[[fqIn2]], 0.25, na.rm = TRUE),
    TOL100_50 = quantile(.data[[fqIn2]], 0.50, na.rm = TRUE),
    TOL100_75 = quantile(.data[[fqIn2]], 0.75, na.rm = TRUE),
    windspeed = quantile(windMag, 0.50, na.rm = TRUE)
  )

names(dailyFQ)
dailyFQ$yr = year(dailyFQ$Date)
dailyFQ$Julian = yday(dailyFQ$Date)
dailyFQ_complete <- dailyFQ %>%
  group_by(yr) %>%
  complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>%  # Fill in missing days
  arrange(yr, Julian) 
### calculate % above ####
percentage_above <- dailyFQ %>%
  mutate(year = year(Date)) %>%  # Create 'year' column from Date
  group_by(year) %>%  # Group by year
  summarise(
    total_count = n(),  # Total number of rows for each year
    count_above = sum(Exceed_50 > ab2, na.rm = TRUE),  # Count of values > 60
    percentage_above = (count_above / total_count) * 100  # Percentage calculation
  )
percentage_above
### graphic titles ####
yrs = unique(dailyFQ_complete$yr)
dailyFQ_complete$facet_title = NA
for ( ii in 1:length(yrs ) ) {
  idx = which(dailyFQ_complete$yr == yrs[ii])
  idx2 =  which(percentage_above$year == yrs[ii])
  dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", round(percentage_above$percentage_above[idx2]),"% above" )
}
### plot ####
TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
for (ii in 1:nrow(TOIs) ) {
  TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
}

pE = ggplot(dailyFQ_complete, aes(x = Julian, y = Exceed_50, group = yr, color = factor(yr))) +
 
  geom_rect(data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
            inherit.aes = FALSE,
            aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
            alpha = 0.2) +
  
   geom_line(size = 1, na.rm = T) +
  #geom_point(size = 2) +
  geom_ribbon(aes(ymin = Exceed_25, ymax = Exceed_75), fill = "gray", alpha = 0.5) +
  #geom_col(aes(y = windspeed) , color = "gray", alpha = 1) +
  facet_wrap(~facet_title, nrow = length(unique(dailyFQ$yr)) ) +
  theme_minimal()+
  scale_x_continuous( breaks = days_of_year_for_months, label = month_names_seq) +  # Show every 30 days
  scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  
  geom_hline(aes(yintercept = ab2), linetype = "dashed", color = "gray", size = .7) +
  #geom_hline(aes(yintercept = 10), linetype = "dashed", color = "gray",size = .5) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, hjust =0, vjust = 0),  # Facet labels inside (centered)
    strip.background = element_blank(),  # Remove background behind facet labels
    panel.spacing = unit(.1, "lines") ) + # Adjust the spacing between facets) +
  
  labs(
    title =paste0("Contribution of ship noise to the soundscape" ),
    subtitle =  paste0(toupper(site), ", a ", tolower(FOI$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ) ,
    caption = paste0("exceedance calculated as the difference in measured sound level from modeled sound levels based on wind speed at 100 Hz
                     threshold for % above = ", ab2, "dB"),
    x = "",
    y = expression(paste("Noise Exceedance (decibels)" ) ),
    color = "Year"  # Label for the color legend
  ) 
pE
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_Exceed100.jpg"), plot = pE, width = 10, height = 10, dpi = 300)

### create output table ####
### calculate % above VSR ####
# add VSR to hourly data
gpsFQ$VSR = "no"
for (tt in 1:nrow( TOIs )){
  idx = which(gpsFQ$UTC >= TOIs$start_date[tt] & gpsFQ$UTC < TOIs$end_date[tt]+1)
  cat(year( TOIs$start_date[tt]) , ": ", as.character( gpsFQ$UTC[idx[1]]), "\n")
  gpsFQ$VSR[idx] = "yes"
  rm(idx)
}
names(gpsFQ)

# percent above wind noise threshold, summarized by year for all three categories (above, below, NA)
percentage_above <- gpsFQ %>%
  mutate(year = year(UTC), 
         mth = month(UTC, label = TRUE)) %>%  # Extract month name
  group_by(year, mth, VSR) %>%
  summarise(
    total_count = n(),
    count_above = sum(Exceed >= ab2, na.rm = TRUE),
    count_below = sum(Exceed < ab2, na.rm = TRUE),
    count_na = sum(is.na(Exceed)),  # Count NAs
    .groups = "drop"
  ) %>%
  mutate(
    percentage_above = (count_above / total_count) * 100,
    percentage_below = (count_below / total_count) * 100,
    percentage_na = (count_na / total_count) * 100
  ) %>%
  select(year, mth, VSR, percentage_above, percentage_below, percentage_na) %>%
  pivot_longer(cols = c(percentage_above, percentage_below, percentage_na),
               names_to = "Threshold_Category",
               values_to = "Percentage")

#output table Year- reformat
percentage_aboveT = percentage_above[percentage_above$Threshold_Category == "percentage_above",]
percentage_aboveT$Percentage = round( percentage_aboveT$Percentage )
df_wide <- percentage_aboveT %>%
  pivot_wider(
    names_from = year,  # The column containing year
    values_from = Percentage,  # The column containing values (percentage)
    #names_prefix = "Year_"  # Optional prefix for year columns
  )
df_wide <- df_wide %>%
  select(-Threshold_Category)

title_grob <- textGrob( paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)"), gp = gpar(fontsize = 16, fontface = "bold"))
table_grob <- tableGrob(df_wide)
combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
ggsave(paste0(outDirG, "table_", site, "_Exceed.jpg"), combined_grob, width = 8, height = 5)

percentage_above$Threshold_Category <- factor(percentage_above$Threshold_Category, 
                                              levels = c("percentage_below", "percentage_above", "percentage_na"))
ggplot(percentage_above, aes(x = mth, y = Percentage, fill = Threshold_Category,color = VSR)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked to show proportion with correct ordering
  facet_wrap(~year, nrow = 1) +  # One row per year
  labs(x = "", y = "Percentage", 
       title = paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)")) +
  scale_fill_manual(values = c("percentage_above" = "tomato",  # 'percentage_above' on bottom
                               "percentage_below" = "gray",
                               "percentage_na" = "lightgray")) +  # Custom colors
  scale_x_discrete(breaks = levels(percentage_above$mth)[seq(1, length(levels(percentage_above$mth)), by = 3)]) +  
  scale_color_manual(values = c("yes" = "black", "no" = NA)) +  # Outline months with VSR
  # Show every third month
  theme_minimal()

## SPECTRA- AIS vs SPL ####
# plot spectra when ships present vs not present- ships present in a given hour
# want to also calculate noise exceedance when AIS ships are present, use gpsFQ
cat("AIS data for ", site, as.character( min(ais$Start) ), " to ", as.character( max(ais$Start) ))
gps$GMT =as.POSIXct(gps$UTC,"GMT")

### truncate gps data ####
# to only data within the AIS range
rm(gpsAIS)
gpsAIS  = gps[ gps$UTC > min(ais$Start) & gps$UTC <=  max(ais$Start), ]
gpsAIS$numAIS = 0  #number of ship transits during that hour
gpsAIS$minAIS = NA #minimum distance for ships during that hour
gpsAIS$avgAIS = NA #average speed of ships during that hour
cat("removed ", nrow(gps) - nrow(gpsAIS), "hours because no AIS data")

### AIS match ####
if ( nrow(ais) > 0 ){
  
  for (ii in 1: nrow(gpsAIS) ){
    
    #get all vessel transits that within the hour?
    AIStmp = ais[ais$Start >= gpsAIS$GMT[ii] & ais$Start < gpsAIS$GMT[ii] + (3600), ] 
    if (  nrow(AIStmp) > 0 ){
      gpsAIS$numAIS[ii] =  nrow(AIStmp)
      gpsAIS$minAIS[ii]  = min(AIStmp$dist_nm, na.rm = T)
      gpsAIS$avgAIS[ii]  = mean(as.numeric( as.character( AIStmp$avg_sog_dw )), na.rm = T)
    }
  }
  
} else { cat("No AIS data for this location")}
gpsAIS[ which.max( gpsAIS$numAIS ),]

### AIS categories ####
gpsAIS$ais_category = NA
gpsAIS <- gpsAIS %>%
  mutate(ais_category = case_when(
    is.na(numAIS) ~ NA_character_,
    numAIS == 0 ~ "0-none",
    numAIS > 0 & numAIS <= 2 ~ "1-low",
    numAIS > 2 & numAIS <= 5 ~ "2-med",
    numAIS > 5 ~ "3-high"
  ))
category_counts <- gpsAIS %>%
  count(ais_category) %>%
  mutate(label = paste(ais_category, ":", n))
subtitle_text <- paste(category_counts$label, collapse = ", ")
category_counts

### quantiles by ais ####
tol_columns = grep("TOL", colnames(gpsAIS))
season_split = split(gpsAIS, gpsAIS$ais_category) # Calculate quantiles for each season
season_quantiles = lapply(season_split, function(season_data) {
  apply(season_data[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
})
unique( gpsAIS$numAIS[ gpsAIS$ais_category == "3-high"] )

tol_columns = grep("TOL", colnames(gpsAIS))
seasonAll = NULL
for (ii in 1: length(season_quantiles) ) {
  tmp = as.data.frame ( season_quantiles[ii] ) 
  colnames(tmp) = colnames(gpsAIS)[tol_columns]
  tmp$Quantile = rownames(tmp)
  tmp$Season = names(season_quantiles)[ii]
  rownames(tmp) = NULL
  seasonAll = rbind(seasonAll,tmp)
}

### format for plot ####
tol_columns = grep("TOL", colnames(seasonAll))
mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
colnames(mallData) = c("Quantile", "AIS", "Frequency" , "SoundLevel" )
names(mallData)
unique(mallData$AIS)
stAIS = as.Date( min(gpsAIS$UTC) )
edAIS = as.Date( max(gpsAIS$UTC) )

### plot ####
lais = ggplot(gpsAIS, aes(x = "", fill = ais_category)) +
  geom_bar(stat = "count", position = "stack") +  # Stacked bar chart
  coord_flip() +  # Flip the coordinates to make it horizontal
  ggtitle("AIS vessels categories") +  # Add the main title
  theme_minimal() +
  labs(x = NULL, y = NULL, subtitle_text = "low < 3, med 3-5, high >5") +  # Remove x-axis label
  theme(
    plot.title = element_text(hjust = 0),  # Align the title to the left
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels (now categories will appear below)
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Optional: remove legend title
    legend.text = element_text(size = 10)  # Optional: adjust legend text size
  ) +
  scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
  scale_fill_manual(values = c("0-none" = "#56B4E9", "1-low" = "#009E73", "3-high" = "#CC79A7", 
                               "2-med" = "#E69F00"))  # Reverse the legend order
dBIncrease = NULL
dBIncrease$dB[1] = round( mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "1-low"]  -
         mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "0-none"] )
dBIncrease$dB[2] = round( mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "1-low"]  -
                            mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "0-none"] )
dBIncrease$dBval[1] = mallData$SoundLevel[mallData$Frequency == 63 & mallData$Quantile == "50%" & mallData$AIS == "0-none"]
dBIncrease$dBval[2] = mallData$SoundLevel[mallData$Frequency == 125 & mallData$Quantile == "50%" & mallData$AIS == "0-none"]

pais = ggplot() +
  # Add shaded area for 25%-75% range
  geom_ribbon(data = mallData %>% 
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = AIS),
              alpha = 0.1) +  # Use alpha for transparency
  
  #median TOL values
  geom_line(data = mallData[mallData$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel, color = AIS), linewidth = 1) +
  
  scale_color_manual(values = c("0-none" = "#56B4E9", 
                                "1-low" = "#009E73", 
                                "3-high" = "#CC79A7", 
                                "2-med" = "#E69F00")) +
  scale_fill_manual(values = c("0-none" = "#56B4E9", "1-low" = "#009E73", 
                               "3-high" = "#CC79A7", "2-med" = "#E69F00")) + 
  
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == "22.6",], 
            aes(x = variable, y = value), color = "black",  linewidth = 1) +
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == "1",], 
            aes(x = variable, y = value), color = "black",  linewidth = 1) +
  geom_vline(aes(xintercept = 125, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  
  geom_vline(aes(xintercept = 63, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  
  geom_text(aes(x = 63,  y = dBIncrease$dBval[1]+5, label = "63 Hz"), angle = 0, vjust = 0, hjust = 0, size = 3) +
  geom_text(aes(x = 125, y = dBIncrease$dBval[1]+5, label = "125 Hz"), angle = 0, vjust = 0, hjust = 0, size = 3) +
  scale_x_log10(labels = label_number()) +  # Log scale for x-axis
   
  # Additional aesthetics
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", hjust = 0)) + 
  labs(
    title =paste0("Contribution of ship noise to the soundscape" ),
    subtitle =  paste0(toupper(site), ", a ", tolower(FOI$Oceanographic.setting[1]), 
                       " site (data summarized from ", st, " to ", ed, ")\n",
                       dBIncrease$dB[1], "dB increase from no AIS at 63 Hz \n",
                       dBIncrease$dB[2], "dB increase from no AIS at 125 Hz" ) ,
    caption = paste0("black lines are modeled wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s]\n",
                     "AIS vessels are those ships transmitting data about position and speed"), 
    x = "Frequency Hz",
    y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
  )
arranged_plot = grid.arrange(pais,lais,heights = c(4, .9))

ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AISNoise.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)

### plot ####
# distributions of 125 
gpsAIS$ais_category  = ( substr(gpsAIS$ais_category, start = 3, stop = 6) )
gpsAIS$ais_category2 = factor(gpsAIS$ais_category, 
                              levels = c("none", "low", "med", "high"), 
                              ordered = TRUE)
medians <- gpsAIS %>%
  group_by(ais_category2) %>%
  summarise(median_value = median(TOL_125, na.rm = TRUE))

# Step 1: Create Pie Chart Data
pie_data <- gpsAIS %>%
  group_by(ais_category2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

# Step 2: Create Pie Chart
pie_chart <- ggplot(pie_data, aes(x = "", y = percentage, fill = ais_category2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  
  scale_fill_brewer(palette = "Set3") +  
  theme_void() +  # Remove background, axes, and gridlines
  theme(legend.position = "none") +
  labs(title = "Proportion of Hours") # Hide legend in pie chart

# Convert pie chart to grob
pie_grob <- ggplotGrob(pie_chart)
pais2 = ggplot(gpsAIS, aes(x = TOL_125, fill = ais_category2)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") + 
  geom_vline(data = medians, aes(xintercept = median_value, color = ais_category2), 
             linetype = "solid", size = 1) +  # Add median lines
  labs(
    title =paste0("Contribution of ship noise to the soundscape" ),
    subtitle =  paste0(toupper(site), ", a ", tolower(FOI$Oceanographic.setting[1]), 
                       " site (data summarized from ", st, " to ", ed, ")\n" ) ,
    caption = "low<3 ships, med 3-5 ships, high>5 ships",
    x = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) ), 
    y = "Count of Hours",
    fill = "Ships transiting nearby",  # Change legend title for fill
    color = "Ships transiting nearby")  +  # Change legend title for color) +
  scale_fill_brewer(palette = "Set3") +  
  scale_color_brewer(palette = "Set3") +  # Match colors for clarity
  theme_minimal() +
  annotation_custom(pie_grob, xmin = max(gpsAIS$TOL_125) - 20, xmax = max(gpsAIS$TOL_125),
                  ymin = min(pie_data$count), ymax =min(pie_data$count)+300 )  

pais2
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AIShist.jpg"), plot = pais2, width = 10, height = 10, dpi = 300)

### table results- per month summaries ####
gpsAIS$mth = month( gpsAIS$UTC )
gpsAIS$yr = year( gpsAIS$UTC )
#Group by month and AIS category, calculate count
pie_table <- gpsAIS %>%
  group_by(mth, ais_category2) %>%
  summarise(count = n(), .groups = "drop")  # Get count for each AIS category in each month
# Calculate percentage of total count per month
pie_table <- pie_table %>%
  group_by(mth) %>%
  mutate(percentage = round(count / sum(count) * 100))  # Percentage relative to total count for each month
pie_table$percentage = round(pie_table$percentage,1)
#Pivot to get months as rows, AIS categories as columns
pie_table_wide <- pie_table %>%
  select(-count) %>%  # Remove count column
  pivot_wider(names_from = ais_category2, values_from = (percentage), values_fill = list(percentage = 0))

print(pie_table_wide)
title_grob <- textGrob( paste0("Proportion of hours in each AIS category \n (", toupper(site), ")"),
                        gp = gpar(fontsize = 16, fontface = "bold"))
table_grob <- tableGrob(as.data.frame( pie_table_wide) )
combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
ggsave(paste0(outDirG, "table_", site, "_AIShist.jpg"), combined_grob, width = 8, height = 5)


### plot ####
# timeseries of 125 
# each day median level for each category....
names(gpsAIS)
gpsAIS$day = as.Date(gpsAIS$UTC)
gpsAIS$ais = "present"
gpsAIS$ais[gpsAIS$ais_category2 == "none"] = "none"

medians <- gpsAIS %>%
  group_by(day, ais) %>%
  summarise(median_value = median(TOL_125, na.rm = TRUE))

medians_diff <- gpsAIS %>%
  group_by(day, ais) %>%
  summarise(median_value = median(TOL_125, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ais, values_from = median_value) %>%  # Make columns for "none" & "present"
  mutate(difference = `present` - `none`)  # Calculate difference
medians_diff$year = year(medians_diff$day )

pais3 = ggplot(medians_diff, aes(x = day, y = difference, fill = difference > 0)) + 
  geom_col(show.legend = FALSE) +  # Use geom_col to plot bars with height defined by difference
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue")) +  # Color bars based on positive or negative difference
  labs(
    title = "Difference in soundscape when vessel nearby",
    subtitle =  paste0(toupper(site), ", a ", tolower(FOI$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ),
    x = "",
    y = ("Daily difference in decibels at 125 Hz \n (vessel - non-vessel)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  
  geom_vline(data = medians_diff, aes(xintercept = as.Date(paste0(year, "-01-01"))), 
             color = "black", linetype = "dashed") + # Add dashed line at zero for reference
  geom_rect(data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
            inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date , ymin = -Inf, ymax = Inf), 
            alpha = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
pais3
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AISTimeSeries.jpg"), plot = pais3, width = 12, height = 8, dpi = 300)
###table OF RESULTS ####
medians_diff$mth = month(medians_diff$day)
medians_diff$yr = year(medians_diff$day)

medians <- medians_diff %>%
  group_by(yr, mth) %>%
  summarise(median_value = round(mean(difference, na.rm = TRUE), 1), .groups = "drop") %>%
  mutate(mth = factor(month.abb[mth], levels = month.abb))  # Ensure correct order

month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Pivot wider first
medians_wide <- medians %>%
  pivot_wider(names_from = mth, values_from = median_value)

# Reorder columns to match the correct month order
medians_wide <- medians_wide %>%
  select(yr, all_of(intersect(month_order, names(medians_wide))))

medians_wide

title_grob <- textGrob( paste0("Monthly average difference in soundscape when vessel nearby \n (", toupper(site), ")"),
                        gp = gpar(fontsize = 16, fontface = "bold"))
table_grob <- tableGrob(as.data.frame( medians_wide) )
combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
ggsave(paste0(outDirG, "table_", site, "_AISabove.jpg"), combined_grob, width = 8, height = 5)


