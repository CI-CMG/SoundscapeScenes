rm(list=ls()) 

#INPUT PARAMS ####
DC = Sys.Date()
project = "ONMS"
site = "oc02" # "sb03" nrs11 mb02"
site1 =  "oc02" #cbnrs11 is weird...
AISUpp = 5 
AISLow = 2
windUpp = 22.6 #which wind model result to show on plot
windLow = 1

# DIRECTORIES ####
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context
if (site == "cb11"){
  outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" ) #products
  site = "nrs11"
}else (
  outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" )#products
)
outDirG = paste0( outDir,"report\\" ) #graphics

# LOAD SOUND DATA ####
inFile = list.files(outDirP, pattern = paste0("data_",site,"_HourlySPL-gfs-season_"), full.names = T)
file_info = file.info(inFile) 
load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
st = as.Date( min(gps$UTC) )
ed = as.Date( max(gps$UTC) )
udays = length( unique(as.Date(gps$UTC)) )
cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))

# LOAD ONMS Metadata ####
metaFile = paste0(outDirC,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
## frequency of interest ####
FOI = as.data.frame ( read.xlsx(metaFile, sheetIndex = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOIs = FOI [ FOI$Sanctuary == substr(site1, 1,2), ]
## seasonality ####
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

# LOAD WIND MODEL ####
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

# LOAD AIS data ####
if (!is.na(siteInfo[24])) {
  load(paste0(outDirC, "Combine_ONMS_AIStransits_dataF.Rda") )
  ais = aisONMS[ tolower(aisONMS$loc_id) == site,]
  ais$Start = as.POSIXct( gsub("[+]00", "", ais$start_time_utc), tz = "GMT" ) 
  ais$End   = as.POSIXct( gsub("[+]00", "", ais$end_time_utc), tz = "GMT" ) 
  cat("AIS data for ", site, as.character( min(ais$Start) ), " to ", as.character( max(ais$Start) ))
}
## truncate sound data to AIS ####
gpsAIS  = gps[ gps$UTC > min(ais$Start) & gps$UTC <=  max(ais$Start), ]
gpsAIS$numAIS = 0  #number of ship transits during that hour
gpsAIS$minAIS = NA #minimum distance for ships during that hour
gpsAIS$avgAIS = NA #average speed of ships during that hour
cat("removed ", nrow(gps) - nrow(gpsAIS), "hours because no AIS data")
gpsAIS$GMT =as.POSIXct(gpsAIS$UTC,"GMT")

# MATCH AIS ####
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
# hour with max # ships: gpsAIS[ which.max( gpsAIS$numAIS ),]

# DEFINE AIS categories ####
gpsAIS$ais_category = NA
gpsAIS <- gpsAIS %>%
  mutate(ais_category = case_when(
    is.na(numAIS) ~ NA_character_,
    numAIS == 0 ~ "0-none",
    numAIS > 0 & numAIS <= AISLow ~ "1-low",
    numAIS > AISLow & numAIS <= AISUpp ~ "2-med",
    numAIS > AISUpp ~ "3-high"
  ))
category_counts <- gpsAIS %>%
  count(ais_category) %>%
  mutate(label = paste(ais_category, ":", n))
subtitle_text <- paste(category_counts$label, collapse = ", ")
category_counts

# save: updated data ####
save(gpsAIS, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs-season-ais_", DC, ".Rda") )

# QUANTILES by ais ####
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

# SPECTRAL ANALYSIS ####
#formating for plot
tol_columns = grep("TOL", colnames(seasonAll))
mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
colnames(mallData) = c("Quantile", "AIS", "Frequency" , "SoundLevel" )
names(mallData)
unique(mallData$AIS)
stAIS = as.Date( min(gpsAIS$UTC) )
edAIS = as.Date( max(gpsAIS$UTC) )
#proportion of samples
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
# save: AIS spectra ####
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AISNoise.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)

# 125Hz DISTRIBUTION ####
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
## save: AIS histogram ####
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AIShist.jpg"), plot = pais2, width = 10, height = 10, dpi = 300)

## table results ####
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
## save: table ####
ggsave(paste0(outDirG, "table_", site, "_AIShist.jpg"), combined_grob, width = 8, height = 5)


# 125Hz TIME SERIES ####
# each day median level for each category....
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
  geom_rect(data = TOIs %>% filter(yr %in% unique(gpsAIS$yr)), 
            inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date , ymin = -Inf, ymax = Inf), 
            alpha = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
pais3
## save: AIS above time series ####
ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_AISTimeSeries.jpg"), plot = pais3, width = 12, height = 8, dpi = 300)
## table results ####
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
title_grob <- textGrob( paste0("Monthly average difference in soundscape when vessel nearby \n (", toupper(site), ")"),
                        gp = gpar(fontsize = 16, fontface = "bold"))
table_grob <- tableGrob(as.data.frame( medians_wide) )
combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
## save: table ####
ggsave(paste0(outDirG, "table_", site, "_AISabove.jpg"), combined_grob, width = 8, height = 5)


