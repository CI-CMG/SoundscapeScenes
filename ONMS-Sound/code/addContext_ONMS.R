#ADD context metadata to hourly TOLs and creates report graphics

#INPUTS: output of HrTOLs_ONMS.R, loads the most recent file; ONMS metadata; wind Model
# works for each monitoring site

# CHECK: Exceedence 100 HZ values by season

# LIBRARIES ####
rm(list=ls()) 
options(java.parameters = "-Xmx2g")
library(PAMscapes)
library(scales)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(reshape)
library(gtable)
library(grid)

# SET UP ####
##  sites ####
ONMSsites = c("sb01", "sb03","mb01","mb02", "pm01","oc02", "cb11")
## directories ####
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context
outDirG = paste0( outDir,"report\\" ) #graphics
## parameters ####
DC = Sys.Date()
project = "ONMS"
fqIn = "TOL_125" 
ab = 70 # threshold for above frequency in
fqIn2 = "TOL_100" # no wind model for 125 Hz- ugh!!!
fqIn2name = "100 Hz"
ab2 = 5
windUpp = 22.6 #which wind model result to show on plot
windLow = 1
windH = 10 #measured wind speeds
windL = 5
## context ####
metaFile = paste0(outDirC,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( xlsx::read.xlsx(metaFile, sheetName  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
## times of interest ####
TOI = as.data.frame ( xlsx::read.xlsx(metaFile, sheetName = "Time period of interest") )
TOI = TOI[!apply(TOI, 1, function(row) all(is.na(row))), ]
## frequency of interest ####
FOI = as.data.frame ( xlsx::read.xlsx(metaFile, sheetName = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
## tol conversion ####
TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)
## wind model ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!

#PROCESS BY SITE ####
for (uu in 1:length(ONMSsites)) {
  
  cat("Processing... ", ONMSsites[uu],"\n" )
  site =  ONMSsites[uu]
  
  if (site == "cb11") {
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" ) #products
    site1 = "NRS11"
    site3 = "cbnrs11"
  } else {
    site1 = site
    site3 = site
    outDirP = paste0( outDir,"products\\", substr(tolower(site), start = 1, stop =2),"\\" )#products
  }

  ## metadata ####
  siteInfo = lookup[lookup$`NCEI ID` == tolower(site1),]
  siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
  ## frequency of interest ####
  FOIs = FOI [ FOI$Sanctuary == substr(site, 1,2), ]
  ## times of interest ####
  TOIs = TOI [ TOI$Site == (site1), ]
  TOIs <- TOIs %>%
    mutate(
      Start_Julian = as.numeric(format(as.Date(start_date), "%j")),
      End_Julian = as.numeric(format(as.Date(end_date), "%j")),
      Mid_Julian = (Start_Julian + End_Julian) / 2  # Midpoint for annotation
    )
  TOIs$yr = TOIs$Year
  ## seasonality ####
  sidx = siteInfo$Seasonality
  #put in alphetical order so plots line up!!!
  if ( length(sidx) == 0 ) {
    season = data.frame(
      Season = c("Fall", "Spring",  "Summer", "Winter"  ),
      Months = c("10,11,12", "4,5,6","7,8,9", "1,2,3"   ) ,
      values = c(   "#E69F00",  "#009E73", "#CC79A7", "#56B4E9") )
  }else if  ( sidx == "other") {
    season = data.frame(
      Season = c("Early", "Peak", "Late", "Non"),
      Months = c("10,11,12", "1,2,3", "4,5,6", "7,8,9") ,
      values = c(  "#56B4E9",  "#009E73","#CC79A7", "#E69F00") )
  }else if  ( sidx == "upwelling") {
    season = data.frame(
      Season = c("Post-Upwelling", "Upwelling", "Winter"),
      Months = c("7,8,9,10,11", "3,4,5,6", "12,1,2") ,
      values = c(  "#CC79A7",  "#009E73","#56B4E9") )
  }else {
    season = data.frame(
      Season = c("Fall", "Spring",  "Summer", "Winter"  ),
      Months = c("10,11,12", "4,5,6","7,8,9", "1,2,3"   ) ,
      values = c(   "#E69F00",  "#009E73", "#CC79A7", "#56B4E9") )
  }
 
  ## spl data ####
  # HOURLY TOLs with wind estimate (gps)
  inFile = list.files(outDirP, pattern = paste0("data_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  Fq = as.numeric( as.character( gsub("TOL_", "",  colnames(gps)[grep("TOL", colnames(gps))] ) ))
  
  ## EFFORT- hours/ month-year ####
  summary <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, month)  # Count occurrences (hours) in each year-month
  summary$dy = round(summary$n/ 24)
  ### effort by year-month ####
  p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    labs(
      caption = paste0(toupper(site), " has ", udays, 
                       " unique days: ", as.character(st), " to ", as.character(ed)),
      x = "",
      y = "Days",
      fill = "Year"
    ) +
    scale_x_discrete(labels = month.abb) +  # Show month names instead of numbers
    #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
    scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels for readability
      legend.position = "top"  # Place the legend at the top
    )
  p1
  #ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_Effort.jpg"), plot = p1, width = 10, height = 6, dpi = 300)
  
  ## add SEASON conditions ####
  seas = unique(season$Season)
  for( ss in 1:length(seas) ){
    moi = as.numeric(unlist(strsplit(as.character(season$Months[ss]), ",")))
    gps$Season[gps$mth %in% moi] = season$Season[ss]
  }
  summary2 <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, Season)  # Count occurrences (hours) in each year-month
  ### effort by season ####
  p2 = ggplot(summary2, aes(x = year, y = n, fill = as.factor(Season))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    labs(
      title = "monitoring effort by season",
      caption  = paste0(toupper(site), " has ", udays, 
                        " unique days: ", as.character(st), " to ", as.character(ed)),
      x = "",      y = "Hours",      fill = "Year"
    ) +
    scale_fill_manual(values = season$values) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels for readability
      legend.position = "none"  # Place the legend at the top
    )
  p2
  
  ## add WIND category ####
  gps$wind_category = NA
  gps <- gps %>%
    mutate(wind_category = case_when(
      is.na(windMag) ~ NA_character_,
      windMag < windL ~ "low",
      windMag >= windL & windV <= windH ~ "med",
      windMag > windH ~ "high"
    ))
  # Calculate the counts for wind each category
  category_counts <- gps %>%
    count(wind_category) %>%
    mutate(label = paste(wind_category, ":", n))
  subtitle_text <- paste(category_counts$label, collapse = ", ")
 
  windInfo = windModel[tolower(windModel$si) == site3,]
  widx = which( as.numeric(as.character( (colnames(windInfo)) ) )  == max(Fq) )
  windInfo = windInfo[,1:widx]
  #re-structure for ggplot
  mwindInfo = melt(windInfo, id.vars = c("windSpeed"), measure.vars = colnames(windInfo)[4:ncol(windInfo)])
  mwindInfo$variable = as.numeric( as.character(mwindInfo$variable ))

  ## SEASONAL ANALYSIS ####
  ## wind bar
  gps$wind_category <- factor(gps$wind_category, levels = c("low", "med", "high"))
  # Create the horizontal stacked bar plot with the counts of wind speed categories
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
  
  ### calculate quantiles ####
  tol_columns = grep("TOL", colnames(gps))
  #overall
  allData = as.data.frame ( apply(gps[,tol_columns ], 2, quantile, na.rm = T) ) # all data
  allData$Quantile = rownames((allData))
  rownames(allData) <- NULL
  #by season
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
  
  ### format for plot  ####
  mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
  mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Season", "Frequency" , "SoundLevel" )
  max(gps$windMag, na.rm = T)
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  
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
    scale_color_manual(values = season$values )+
    scale_fill_manual(values  = season$values ) +
    
    # Wind model values
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    scale_x_log10(labels = label_number(),limits = (c(10,fqupper))) +  # Log scale for x-axis
    
    # Add vertical lines at FQstart
    geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    # Add labels at the bottom of each line
    geom_text(data = FOIs, aes(x = FQstart, y = 50, label = Label), angle = 90, vjust = 1, hjust = 0.5, size = 3) +
    
    # Additional aesthetics
    theme_minimal()+
    theme(legend.position = "top",
          plot.title = element_text(size = 16, face = "bold", hjust = 0)) +  # This line removes the legend
    labs(
      title = paste0(toupper(site), " -", 
                     tolower(FOIs$Oceanographic.setting[1]), " monitoring site" ),
      #subtitle = paste0("data summarized from ", st, " to ", ed, "\n vertical lines indicate frequencies for sounds of interest in this soundscape" ),
      caption = paste0("vertical lines indicate frequencies for sounds of interest in this soundscape \n black lines are expected wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s]"), 
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    )
  
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  arranged_plot = grid.arrange(p, separator, p2, heights =c(4, 0.1, 1))
  ### save: plot seasonal spectra ####
  ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_SeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)
  
  ## YEARLY ANALYSIS ####
  if (sidx == "other"){ #only keep peak
    gpsAll = gps
    gps = gps[gps$Season == "Peak",]
    #unique( gps$yr )
  }
  tol_columns = grep("TOL", colnames(gps))
  season_split = split(gps, gps$yr) # Calculate quantiles for each year
  
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
  yearAllbb = yearAll # save the bb measurement
  
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
  # max(gps$windMag, na.rm = T)
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  
  p = ggplot() +
    geom_ribbon(data = mallData %>% 
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year),
                alpha = 0.1) +  # Use alpha for transparency
    
    #median TOL values
    geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year), linewidth = 2) +
    geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
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
    
    # Additional aesthetics
    theme_minimal() +
    # This line removes the legend
    labs(
      title =  paste0(toupper(site), "- ", 
                      tolower(FOIs$Oceanographic.setting[1]), " monitoring site" ),
      #subtitle = paste0( "Data summarized from ", st, " to ", ed),
      caption  = paste0("Vertical lines indicate frequencies for sounds of interest in this soundscape \n",
                        "black lines are modeled wind noise at this depth [", windLow,"m/s & ",windUpp, "m/s] ",
                        "\n Seasonality = ", sidx),
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa/Hz)" ) )
    ) +
    theme(legend.position = "top",
          plot.title = element_text(size = 16, face = "bold", hjust = 0)) 
  p
  
  summary <- gps %>%
    mutate(
      year = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, month)  # Count occurrences (hours) in each year-month
  summary$month
  summary$dy = round(summary$n/ 24)
  
  p1 = ggplot(summary, aes(x = month, y = n, fill = as.factor(year))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    labs(
      title = "monitoring effort by year",
      caption = paste0(toupper(site), " has ", udays, 
                       " unique days: ", as.character(st), " to ", as.character(ed)),
      x = "",
      y = "Days",
      fill = "Year"
    ) +
    scale_x_discrete(labels = month.abb) +  # Show month names instead of numbers
    #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
    scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels for readability
      legend.position = "none"  # Place the legend at the top
    )
  
  p1
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  pYear = grid.arrange(p, separator, p1, heights =c(4, 0.1, 1.5))
  ### save: plot yearly spectra ####
  ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_YearSPL.jpg"), plot = pYear, width = 10, height = 6, dpi = 300)
  
  ## CONVERT ALL DATA TO 1 Hz  ####
  gpsBB = gps
  tol_columns = grep("TOL", colnames(gps))
  #divide unlog, by the bandwidth, and re-log
  for( cc in 1:length(tol_columns)) {
    toltmp = colnames(gps)[tol_columns[cc]]
    bw = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
    dtmp = gps[, tol_columns[cc] ]
    gps[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )
  }
  #gpsBB$TOL_125[1] - gps$TOL_125[1]
  
  ## TIME SERIES - 125 Hz TOL ####
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
  dailyFQ$yr = year(dailyFQ$Date)
  dailyFQ$Julian = yday(dailyFQ$Date)
  #fill in days
  dailyFQ_complete <- dailyFQ %>%
    group_by(yr) %>%
    complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>%  # Fill in missing days
    arrange(yr, Julian) 
  monthly_sequence <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
  month_names_seq <- format(monthly_sequence, "%b")  # Extracts full month names
  days_of_year_for_months <- yday(monthly_sequence)
  ### calculate percentage above threshold ####
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
  if (nrow( TOIs)  > 0) {
    for (ii in 1:nrow(TOIs) ) {
      TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
    }
  }
  
  p0 = ggplot(dailyFQ_complete, aes(x = Julian, y = TOL100_50, group = yr, color = factor(yr))) 
  # Add shaded background layer first
  if (nrow(TOIs) > 0) {
    p0 <-  p0 + geom_rect(
      data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
      inherit.aes = FALSE,
      aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
      fill = "grey", alpha = 0.2  # Optional styling
    )
  }

  p <- p0 + geom_line() +
    
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year)))))  +
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
      title = paste0("Soundscape at 125 Hz" ) ,
      subtitle =  paste0(toupper(site), ", a ", tolower(FOIs$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ),
      caption = '',
      x = "",
      y = expression(paste("Sound Levels (125 Hz dB re 1", mu, " Pa/Hz)" ) ),
      color = "Year"  # Label for the color legend
    ) 
  p
  
  ### save: plot 125 Hz time series ####
  ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_125Hz.jpg"), plot = p, width = 10, height = 10, dpi = 300)
  
  
  ## TIME SERIES - exceedence 100 Hz  ####
  cols_to_select = c("UTC", "windMag","wind_category", "Season", fqIn2)
  gpsFQ = gps %>% select(all_of(cols_to_select))
  #ADD wind speed estimate for each hour of data in frequency of interest
  wspeeds = unique( (windModel$windSpeed) )
  gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  # what is the spl values for that windspeed?
  fqIdx = which( colnames( windInfo) == substr( fqIn2, 5,8)) #'100'
  wsIdx <- match(gpsFQ$closest_windMag, windInfo$windSpeed)
  gpsFQ$WindModel100 <- windInfo[wsIdx, fqIdx]
  #names(gpsFQ)
  gpsFQ$Exceed = gpsFQ$TOL_100 -  gpsFQ$WindModel100 #actual - model
  gpsFQ$yr = year(gpsFQ$UTC)
  
  gpsFQ$Windthres = "unk"
  gpsFQ$Windthres[gpsFQ$Exceed <= ab2] = "below"
  gpsFQ$Windthres[gpsFQ$Exceed > ab2] = "above"
  
  # above 
  #names(gpsFQ)
  rm(seasonalNE)
  seasonalNE = gpsFQ %>%
    mutate(Date = as.Date(UTC)) %>%
    group_by(Season, yr) %>%
    summarise(
      hrs = n(),  # Count of observations
      percent_above = sum(Windthres == "above", na.rm = TRUE) / hrs * 100,
      #Exceed_25 = quantile(Exceed, 0.25, na.rm = TRUE),
      Exceed_50 = quantile(Exceed, 0.50, na.rm = TRUE), # Median
      #Exceed_75 = quantile(Exceed, 0.75, na.rm = TRUE),
      #TOL100_25 = quantile(.data[[fqIn2]], 0.25, na.rm = TRUE),
      TOL100_50 = quantile(.data[[fqIn2]], 0.50, na.rm = TRUE),
      #TOL100_75 = quantile(.data[[fqIn2]], 0.75, na.rm = TRUE),
      windspeed = quantile(windMag, 0.50, na.rm = TRUE)
    )
  
  seasonalNE = as.data.frame( seasonalNE )
  
  # ERROR ####
  seasonalNE = seasonalNE %>%
    mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))
  
  seasonalTime_wide = seasonalNE %>%
    select(Season, yr, percent_above) %>%  
    mutate(percent_above = round(percent_above, 0) ) %>% 
    pivot_wider(names_from = yr, values_from = percent_above) %>%
    arrange(Season) 
  seasonalTime_wide <- seasonalTime_wide %>%
    select(Season, sort(names(seasonalTime_wide)[-1]))  # Skip the Season column and sort the year columns
  
  
  seasonalNE_wide <- seasonalNE %>%
    select(Season, yr, Exceed_50) %>%  
    mutate(Exceed_50 = round(Exceed_50, 1),
           yr = factor(yr, levels = sort(unique(yr))) 
    ) %>%
    pivot_wider(names_from = yr, values_from = Exceed_50 ) %>%
    arrange(Season) 
  seasonalNE_wide <- seasonalNE_wide %>%
    select(Season, sort(names(seasonalNE_wide)[-1]))
  ### save: table dB above ####
  title_grob = textGrob( paste0("% Time Above Wind Noise (", site, ") \n", 
                                "(hourly values ", ab2 , " dB or more above wind model value at ", fqIn2name,")"), 
                         gp = gpar(fontsize = 12, fontface = "bold") , vjust = 1)
  table_grob = tableGrob(as.data.frame( seasonalTime_wide), rows = NULL )
  title_grob2 = textGrob( paste0("Decibels Above Wind Noise \n",
                                 "(median decibels difference from wind model at ", fqIn2name,")"), 
                          gp = gpar(fontsize = 12, fontface = "bold") , vjust = 1)
  table_grob2 = tableGrob(as.data.frame( seasonalNE_wide), rows = NULL )
  combined_grob = arrangeGrob(title_grob, table_grob, title_grob2, table_grob2, ncol = 2)  
  
  ggsave(paste0(outDirG, "table_", site, "_AboveWind.jpg"), combined_grob, width = 10, height = 8)
  
  
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
  
  #names(dailyFQ)
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
  #percentage_above
  #graphic titles
  yrs = unique(dailyFQ_complete$yr)
  dailyFQ_complete$facet_title = NA
  for ( ii in 1:length(yrs ) ) {
    idx = which(dailyFQ_complete$yr == yrs[ii])
    idx2 =  which(percentage_above$year == yrs[ii])
    dailyFQ_complete$facet_title[idx] = paste0(yrs[ii], "- ", round(percentage_above$percentage_above[idx2]),"% above" )
  }
  
  TOIs = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr))
  if(nrow( TOIs ) > 0 ) {
    for (ii in 1:nrow(TOIs) ) {
      TOIs$facet_title[ii] = dailyFQ_complete$facet_title[which(dailyFQ_complete$yr == TOIs$Year[ii])] [1]
    }
  }
  
  pE0 = ggplot(dailyFQ_complete, aes(x = Julian, y = Exceed_50, group = yr, color = factor(yr))) 
    
  if (nrow(TOIs) > 0) {
    pE0 <-  pE0 + geom_rect(
      data = TOIs %>% filter(yr %in% unique(dailyFQ_complete$yr)), 
      inherit.aes = FALSE,
      aes(xmin = Start_Julian, xmax = End_Julian, ymin = -Inf, ymax = Inf), 
      fill = "grey", alpha = 0.2  # Optional styling
    )
  }
  
  pE <- pE0 + geom_line(size = 1, na.rm = T) +
    
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
      title =paste0("Decibels Above Wind Noise" ),
      subtitle =  paste0(toupper(site), ", a ", tolower(FOIs$Oceanographic.setting[1]), " monitoring site \nshaded areas represents ", TOIs$Label[1] ) ,
      caption = paste0("difference between sound level and modeled sound levels based on wind speed at ", fqIn2name, "
                      threshold for % above = ", ab2, "dB"),
      x = "",
      y = expression(paste("Decibels Above Wind Noise" ) ),
      color = "Year"  # Label for the color legend
    ) 
  pE
  ### save: plot NE time series ####
  ggsave(filename = paste0(outDirG, "plot_", tolower(site), "_Exceed100.jpg"), plot = pE, width = 10, height = 10, dpi = 300)
  
  
  ## SAVE UPDATED DATA ####
  save(gps, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs-season_", DC, ".Rda") )
  
  
  
}

# NOT USED ####
## calculate % above VSR ####
# add VSR to hourly data

# gpsFQ$VSR = "no"
# for (tt in 1:nrow( TOIs )){
#   idx = which(gpsFQ$UTC >= TOIs$start_date[tt] & gpsFQ$UTC < TOIs$end_date[tt]+1)
#   cat(year( TOIs$start_date[tt]) , ": ", as.character( gpsFQ$UTC[idx[1]]), "\n")
#   gpsFQ$VSR[idx] = "yes"
#   rm(idx)
# }
# names(gpsFQ)
# 
# # percent above wind noise threshold, summarized by year for all three categories (above, below, NA)
# percentage_above <- gpsFQ %>%
#   mutate(year = year(UTC), 
#          mth = month(UTC, label = TRUE)) %>%  # Extract month name
#   group_by(year, mth, VSR) %>%
#   summarise(
#     total_count = n(),
#     count_above = sum(Exceed >= ab2, na.rm = TRUE),
#     count_below = sum(Exceed < ab2, na.rm = TRUE),
#     count_na = sum(is.na(Exceed)),  # Count NAs
#     .groups = "drop"
#   ) %>%
#   mutate(
#     percentage_above = (count_above / total_count) * 100,
#     percentage_below = (count_below / total_count) * 100,
#     percentage_na = (count_na / total_count) * 100
#   ) %>%
#   select(year, mth, VSR, percentage_above, percentage_below, percentage_na) %>%
#   pivot_longer(cols = c(percentage_above, percentage_below, percentage_na),
#                names_to = "Threshold_Category",
#                values_to = "Percentage")
# 
# #output table Year- reformat
# percentage_aboveT = percentage_above[percentage_above$Threshold_Category == "percentage_above",]
# percentage_aboveT$Percentage = round( percentage_aboveT$Percentage )
# df_wide <- percentage_aboveT %>%
#   pivot_wider(
#     names_from = year,  # The column containing year
#     values_from = Percentage,  # The column containing values (percentage)
#     #names_prefix = "Year_"  # Optional prefix for year columns
#   )
# df_wide <- df_wide %>%
#   select(-Threshold_Category)
# 
# title_grob <- textGrob( paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)"), gp = gpar(fontsize = 16, fontface = "bold"))
# table_grob <- tableGrob(df_wide)
# combined_grob <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.1, 0.9))  # Adjust title height
# 
# # ggsave(paste0(outDirG, "table_", site, "_Exceed.jpg"), combined_grob, width = 8, height = 5)
# percentage_above$Threshold_Category <- factor(percentage_above$Threshold_Category, 
#                                               levels = c("percentage_below", "percentage_above", "percentage_na"))
# ggplot(percentage_above, aes(x = mth, y = Percentage, fill = Threshold_Category,color = VSR)) +
#   geom_bar(stat = "identity", position = "stack") +  # Stacked to show proportion with correct ordering
#   facet_wrap(~year, nrow = 1) +  # One row per year
#   labs(x = "", y = "Percentage", 
#        title = paste0("Percent Time Above Wind Noise Threshold (", ab2, "dB)")) +
#   scale_fill_manual(values = c("percentage_above" = "tomato",  # 'percentage_above' on bottom
#                                "percentage_below" = "gray",
#                                "percentage_na" = "lightgray")) +  # Custom colors
#   scale_x_discrete(breaks = levels(percentage_above$mth)[seq(1, length(levels(percentage_above$mth)), by = 3)]) +  
#   scale_color_manual(values = c("yes" = "black", "no" = NA)) +  # Outline months with VSR
#   # Show every third month
#   theme_minimal()