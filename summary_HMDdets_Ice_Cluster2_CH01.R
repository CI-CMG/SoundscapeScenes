# Combine HMDdets with clusters to compare results across acoustic scene analyses
# purpose is to review performance of clustering with scene categories
# modified from summary_HMDdets_Wind_RRPCA_Cluster2.R 
# calculates a concentration score to see how clustering worked

rm(list=ls()) 

# INPUT: specific detections + wind + cluster analysis

# UP NEXT ####
# extract PSD percentile values for each acoustic scene category
# add RRPCA results
# export graphcs to review

# LIBRARIES ####
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(ggmosaic)
library(formattable)
library(tidyr)
library(dplyr)
library (DT)
library(formattable)
library(htmltools)
library(htmlwidgets)
library(scales)
library(gridExtra)

# PARAMS ####
siteN = "AU_CH01"
ver = "v2"

gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
hmddrive = "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\"
cldrive = "\\SoundCoop_AcousticScene\\ClusterAnalysis\\C_outputCC\\"
outdrive = paste0(gdrive, cldrive, siteN,"\\", ver)

iter = 2 # iteration file from clustering
category_colors <- c("#26828EFF", "#440154FF", "#2DB27DFF", "#B4DE2CFF")

# FUNCTIONS ####
# Function to create a color gradient based on min and max values in a row
color_gradient_row <- function(row_values) {
  normalized <- rescale(as.numeric(row_values))  # Normalize values to [0, 1] range
  colors <- colorRampPalette(c("white", "lightyellow", "yellow", "orange"))(100)
  colors[round(normalized * 99) + 1]
}
# Function to preserve custom HTML formatting
row_formatter_html <- function(df) {
  for (i in seq_len(nrow(df))) {
    row_colors <- color_gradient_row(df[i, -1])  # Exclude the first column (Name)
    for (j in seq_along(row_colors)) {
      df[i, j + 1] <- htmltools::HTML(sprintf('<span style="display: block; padding: 5px; border-radius: 4px; background-color: %s;">%s</span>',
                                              row_colors[j], df[i, j + 1]))
    }
  }
  df
}
# Function to create a color gradient based on min and max values in a column
# not working....
# Function to calculate entropy for each row in a data frame
# Function to calculate entropy with emphasis on diversity
# Function to calculate evenness score for each row
calculate_concentration_score <- function(row_values) {
  row_sum <- sum(row_values)  # Calculate the sum of values in the row
  normalized_values <- row_values / row_sum  # Divide each value by the row sum
  max_value <- max(normalized_values)  # Find the maximum value
  max_value_rounded <- round(max_value, digits = 2)  # Round the maximum value to the nearest tenth
  return(max_value_rounded)
}

# HMD+ ####
dirIn =  paste0(gdrive, hmddrive, siteN)  #list.files(dirIn)
load(paste0(dirIn,"\\", siteN, "_HmdDetsAS"))
cat("HMD data: ", as.character(min(AS$dateTime)),  as.character(max(AS$dateTime)) )
# ASall = AS # to save time re-loading
AS$yr = year(AS$dateTime)
AS$dy = as.Date(AS$dateTime)

## FILL in season dates ####
useason = unique(AS$season)
AS$season2 = "unk"
for (ss in 2:length(useason)) {
  # all unique days in specific season
  dates_to_match = unique( AS$dy[AS$season == useason[ss] ] ) 
  AS$season2[AS$dy %in% dates_to_match] = useason[ss]
  cat("HMD Dates for ", useason[ss],"days = ", length(dates_to_match), as.character( min(dates_to_match)), 
      as.character( max(dates_to_match)),"\n" )
} # unique(AS$season2)

## LF AcousticScenes #### 
# idx = which(AS$AnyBaleen >= 1 | AS$AnyPinn >= 1)
# AS$BioLF = 0
# AS$BioLF[idx] = 1
# AS$Category2 = "ambient"
# AS$Category2[AS$Ant > 0  & AS$BioLF > 0] = "bioLFAnthro"
# AS$Category2[AS$Ant > 0  & AS$BioLF == 0] = "anthro"
# AS$Category2[AS$Ant == 0 & AS$BioLF > 0 ] = "bioLF"
# unique(AS$Category2)

## EXPANDED AcousticScenes #### 
AS$Category2 = "UNK"

idx = which(AS$AnyBaleen >= 1 | AS$AnyPinn >= 1)
AS$BioLF = 0
AS$BioLF[idx] = 1

idx = which(AS$Nothing < 1 & AS$AnyPinn == 0 & AS$AnyBaleen == 0 & AS$AnyVess == 0) # this is truely nothing!
# tmp = AS[idx, 999:1018]
AS$Category2[idx] =  "1_Ambient"

idx = which(AS$NothingIceOK > 0 & AS$AnyPinn == 0 & AS$AnyBaleen == 0 & AS$AnyVess == 0) # just ice sounds
# AS[idx, 999:1018]
AS$Category2[idx] =  "2_AmbientIce"
# check for bio = which(AS$Category2 ==  "2_AmbientIce" & AS$AnyBio > 1) # just ice sounds

idx = which( AS$BioLF == 0 & AS$AnyVess >= 1 ) 
# tmp = AS[idx, 999:1018]
AS$Category2[idx] =  "3_Vess"

idx = which( AS$BioLF >= 1 &  AS$AnyVess >= 1) #bio + vess
# AS[idx, 999:1018]
AS$Category2[idx ] =  "4_VessBio"

idx = which( AS$AnyVess == 0  & AS$AnyPinn >= 1 & AS$AnyBaleen == 0) #just seals
# AS[idx, 999:1018]
AS$Category2[idx ] =  "5_Pinn"

idx = which( AS$AnyBaleen >= 1 & AS$AnyVess == 0 & AS$AnyPinn == 0) #just baleen
# AS[idx, 999:1018]
AS$Category2[idx ] =  "6_Baleen"

idx = which( AS$AnyBaleen >= 1 & AS$AnyPinn >= 1 &  AS$AnyVess ==0 ) #seals+whales
# AS[idx, 999:1018]
AS$Category2[idx ] =  "7_BaleenPinn"
unique(AS$Category2)

#jsut some checking...
tmp  = AS[AS$Category2 == "7_BaleenPinn", 999:1018]
unique( tmp$NothingIceOK )

custom_colors <- c(
  "1_Ambient" = "lightgray",
  "2_AmbientIce" = "darkgray",
  "3_Vess" = "darksalmon",
  "4_VessBio" = "lightsalmon",
  "5_Pinn" = "turquoise",
  "6_Baleen" = "darkturquoise",
  "7_BaleenPinn" = "cyan"
)

## AGGERGRATE data for pie chart by season ####
tal = as.data.frame( AS %>% group_by(season2, Category2) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
data = as.data.frame( tal )
agg_data <- data %>%
  group_by(season2, Category2) %>%
  summarize(PerTime = sum(PerTime))
# Calculate total time in days for each season
total_time <- data %>%
  group_by(season2) %>%
  summarize(total_n = sum(n))
total_time$total_days <- total_time$total_n / (60 * 24)
# Create pie charts for each season with total time in days in the title
p1 <- ggplot(agg_data %>% filter(season2 == "break"), aes(x = "", y = PerTime, fill = Category2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
  labs(title = paste("Break Season - Total Time:", round(total_time$total_days[total_time$season2 == "break"], 2), "days"),
       fill = "Category")
p2 <- ggplot(agg_data %>% filter(season2 == "form"), aes(x = "", y = PerTime, fill = Category2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
  labs(title = paste("Form Season - Total Time:", round(total_time$total_days[total_time$season2 == "form"], 2), "days"),
       fill = "Category")
p3 <- ggplot(agg_data %>% filter(season2 == "ice"), aes(x = "", y = PerTime, fill = Category2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
  labs(title = paste("Ice Season - Total Time:", round(total_time$total_days[total_time$season2 == "ice"], 2), "days"),
       fill = "Category")
p4 <- ggplot(agg_data %>% filter(season2 == "open"), aes(x = "", y = PerTime, fill = Category2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
  labs(title = paste("Open Season - Total Time:", round(total_time$total_days[total_time$season2 == "open"], 2), "days"),
       fill = "Category")
grid.arrange(p1, p2, p3, p4, ncol = 2)

# CLUSTERS ####
inDirCC = outdrive
inFilesCC = list.files( inDirCC, pattern = paste0("^", siteN, "_.*_Bouts.csv$"), full.names = T)
clustAll = NULL
for (f in 1:length(inFilesCC )) {
  clust = read.csv(inFilesCC[f])
  clust$dateTime <- as.POSIXct(  clust$StartTime,   format = "%d-%b-%Y %H:%M:%S", tz = "GMT") 
  clust$season = sapply(strsplit(basename(inFilesCC[f]), "_"), "[[", 3)
  
  cat("Cluster data: ",  unique(clust$season), as.character(min(clust$dateTime)),  as.character(max(clust$dateTime)),"\n" )
  
  clusttmp = select(clust, c(season, dateTime, ClusterIDNumber) )
  clustAll = rbind(clustAll, clusttmp)
}
clustAll$ucluster = paste( clustAll$ClusterIDNumber, clustAll$season, sep = "_")
tal = as.data.frame( clustAll %>% group_by(season) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
tal # not all minutes with clusters because % isolated and downsample

# MERGE ####
merged_df = merge(AS, clustAll[, c("dateTime", "ucluster","ClusterIDNumber")], by = "dateTime", all.x = TRUE)
clusts = as.data.frame( unique(merged_df$ucluster) )
tal = as.data.frame( merged_df %>% group_by(season2, ucluster) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
tal

# CHECKING CATEGORIES ####
i =2 #season
uSeason = unique( merged_df$season2)
uSeason[i]
tmp = as.data.frame( merged_df[ merged_df$season == uSeason[i], ])
#bar chart plot
ggplot(data = tmp, aes(fill = Category2, x = ucluster)) +
  geom_bar(position="fill") +
  ggtitle( uSeason[i]) +
  xlab("cluster") + ylab("") +
  scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
  theme_minimal()

# look at individual cluster results to see what species...
c = 15 #cluster number
#unique( as.numeric(tmp$ClusterIDNumber) )
tmp[ !is.na(tmp$ClusterIDNumber) & tmp$ClusterIDNumber  == c, 999:1018]

# RESULTS ####
# find which clusters have different AS categories
# report by season

clusterScores = NULL
for (i in 1:length(uSeason)) { # 
  #i = 4
  tmp = merged_df[ merged_df$season == uSeason[i], ]
  

  #bar chart plot
    ggplot(data = tmp, aes(fill = Category2, x = ucluster)) +
      geom_bar(position="fill") +
      ggtitle( uSeason[i]) +
      xlab("cluster") + ylab("") +
      scale_fill_manual(name = "Acoustic Scene", values = custom_colors )+
      theme_minimal()
  
  #mosaic plot
  ggplot(data = tmp) +
    geom_mosaic(aes(x=product(ucluster), fill = Category2)) +
    ggtitle( uSeason[i])
  
  tmpTable = as.data.frame ( table( tmp$ClusterIDNumber, tmp$Category2, tmp$season) )
  Nsamples = sum(tmpTable$Freq)
  
  result_wide = pivot_wider(tmpTable, names_from = Var2, values_from = Freq)
  colnames( result_wide )[1] = paste0("cluster_",uSeason[i])
  colnames( result_wide )[2] = "season"
  
  data = as.data.frame(result_wide)
  data <- data[, -which(names(data) == "season")]
  data$concentration_score <- apply(data[, -1], 1, calculate_concentration_score)
  ed = ncol(data)-1
  data$total = rowSums(data[,2:ed])
  samplsInUnique = sum(data$total[ which(data$concentration_score ==1 ) ] )/ sum(data$total)
  # colSums(data[,2:ed])
  
  clusterScores = rbind(clusterScores, c( uSeason[i], nrow(data),Nsamples,
    sum( data$concentration_score * data$total )/ sum(data$total), samplsInUnique, ver) )
 
  #apply the custom formatter with HTML- which category has most samples in cluster?
  formatted_data_html <- row_formatter_html(data)
  formatted_table_html <- as.htmlwidget(formattable(formatted_data_html))
  formatted_table_html
  outname =  paste0(outdrive, "\\", ver, "_", siteN, "_", uSeason[i], ".html")
  saveWidget(formatted_table_html, outname)
  rm(formatted_table_html)

}
clusterScores


# OCEAN SCIENCES Poster ####
talSSD = NULL
AS = NULL

for (f in 1: length(inFiles) ) {# f = 1 # one site at a time
  load( inFiles[f]) # head(HMDdet)
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 3) #site name
  dp =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 4) #site name
  # min(HMDdet$dateTime) max(HMDdet$dateTime)
  
  ##  WIND ####
  # only SB03
  inDirW = paste0(inDir,  "\\wind" )
  inFilesW = list.files( inDirW, pattern = "env", full.names = T)
  inFilesW = inFilesW[grepl(st, inFilesW)] #remove median spectra files
  if (length(inFilesW) >0 ) {
    WINDdata = read.csv(inFilesW)
    #head(WINDdata)
    WINDdata$time = gsub("[+]00:00", "", WINDdata$time )
    WINDdata$dateTime = as.POSIXct( WINDdata$time, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
    WINDdata$yr = year(WINDdata$dateTime)
    min(WINDdata$dateTime)
    max(WINDdata$dateTime)
    # WINDdata = WINDdata[WINDdata$yr == yoi,]
    ixd = which(!is.na(WINDdata$wind_speed))
    WINDdata = WINDdata[ixd, ]
    # min(WINDdata$dateTime)
    # max(WINDdata$dateTime)
    
    HMDdet$wind_speed = NA
    #HMDdet$sea_water_temperature = NA
    #HMDdet$sea_surface_wave_significant_height = NA
    for (ww in 1:nrow(WINDdata) ){ # ww = 2164 
      idx = which (HMDdet$dateTime >= WINDdata$dateTime[ww] & HMDdet$dateTime < (WINDdata$dateTime[ww] + (60*60) ) )  
      # if(length(idx) > 0) { cat(ww, "\n")}
      # HMDdet$dateTime[idx]
      
      HMDdet$wind_speed[idx] =  WINDdata$wind_speed[ww] 
      #HMDdet$sea_water_temperature[idx] =  WINDdata$sea_water_temperature[ww] 
      #HMDdet$sea_surface_wave_significant_height[idx] =  WINDdata$sea_surface_wave_significant_height[ww] 
    }
  } # unique(HMDdet$wind_speed)
  idxW = which( HMDdet$wind_speed > 10)
  HMDdet$windCat = "low"
  HMDdet$windCat[idxW] = "high"
  
  HMDdet0 = HMDdet # for testing- HMDdet = HMDdet0
  
  ## CLUSTER RESULTS
  inDirCC = paste0("F:\\SoundCoop\\ClusterAnalysis\\outputCF" )
  inFilesCC = list.files( inDirCC, pattern = "cluster", full.names = T)
  clust = read.csv(inFilesCC)
  colnames(clust) = c("year","month","day","hour","minute","second","CC")
  clust$dateTime <- as.POSIXct(   paste(clust$year, clust$month, clust$day,
                                        clust$hour, clust$minute, clust$second),   format = "%Y %m %d %H %M %S", tz = "GMT") 
  clust = dplyr::arrange(clust, dateTime)
  
  merged_df = merge(HMDdet, clust[, c("dateTime", "CC")], by = "dateTime", all.x = TRUE)
  unique(merged_df$CC)
  # some rows with NA when merged... why would we not have cluster results for some of the minutes???
  # merged_df$dateTime[222] ... it is missing
  #truncate cluster to only time in the current deployment
  #idx = which (clust$dateTime >= min(HMDdet$dateTime)  & clust$dateTime <= max(HMDdet$dateTime) )  
  #clustT = clust[idx,]
  
  ## ACOUSTIC SCENES ####
  ### SSD: Specific detectors + wind ####
  HMDdet = merged_df
  HMDdet$SSD = paste(HMDdet$windCat, HMDdet$Category, sep = "_" )
  idNa = which ( is.na(HMDdet$wind_speed) )
  HMDdet$SSD[idNa] = "Unk"
  HMDdet$mth = month(HMDdet$dateTime)
  
  tal = as.data.frame( HMDdet %>% group_by(SSD, mth) %>% tally() )
  tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
  tal$Site = paste(st,dp,sep = "_" )
  # tal # copied to google sheets to make a donut plot
  # https://docs.google.com/spreadsheets/d/11mHgePBe4sNZDqKprJI9KkcAXtcGO1Hxn77MnvbgHqg/edit#gid=1366513411
  talSSD = rbind(talSSD, tal)
  
  ### RRPCA ####
  # + lowrank sum above 25th percentile of all values
  thrSP = round( quantile (HMDdet$Sparce, .75, na.rm = T )) # expect sparce to be higher
  thrLR = round( quantile (HMDdet$LowRanK, .25,na.rm = T )) # but low rank not as much because wind speed might be driving the super high values
  
  # without considering conditions
  indx = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP  ) )
  HMDdet$RRPCA1 = "Ambient"
  HMDdet$RRPCA1[indx] = "Transient"
  
  # considering SSD conditions
  HMDdet$RRPCA = HMDdet$Category 
  idxAnth = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Anthro") )
  HMDdet$RRPCA[idxAnth] = "Anthro+"
  idxAmb = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Ambient") )
  HMDdet$RRPCA[idxAmb] = "Ambient+"
  idxBio = ( which( HMDdet$LowRanK > thrLR & HMDdet$Sparce > thrSP & HMDdet$Category == "Bio") )
  HMDdet$RRPCA[idxBio] = "Bio+"
  unique( HMDdet$RRPCA )
  HMDdet$RRPCAw = paste(HMDdet$windCat, HMDdet$RRPCA, sep = "_" )
  
  save(HMDdet, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st, "_", dp, ".Rda") )
  
  ## PLOT: mosaic plot ####
  ggplot(data = HMDdet) +
    geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
    theme_minimal()
  
  HMDdet$clust = as.character( HMDdet$CC )
  idNa = which ( is.na(HMDdet$clust) )
  HMDdet$clust[idNa] = "Unk"
  
  ggplot(HMDdet, aes(fill = RRPCAw, x = clust)) +
    geom_bar(position="fill") +
    theme_minimal()
  
  ggplot(data = HMDdet) +
    geom_mosaic(aes(x=product(clust), fill = RRPCAw)) + 
    theme_minimal()
  
  ## COMBINE DATA- just scenes
  AS = rbind ( AS, select(HMDdet, c('dateTime','SSD','windCat', 'RRPCA', 'RRPCAw','clust','wind_speed') ) )
  
}

write.csv(talSSD, file = paste0(inDir, "\\SSDsummary_", st,".csv") )
write.csv(AS, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st,".csv") )
save(AS, file = paste0(inDir, "\\HMD_DetsWindRpcaCluster_", st,".Rda") )

# FULL YEAR PLOTS ####
##SSD for pie chart--- samples over multiple months
month_talSSD = talSSD %>%
  group_by(mth, SSD) %>%
  summarize(Sum_Value = sum(n))

total_talSSD = talSSD %>%
  group_by(SSD) %>%
  summarize(Sum_Value = sum(n))
write.csv(month_talSSD, file = paste0(inDir, "\\SSDsummary_", st,".csv") )

sum(total_talSSD$Sum_Value)/(60*24)
# plot in google sheets

##SSD vs RRPCA
ggplot(data = AS) +
  geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
  theme_minimal()

AS$mth = month(AS$dateTime)
ggplot(data = AS[AS$mth==7,]) +
  geom_mosaic(aes(x=product(SSD), fill = RRPCAw)) + 
  theme_minimal()

##RRPCAw vs cluster
unique( AS$clust )
#remove NAs-- too big
length( which(AS$clust=="Unk" ) )/nrow(AS)
idx = ( which(AS$clust!="Unk" ) )
ASt = AS[idx,]
ggplot(data = ASt) +
  geom_mosaic(aes(x=product(clust), fill = RRPCAw)) + 
  theme_minimal()

ggplot(ASt, aes(fill = RRPCAw, x = clust)) +
  geom_bar(position="fill") +
  theme_minimal()

ggplot(ASt[ASt$mth==7,], aes(fill = RRPCAw, x = clust)) +
  geom_bar(position="fill") +
  theme_minimal()



# READ IN outputs
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
total_talSSD = read.csv(paste0(inDir,"\\SSDsummary_SB03.csv"))
#summarize within category
total_talSSD %>%
  group_by(SSD) %>%
  summarize(Sum_Value = sum(Sum_Value))


# REMOVED FROM THIS ANALYSIS
## PLOT: acoustic Scene over time ####
HMDdet = HMDdet %>%  mutate(test = case_when(SSD == lag(SSD) ~ "No", TRUE ~ "Yes"))
idx = which(HMDdet$test == "Yes") #start of new scene
AS = NULL
AS$Start    = HMDdet$dateTime[idx]
AS$Category = HMDdet$SSD[idx]
AS$End      = c(  HMDdet$dateTime[ idx[2:length(idx) ] - 1 ] , HMDdet$dateTime[nrow(HMDdet)]  )
AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
AS$mth = month(AS$Start)
AS = as.data.frame (AS)

p1 = ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=10) +
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 10, colour="black"),
          axis.text.x = element_text(size = 10, colour="black"),
          plot.caption = element_text(size = 8) )
p1

#ggsave(p1, file = paste0(inDir, "\\AcousticScene_",fqr, "_" , st,".png"), width = 1500, height = 700, units = "px")

# HMD values for each scene ####
## FOR no wind sites ####
endCol = ncol(HMDdet) - 15 # 
colnames(HMDdet)[endCol]
colnames(HMDdet)[2]
FQlow = as.numeric( as.character( gsub("X","", colnames(HMDdet)[2:endCol]) ) )

unique(HMDdet$Category3)
HMDdet$Category4[HMDdet$Category3 == "low_Ambient_Ambient"] = "Low wind"
HMDdet$Category4[HMDdet$Category3 == "low_Ambient_Ambient+"] = "Low transient"
HMDdet$Category4[HMDdet$Category3 == "Unk"] = "Unknown"
HMDdet$Category4[HMDdet$Category3 == "high_Ambient_Ambient+"] = "High transient"
HMDdet$Category4[HMDdet$Category3 == "high_Ambient_Ambient"] = "High wind"
HMDdet$Category4[HMDdet$Category3 == "low_Anthro_Anthro"] = "Low vessels"
HMDdet$Category4[HMDdet$Category3 == "low_Bio+Anthro_Bio+Anthro"] = "Low vessesl + transient"
HMDdet$Category4[HMDdet$Category3 == "high_Bio+Anthro_Bio+Anthro"] = "High vessesl + transient"
HMDdet$Category4[HMDdet$Category3 == "high_Anthro_Anthro"] = "High vessels"

unique(HMDdet$Category4)
# wind conditions
LWi <- HMDdet %>% filter(Category4 =="Low wind")
LWiM = LWi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LWiM$LRfq = as.numeric( as.character( gsub("X","", LWiM$key ) ) ) 

HWi <- HMDdet %>% filter(Category4 =="High wind")
HWiM = HWi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
HWiM$LRfq = as.numeric( as.character( gsub("X","", HWiM$key ) ) ) 
#transient conditions
LTr <- HMDdet %>% filter(Category4 =="Low transient")
LTrM = LTr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LTrM$LRfq = as.numeric( as.character( gsub("X","", LTrM$key ) ) ) 

HTr <- HMDdet %>% filter(Category4 =="High transient")
HTrM = HTr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
HTrM$LRfq = as.numeric( as.character( gsub("X","", HTrM$key ) ) ) 

LTV <- HMDdet %>% filter(Category4 =="Low vessesl + transient")
LTVM = LTV %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LTVM$LRfq = as.numeric( as.character( gsub("X","", LTVM$key ) ) ) 

LVe <- HMDdet %>% filter(Category4 =="Low vessels")
LVeM = LVe %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
LVeM$LRfq = as.numeric( as.character( gsub("X","", LVeM$key ) ) ) 


# START HERE ####
ggplot(LWiM, aes(x=LRfq , y=med.x) )  +
  #wind
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  geom_line(data = HWiM, aes(x=LRfq , y=med.x), linewidth = 2, color = "black") +
  #transient
  geom_line(data = LTrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "lightblue") +
  geom_line(data = HTrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "royalblue") +
  #vessels
  geom_line(data = LVeM, aes(x=LRfq , y=med.x), linewidth = 2, color = "indianred1") +
  #vessel + transients
  geom_line(data = LTVM, aes(x=LRfq , y=med.x), linewidth = 2, color = "tan1") +
  
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,95)) +
  theme_bw()+ 
  ggtitle(paste0( "Acoustic Scene | ", st) ) +
  theme(legend.position="top")+
  theme(text = element_text(size =16) )

#ggplotly(p1)

## FOR no wind sites ####
endCol = ncol(HMDdet) - 12 # 
colnames(HMDdet)[endCol]
colnames(HMDdet)[2]
FQlow = as.numeric( as.character( gsub("X","", colnames(HMDdet)[2:endCol]) ) )
HMDdet$Category4[HMDdet$Category3 == "Bio+Anthro_Bio+Anthro"] = "Vessels + Transient"
HMDdet$Category4[HMDdet$Category3 == "Anthro_Anthro"] = "Vessels"
HMDdet$Category4[HMDdet$Category3 == "Ambient_Ambient+"] = "Transient"
HMDdet$Category4[HMDdet$Category3 == "Ambient_Ambient"] = "Wind"
ucats = unique(HMDdet$Category4)

VT <- HMDdet %>% filter(Category4 =="Vessels + Transient")
VTM = VT %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
VTM$LRfq = as.numeric( as.character( gsub("X","", VTM$key ) ) ) 

Ve <- HMDdet %>% filter(Category4 =="Vessels")
VeM = Ve %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
VeM$LRfq = as.numeric( as.character( gsub("X","", VeM$key ) ) ) 

Tr <- HMDdet %>% filter(Category4 =="Transient")
TrM = Tr %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
TrM$LRfq = as.numeric( as.character( gsub("X","", TrM$key ) ) ) 

Wi <- HMDdet %>% filter(Category4 =="Wind")
WiM = Wi %>% gather(key, value, 2:endCol) %>% group_by(key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                   med.x  = quantile(value, probs = 0.5),
                   upper.x = quantile(value, probs = 0.75))
WiM$LRfq = as.numeric( as.character( gsub("X","", WiM$key ) ) ) 

ggplot(WiM, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  
  geom_line(data = TrM, aes(x=LRfq , y=med.x), linewidth = 2, color = "royalblue") +
  #geom_line(data = TrM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "blue" , linetype="dotted") + 
  #geom_line(data = TrM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "blue", linetype="dotted") +
  
  geom_line(data = VeM, aes(x=LRfq , y=med.x), linewidth = 2, color = "red3") +
  #geom_line(data = VeM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "red" , linetype="dotted") + 
  #geom_line(data = VeM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "red", linetype="dotted") +
  
  geom_line(data = VTM, aes(x=LRfq , y=med.x), linewidth = 2, color = "darkorange") +
  #geom_line(data = VTM, aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "orange" , linetype="dotted") + 
  #geom_line(data = VTM, aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "orange", linetype="dotted")+

  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(60,90)) +
  theme_bw()+ 
  ggtitle(paste0( "Acoustic Scene | ", st) ) +
  theme(legend.position="top")+
  theme(text = element_text(size =16) )

#ggplotly(p1)


# not used ####
Ambient = rbind (Ambient, tst[tst$Category == "Ambient",] )
Anthro = rbind (Anthro, tst[tst$Category == "Anthro",] )
Bio = rbind (Bio, tst[tst$Category == "Bio",] )
BioAnthro = rbind (BioAnthro, tst[tst$Category == "Bio+Anthro",] )

totalMinsAmb = totalMinsAmb + nrow(tst[tst$Category == "Ambient",])
totalMinsBio = totalMinsBio + nrow(tst[tst$Category == "Bio",])
totalMinsBoth = totalMinsBoth + nrow(tst[tst$Category == "Bio+Anthro",])
totalMinsAnthro= totalMinsAnthro + nrow(tst[tst$Category == "Anthro",])

# START HERE ####

p2 = ggplot(Anthro, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Anthropogenic sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_Anthro_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")


p2 = ggplot(Bio, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Biological sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_Bio_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")


p2 = ggplot(BioAnthro, aes(x=Fq , y=mean.x, color = Site) )  +
  geom_line( size = 1) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  ylim(c(50,100)) +
  theme_bw()+ 
  ggtitle(paste0( "Biological+Anthropogenic sources (only low-frequency)")) +
  theme(legend.position="top")+
  theme(text = element_text(size =10) )
p2
ggsave(p2, file = paste0(inDir, "\\ASspectra_BioAnthro_",fqr, "_", st,".png"), width = 1500, height = 1000, units = "px")

