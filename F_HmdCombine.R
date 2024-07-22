# Creates standard plots for HMD data with detections
# INPUT: output of B_HMDwithDETS.R

# WORKS one site at a time
#AU_CH01

# INPUT
# output of B_HmdDets.R (Rdat)
# output of C_HmdDetsAS.R (Rdat)
# output of C_HmdRrpca.R (Rdat)
# output of C_HmdClust.R (Rdat)

# OUTPUT
# plots
# result summaries

# NEXT
rm(list=ls()) 

# LIBRARIES ####
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggmosaic)
library(viridis)
library(gridExtra)
library(tidyverse)

# PARAMS ####
siteN = "AU_CH01"
LB = "LF" #LF = low frequency
filepat = paste0("_",LB, "_")
DC = Sys.Date()
ver = "v5"

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
# Function to calculate "evenness score" for each row
calculate_concentration_score <- function(row_values) {
  row_sum <- sum(row_values)  # Calculate the sum of values in the row
  normalized_values <- row_values / row_sum  # Divide each value by the row sum
  max_value <- max(normalized_values)  # Find the maximum value
  max_value_rounded <- round(max_value, digits = 2)  # Round the maximum value to the nearest tenth
  return(max_value_rounded)
}

# DIRECTORIES ####
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\", siteN )
inFiles = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
#only keep cluster version of interest
inFilesF = inFiles[ grep(paste0(siteN, "_HmdFeature-", ver), inFiles)]
inFiles2 = inFiles[!grepl("_HmdFeature", inFiles)]
inFiles = c(inFilesF, inFiles2 )
basename(inFiles)

# GET DATA ####
#soundscape metrics
fdx = grep("HmdLabels_", inFiles)
load(inFiles[fdx])#  # output of B_HmdDets.R (Rdat) 
#soundscape categories or acoustic scenes
fdx = grep("HmdScene", inFiles)
load(inFiles[fdx]) # output of C_HmdDetsAS.R (Rdat) 
#soundscape condition
fdx = grep("HmdCondition", inFiles)
load(inFiles[fdx]) #output of C_HmdRrpca.R (Rdat)
#soundscape features
fdx = grep( paste0("HmdFeature-",ver ), inFiles)
load(inFiles[fdx]) #output of D_HmdClust.R (Rdat)

# COMBINE Data ####
df = cbind(HmdDets, HmdAS["Category"], HmdAS["Category2"], 
           outClust["ucluster"], outClust["ClusterIDNumber"], 
           outRrpca["LowRanK"], outRrpca["Sparce"], outRrpca["LRfq"],outRrpca["RRPCA"])
useason = unique(df$season)
ucat = unique(df$Category)
ucat2  = unique(df$Category2)
uclust = unique(df$ClusterIDNumber)
urrpca = unique(df$RRPCA)

numeric_columns = grep("^\\d", names(df) )  
hix = names( df)[numeric_columns]

# SUMMARIZE Data ####
# minutes in each of the analysis groups and types
outSummary = NULL
tmp  = df %>%   group_by(Category) %>%   summarise(n = n())
tmp$Group = "Category"
colnames(tmp) = c("Type","N","Group")
outSummary = rbind(outSummary, tmp)

tmp  = df %>%   group_by(season) %>%   summarise(n = n())
tmp$Group = "season"
colnames(tmp) = c("Type","N","Group")
outSummary = rbind(outSummary, tmp)

tmp  = df %>%   group_by(RRPCA) %>%   summarise(n = n())
tmp$Group = "Condiiton"
colnames(tmp) = c("Type","N","Group")
outSummary = rbind(outSummary, tmp)

tmp  = df %>%   group_by(ClusterIDNumber) %>%   summarise(n = n())
tmp$Group = "Feature"
colnames(tmp) = c("Type","N","Group")
outSummary = rbind(outSummary, tmp)

# EVALUATE soundscape interpretations ####
# do features align with categories?
tmpTable = as.data.frame ( table( df$ClusterIDNumber, df$Category, df$season) )
result_wide = pivot_wider(tmpTable, names_from = Var2, values_from = Freq)
colnames( result_wide )[1] = "feature"
colnames( result_wide )[2] = "season"
data = as.data.frame(result_wide)
data$concentration_score =  apply(data[, 3:6], 1, calculate_concentration_score)
ed = ncol(data)-1
data$total = rowSums(data[,3:ed])
#samplsInUnique = sum(data$total[ which(data$concentration_score == 1 ) ] )/ sum(data$total)
data$ver = ver
featureResults = data

# do conditions align with categories?
tmpTable = as.data.frame ( table( df$RRPCA, df$Category, df$season) )
result_wide = pivot_wider(tmpTable, names_from = Var2, values_from = Freq)
colnames( result_wide )[1] = "condition"
colnames( result_wide )[2] = "season"
data = as.data.frame(result_wide)
data$concentration_score =  apply(data[, 3:6], 1, calculate_concentration_score)
ed = ncol(data)-1
data$total = rowSums(data[,3:ed])
#samplsInUnique = sum(data$total[ which(data$concentration_score == 1 ) ] )/ sum(data$total)
conditionResults = data

# how do conditions align with features?
tmpTable = as.data.frame ( table(  df$ClusterIDNumber, df$RRPCA, df$season) )
result_wide = pivot_wider(tmpTable, names_from = Var2, values_from = Freq)
colnames( result_wide )[1] = "feature"
colnames( result_wide )[2] = "season"
data = as.data.frame(result_wide)
ed = ncol(data)
data$concentration_score =  apply(data[, 3:ed], 1, calculate_concentration_score)
ed = ncol(data)-1
data$total = rowSums(data[,3:ed])
#samplsInUnique = sum(data$total[ which(data$concentration_score == 1 ) ] )/ sum(data$total)
conditionFeatureResults = data

# COPY TABLE HERE: https://docs.google.com/spreadsheets/d/16jZ49zHfwljdpuwOzENMWmmy-yqSPA0KTt1jwN7z86I/edit?gid=0#gid=0

# PLOTS ####

numeric_columns = grep("^\\d", names(HmdDets) )  
hix = names(HmdDets)[numeric_columns]
useason = unique(HmdDets$season)
dfT = NULL
for (s in 1:length(useason)) {
  tmpD  = HmdDets %>% filter(season == useason[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$Category = useason[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
ggplot(dfT, aes(x=as.numeric( as.character(key) ) , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min PSD median") + xlab("HMD Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Metrics by Seasons")) +
  theme(text = element_text(size = 16) )


# DATES OF Seasonal TRANSITION
df$Date = as.Date(df$dateTime )
useason = unique(df$season)
season = data.frame(
  Type = character(4),
  beginDate = as.POSIXct(rep("", 4), format = "%Y-%m-%d %H:%M:%S"),  # Specify format
  endDate = as.POSIXct(rep("", 4), format = "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE
)
colnames(season) <- c("Type", "beginDate", "endDate")  # Optionally name the columns
season$Type = as.data.frame( useason )
for (s in 1:length(useason) ){
  season$beginDate[s] =  ( as.POSIXct( min( df$dateTime[df$season == useason[s] ] ) ) )
  season$endDate[s]   =  as.POSIXct( max( df$dateTime[df$season == useason[s]] ) )
}
## COLORS FOR CATEGORIES ####
if (siteN == "AU_CH01") {
  season_colors= c(
    "open" = "#26828EFF",
    "form" = "#440154FF",
    "ice" = "#2DB27DFF",
    "break" = "#B4DE2CFF" )
  category_colors <- c(
    "1_AMBIENT" = "lightgray",
    "2_BIO" = "turquoise",
    "3_ANTHRO" = "darksalmon",
    "4_BIO-ANTHRO" = "lightsalmon" )
  
  category_colors2 <- c(
    "1_Ambient" = "lightgray",
    "2_AmbientIce" = "darkgray",
    "3_Vess" = "darksalmon",
    "4_VessBio" = "lightsalmon",
    "5_Pinn" = "turquoise",
    "6_Baleen" = "darkturquoise",
    "7_BaleenPinn" = "cyan"
  )
  #? why no anthro category ??? #### 
}

## Samples per Soundscape Conditions ####
### mosaic by season, category ####
# How are the data sampled across seasons and categories?
ggplot(data = df) +
  geom_mosaic(aes(x = product(season), fill = Category)) + 
  scale_fill_manual(name = "Soundscape Category", values = category_colors ) +
  ggtitle("How are the data sampled across seasons and soundscape categories", subtitle = 
            paste0( as.Date(min(df$dateTime)), " to ", as.Date(max(df$dateTime))))+
  theme_minimal()

### bar by season, category & cluster ####
nasamp = round( (sum( is.na(df$ClusterIDNumber) ) / nrow(df) ) *100)
ggplot(df, aes(fill = Category, x = ClusterIDNumber)) +
  scale_fill_manual(name = "Soundscape Category", values = category_colors ) +
  geom_bar(position="fill") +
  facet_wrap(~season)+
  xlab(paste0( "cluster-", ver))+
  ggtitle("What soundscape categories fall in each cluster feature", 
          subtitle = paste0(nasamp, "% samples isolated") ) +
  theme_minimal()

### mosaic by season, category & RRPCA ####
ggplot(data = df) +
  geom_mosaic(aes(x = product(RRPCA), fill = Category)) + 
  scale_fill_manual(name = "Soundscape Category", values = category_colors ) +
  facet_wrap(~season)+
  ggtitle("What soundscape categories fall in soundscape conditions")+
  theme_minimal()
df$ClusterIDNumberF = as.factor( df$ClusterIDNumber )

dft = df[df$ClusterIDNumber > 0,]
ggplot(data = dft) +
  geom_mosaic(aes(x = product(Category), fill = ClusterIDNumberF)) + 
  #scale_fill_manual(name = "Soundscape Features", values = category_colors ) +
  #facet_wrap(~season)+
  ggtitle("What soundscape features fall in categories")+
  theme_minimal()

# FOR COGS presentation 2024-07-15 ####
ggplot(data = dft) +
  geom_mosaic(aes(x = product(season), fill = ClusterIDNumberF)) + 
  #scale_fill_manual(name = "Soundscape Features", values = category_colors ) +
  #facet_wrap(~season)+
  ggtitle("What soundscape features fall in seasons")+
  theme_minimal()
as.data.frame( dft %>% group_by(ClusterIDNumberF, Category ) %>% tally() )


### scatter by season, category & RRPCA values ####
ggplot(data = df, aes(x = LowRanK, df$Sparce, color = Category)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_manual(name = "Soundscape Category", values = category_colors ) +
  facet_wrap(~season)+
  ggtitle("How soundscape conditions group by category")+
  theme_minimal()

## Soundscapes over time ####
### tile by category color FQ ####
df = df %>%  mutate(test = case_when(Category == lag(Category) ~ "No", TRUE ~ "Yes"))
idx = which(df$test == "Yes") #start of new scene
AS = NULL
AS$Start    = df$dateTime[idx]
AS$Category = df$Category[idx]
AS$End      = c(  df$dateTime[ idx[2:length(idx) ] - 1 ] , df$dateTime[nrow(df)]  )
AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
AS$mth = month(AS$Start)
AS = as.data.frame (AS)

ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw() + 
  #scale_x_date(limits = c(min(AS$Start), max(AS$End)) ) +
  geom_vline(xintercept = as.numeric(season$endDate), size = 1, linetype = "dashed", color = "gray") +
  #scale_y_discrete(limits = rev(levels(factor(AS$Category))))
  scale_y_discrete(limits=rev) + 
  geom_segment(size=10) +
  xlab("")+  ylab("") + 
    scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 10, colour="black"),
          axis.text.x = element_text(size = 10, colour="black"),
          plot.caption = element_text(size = 8) )

### tile by condition color FQ ####
df = df %>%  mutate(test = case_when(RRPCA == lag(RRPCA) ~ "No", TRUE ~ "Yes"))
idx = which(df$test == "Yes") #start of new scene
AS = NULL
AS$Start    = df$dateTime[idx]
AS$Category = df$RRPCA[idx]
AS$End      = c(  df$dateTime[ idx[2:length(idx) ] - 1 ] , df$dateTime[nrow(df)]  )
AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
AS$mth = month(AS$Start)
AS = as.data.frame (AS)
ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw() + 
  geom_vline(xintercept = as.numeric(season$endDate), size = 1, linetype = "dashed", color = "gray") +
  scale_y_discrete(limits=rev) + 
  geom_segment(size=20) +
  xlab("")+  ylab("") + 
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 10, colour="black"),
          axis.text.x = element_text(size = 10, colour="black"),
          plot.caption = element_text(size = 8) )

### tile by features color FQ ####
df = df %>%  mutate(test = case_when(ClusterIDNumber == lag(ClusterIDNumber) ~ "No", TRUE ~ "Yes"))
idx = which(df$test == "Yes") #start of new scene
AS = NULL
AS$Start    = df$dateTime[idx]
AS$Category = df$ClusterIDNumber[idx]
AS$End      = c(  df$dateTime[ idx[2:length(idx) ] - 1 ] , df$dateTime[nrow(df)]  )
AS$Hours    = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
AS$mth = month(AS$Start)
AS = as.data.frame (AS)
AS = AS[!is.na( AS$Category) , ]
AS$Category = as.factor(AS$Category)
AS = AS [as.numeric( as.character( AS$Category ))> 0,]
ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw() + 
  geom_vline(xintercept = as.numeric(season$endDate), size = 1, linetype = "dashed", color = "gray") +
  scale_y_discrete(limits=rev) + 
  geom_segment(size=20) +
  xlab("")+  ylab("") + 
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 10, colour="black"),
          axis.text.x = element_text(size = 10, colour="black"),
          plot.caption = element_text(size = 8) )


## SPECTRA ####
#What are the spectral properties of each category
### PERCENTILE SPECTRA ####
#### by season ####
dfT = NULL
for (s in 1:length(useason)) {
  tmpD  = df %>% filter(season == useason[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$LRfq = as.numeric( as.character( gsub("X","", tmpP$key ) ) ) 
  tmpP$Category = useason[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
ggplot(dfT, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  #geom_line(data = dfT, aes(x=LRfq , y=med.x), linewidth = 2, color = "black") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min HMD median") + xlab("Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Seasons (environmental data)")) +
  theme(text = element_text(size =16) )

#### by category (detections) ####
dfT = NULL
for (s in 1:length(ucat)) {
  tmpD  = df %>% filter(Category == ucat[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$LRfq = as.numeric( as.character( gsub("X","", tmpP$key ) ) ) 
  tmpP$Category = ucat[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
ggplot(dfT, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  #geom_line(data = dfT, aes(x=LRfq , y=med.x), linewidth = 2, color = "black") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min HMD median") + xlab("Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Categories (detections)")) +
  theme(text = element_text(size =16) )

#### by condition (RRPCA) ####
dfT = NULL
for (s in 1:length(urrpca)) {
  tmpD  = df %>% filter(RRPCA == urrpca[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$LRfq = as.numeric( as.character( gsub("X","", tmpP$key ) ) ) 
  tmpP$Category = urrpca[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
ggplot(dfT, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  #geom_line(data = dfT, aes(x=LRfq , y=med.x), linewidth = 2, color = "black") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min HMD median") + xlab("Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Conditions (rrpca)")) +
  theme(text = element_text(size =16) )

#### by features (Cluster) ####
dfT = NULL
for (s in 1:length(uclust)) {
  tmpD  = df %>% filter(ClusterIDNumber == uclust[s])
  tmpP = tmpD %>% gather(key, value, numeric_columns) %>% group_by(key) %>% 
    dplyr::summarise(n = n(),  # Number of sample
                     lower.x = quantile(value, probs = 0.25),
                     med.x  = quantile(value, probs = 0.5),
                     upper.x = quantile(value, probs = 0.75))
  tmpP$LRfq = as.numeric( as.character( gsub("X","", tmpP$key ) ) ) 
  tmpP$Category = uclust[s]
  dfT = rbind(dfT, tmpP) 
  rm(tmpD, tmpP)
}
sample_counts <- dfT %>%
  group_by(Category) %>%
  summarise(n = unique(n))

ggplot(dfT, aes(x=LRfq , y=med.x) )  +
  geom_line( linewidth = 2, color = "gray") +
  geom_line(aes (y=lower.x),  alpha = .5,  linewidth = 1,color = "gray" , linetype="dotted") + 
  geom_line(aes (y=upper.x),  alpha = .5,  linewidth = 1, color = "gray", linetype="dotted") +
  facet_wrap(~Category) +
  scale_x_log10() +  ylab("1-min HMD median") + xlab("Frequency (Hz)")+
  ylim(c(50,95)) +   theme_bw()+ 
  ggtitle (paste("Soundscape Features (clusters)")) +
  theme(text = element_text(size =16) )


### SPECTROGRAM-like ####
#### by category (detections) ####
# NOTE: takes a bit to plot because so big!
df$sample = 1:nrow(df)
i=3
df_subset <- subset(df, Category == ucat[i])
df_subset$Index <- seq_len(nrow(df_subset))

# Melt the data frame to long format suitable for ggplot2
df_melted <- reshape2::melt(df_subset, id.vars = c("Category", "dateTime", "Index"),
                            measure.vars = hix,
                            variable.name = "Frequency", 
                            value.name = "dB")
df_melted$Frequency2 = ( as.numeric(as.character( df_melted$Frequency)) )

ggplot(df_melted, aes(x = Index, y = (Frequency2), fill = dB)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"),
                       na.value = "grey50",
                       name = "dB") +
  labs(title = paste("Spectrogram for Category:", ucat[i]),
       x = "dateTime",
       y = "Frequency") +
  theme_minimal()

#What are the spectral properties of each season

#What are the spectral properties of each season-category

#What are the spectral properties of each season-cluster

#What are the spectral properties of each RRPCA

## EXTRA ####
### pie charts by season and category ####
tal = as.data.frame( df %>% group_by(season, Category) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
data = as.data.frame( tal )
agg_data <- data %>%   group_by(season, Category) %>%   summarize(PerTime = sum(PerTime))
total_time <- data %>%   group_by(season) %>%   summarize(total_n = sum(n))
total_time$total_days <- total_time$total_n / (60 * 24)
p1 <- ggplot(agg_data %>% filter(season == useason[1]), aes(x = "", y = PerTime, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  theme_void() +   scale_fill_manual(name = "Acoustic Scene", values = category_colors )+
  labs(title = paste(useason[1],"- Total Time:", 
                     round(total_time$total_days[total_time$season == useason[1]], 2), "days"), fill = "Category")
p2 <- ggplot(agg_data %>% filter(season == useason[2]), aes(x = "", y = PerTime, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +   theme_void() +   scale_fill_manual(name = "Acoustic Scene", values = category_colors )+
  labs(title = paste(useason[2],"- Total Time:", 
                     round(total_time$total_days[total_time$season == useason[2]], 2), "days"), fill = "Category")
p3 <- ggplot(agg_data %>% filter(season == useason[3]), aes(x = "", y = PerTime, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +   theme_void() +   scale_fill_manual(name = "Acoustic Scene", values = category_colors )+
  labs(title = paste(useason[3],"- Total Time:",
                     round(total_time$total_days[total_time$season == useason[3]], 2), "days"), fill = "Category")
p4 <- ggplot(agg_data %>% filter(season == useason[4]), aes(x = "", y = PerTime, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  theme_void() +  scale_fill_manual(name = "Acoustic Scene", values = category_colors )+
  labs(title = paste(useason[4],"- Total Time:", 
                     round(total_time$total_days[total_time$season == useason[4]], 2), "days"),  fill = "Category")
grid.arrange(p1, p2, p3, p4, ncol = 2)

