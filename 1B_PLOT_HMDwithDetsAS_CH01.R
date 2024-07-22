rm(list=ls()) 

# LIBRARIES ####
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(viridis)

siteN = "AU_CH01"
gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
dirIn =  paste0( gdrive, "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\",siteN )
load(paste0(dirIn,"\\", siteN, "_HmdDetsAS"))
unique(AS$season)
AS <- mutate(AS, season = ifelse(season == "notFilled", "no label", season))
AS$season <- factor(AS$season, ordered = TRUE, levels = c("form", "ice", "break", "open","no label"))

first_five_colors <- viridis_pal()(15)
print(first_five_colors)# Display the first 5 colors
#"#440154FF" "#481B6DFF" "#46337EFF" "#3F4889FF" "#365C8DFF" "#2E6E8EFF" "#277F8EFF" "#21908CFF" "#1FA187FF" "#2DB27DFF" "#4AC16DFF" "#71CF57FF" "#9FDA3AFF" "#CFE11CFF"
#"#FDE725FF"
category_colors <- c("#26828EFF", "#440154FF", "#2DB27DFF", "#B4DE2CFF")
#category_colors <- viridis_pal()(nlevels(as.factor(AS$Category)))

# PLOT: Season vs AS ####
ggplot(data = AS) +
  geom_mosaic(aes(x=product(season), fill = Category)) + 
  scale_fill_manual(values = category_colors) + 
  theme_minimal() +
  ylab("Acoustic Scene")+ xlab("Arctic Season")+
  ggtitle(paste0( as.Date(min(AS$dateTime)), " to ", as.Date(max(AS$dateTime))) ) 
AS$time <- as.POSIXct(AS$dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") 
min(AS$time)
max(AS$time)

# when do the "no labels" occur? #### 
df = AS[AS$season == "no label",]
df$time <- as.POSIXct(df$dateTime, format = "%Y-%m-%d %H:%M:%S") 
df$time_difference_minutes <- c(0, as.numeric(diff(df$time, units = "mins")))
plot(df$X, df$time) # across all the months!

## PLOT: acoustic Scene over time ####
df = AS #[AS$season == "ice",]
df$time <- as.POSIXct(df$dateTime, format = "%Y-%m-%d %H:%M:%S") 
df$time_difference_minutes <- c(0, as.numeric(diff(df$time, units = "mins")))
#why is this so big?? it is the same as all other jumps
df[which.max( (df$time_difference_minutes) ),c(1,2, 1020, 1021) ]
df[which.max( (df$time_difference_minutes) )-1,c(1,2, 1020,1021) ]
p = ggplot(data = df, aes(x = time, y = time_difference_minutes)) +
  geom_point() +
  #geom_line() +
  labs(title = "Time Differences in Minutes", x = "Time", y = "Time Difference (Minutes)")
p


