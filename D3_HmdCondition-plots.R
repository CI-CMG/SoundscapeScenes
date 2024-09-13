rm(list=ls()) 

# LIBRARIES ####
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(rsvd)
library(rlang)
library(tibble)
library(plotly)

# Custom function to calculate median and standard error
custom_fun <- function(x) {
  n <- length(x)
  c(median = median(x), se = sd(x) / sqrt(n))
}
quantile_25_75 <- function(x) {
  quantile(x, probs = c(0.25, 0.75))
}

# GET DATA ####
siteN = "CaseStudy2"
siteN = "AU_CH01-all"
siteN = "NRS01"

gdrive = "F:\\SoundCoop\\hmd_downloadedGCP\\"
dirOut =  paste0( gdrive, siteN )
inFiles = list.files( dirOut, pattern = paste0(siteN, "_RRPCA"),full.names = T )
load(inFiles[1] )
load(inFiles[2] )

# PLOT ALL SITES/years ####
Rout = rownames_to_column(Rout, var = "quantiles")
Rout = as.data.frame(Rout)
colnames(Rout)
hix = as.numeric ( as.character( colnames(Rout) [2:(ncol(Rout)-2) ] ) )
rownames(Rout)

melted_Rout = reshape2::melt(Rout, id.vars = c("Site", "quantiles"),  measure.vars = colnames(Rout) [2:(ncol(Rout)-2) ] ) 
melted_Rout$Fq = as.numeric(as.character(melted_Rout$variable))
melted_Rout$Quant = as.factor(as.character(sapply(strsplit(melted_Rout$quantiles, "%"), `[`, 1) ))
melted_RoutT =  melted_Rout %>% filter(!Quant %in% c("0", "100", "25", "75"))
melted_RoutT$Year <- gsub(".*-(\\d{4}).*", "\\1", melted_RoutT$Site)
start_points <- melted_RoutT %>%
  group_by(Site, Quant) %>%
  slice_min(order_by = Fq, n = 1)


start_points$Fq = 80
ggplot() +
  geom_line(data = melted_RoutT, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                                     color = Site, linetype = Quant), linewidth = 1) +
  labs(subtitle = "low-rank representation of sound levels") +
  theme_minimal() +     ylim(c(45,80))+
  theme( text = element_text(size = 15),  # Set all text to size 16
         plot.subtitle = element_text(face = "italic")  ) +
  labs(  title = "Residual Soundscape Condition - all data",
         x = "Frequency (Hz)",
         y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))     ) +
  scale_color_brewer(palette = "Blues") +
  geom_text(data = start_points, aes(x = Fq, y = value, label = Year), 
            hjust = -0.1, size = 3, color = "black")

# PLOT specific site/month ####
Rmth = as.data.frame(Rmth)
Rmth = rownames_to_column(Rmth, var = "quantiles")
colnames(Rmth)
hix = as.numeric ( as.character( colnames(Rmth) [2:(ncol(Rmth)-3) ] ) )
rownames(Rmth)

melted_Rmth = reshape2::melt(Rmth, id.vars = c("Site", "quantiles"),  measure.vars = colnames(Rmth) [2:(ncol(Rmth)-3) ] ) 
melted_Rmth$Fq = as.numeric(as.character(melted_Rmth$variable))
melted_Rmth$Quant = as.factor(as.character(sapply(strsplit(melted_Rmth$quantiles, "%"), `[`, 1) ))
melted_RmthT =  melted_Rmth %>% filter(!Quant %in% c("0", "100", "25", "75"))

melted_RmthT$Year <- gsub(".*-(\\d{4}).*", "\\1", melted_RmthT$Site)
start_points <- melted_RmthT %>%
  group_by(Site, Quant) %>%
  slice_min(order_by = Fq, n = 1)

start_points$Fq = 70
p = ggplot() +
  geom_line(data = melted_RmthT, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                                     color = Site, linetype = Quant), linewidth = 1) +
  labs(subtitle = "low-rank representation of sound levels") +
  theme_minimal() +     ylim(c(45,80))+
  theme( text = element_text(size = 15),  # Set all text to size 16
         plot.subtitle = element_text(face = "italic")  ) +
  labs(  title = "Residual Soundscape Condition - October",
         x = "Frequency (Hz)",
         y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))     ) +
  scale_color_brewer(palette = "Blues") +
  geom_text(data = start_points, aes(x = Fq, y = value, label = Year), 
            hjust = -0.1, size = 3, color = "black")
# missing 2017 and 2022

ggplotly(p)
p
