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
dirIn =  paste0( gdrive, siteN )
filepat = "_RrpcaResults_"# "_HmdLabels_LF_"
inFilesModel = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
filepat ="_Hmd_LF_"# _HmdLabels_LF_"
inFilesData = list.files( dirIn, pattern = filepat, recursive = F, full.names = T )
dirOut = dirIn


voi= "mth" #variable of interst
moi = 10
moiN = "October"

Rout = NULL
Rmth= NULL

for (ii in 1: length (inFilesModel) ){
 
  #original data
  split_string <- strsplit(basename( inFilesData[ii]), "_")[[1]]
  site <- paste0(split_string[1],split_string[2])
  
  cat("Running..", site, ii, " of ",length (inFilesModel) )
  
  load( inFilesData[ii] )
  HmdTrim$mth = month(HmdTrim$dateTime)
  HmdDets = HmdTrim
  idNA = ( which(is.na(HmdDets)))
  stp = which( names( HmdDets) ==  "1001.2")
  HmdDets2 = HmdDets[,1:stp]
  numeric_columns <- grep("^\\d", names( HmdDets2) ) 
  hix = names( HmdDets2)[numeric_columns]
  Nv   =  HmdDets2[, numeric_columns]  #dB values
  
  #low rank model
  load( inFilesModel[ii]) 
  Lr = as.data.frame(nvpcaTOL$L) 
  colnames(Lr) = hix
  LrDB = 10*log10( Lr^2 )  #CHECK: median(LrDB$'100'), no negative values, just values without transients
  colnames(LrDB) = hix
  
  ## RRPCA thresholds ####
  # sum of difference across frequencies for each minute
  LRdiff = as.data.frame ( rowSums( abs ( (LrDB - Nv) ) ) )
  colnames(LRdiff) = 'LRdiff'
  # min(LRdiff$LRdiff)
  # which frequency had the max LF diff
  LRfq   = as.data.frame ( as.numeric ( colnames(LrDB) [apply(LrDB, 1, (which.max) )] ) )
  colnames(LRfq) = 'LRfq'
  # median( LRfq$LRfq )
  
  ## LABEL HMD files ####
  HmdDets$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
  
  ### scatter by month (time of interest) RRPCA values (save plot)####
  HmdDets$Day = as.Date(HmdDets$dateTime)
  HmdDets$mth = month(HmdDets$dateTime)
  
  result_dplyr <- HmdDets %>%
    group_by(Day, !!sym(voi)) %>%
    summarise(
      LowRankDiff = mean(LowRanK, na.rm = TRUE),
      se = sd(LowRanK, na.rm = TRUE) / sqrt(n())
    )

  
  p = ggplot(data = result_dplyr, aes(x = Day, y = LowRankDiff, color = as.factor(!!sym(voi)))) +
    geom_point(size = 1) + 
    geom_errorbar(aes(ymin = LowRankDiff - se, ymax = LowRankDiff + se), width = 0.2) +
    ggtitle(paste0(site, " Daily Low-Frequency Residual Soundscape Condition")) +
    xlab("")+ ylab("Summed difference \n low-rank and origional data") +
    labs(subtitle = "Higher values indicate more transient sounds present", color = "Month") +
    theme_minimal() +
    #ylim(c(50,95)) +
    theme(text = element_text(size = 15),
          plot.subtitle = element_text(face = "italic"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10) ) )
  
  ggsave(
    filename = paste0(dirOut, "\\", site, "DailyLow-FrequencyResidual.png"),
    plot = p,
    width = 6, height = 4, units = "in",
    bg = "white"
  )
  
  
  ### Spectra by month (time of interest) (save plot) ####
  LrDB$Day = HmdDets$Day
  LrDB$mth = HmdDets$mth  
  LrDB$site = HmdDets$site  
  RSoundscape = aggregate( LrDB[,hix], by = list(Season = get(voi, LrDB)), FUN = (median) )
  RSoundscape25 = aggregate(
    LrDB[, hix], 
    by = list(Season = get(voi, LrDB)), 
    FUN = quantile, 
    probs = 0.25
  )
  
  RSoundscape75 = aggregate(
    LrDB[, hix], 
    by = list(Season = get(voi, LrDB)), 
    FUN = quantile, 
    probs = 0.75
  )
  
  RSoundscape = as.data.frame(RSoundscape)
  melted_df50 = reshape2::melt(RSoundscape, id.vars = c("Season"),  measure.vars = hix ) 
  melted_df25 = reshape2::melt(RSoundscape25, id.vars = c("Season"),  measure.vars = hix ) 
  melted_df75 = reshape2::melt(RSoundscape75, id.vars = c("Season"),  measure.vars = hix ) 
  melted_df50$Fq = as.numeric(as.character(melted_df50$variable))
  melted_df25$Fq = as.numeric(as.character(melted_df25$variable))
  melted_df75$Fq = as.numeric(as.character(melted_df75$variable))
  
  p2 = ggplot()+
    geom_line(data = melted_df50, aes(x = Fq, y = value, color = as.factor(Season), group = as.factor(Season)), linewidth = 1 ) +
    geom_line(data = melted_df25, aes(x = Fq, y = value, color = as.factor(Season), group = as.factor(Season)), linewidth = .3, alpha = .5, linetype = "dashed") +
    geom_line(data = melted_df75, aes(x = Fq, y = value, color = as.factor(Season), group = as.factor(Season)), linewidth = .3, alpha = .5, linetype = "dashed" ) +
    scale_x_log10() +
    #facet_wrap(~Season)+
    labs(subtitle = "Low-rank representation of sound levels", color = "Month") +
    theme_minimal() +
    ylim(c(50,95))+
    theme(
      text = element_text(size = 15),  # Set all text to size 16
      plot.subtitle = element_text(face = "italic"),
      #axis.title = element_text(size = 12),  # Axis titles
      #axis.text = element_text(size = 12),  # Axis labels
      legend.text = element_text(size = 12),  # Legend text
      #strip.text = element_text(size = 12)  # Facet labels
    ) +
    labs(
      title = paste0(site, " Residual Soundscape Condition"),
      x = "Frequency (Hz)",
      y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))
    )
  p2
  ggsave(
    filename = paste0(dirOut, "\\", site, "Low-FrequencyResidualSPL.png"),
    plot = p2,
    width = 6, height = 4, units = "in",
    bg = "white"
  )
  p3 = ggplot()+
    geom_line(data = melted_df50, aes(x = Fq, y = value),  linewidth = 1 ) +
    geom_line(data = melted_df25, aes(x = Fq, y = value),  linewidth = .3, alpha = .5, linetype = "dashed") +
    geom_line(data = melted_df75, aes(x = Fq, y = value),  linewidth = .3, alpha = .5, linetype = "dashed" ) +
    scale_x_log10() +     facet_wrap(~Season)+
    labs(subtitle = "Low-rank representation of sound levels", color = "Month") +
    theme_minimal() +     ylim(c(50,95))+
    theme( text = element_text(size = 15),  # Set all text to size 16
      plot.subtitle = element_text(face = "italic")  ) +
    labs(  title = paste0(site, " Residual Soundscape Condition"),
      x = "Frequency (Hz)",
      y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))     )
  p3
  ggsave(
    filename = paste0(dirOut, "\\", site, "Low-FrequencyResidualSPL_mth.png"),
    plot = p3,
    width = 6, height = 4, units = "in",
    bg = "white"
  )
  
  
  ### Spectra site (save data) ####

  RSoundscapeT = apply(LrDB[,hix], 2, quantile, probs = c(0, 0.25, 0.5, 0.75, 1))
  RSoundscapeT = as.data.frame( RSoundscapeT )
  RSoundscapeT$Site = site
  RSoundscapeT$minutes = nrow(LrDB)
  Rout = rbind(Rout, RSoundscapeT)
  colnames(RSoundscapeT)
  
  ### Spectra site (save data) ####
  colnames(LrDB)
  LrDBT  = LrDB[LrDB$mth == moi,]
  
  RSoundscapeM = apply(LrDBT[,hix], 2, quantile, probs = c(0, 0.25, 0.5, 0.75, 1))
  RSoundscapeM = as.data.frame( RSoundscapeM )
  RSoundscapeM$Site = site
  RSoundscapeM$mth = moi
  RSoundscapeM$minutes = nrow(LrDBT)
  colnames(RSoundscapeM)
  Rmth = rbind(Rmth, RSoundscapeM)
  
} 

DC = Sys.Date()
save(Rout, file = paste0(dirOut, "\\", siteN, "_RRPCA", "_", DC, ".Rda") )
save(Rmth, file = paste0(dirOut, "\\", siteN, "_RRPCA-", moi, "_", DC, ".Rda") )

# PLOT ALL SITES/years ####
Rout = as.data.frame(Rout)
rownames(Rout)
library(tibble)
Rout = rownames_to_column(Rout, var = "quantiles")

colnames(Rout)
melted_Rout = reshape2::melt(Rout, id.vars = c("Site", "quantiles"),  measure.vars = hix ) 
melted_Rout$Fq = as.numeric(as.character(melted_Rout$variable))
melted_Rout$Quant= as.factor(as.character(sapply(strsplit(melted_Rout$quantiles, "%"), `[`, 1) ))

melted_RoutT=  melted_Rout %>% filter(!Quant %in% c("0", "100", "25", "75"))

ggplot() +
  geom_line(data = melted_RoutT, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                                    color = Site, linetype = Quant), linewidth = 1) +
  labs(subtitle = "Low-rank representation of sound levels") +
  theme_minimal() +     ylim(c(55,80))+
  theme( text = element_text(size = 15),  # Set all text to size 16
         plot.subtitle = element_text(face = "italic")  ) +
  labs(  title = "Residual Soundscape Condition",
         x = "Frequency (Hz)",
         y =expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)"))     )

# CHO1- modify plot ####
# select separate decades
colnames(Rout)
#original data
Rout$yr <- as.numeric(as.character( sapply(strsplit(Rout$Site, "-"), function(x) x[3])))
melted_Rout = reshape2::melt(Rout, id.vars = c("Site", "quantiles","yr"),  measure.vars = hix ) 
melted_Rout$Fq = as.numeric(as.character(melted_Rout$variable))
melted_Rout$Quant= as.factor(as.character(sapply(strsplit(melted_Rout$quantiles, "%"), `[`, 1) ))
melted_RoutT=  melted_Rout %>% filter(!Quant %in% c("0", "100", "25", "75"))

# Filter data for each decade
melted_RoutT$decade <- "2000"
melted_RoutT$decade[melted_RoutT$yr > 2012] <- "2010"

data_2000 <- melted_RoutT[melted_RoutT$decade == "2000", ]
data_2010 <- melted_RoutT[melted_RoutT$decade == "2010", ]

# Plot for 2000s decade
plot_2000 <- ggplot(data_2000) +
  geom_line(aes(x = Fq, y = value, group = interaction(Site, Quant), 
                color = Site, linetype = Quant), linewidth = 1) +
  labs(subtitle = "2000s: Low-rank representation of sound levels") +
  theme_minimal() +
  ylim(c(55, 85)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(title = "Residual Soundscape Condition (2000s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Plot for 2010s decade
plot_2010 <- ggplot(data_2010) +
  geom_line(aes(x = Fq, y = value, group = interaction(Site, Quant), 
                color = Site, linetype = Quant), linewidth = 1) +
  labs(subtitle = "2010s: Low-rank representation of sound levels") +
  theme_minimal() +
  ylim(c(55, 85)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(title = "Residual Soundscape Condition (2010s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Arrange plots side by side or on top of each other
library(gridExtra)
grid.arrange(plot_2000, plot_2010, ncol = 1)
plot_2000

# CHO1- modify plot- Rmth ####
# select separate decades for just October results
Rmth = rownames_to_column(Rmth, var = "quantiles")
Rmth$yr <- as.numeric(as.character( sapply(strsplit(Rmth$Site, "-"), function(x) x[3])))
melted_Rout = reshape2::melt(Rmth, id.vars = c("Site", "quantiles","yr"),  measure.vars = hix ) 
melted_Rout$Fq = as.numeric(as.character(melted_Rout$variable))
melted_Rout$Quant= as.factor(as.character(sapply(strsplit(melted_Rout$quantiles, "%"), `[`, 1) ))
melted_RoutT=  melted_Rout %>% filter(!Quant %in% c("0", "100", "25", "75"))
# Filter data for each decade
melted_RoutT$decade <- "2000"
melted_RoutT$decade[melted_RoutT$yr > 2012] <- "2010"
data_2000 <- melted_RoutT[melted_RoutT$decade == "2000", ]
data_2010 <- melted_RoutT[melted_RoutT$decade == "2010", ]

# Plot for 2000s decade
ldata <- data.frame(
  x1 = c(94, 94, 94, 94, 94), 
  y1 = data_2000$value[data_2000$Fq == 100], 
  lb = data_2000$yr[data_2000$Fq == 100]
)
plot_2000 <- ggplot() +
  geom_line(data = data_2000, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                color = Site, linetype = Quant), linewidth = 1) +
  geom_text(data = ldata, aes(x = x1, y = y1, label = lb),size = 2, fontface = "bold") +
  theme_minimal() +
  ylim(c(55, 95)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(subtitle = paste0("2000s: Low-rank representation of sound levels-", moiN)) +
  labs(title = "Residual Soundscape Condition (2000s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Plot for 2010s decade
ldata <- data.frame(
  x1 = c(94, 94, 94, 94, 94), 
  y1 = data_2010$value[data_2010$Fq == 100], 
  lb = data_2010$yr[data_2010$Fq == 100]
)
plot_2010 <- ggplot() +
  geom_line(data = data_2010, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                color = Site, linetype = Quant), linewidth = 1) +
 
  geom_text(data = ldata, aes(x = x1, y = y1, label = lb),size = 2, fontface = "bold") +
  labs(subtitle = paste0("2010s: Low-rank representation of sound levels-", moiN)) +
  theme_minimal() +
  ylim(c(55, 95)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(title = "Residual Soundscape Condition (2010s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Arrange plots side by side or on top of each other
library(gridExtra)
grid.arrange(plot_2000, plot_2010, ncol = 1)


# NRS01- modify plot- Rmth ####
# select separate decades for just October results
Rmth = rownames_to_column(Rmth, var = "quantiles")
Rmth$yr <- as.numeric(as.character( sapply(strsplit(Rmth$Site, "-"), function(x) x[3])))
melted_Rout = reshape2::melt(Rmth, id.vars = c("Site", "quantiles","yr"),  measure.vars = hix ) 
melted_Rout$Fq = as.numeric(as.character(melted_Rout$variable))
melted_Rout$Quant= as.factor(as.character(sapply(strsplit(melted_Rout$quantiles, "%"), `[`, 1) ))
melted_RoutT=  melted_Rout %>% filter(!Quant %in% c("0", "100", "25", "75"))
# Filter data for each decade
melted_RoutT$decade <- "2000"
melted_RoutT$decade[melted_RoutT$yr > 2012] <- "2010"
data_2000 <- melted_RoutT[melted_RoutT$decade == "2000", ]
data_2010 <- melted_RoutT[melted_RoutT$decade == "2010", ]

# Plot for 2000s decade
ldata <- data.frame(
  x1 = c(94, 94, 94, 94, 94), 
  y1 = data_2000$value[data_2000$Fq == 100], 
  lb = data_2000$yr[data_2000$Fq == 100]
)
plot_2000 <- ggplot() +
  geom_line(data = data_2000, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                                  color = Site, linetype = Quant), linewidth = 1) +
  geom_text(data = ldata, aes(x = x1, y = y1, label = lb),size = 2, fontface = "bold") +
  theme_minimal() +
  ylim(c(55, 95)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(subtitle = paste0("2000s: Low-rank representation of sound levels-", moiN)) +
  labs(title = "Residual Soundscape Condition (2000s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Plot for 2010s decade
ldata <- data.frame(
  x1 = c(94, 94, 94, 94, 94), 
  y1 = data_2010$value[data_2010$Fq == 100], 
  lb = data_2010$yr[data_2010$Fq == 100]
)
plot_2010 <- ggplot() +
  geom_line(data = data_2010, aes(x = Fq, y = value, group = interaction(Site, Quant), 
                                  color = Site, linetype = Quant), linewidth = 1) +
  
  geom_text(data = ldata, aes(x = x1, y = y1, label = lb),size = 2, fontface = "bold") +
  labs(subtitle = paste0("2010s: Low-rank representation of sound levels-", moiN)) +
  theme_minimal() +
  ylim(c(55, 95)) +
  theme(text = element_text(size = 15), 
        plot.subtitle = element_text(face = "italic")) +
  labs(title = "Residual Soundscape Condition (2010s)",
       x = "Frequency (Hz)",
       y = expression(paste("Residual Sound Pressure Level (dB re: 1", mu, "Pa)")))

# Arrange plots side by side or on top of each other
library(gridExtra)
grid.arrange(plot_2000, plot_2010, ncol = 1)


