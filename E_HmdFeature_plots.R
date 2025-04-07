# ompare results across acoustic scene analyses-- cluster in Triton

# purpose plot the spectra features (spectral plot) and occurence of the features (tile plot)

rm(list=ls()) 

# LIBRARIES ####
library(tidyr)
library(dplyr)
library(rmatio)

# PARAMS ####
ver = "ec100"
DC = Sys.Date()

matlab_to_r_timestamp <- function(matlab_date) {
  # MATLAB date to POSIXct conversion (seconds)
  # The number of days between MATLAB's 0-date and POSIX epoch
  matlab_to_posix_epoch_diff <- 719529 * 86400  # 86400 seconds per day
  
  # Convert MATLAB date to POSIXct
  posix_time <- as.POSIXct((matlab_date - 1) * 86400 - matlab_to_posix_epoch_diff, origin = "1970-01-01", tz = "UTC")
  
  return(posix_time)
}

## CLUSTER RESULTS ####
dirClus = "F:\\ONMS\\feature\\CC"
inFilesCC = list.files( dirClus, pattern = "*_Bouts.csv$", full.names = T)
inFilesCC = inFilesCC[grepl(ver,inFilesCC) ]
clustAll = NULL
for (f in 1:length(inFilesCC )) {
  clust = read.csv(inFilesCC[f])
  clust$dateTime <- as.POSIXct(  clust$StartTime,   format = "%d-%b-%Y %H:%M:%S", tz = "GMT") 
  clust$site = substr ( sapply( strsplit(basename(inFilesCC[f]), "_"), "[[", 1), start = 1, stop =4 )
 
  cat("Cluster data: ",  unique(clust$site), length(unique(clust$ClusterIDNumber)), "clusters " , as.character(min(clust$dateTime)),  as.character(max(clust$dateTime)),"\n" )
  
  clusttmp = select(clust, c(site, dateTime, ClusterIDNumber) )
  clustAll = rbind(clustAll, clusttmp)
}
clustAll$ucluster = paste( clustAll$ClusterIDNumber, clustAll$site, sep = "_")

## PIE FOR CLUSTERS ####
tal = as.data.frame( clustAll %>% group_by(ClusterIDNumber, site) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
tal_complete <- tal %>%
  complete(site, ClusterIDNumber, fill = list(n = 0)) %>%  # Fill missing clusters with n = 0
  group_by(site) %>%
  mutate(PerTime = round((n / sum(n)) * 100, 2))

ggplot(tal_complete, aes(x = "", y = PerTime, fill = as.factor(ClusterIDNumber))) +
  geom_bar(stat = "identity", width = 1) +  # Create bars (which become pie slices)
  coord_polar(theta = "y") +  # Convert to pie chart
  facet_wrap(~ site) +  # One pie per site
  theme_minimal() +
  labs(
    title = "Cluster Distribution per Site",
    fill = "Cluster ID",
    x = NULL, y = NULL
  ) +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank(), 
        panel.grid = element_blank())  # Remove grid lines

## SPECTRA FOR CLUSTERS ####
inFilesS = list.files( dirClus, pattern = "*_types_all.mat$", full.names = T)
inFilesS = inFilesS[grepl(ver,inFilesS) ]
for (ii in 1:length(inFilesS)) {
  df = read.mat(inFilesS[ii])
  site = substr ( sapply( strsplit(basename(inFilesS[ii]), "_"), "[[", 1), start = 1, stop =4 )
  
  dSpectra = as.data.frame ( unlist( df$f ) )
  colnames(dSpectra ) = 'FQ'
  nClusters = length(df$compositeData$spectraMeanSet)
  
  for(cc in 1:nClusters) {
    tmp = as.data.frame ( unlist( df$compositeData$spectraMeanSet[cc] ) )
    dSpectra = cbind(dSpectra,tmp)
    colnames(dSpectra )[cc+1] = paste0("cluster",cc)
  }
  
  dSpectraM = melt(dSpectra, id.vars = c("FQ"), measure.vars = colnames(dSpectra)[2:nClusters+1])
  p1 = ggplot(dSpectraM, aes(FQ, value,color = as.factor( variable )) )+
    geom_line(size = 2)+
    scale_x_log10()+
    coord_cartesian(xlim = c(20, 2000), ylim = c(50, 110)) +
    theme_minimal() +
    
    labs( title = paste0(site, " Soundscape Features"),
          x = "Frequency [Hz]",
          y =  "Sound Level",
          color = NULL)+
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3)) 
  print(p1) 
  
  
  tal_site = tal_complete[tal_complete$site ==site,]
    
  p2 = ggplot(tal_site, aes(x = "", y = PerTime, fill = as.factor(ClusterIDNumber))) +
    geom_bar(stat = "identity", width = 1,color = "white", size = 0.2) +  
    #coord_polar(theta = "y") +  
    theme_minimal() +
    labs(
      title = "",
      fill = "",
      x = NULL, y = "Percent Time" ) +
    theme(axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          legend.position = "none")  # Remove grid lines
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(1, "npc"), width = unit(1, "pt"))
  arranged_plot = grid.arrange(p1, separator, p2, widths =c(4, 0.1, .8))
  
  ggsave(filename = paste0(dirClus, "\\plot_", tolower(site), "_ClusterSpectra.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)
  
  ## tile  
  
}
