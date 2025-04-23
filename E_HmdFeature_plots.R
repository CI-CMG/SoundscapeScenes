# Compare results across acoustic scene analyses-- cluster in Triton

# purpose plot the spectra features (spectral plot) and occurence of the features (tile plot)

rm(list=ls()) 

# LIBRARIES ####
library(tidyr)
library(dplyr)
library(rmatio)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(lubridate)

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
AllmaxSpectra = NULL
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
uSites = unique(tal$site)
tal_complete = NULL
for ( ss in 1:length(uSites) ){
  #separate by site
  tmp = tal[ tal$site == uSites[ss], ]
  #get percent time in each hour for the cluster
  tmp$PerTime = round((tmp$n / sum(tmp$n)) * 100, 2) # sum( tmp$PerTime)
  tal_complete = rbind(tal_complete, tmp)
}


ggplot(tal_complete, aes(x = "", y = PerTime, fill = as.factor(ClusterIDNumber))) +
  geom_bar(stat = "identity", width = 1) +  # Create bars (which become pie slices)
  coord_polar(theta = "y") +  # Convert to pie chart
  facet_wrap(~ site) +  # One pie per site
  theme_minimal() +
  labs(
    title = "Cluster Distribution per Site (June 2023)",
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
    colnames(dSpectra )[cc+1] = paste0("cluster", cc)
  }
  head( dSpectra )
  
  tal_site = tal_complete[tal_complete$site == site,]
  tal_site = tal_site[tal_site$PerTime >  0,]
  
 
  dSpectraM = melt(dSpectra, id.vars = c("FQ"), measure.vars = colnames(dSpectra)[1:nClusters+1])
  dSpectraM$ClusterIDNumber = as.numeric( as.character( substr(dSpectraM$variable, start = 8, stop=10) ))
  per_time_lookup = setNames(tal_site$PerTime, tal_site$ClusterIDNumber)
  bigCluster = as.numeric( tal_site$ClusterIDNumber[tal_site$PerTime > 10 ])
  dSpectraM$PerTime = .5
  dSpectraM$PerTime[dSpectraM$ClusterIDNumber %in% bigCluster] = 2
  p1 = ggplot(dSpectraM, aes(x = FQ, y = value, color = as.factor( variable ) , size = PerTime ) )+
    geom_line(show.legend = FALSE)+
    scale_x_log10()+
    scale_size_identity() + 
    coord_cartesian(xlim = c(20, 2000), ylim = c(50, 110)) +
    theme_minimal() +
    
    labs( title = paste0("Spectra of soundscape features at ", site  ),
          x = "Frequency [Hz]",
          y =  "Sound Level",
          color = NULL)+
    theme(legend.position = "none") +
    guides(color = guide_legend(nrow = 3)) 
  #print(p1) 
  
  #save the spectra with max clusters
  max_clust = tal_site$ClusterIDNumber[ which.max(tal_site$PerTime)  ]
  maxSpectra = dSpectraM[ dSpectraM$ClusterIDNumber == max_clust, ]
  maxSpectra$site = site
  AllmaxSpectra = rbind(AllmaxSpectra, maxSpectra)
  
  p2 = ggplot(tal_site, aes(x = "", y = PerTime, fill = as.factor(ClusterIDNumber))) +
    geom_bar(stat = "identity", width = 1,color = "white", size = 0.2) + 
    #coord_polar("y") +
    geom_text( aes( label = ClusterIDNumber), 
              position = position_stack(vjust = 0.5), color = "black", size = 3) +
    theme_minimal() +
    labs(
      title = "",
      fill = "",
      x = NULL, y = NULL ) +
    theme(axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          legend.position = "none")  # Remove grid lines
  #p2
  
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(1, "npc"), width = unit(1, "pt"))
  arranged_plot = grid.arrange(p1, separator, p2, widths =c(4, 0.1, .8))
  
  #ggsave(filename = paste0(dirClus, "\\plot_", tolower(site), "_ClusterSpectra.jpg"), plot = arranged_plot, width = 10, height = 10, dpi = 300)
  
  ## TILE FOR CLUSTERS ####
  names(clustAll)
  dataALLm = reshape2 :: melt(clustAll, id.vars = c("dateTime", "site"), measure.vars = c("ClusterIDNumber" ))
  names(dataALLm)
  dataALLmS = dataALLm[dataALLm$site == site,]
  dataALLmS = as.data.frame(dataALLmS)

  pT = ggplot(dataALLmS, aes(dateTime, as.factor( value ), fill= as.factor(value)) ) + 
    geom_tile() +
    labs(title = paste0("Occurence of soundscape features at ", site) )+
    xlab("") +
    ylab("Cluster number") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          legend.position = "none") 
    
  left_stack = arrangeGrob(p1, separator, p2, widths = c(4, 0.1, 0.4))
 pall =  grid.arrange(left_stack, pT, nrow = 2, heights = c(5,4))
 #ggsave(filename = paste0(dirClus, "\\plot_", tolower(site), "_ClusterAll.jpg"), plot = pall, width = 10, height = 10, dpi = 300)
  #Check if the colors for clusters matching up??
  #bout output
  #sort( as.numeric( as.character( unique( dataALLmS$value )) ) )
  #spectra output
  #as.numeric( as.character( unique( substr(dSpectraM$variable , start = 8, stop = 10) )  ) )
  #tal_site
 
 # cluster daily patterns
 dataALLmS$Hr = hour(  dataALLmS$dateTime)
 unique(dataALLmS$site)
 talHR = as.data.frame( dataALLmS %>% group_by(Hr, value) %>% tally() )
 #need to fill in missing hours for each cluster 
 uclust = unique(dataALLmS$value )
 talHRp = NULL
 clSig = NULL
 for ( uu in 1:length(uclust) ){
   #separate by cluster
   tmp = talHR[ talHR$value == uclust[uu], ]
   #fill in missing hours for each cluster
   tmp = tmp %>% complete(Hr = 0:23, fill = list(n = 0, value = uclust[uu]))
   #get percent time in each hour for the cluster
   tmp$PerTime = round((tmp$n / sum(tmp$n)) * 100, 2)
   
   #should result in 100 for each cluster
   #cat(sum(tmp$PerTime), "\n")
   #check distribution over hours of the day
   tmp$angle <- (tmp$PerTime / 100) * 360
   result = rayleigh.test(circular(tmp$angle, type = "angles"))
   tmp$sig = result$p.value
   if(result$p.value < .05){
   cat(site, ": ", uclust[uu], "- ", result$p.value, "\n") 
     clSig = c(clSig,uclust[uu] )  }
   
   talHRp = rbind(talHRp, tmp)
   
 }
 
 
 pHr = ggplot(talHRp, aes(x = as.factor(value) , y = PerTime, fill = as.factor(Hr))) +
   geom_bar(stat = "identity", width = 1,color = "white", size = 0.2) +
   theme_minimal()+
   labs(x = "Cluster" , y = "Percent of hour", 
        caption = "asterisk indicates concentration of the cluster around specific hours using in Rayleigh test ") +
   ggtitle (paste0( "Hourly occurence of soundscape features at ", site) ) +
   theme(panel.grid = element_blank(),
         legend.position = "none")+
   scale_fill_grey(start = 0.1, end = 0.9)+
   #add signifiance to the graphic
   annotate("text", x = clSig, y = rep(100, length(clSig)), label = "*", size = 6, color = "black")
 #pHr
 
 pall2 =  grid.arrange(left_stack, pT, pHr, nrow = 3, heights = c(5,4,4))
 ggsave(filename = paste0(dirClus, "\\plot_", tolower(site), "_ClusterAll2.jpg"), plot = pall2, width = 10, height = 10, dpi = 300)
 
 
}
tmp = AllmaxSpectra[ AllmaxSpectra$site == "mb01",]
AllmaxSpectra2 = AllmaxSpectra[ AllmaxSpectra$site != "mb01",]
AllmaxSpectra2 = AllmaxSpectra2[ AllmaxSpectra2$site != "sb01",]
AllmaxSpectra2$region[ AllmaxSpectra2$site == "sb03"] = "east coast-north"
AllmaxSpectra2$region[ AllmaxSpectra2$site == "fgb0"] = "gulf coast"
AllmaxSpectra2$region[ AllmaxSpectra2$site == "fk05"] = "east coast-south"
AllmaxSpectra2$region[ AllmaxSpectra2$site == "oc02"] = "west coast-north"
AllmaxSpectra2$region[ AllmaxSpectra2$site == "mb02"] = "west coast-central"
AllmaxSpectra2$region[ AllmaxSpectra2$site == "pm01"] = "pacific"

ggplot(AllmaxSpectra2, aes(x = FQ, y = value, color = as.factor( region ) ) ) +
  geom_line(size = 2)+
  scale_x_log10()+
  theme_minimal() +
  labs( title = paste0("Spectra for most common cluster" ),
        x = "Frequency [Hz]",
        y =  "Sound Level",
        color = NULL)+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1)) 
