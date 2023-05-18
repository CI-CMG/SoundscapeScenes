# PLOT HMDdet

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)

inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()
pltf = 0

load( inFile)

#FORMATTTING ####
st =  sapply(strsplit(basename( inFile), "_"), "[[", 2) #site name
dpl = gsub(".Rda", "", sapply(strsplit(basename( inFile ), "_"), "[[", 3) ) # deployment name
efq = ncol(HMDdet)-5
fq = as.numeric(as.character( gsub("X","", colnames(HMDdet[2:efq] )) ) ) # Frequency range: truncate to 100-2000 Hz
unique( HMDdet$Type )
unique( HMDdet$Dets )
# Acoustic Scenes over time ####
# get start and end of different AS, so segment works for plotting
HMDdet = HMDdet %>%  mutate(test = case_when(Category == lag(Category) ~ "No", TRUE ~ "Yes"))
HMDdet$test
idx = which(HMDdet$test == "Yes") #start of new scene
#rm(AS)
AS = as.data.frame(matrix(nrow = length(idx), ncol=3))
AS$Start = HMDdet$dateTime[idx]
AS$Category= HMDdet$Category[idx]
AS$End = c( HMDdet$dateTime[ idx[2:length(idx)] +1 ] , HMDdet$dateTime[nrow(HMDdet)] )
AS$Hours = as.numeric( as.character( difftime(AS$End, AS$Start, units = "hours") ))
tal = as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
tal$PerTime = round( (tal$n/ sum(tal$n) * 100) , digits = 2 )
AS$mth = month(AS$Start)

ggplot(AS, aes(x=Start, xend=End, y=Category, yend=Category, color = Hours)) +
  geom_segment() +
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=20) +
  xlab("")+  ylab("")+ ggtitle(paste("Acoustic Scenes at ", st, " (1-min)", sep = "") )  +
  labs(caption = (paste0("Percent time in each category: ", 
                         tal$Category[1], "=", tal$PerTime[1], " | ", 
                         tal$Category[2], "=", tal$PerTime[2], " | ",
                         tal$Category[3], "=", tal$PerTime[3], " | ",
                         tal$Category[4], "=", tal$PerTime[4] ))) +
  scale_color_gradientn(colours = viridis(10))+
  #facet_wrap(~mth) +
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 20) )

# Labeled spectra- one day ####
HMDdet$Day = as.Date( HMDdet$dateTime )
dy = "2019-04-11"
tmpD = HMDdet[ HMDdet$Day == dy, ] 
ed = ncol(tmpD) - 7
head(tmpD)

if (pltf == 1) {
  
  medSPLm = reshape::melt (tmpD, id.vars = c("dateTime", "Category"),  measure.vars = colnames(tmpD)[ 2 : ed ] )
  colnames( medSPLm)  = c("date", "AS", "Fq", "SPL")
  medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
  
  ggplot(medSPLm, aes(x=Fq, y=SPL, group = date ) ) +
    geom_line(alpha = .2 ) + 
    scale_x_log10() +
    ylab("1-min HMD")+ xlab("Frequency (Hz)")+
    facet_wrap(~AS)+
    theme_minimal() +
    ggtitle(paste0( st, " on ", dy) ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Median spectra for each day + Category ####
udys = unique(HMDdet$Day)
as.data.frame(udys)
as.data.frame( colnames(HMDdet)[ 2 : ed ] )

tst = HMDdet %>% group_by(c(Category))  %>% summarise(median100 = median(X100, na.rm = TRUE))

library(tidyverse)

# single FQ
HMDdet$Mth = month( HMDdet$dateTime )
HMDdet %>% group_by(Category,Mth) %>%  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),  X100 = quantile(X100, c(0.25, 0.5, 0.75)))

# percent_rank-- cool conversion, but not condense
#tst = as.data.frame ( HMDdet %>%   group_by(Category) %>%   mutate(across(colnames(HMDdet)[ 2 : ed ], percent_rank) ) )


tst = HMDdet %>% gather(key, value, 2:ed) %>% group_by(Category, key) %>% 
  dplyr::summarise(lower.x = quantile(value, probs = 0.25),
                    mean.x = quantile(value, probs = 0.5),
                    upper.x = quantile(value, probs = 0.75))
head(tst)
tst$Fq = as.numeric(as.character( gsub("X","", tst$key ) ) )
tst = as.data.frame(tst)

ggplot(tst, aes(x=Fq , y=mean.x, color = Category) )  +
  geom_line( size = 2) +
  scale_x_log10() +
  ylab("1-min HMD median")+ xlab("Frequency (Hz)")+
  theme_minimal() +
  ggtitle( st) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14))+
  theme(legend.position="top")+
  theme(text = element_text(size =20) )

ggplot(tst, aes(x=Fq, color=Category ) ) +
  geom_line(aes (y=mean.x), size = 2   ) + 
  geom_line(aes (y=lower.x),  alpha = .5,  size = 1 ) + #linetype="dotted",
  geom_line(aes (y=upper.x),  alpha = .5,  size = 1 ) +
  scale_x_log10() +
  ylab("1-min HMD Percentiles (25,50,95)")+ xlab("Frequency (Hz)")+
  theme_minimal() +
  ylim(c(50,100)) +
  #labs(caption =unique( HMDdet$Type ) ) +
  ggtitle( paste0( st, ": ",min(HMDdet$Day), " to ", max(HMDdet$Day), "\n (Total min=", nrow(HMDdet),")" )) +
 # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position="top")+ 
  theme(text = element_text(size =20) )

