# Soundscape Integration
# AK soundscape project (WCS)
# http://localhost:8888/notebooks/Arctic%20Acoustic%20Scenes.ipynb


rm(list=ls())
DC = Sys.Date()

library(stringr)
library(lubridate)

# PURPOSE ####

# INPUTS ####



#IMPORT DATA ####
# for Gambell site for entire year 2015-2016
load("F:\\MANUSCRIPTS\\1.PUBLISHED\\2021_MM_JASA_AKsoundscape\\data\\dataSpAShWeTiIce")
dataSoundscape = dataSpAShWeTiIce

# FORMATE DATA ####
#re-code nship as 0 if both distances are >20 km
for (ii in 1:nrow(dataSoundscape)){
  #change ship presence if min distance is > 20 to 0
  if( dataSoundscape$nShips[ii]  == 1 ){
    tmpd = as.numeric( sapply(strsplit(as.character( dataSoundscape$mnDist[ii]),","), `[`, 1) )
    if(tmpd/1000 > 20){ dataSoundscape$nShips[ii] = 0  }
  }else if ( dataSoundscape$nShips[ii]  == 2 ){
    tp1 = as.numeric( sapply(strsplit(as.character( dataSoundscape$mxDist[ii]),","), `[`, 1) )
    if(tp1/1000 > 20){ dataSoundscape$nShips[ii] = dataSoundscape$nShips[ii] - 1  }
    tp2 = as.numeric( sapply(strsplit(as.character( dataSoundscape$mxDist[ii]),","), `[`, 2) )
    if(tp2/1000 > 20){ dataSoundscape$nShips[ii] =  dataSoundscape$nShips[ii] - 1  }
  }
  
}
#Check: sum( dataSoundscape$nShips == 1 ) #only 36 samples (hours) with ships within 20 km!!
colnames(dataSoundscape)[1] = "dateTime"
#as.data.frame(colnames(dataSoundscape))
dataSoundscapeT = dataSoundscape %>%
  select(dateTime, nShips, ice_concentration_20km, WaterLevelDiff, HourlyWindSpeed,
         Unk,Ice,Anth,
         Bmy,Dle,Oro,Eba,Hfa,Mbo,Bac,Uba,Bal,Ubi,
        `20`,`25`,`31.5`,`40`,`50`,`63`,`80`,`100`,`125`,`160`,`200`,`250`,`315`,`400`,`500`,`630`,`800`,`1000`,`1250`,`1600`,`2000`,`2500`,
        `3150`,`4000`,`5000`,`6300`,`8000`,`10000`)

cols=sapply(dataSoundscapeT, is.logical)
dataSoundscapeT[,cols] = lapply(dataSoundscapeT[,cols], as.numeric)
head(dataSoundscapeT)
as.data.frame (colnames(dataSoundscapeT) )
dataSoundscapeT$Bio = rowSums(dataSoundscapeT[,9:18])

## ACOUSTIC SCENE LABELS- wind categories ####
dataSoundscapeT$AS = 0 # no scene label
dataSoundscapeT = dataSoundscapeT[!is.na( dataSoundscapeT$HourlyWindSpeed ),] #lots of hours with out wind speed??

dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed > 20 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips == 0 ] = "1H"      #high wind + no bio + no ship
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed > 20 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips == 0]  = "2H+b"    #high wind + bio + no ships
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed > 20 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips > 0 ]  = "4H+b+s"  #high wind + bio + ships (no samples!)
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed > 20 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips > 0 ] =  "3H+s"    #high wind +  no bio + ships

dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed <= 20 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips == 0] =  "5L"   #low wind + no bio + no ships
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed <= 20 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips == 0]  = "6L+b"  #low wind + bio + no ships
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed <= 20 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips > 0 ] =  "7L+s"   #low wind + no bio + ships
dataSoundscapeT$AS [dataSoundscapeT$HourlyWindSpeed <= 20 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips > 0 ]  = "8L+b+s"  #low wind +  bio + ships

#boxplot of acoustic scenes- only one frequency shown
ggplot(dataSoundscapeT, aes(y= `125`, x = as.factor(AS) ) ) +
  geom_boxplot()+
  geom_jitter(shape =1 , position=position_jitter(), alpha = .1 ) +
  ggtitle("Arctic Acoustic Scenes- wind categories") +
  xlab("")+
  theme_minimal()

## ACOUSTIC SCENE LABELS- ice categories ####
dataSoundscapeT$ASi = 0 # no scene label
#hist(dataSoundscapeT$ice_concentration_20km)

dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km > .5 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips == 0 ] = "1H"      #high ice + no bio + no ship
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km > .5 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips == 0]  = "2H+b"    #high ice + bio + no ships
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km > .5 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips > 0 ]  = "4H+b+s"  #high ice + bio + ships (no samples!)
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km > .5 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips > 0 ] =  "3H+s"    #high ice + no bio + ships (no samples!)
 
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km <= .5 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips == 0] =  "5L"   #low ice + no bio + no ships
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km <= .5 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips == 0]  = "6L+b"  #low ice + bio + no ships
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km <= .5 & dataSoundscapeT$Bio == 0 & dataSoundscapeT$nShips > 0 ] =  "7L+s"   #low ice + no bio + ships
dataSoundscapeT$ASi [dataSoundscapeT$ice_concentration_20km <= .5 & dataSoundscapeT$Bio > 0  & dataSoundscapeT$nShips > 0 ]  = "8L+b+s"  #low ice +  bio + ships

unique(dataSoundscapeT$ASi)

#boxplot of acoustic scenes- only one frequency shown
ggplot(dataSoundscapeT, aes(y= `125`, x = as.factor(ASi) ) ) +
  geom_boxplot()+
  geom_jitter(shape =1 , position=position_jitter(), alpha = .1 ) +
  ggtitle("Arctic Acoustic Scenes- ice categories") +
  xlab("")+
   theme_minimal()+
  theme(text = element_text(size = 20))

#tile plot of Acoustic scenes over time
ggplot(dataSoundscapeT, aes(x = dateTime, y = as.factor(ASi) , fill = `125`) ) +
  ggtitle("Arctic Acoustic Scenes- ice categories") +
  geom_tile() +
  xlab("")+ ylab("")+
  theme_minimal()+
  theme(text = element_text(size = 20))

#spectra of acoustic scenes
# as.data.frame(colnames(dataSoundscapeT))
spllab = dataSoundscapeT[,c(1,49, 19:46)]
spllab = data.frame(spllab, stringsAsFactors = F)
nms = colnames( spllab) [3:30]
NvMO = reshape :: melt(spllab, id.vars = c("dateTime","ASi"), measure.vars =  nms)
NvMO$FQ = as.numeric(as.character(gsub("X","",NvMO$variable)))

ggplot(NvMO, aes(FQ, value, group = as.factor(dateTime) , color = ASi))+ 
  geom_line(alpha=.04) + 
  scale_x_continuous(trans = "log10")+ 
  xlab("") + ylab("")+
  theme_minimal()+
  theme(text = element_text(size = 20))+ 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


#table of samples per acoustic scene
aggregate(dataSoundscapeT$`125`, by=list(dataSoundscapeT$ASi), mean, na.rm=T)
dataSoundscapeT %>% count(ASi)

