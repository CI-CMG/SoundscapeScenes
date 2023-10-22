# De-noising methods for ambient samples 
# reads in all 1-min HMD data with AS labels
# selects only Ambient samples, and runs RRPCA
# saves out updated persite files

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(viridis)
library(tidyverse)
library(rsvd)

#GET DATA ####
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T)
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T, recursive = T)

pltf = 0
fqr  = "LF"  #append this to output names
site = "All"
DC = Sys.Date()
dirOut = "F:\\SanctSound\\analysis\\combineFiles_AcousticScene"

#PREP DATA ####
Ambient = NULL
for (f in 1: length(inFiles)-1 ) { # f = 6 for testing
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  HMDdet$Site = st
  Ambient = rbind (Ambient, HMDdet[HMDdet$Category == "Ambient",] )
}

#CHECKS
unique(Ambient$Site)
length( which(is.na(Ambient)))
save(Ambient, file = paste0(dirOut, "\\Ambient_HMD_allSites_", DC, ".Rda") )

idNA = ( which(is.na(Ambient))) # check for NAs, as.data.frame( colnames( Ambient )[1:10] )
idx  = grep("^X", colnames(Ambient))
hix  = as.numeric( gsub("X","", names(Ambient)[idx]) )
Nv   =  Ambient[, idx]  #dB values
NvP  = 10^(Nv/20)     #pressure values
nvDate = Ambient$dateTime

# truncate to 100-1kHz Acoustic Scene??
fe =  which(hix == 1001.2)
NvPt = NvP[,1:fe]
Nv   = Nv[ ,1:fe]
hix  = hix[1:fe]

#ANALYSIS ####
# Robust principal components analysis separates a matrix into a low-rank plus sparse component
#a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L and a sparse component S
# input = ( NvP ) 
lamd = max(NvPt)^-0.5 #default settings
nvpcaTOL = rrpca(NvPt)
sampleHours = nrow(NvP)
save(nvpcaTOL, file = paste0(dirOut, "\\RRPCA_HMD_allSites_", DC, ".Rda") )

# START HERE TO LOAD FILES
load( paste0(dirOut, "\\Ambient_HMD_allSites_2023-08-15.Rda") )

idx  = grep("^X", colnames(Ambient))
hix  = as.numeric( gsub("X","", names(Ambient)[idx]) )
Nv   =  Ambient[, idx] #dB values
NvP  = 10^(Nv/20)     #pressure values
NvP  = NvP[,1:698]
Nv   = Nv[,1:698]
hix  = hix[1:698]

load( paste0(dirOut, "\\RRPCA_HMD_allSites_2023-08-14.Rda" )) 

#SUMMARIZE OUTPUT ####
#low rank
Lr = as.data.frame(nvpcaTOL$L) 
colnames(Lr) = hix
LrDB = 10*log10( Lr^2 )  #CHECK: min(LrDB$`63`), no negative values, just values without transients
colnames(LrDB) = hix
#sparse matrix
Sp = as.data.frame(nvpcaTOL$S) 
colnames(Sp) = hix
SpDB = 10*log10( (Sp)^2 ) # negative and zero values-- does not make sense to convert back to dB
colnames(SpDB) = hix

# Values for each minute ####
# sum of difference across frequencies for each minute
LRdiff = as.data.frame ( rowSums( abs ( (LrDB - Nv) ) ) )
colnames(LRdiff) = 'LRdiff'
LRfq   = as.data.frame ( as.numeric ( colnames(LrDB) [apply(LrDB, 1, (which.max) )] ) )
colnames(LRfq) = 'LRfq'
# sum of sparce across frequencies for each minute
SPsum = as.data.frame  ( rowSums( abs ( Sp ) ) )
colnames(SPsum) = 'SPsum'

#RECOMBINE WITH Site Files ACOUSTIC SCENE FILES ####
Ambient$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
Ambient$Sparce  = as.numeric( as.character(SPsum$SPsum  ) )
Ambient$LRfq = LRfq$LRfq

RRPCAsumOUT = NULL
for (f in 1: length(inFiles) ) { # f = 6 for testing
  
  # all HMD data for a site
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  HMDdet$Site = st
  
  # WHY ARE THEIR DUPLICATED ROWS!!! with different values
  # which(  duplicated( HMDdet$dateTime))

  # only Ambient data for a specific site
  tmp =  Ambient[ Ambient$Site == st, c(1, 1006:1008)]
  # nrow( HMDdet[HMDdet$Category == "Ambient", ]) 
  
  # merge data to include rrpca analysis results to HMD data
  tst =  merge(HMDdet, tmp, by = "dateTime", all.x = T)
  #hist( tst$LRfq )
  #hist( tst$LowRanK )
  #hist( tst$Sparce )
  RRPCAsum = as.data.frame ( rbind ( quantile(tst$LowRanK, na.rm = T),
  quantile(tst$LRfq, na.rm = T),
  quantile(tst$Sparce, na.rm = T) ) )
  RRPCAsum$Site = st
  RRPCAsum$RRPCAmetric = c("LR-sum","LR-freq","SP-sum")
    RRPCAsumOUT = rbind(RRPCAsumOUT, RRPCAsum )
  # write out new HMD+ file per site
    
    save(tst, file = paste0(dirOut, "\\HMDdetsRpca_", st, "_", DC, ".Rda") )
    

 }
RRPCAsumOUT


## NOT USED ####
# TRUNCATE TO FIGURE OUT WHAT IS GOING ON ####
tLrDB = LrDB[1:100,]
tSpDB = Sp[1:100,]
tAm   = Nv[1:100,]
tnvDate = Ambient$dateTime[1:100]

#original matrix
NvMO   = reshape :: melt(t(tAm)  )
head(NvMO)
NvMO$X1 = as.numeric( gsub("X","", NvMO$X1) )
pO = ggplot(NvMO, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ 
  scale_x_continuous(trans = "log10")+ 
  xlab("Frequency (HMD)") + ylab("1-min HMD")+
  theme_minimal()
pO
#LR matrix
LRMO   = reshape :: melt(t(tLrDB)  )
head(LRMO)
LRMO$X1 = as.numeric( gsub("X","", LRMO$X1) )
pL = ggplot(LRMO, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ 
  scale_x_continuous(trans = "log10")+ 
  xlab("Frequency (HMD)") + ylab("1-min HMD")+
  theme_minimal()
pL
#SP matrix
SPMO   = reshape :: melt(t(tSpDB)  )
head(SPMO)
SPMO$X1 = as.numeric( gsub("X","", SPMO$X1) )
pS = ggplot(SPMO, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ 
  scale_x_continuous(trans = "log10")+ 
  xlab("Frequency (HMD)") + ylab("1-min HMD")+
  theme_minimal()
pS


#RECOMBINE WITH DAILY Site ACOUSTIC SCENE FILES ####
# not complete
inFilesDay = list.files( inDir, pattern = "DAILY_MILLIDEC_MinRes_LFAS.csv", full.names = T)

for (f in 1: length(inFilesDay)) { # f = 6 for testing
  
  zmp = read.csv ( inFilesDay[f])
  
  st =  sapply(strsplit(basename( inFilesDay[f]), "_"), "[[", 1) #site name
  
}


# IS there sparce detection present-- how to summmarize?
library(dplyr)
idx = 2
iN = subset(NvMO, X2 == idx)
iL = subset(LRMO, X2 == idx)
iS = subset(SPMO, X2 == idx)
ggplot() + 
  geom_line(data=iN, aes(x=X1, y = value), color= "red") + 
  geom_line(data=iL, aes(x=X1, y=value), color = "blue") + 
  geom_line(data=iS, aes(x=X1, value), color = "green") +
  theme_minimal()

# what is the difference between ambient and low rank?
NvL = abs ( diff(iN$value - iL$value) )
hist( NvL )
quantile(abs( SPMO$value) )
sum(NvL)

# sum of difference across frequencies for each minute
LRdiff = as.data.frame ( rowSums( abs ( (tLrDB - tAm) ) ) )

# sum of sparce across frequencies for each minute
SPsum = hist ( rowSums( abs ( tSpDB ) ) )

