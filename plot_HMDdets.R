# PLOT HMDdet

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()

load( inFile)
as.data.frame( HMDdet %>% group_by(Category) %>% tally() )
