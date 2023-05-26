# compare SPL data for specific datetimes in different MANTA versions
library(ggplot2)
library (plotly)

inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()
load( inFile)
HMD_12 = HMDdet
HMD_12$Day = as.Date( HMD_12$dateTime )


inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_AcousticScene\\"
inFile = choose.files()
load( inFile)
HMD_13 = HMDdet
HMD_13$Day = as.Date( HMD_13$dateTime )

st =  sapply(strsplit(basename( inFile), "_"), "[[", 2) #site name
dpl = gsub(".Rda", "", sapply(strsplit(basename( inFile ), "_"), "[[", 3) ) # deployment name
efq = ncol(HMDdet)-5
fq = as.numeric(as.character( gsub("X","", colnames(HMDdet[2:efq] )) ) ) # Frequency range: truncate to 100-2000 Hz



dy = "2019-04-11"
tmpD_12 = HMD_12[ HMD_12$Day == dy, c(1,2)] 
tmpD_13 = HMD_13[ HMD_13$Day == dy, c(1,2)] 


p = ggplot() +
  geom_line(data = tmpD_13, aes(x = dateTime , y = X100), color = "green", linetype = "dotted") +
  geom_line(data = tmpD_12, aes(x = dateTime , y = X100), color = "red", linetype = "dotted") +
  theme_minimal()
p
ggplotly(p)


# some missmatch of values at 10:00:00.. this is when data were dropped-

#values are correct at 
tmpD_12[ tmpD_12$dateTime == "2019-04-11 10:07:00 GMT" ,] # red-- missing data 9:59-10:06
tmpD_13[ tmpD_13$dateTime == "2019-04-11 10:07:00 GMT" ,] # green
