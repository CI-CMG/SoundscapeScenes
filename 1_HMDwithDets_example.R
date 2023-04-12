# 1) Label and Integrate 1-min hybrid-milli-decade data with event based detections
# eg. bio_species Code

# Event detections: detection periods with start and end time

# 2) Label into specific predefined acoustic scene categories
# low-frequency- frequency range 100- 500 Hz (Kait is 500 Hz, so limiting, why not 1 or 2 kHz)
# bio is very generic
# bio + anthro
# anthro
# unknown?

# 3) Label with context variables: ice (1-4), wind (1 or 2), AIS  (bring in once acoustic scene analysis)
# this is a generic need in community- is labeling data- wind and event-based detections?
# axiom help with data integration- no just visualization + data products
# show example of wind and what I needed do to get to this

# Directory with daily HMD files #### 
inFile = choose.files() 
inData = read.csv(inFile)
pr = sapply(strsplit(basename( inFile ), "_"), "[[", 1)
st = sapply(strsplit(basename( inFile ), "_"), "[[", 2)
dy = as.Date ( gsub(".csv","", sapply(strsplit(basename( inFile ), "_"), "[[", 6)
                    ), format="%Y%m%d" )
cat("Processing... ",st," on " ,as.character( dy) )
## FORMATTING ####
# Date format: format the date (? will netCDF files be the same?)
inDataT$TimeStamp = as.POSIXct( gsub(".000Z", "", gsub("T", " ", inDataT$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" )
# Frequency range: truncate to 100-2000 Hz
fq = as.numeric(as.character( gsub("PSD_","", colnames(inData[2:ncol(inData)] )) ) ) 
st = which(fq == 100) +1 
ed = which(fq == 2000) +1 
inDataT = as.data.frame( inData[,c(1,st:ed)] )
rm(ed,st,inFile,inData)

# Directory with daily detection files #### 
inDir= choose.dir() #all detections
list.dirs(inDir)
## FORMATTING ####
 