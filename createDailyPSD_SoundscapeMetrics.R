# Parse large CSV files from SanctSound Soundscape Metrics
rm(list=ls()) 
library(data.table)
library(lubridate)

# get all PSD files
inDir   = choose.dir(default = "F:\\SanctSound\\AcousticScene_1min" , caption = "directory with PSD csv files" ) # CI03_04
inFiles = list.files(inDir, pattern = "_PSD_1min.csv", full.names = T, recursive = T)


ii = 1
# file naming
inFile = inFiles[ii] 
outDir = dirname(inFile)   
fname = gsub(".csv", "", basename(inFile) )

# read in first column to get the dates
dat <- fread(inFile, select = 1)
dat$TimeStamp = as.POSIXct( dat$`yyyy-mm-ddTHH:MM:SSZ`, tz = "GMT" )
dat$dy = as.Date(dat$TimeStamp)
udy = as.data.frame( unique(dat$dy) ) 
colnames(udy) = "Date"

# read in heading
hd = as.data.frame (read.table(inFile, head = TRUE,  nrows = 1, sep = ",")[- 1, ] )


for (dd in 1:nrow(udy)) {
  cat("Processing... ", ii, "(of ", length(inFiles), ") Day ", as.character(udy$Date[dd]), ":", dd , "(of ", nrow(udy), ") \n")

  udy$start_col[dd] =  min( which(dat$dy == udy$Date[dd]) ) # start row
  udy$end_col[dd] =  max( which(dat$dy == udy$Date[dd]) )   # end row
  
  # read in part of files 
  tmp = as.data.frame ( read.table(inFile, skip = min( which(dat$dy == udy$Date[dd]) ), nrow= max( which(dat$dy == udy$Date[dd]) ), header=T, sep=",") )
  rw1 = t( as.data.frame( gsub("X", "", colnames(tmp) ) ) )
  colnames(rw1) = colnames(hd)
  colnames(tmp) = colnames(hd)
  
  tmp2 = rbind(rw1, tmp)
  row.names(tmp2) = NULL
 # tmp2[1:3,1:10]
  
  # write our a daily .csv file
  write.csv(tmp2, paste0(outDir, "//", fname, "_", udy$Date[dd], ".csv")  ) 
  
  rm(tmp, tmp2)
}


