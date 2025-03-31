rm(list=ls())

# DIRECTORIES ####
typ = "products/sound_level_metrics"
gcpDir  = paste0("gs://noaa-passive-bioacoustic/onms/",typ) 
localData = "F:\\ONMS"
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outputDir = paste0( outDir,"products\\onms\\")
outDirC = paste0( outDir,"context\\") #context
outDirP = paste0( outDir,"products\\onms\\")#products
outDirG = paste0( outDir,"report\\" ) #graphics
outputProject = paste0( outDir,"products\\feature\\")

# GET LIST OF FILES on GCP ####
command = "gsutil"
args =  c("ls", gcpDir)
subdirs = system2(command, args, stdout = TRUE, stderr = TRUE) 
args_subdirs <- c("ls", "-d", paste0(gcpDir, "/*/"))  # "-d" ensures only directories
subdirs <- system2(command, args_subdirs, stdout = TRUE, stderr = TRUE)
all_files <- lapply(subdirs, function(subdir) {
  system2(command, c("ls", subdir), stdout = TRUE, stderr = TRUE)
})
all_files <- unlist(all_files)
print(all_files)

outData = NULL
outData$site = sapply( strsplit(basename(all_files), "_"), "[[",2)
tmp = sapply( strsplit(basename(all_files), "_"), "[[",3)
outData$start =  as.Date (sapply( strsplit(tmp, "-"), "[[",1), format = "%Y%m%d")
outData$end=  as.Date (sapply( strsplit(tmp, "-"), "[[",2), format = "%Y%m%d")
outData = as.data.frame(outData)

POI = c( as.Date("2023-06-01"), as.Date("2023-07-01") )
outdateT = outData[outData$start <= POI[1] & outData$end >= POI[2], ]
outdateT

# Function to count days per month
count_days_per_month <- function(start, end) {
  seq_dates <- seq(start, end, by = "day")  # Expand date range
  tibble(month = floor_date(seq_dates, "month")) %>%  # Group by month
    count(month)  # Count days
}

# Apply function to each row and combine results
result <- outData %>%
  rowwise() %>%
  do(count_days_per_month(.$start, .$end)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(days_count = sum(n))  # Aggregate counts across ranges

result$month[ which.max(result$days_count) ]

# SUBSAMPLE DATA ####
#only in specific directories- 
dirs = list.dirs(localData, recursive = FALSE)  # Get all directories
matched_dirs = dirs[grepl(paste(outdateT$site, collapse = "|"), dirs)] 

for (ii in 1:length (matched_dirs) ) {
  
  inFiles = list.files( matched_dirs[ii], pattern = "MinRes", recursive = T)
  inFiles = inFiles[!grepl(".png",inFiles) ]
  inFiles = inFiles[!grepl(".csv",inFiles) ]
  inFiles = inFiles[!grepl("_netCDF",inFiles) ]
  filDates = as.Date( sapply( strsplit(basename(inFiles), "_"), "[[",5), format= "%Y%m%d")
  idxF = which(filDates >= POI[1] & filDates <= POI[2])
  inFiles = inFiles[idxF]
  
  setwd( matched_dirs[ii] )
  for( ff in 1:length(inFiles) ){
    tmpFile = inFiles[ff]
    
    file.copy(from = normalizePath( tmpFile ), to = "F:\\ONMS\\feature")
  }
  
}

