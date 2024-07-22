#separate HMD files by seasonal condition before running cluster analysis
rm(list=ls()) 

# CH01 ####
siteN = "AU_CH01"
netcdf = paste0("F:\\SoundCoop\\hmd_downloadedGCP\\",siteN)
setwd(netcdf)
dirs = list.dirs(full.names = TRUE, recursive=TRUE )
destination_dir = paste0( netcdf, "\\subSet")
#dir.create(destination_dir, recursive = TRUE)
n = 16

for (ss in 3:length(dirs)-1 ) {
  tmpFiles = list.files(dirs[ss], full.names = T)
  cat(dirs[ss], ":", length(tmpFiles), "\n")
  
  if (length(tmpFiles) > 16 ) {
    # Generate a list of 16 random numbers between 1 and length of files
    random_numbers <- sample(1:length(tmpFiles), n, replace = FALSE)
    destination_files = tmpFiles[random_numbers]
    file.copy(destination_files, destination_dir)
    
  } else {
    destination_files = tmpFiles
    file.copy(destination_files, destination_dir)
  }
}

# SB03 ####
siteN = "SB03"
netcdf = paste0("F:\\SoundCoop\\hmd_downloadedGCP\\",siteN)
setwd(netcdf)
#dirs = list.dirs(full.names = TRUE, recursive=TRUE )
destination_dir = paste0( netcdf, "\\subSet")
dir.create(destination_dir, recursive = TRUE)
tmpFiles = list.files(paste0( netcdf, "\\all"), full.names = T)
n = round( length(tmpFiles) *.15 )

# Generate a list of 16 random numbers between 1 and length of files
random_numbers <- sample(1:length(tmpFiles), n, replace = FALSE)
destination_files = tmpFiles[random_numbers]
file.copy(destination_files, destination_dir)

# CASE STUDY #2  ####
netcdf = paste0("F:\\SoundCoop\\hmd_downloadedGCP\\")
setwd(netcdf)
siteN = c("AU_CH01/ALL","NRS11", "AEON5", "PManan", "SB03/all","Monh")
combined_pattern <- paste(siteN, collapse = "|")
dirs = list.dirs(full.names = TRUE, recursive=TRUE )
matching_dirs <- dirs[grepl(combined_pattern, dirs)]
# get 20 days from each site...
n = 20
destination_dir = paste0( netcdf, "subSet_CaseStudy2")
dir.create(destination_dir, recursive = TRUE)
for (ss in 1:length(matching_dirs) ) {
  tmpFiles = list.files(matching_dirs[ss], full.names = T)
  random_numbers <- sample(1:length(tmpFiles), n, replace = FALSE)
  destination_files = tmpFiles[random_numbers]
  file.copy(destination_files, destination_dir)
}

# number of samples to process in clustering...will need to subsample int mk_TPWS step tp
# 30,0000 minutes but too hard to do in this step because day files, maybe best to subsample minutes in the mkTPWS step?
n * length(matching_dirs) * 1440
