#separate HMD files by seasonal condition before running cluster analysis
rm(list=ls()) 

# IN DATA ####
siteN = "AU_CH01"
netcdf = paste0("F:\\SoundCoop\\hmd_downloadedGCP\\",siteN)
inFiles = list.files(netcdf,pattern = "*.nc", full.names = T)

gdrive = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\"
hmddrive = "SoundCoop_AcousticScene\\CombineData\\A_outputHMDDETS\\"

# HMD
inFileDates = as.Date( sapply(strsplit(basename(inFiles), "_"), "[[", 4), format="%Y%m%d")  

## HMD+ ####
dirIn =  paste0(gdrive, hmddrive, siteN)  #list.files(dirIn)
load(paste0(dirIn,"\\", siteN, "_HmdDetsAS"))
cat("HMD data: ", as.character(min(AS$dateTime)),  as.character(max(AS$dateTime)) )
AS$yr = year(AS$dateTime)
AS$dy = as.Date(AS$dateTime)
useason = unique(AS$season)
AS$season2 = "unk"
for (ss in 2:length(useason)) {
  # all unique days in specific season
  dates_to_match = unique( AS$dy[AS$season == useason[ss] ] ) 
  filestoMove = which(inFileDates %in% dates_to_match)
  destination_files = inFiles[filestoMove]
  destination_dir = paste0( netcdf, "\\",useason[ss])
  if (!file.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  
  file.copy(destination_files, destination_dir)
  #AS$season2[AS$dy %in% dates_to_match] = useason[ss]
} # unique(AS$season2)


# get season dates (truncate clustering and RRPCA analysis)
tal = as.data.frame( AS %>% group_by(season) %>% tally() )
tal
evn = c("open","break","form","ice","notFilled")
AS1 = AS[AS$season == evn[1], ]
cat(evn[1], ": ", as.character(min(AS1$dateTime)),  as.character(max(AS1$dateTime)) )
AS2 = AS[AS$season == evn[2], ]
cat(evn[2], ": ", as.character(min(AS2$dateTime)),  as.character(max(AS2$dateTime)) )
AS3 = AS[AS$season == evn[3], ]
cat(evn[3], ": ", as.character(min(AS3$dateTime)),  as.character(max(AS3$dateTime)) )
AS4 = AS[AS$season == evn[4], ]
cat(evn[4], ": ", as.character(min(AS4$dateTime)),  as.character(max(AS4$dateTime)) )
AS5 = AS[AS$season == evn[5], ]
cat(evn[5], ": ", as.character(min(AS5$dateTime)),  as.character(max(AS5$dateTime)) )

dirIn =  paste0(gdrive, hmddrive, siteN)  #list.files(dirIn)
load(paste0(dirIn,"\\", siteN, "_HmdDetsAS"))

# old version ####
# #get season dates (truncate clustering and RRPCA analysis)
# evn = c("open","break","form","ice","notFilled")
# 
# for (ii in 1:length(evn)){
#   AS1 = AS[AS$season == evn[ii], ]
#   tdates = unique( as.Date( AS1$dateTime ) )
#   filestoMove = which(inFileDates %in% tdates)
#   destination_files = inFiles[filestoMove]
#   destination_dir = paste0( netcdf, "\\",evn[ii])
#   if (!file.exists(destination_dir)) {
#     dir.create(destination_dir, recursive = TRUE)
#   }
#   file.copy(destination_files,destination_dir)
#   
#   
# }