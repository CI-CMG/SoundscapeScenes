# COMPILE SOUNDSCAPE METRICS

# assumes data are already downloaded from cloud, stored locally
# runs for SanctSound and SoundCoop data files (combines)
# runs one site at a time
# checks for files already processed
# adds wind estimate from PAMscapes for any new data

# outputs hourly TOLs values with wind speed and list of files processed

rm(list=ls()) 
library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(xlsx)

# SET UP PARAMS ####
DC = Sys.Date()
site  = "MB02" #MB02
site = tolower(site) # "mb01"

dirSS = "F:\\SanctSound" # SANCTSOUND
dirGCP = paste0( "F:/ONMS/", site,"/") #NCEI GCP 
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context
outDirP = paste0( outDir,"products\\", substr(tolower(site),start = 1, stop =2),"\\" )#products
outDirG = paste0( outDir,"report\\" ) #graphics

# LOAD ONMS Metadata ####
metaFile = paste0(outDirC, "ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
siteInfo
# CHECK FOR PROCESSED FILES #### 
pFile = list.files(path = (outDirP), pattern = paste0("filesProcesed_",site), full.names = T, recursive = T)
if (length(pFile) > 0 ) {
  load(pFile)
  cat("Already processed ", length(processedFiles), " files for ", site)
  
  # files have been processed- read in data and append
  inFile = list.files((outDir), pattern = paste0("data_", site, "_HourlySPL-gfs_\\d{4}-\\d{2}-\\d{2}\\.Rda$"), full.names = T, recursive = T)
  file_info = file.info(inFile)
  load( inFile[which.max(file_info$ctime)] )
  aData = gps
  rm(gps)
  
  # for testing remove first and last
  # processedFiles[1]
  # ed = (length(processedFiles))-1
  # processedFiles = processedFiles[1:ed]
  # processedFiles[1]
  
  
} else {
  cat("No processed files for ", site)
  processedFiles = NULL
  aData = NULL # this is the already processed data that needs to be binded to new data
}

## SanctSound FILES - ERDAP ####
# NOTE- might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles = list.files(path = dirSS, pattern = toupper(site), full.names = T, recursive = T)
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
inFiles = filesTOL[!grepl("/analysis/", filesTOL)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!basename(inFiles) %in% processedFiles]
cat("processing ", length(inFiles), "new files")
sData = NULL
if (length(inFiles) > 0 ) {
  for (ii in 1:length(inFiles)) { # ii = 3
    
    tmpFile = inFiles[ii]
    typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
    tmp = loadSoundscapeData( inFiles[ii], extension = typ)
    cat( inFiles[ii], "Start = ", as.character( as.Date( min(tmp$UTC) ) ),"\n")
    sData = rbind(sData, tmp)
  }
  sData$site = tolower(site)
  sData$yr   = year(sData$UTC)
  sData$mth  = month(sData$UTC)
  inFilesS = inFiles
}

## Manta Files- NCEI ####
# download before running... 
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/nrs/products/sound_level_metrics/11 F:/ONMS/nrs11
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/mb01 F:/ONMS/mb01
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/sb01 F:/ONMS/sb01
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/oc02 F:/ONMS/oc02
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/mb02  F:\ONMS\mb02
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/pm01 F:/ONMS/pm01
# gsutil -m rsync -r gs://noaa-passive-bioacoustic/onms/products/sound_level_metrics/pm02 F:/ONMS/pm02

inFiles = list.files(dirGCP, pattern = "MinRes", recursive = T, full.names = T)
inFiles = inFiles[!grepl(".png",inFiles) ]
inFiles = inFiles[!grepl(".csv",inFiles) ]
inFiles = inFiles[!grepl("_netCDF",inFiles) ]
inFiles = inFiles[!basename(inFiles) %in% processedFiles]
cat("processing ", length(inFiles), "new files")
cData = NULL  
if (length(inFiles) > 0 ) {
  for (f in 1:length(inFiles) ){
    ncFile = inFiles[f]
    hmdData = loadSoundscapeData(ncFile)
    tolData = createOctaveLevel(hmdData, type='tol')
    tolData$site = site
    cData = rbind( cData, tolData )
  }
  cData$yr  = year(cData$UTC)
  cData$mth = month(cData$UTC)
  min(cData$UTC)
  cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median") )
}

## COMBINE DATA ####
if( length(sData) == 0 & length(cData)==0 ){ # No new files
  
  cat("No new files to process...") 
  #nothing new saved
  
} else if ( length(sData) > 0 & length(aData) == 0 ) { 
  
  #SanctSound + ONMS data but no processed data 
  aData = NULL  
  sData$Latitude  = cDatah$Latitude[1]
  sData$Longitude = cDatah$Longitude[1]
  cData_mismatched = setdiff(colnames(cDatah), colnames(sData))
  cData_cleaned = cDatah[, !colnames(cDatah) %in% cData_mismatched]
  cData_cleaned = cData_cleaned[, colnames(sData)]
  aData = rbind(aData, cData_cleaned, sData)
  
  #SAVE DATA ####
  save(aData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL_", DC, ".Rda") )
  
  #SAVE FILES PROCESSED ####
  processedFiles  = c(basename(inFilesS), basename(inFiles)) 
  save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
  
  #GET WIND/WEATHER DATA
  gps = matchGFS(aData) #PAMscapes function that matches weather to all 
  save(gps, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
 # names(gps)
  # gps[is.na(gps$windMag),]
 
  } else if (length(sData) == 0 & length(cData) > 0) {  # just onms data
 
  
  if (length(aData) > 0){ #already processed data
    
    cData_mismatched = setdiff(colnames(cDatah), colnames(aData))
    cData_cleaned = cDatah[, !colnames(cDatah) %in% cData_mismatched]
    aData = rbind(aData, cData_cleaned)
    
    #SAVE DATA ####
    save(aData, file = file = paste0(outDirP, "data_", tolower(site), "_HourlySPL_", DC, ".Rda") )
    
    #SAVE FILES PROCESSED ####
    processedFiles  = c(basename(inFiles)) 
    save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
    
    #?? do not re-run this for already processed data ####
    #need to update
    #GET WIND/WEATHER DATA
    #gps = matchGFS(aData) #PAMscapes function that matches weather to all 
    #save(gps, file = paste0(outDir, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
    
  } else { #no already processed data
    
    aData = cDatah
    #SAVE DATA ####
    save(aData, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL_", DC, ".Rda") )
    #SAVE FILES PROCESSED ####
    processedFiles  = c(basename(inFiles)) 
    save(processedFiles, file = paste0(outDirP, "filesProcesed_", tolower(site), "_HourlySPL.Rda") )
    
    #GET WIND/WEATHER DATA
    gps = matchGFS(aData) #PAMscapes function that matches weather to all 
    save(gps, file = paste0(outDirP, "data_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  }
  
}

# EXTRA ####
# (below is just testing)
# ADD SEASON LABEL ####
# aData$Season[aData$mth == 12] = "winter" 
# aData$Season[aData$mth == 1] = "winter"
# aData$Season[aData$mth == 2] = "winter"
# aData$Season[aData$mth == 3] = "spring"
# aData$Season[aData$mth == 4] = "spring"
# aData$Season[aData$mth == 5] = "spring"
# aData$Season[aData$mth == 6] = "summer"
# aData$Season[aData$mth == 7] = "summer"
# aData$Season[aData$mth == 8] = "summer"
# aData$Season[aData$mth == 9] = "fall"
# aData$Season[aData$mth == 10] = "fall"
# aData$Season[aData$mth == 11] = "fall"
# seasons = unique(aData$Season)
# 
# # GET PERCENTILES of hourly medians values by SEASON and year ####
# tol_columns = grep("TOL", colnames(aData))
# unique_years = unique(aData$yr)
# aData$seasonYr = paste(aData$Season, aData$yr,sep = "-")
# unique( aData$seasonYr )
# YrSeasonData = NULL
# for (yy in 1:length(unique_years) ) { # yy = 1
#   # Get all unique seasons for the current year
#   unique_seasons = unique( aData$Season[aData$yr == unique_years[yy] ])
#   cat("seasons for ", unique_years[yy], ":", unique_seasons, "\n")
#   # Loop through each season for the current year
#   for (ss in 1:length(unique_seasons)) { # ss = 1
#     # Subset the data for the specific year and season
#     filtered_data = subset(aData, yr == unique_years[yy] & Season == unique_seasons[ss])
#     nHrs = nrow(filtered_data)
#     
#     # Select columns that contain "TOL"
#     tol_columns=grep("TOL", colnames(filtered_data))
#     
#     # Calculate quantiles for the specified columns
#     #quantiles_df = as.data.frame(apply(filtered_data[, tol_columns], 2, quantile, na.rm = TRUE))
#     quantiles_df = as.data.frame(apply(filtered_data[, tol_columns], 2, function(x) {
#       quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
#     }))
#     # Add columns to indicate the year and season
#     quantiles_df$Year = unique_years[yy]
#     quantiles_df$Season= unique_seasons[ss]
#     quantiles_df$Hours = nHrs
#     quantiles_df$Quantile = rownames((quantiles_df))
#     rownames(quantiles_df) <- NULL
#     
#     # Append the quantiles_df to the list
#     YrSeasonData = rbind(YrSeasonData, quantiles_df )
#   }
# }
# 
# # GET PERCENTILES of hourly medians values by SEASON ####
# seasonData = NULL
# for (ii in 1:length(seasons)){ # ii=1
#   
#   idx = which( aData$Season  == seasons[ii] )
#   nSamples = length(idx) # number of hours
#   
#   subset_data = aData[idx,]
#   quants = as.data.frame ( apply(subset_data[,tol_columns ], 2, quantile, na.rm = T) )
#   quants$Season = seasons[ii]
#   quants$Hours = nSamples
#   quants$Quantile = rownames((quants))
#   rownames(quants) <- NULL
#   
#   # combine so data site, season, quantiles, TOLs
#   seasonData = rbind(seasonData, quants )
#   
# }
# 
# # GET PERCENTILES of hourly medians values ALL DATA ####
# allData = as.data.frame ( apply(aData[,tol_columns ], 2, quantile, na.rm = T) )
# allData$Hours = nrow (aData)
# allData$Quantile = rownames((allData))
# rownames(allData) <- NULL
# 
# # SAVE DATA PARAMS ####
# st = as.Date( min(aData$UTC) )
# ed = as.Date( max(aData$UTC) )
# udays = length( unique(as.Date(aData$UTC)) )
# outDir = paste0(outputDir, tolower(site),"/" )
# 
# # PLOT and DATA : season per year ####
# # lines for each season, plot for each year with seasonal average
# # YrSeasonData
# tol_columns = grep("TOL", colnames(YrSeasonData))
# colnames(YrSeasonData)
# mYrSeasonData = melt(YrSeasonData, id.vars = c("Quantile","Season","Year"), measure.vars = tol_columns)
# colnames( mYrSeasonData )
# mYrSeasonData$variable = as.numeric( as.character( gsub("TOL_", "", mYrSeasonData$variable )))
# colnames(mYrSeasonData) = c("Quantile", "Season",   "Year" ,    "frequency" , "SoundLevel" )
# 
# p = ggplot()+
#   geom_line(data = mYrSeasonData[ mYrSeasonData$Quantile == "50%",], aes(x = frequency, y = SoundLevel, color = Season), size = 2 ) +
#   
#   facet_wrap(~Year,nrow = length(unique(YrSeasonData$Year))) +
#   scale_x_log10() +
#   scale_color_manual(values = c("fall" = "#dfc27d", "spring" = "#80cdc1","winter" = "#a6611a","summer" = "#018571") ) +
#   theme_minimal() +
#   labs(
#     title =  paste0("ONMS ", tolower(site), ": ", st, " to ", ed),
#     x = "Frequency Hz",
#     y = expression(paste("Measured Sound Levels (dB re 1", mu, " Pa)" ) )
#   )
# p
# 
# save(      mYrSeasonData, file = paste0(outDir, "data_", tolower(site), "_SeasonalSPL_", DC, ".Rda") )
# write.csv( mYrSeasonData, file = paste0(outDir, "data_", tolower(site), "_SeasonalSPL_", DC, ".csv") )
# ggsave(filename = paste0(outDir, "plot_", tolower(site), "_SeasonalSPL_", DC,  ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
# 
# # PLOT and DATA : all data ####
# #colnames( allData )
# tol_columns = grep("TOL", colnames(allData))
# mallData = melt(allData, id.vars = c("Quantile"), measure.vars = tol_columns)
# mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
# colnames(mallData) = c("Quantile", "frequency" , "SoundLevel" )
# 
# p = ggplot()+
#   geom_line(data = mallData[ mallData$Quantile == "50%",], aes(x = frequency, y = SoundLevel), size = 2 ) +
#   geom_line(data = mallData[ mallData$Quantile == "25%",], aes(x = frequency, y = SoundLevel), size = 1 ) +
#   geom_line(data = mallData[ mallData$Quantile == "75%",], aes(x = frequency, y = SoundLevel), size = 1 ) +
#   scale_x_log10() +
#   theme_minimal() +
#   labs(
#     title = paste0("ONMS ", tolower(site), ": ", st, " to ", ed),
#     x = "Frequency Hz",
#     y = expression(paste("Measured Sound Levels (dB re 1", mu, " Pa)" ) )
#   )
# p
# save(      mallData, file = paste0(outDir, "data_", tolower(site), "_AllSPL_", st,"-", ed, "_", DC, ".Rda") )
# write.csv( mallData, file = paste0(outDir, "data_", tolower(site), "_AllSPL_", st,"-", ed, "_", DC, ".csv") )
# ggsave(filename = paste0(outDir, "plot_", tolower(site), "_AllSPL_", st,"-", ed, "_", DC, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
# 
# # PLOT and DATA : difference year 1 per season ####
# #difference in the seasonal median values 
# #plotPSD(data, by='season', reference='Winter',returnData = TRUE)- not working yet!
# YrSeasonData50 = YrSeasonData[YrSeasonData$Quantile =="50%",]
# seasons = unique(YrSeasonData50$Season)
# dfya = NULL
# year1Season = NULL
# for (ss in 1:length(seasons)){
#   tmp = YrSeasonData50[YrSeasonData50$Season == seasons[ss], ]
#   yr1 = min( tmp$Year )
#   cat("year 1 for ", seasons[ss], " is ", yr1, "\n")
#   year1Season = rbind(year1Season, c( seasons[ss],yr1) )
#   yr1dt = tmp[tmp$Year == yr1,]
#   yrOdt = tmp[tmp$Year != yr1,] #other years
#   tol_columns = grep("TOL", colnames(yrOdt))
#   
#   for (yy in 1:nrow(yrOdt )){
#     tmp2 = as.data.frame( yr1dt[tol_columns] - yrOdt[yy, tol_columns] )
#     dfya = rbind(dfya, c( yrOdt$Year[yy], yrOdt$Season[yy], tmp2 ))
#   }
# }
# dfya = as.data.frame(dfya)
# colnames( dfya )[1] = "Year"
# colnames( dfya )[2] = "Season"
# df_clean = as.data.frame( dfya[!is.na(dfya$TOL_6300), ] )
# tol_columns = ( grep("TOL", colnames(df_clean)) ) 
# df_clean[tol_columns] = as.numeric(unlist( df_clean[tol_columns]) )
# df_clean[1] = as.numeric(unlist( df_clean[1]) )
# df_clean[2] = (unlist( df_clean[2]) )
# df_cleanM = melt(df_clean, id.vars = c("Year", "Season"), measure.vars = tol_columns )
# 
# df_cleanM$variable = as.numeric( as.character( gsub("TOL_", "", df_cleanM$variable )))
# colnames(df_cleanM) = c("Year", "Season", "frequency" , "SoundLevel" )
# 
# #add the year to the season- year1Season
# p = ggplot()+
#   geom_line(data = df_cleanM, aes(x = frequency, y = SoundLevel, color = as.factor( Year)) ) +
#   scale_x_log10() +
#   theme_minimal() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   facet_wrap(~Season) +
#   scale_color_manual(values = c("2019" = "red", "2020" = "orange","2021" = "yellow","2022" = "green","2023" = "blue") ) +
#   labs(
#     title = paste0("ONMS ", tolower(site), ": ", st, " to ", ed),
#     x = "Frequency (Hz)",
#     color = "",
#     y = expression(paste("Difference in Sound Levels from year 1 (dB)" ) )
#   )
# p
# 
# save(      df_cleanM, file = paste0(outDir, "data_", tolower(site), "_DiffSPL_", st,"-", ed, "_", DC, ".Rda") )
# write.csv( df_cleanM, file = paste0(outDir, "data_", tolower(site), "_DiffSPLL_", st,"-", ed, "_", DC, ".csv") )
# ggsave(filename = paste0(outDir, "plot_", tolower(site), "_DiffSPL_", st,"-", ed, "_", DC, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
# 
# 
