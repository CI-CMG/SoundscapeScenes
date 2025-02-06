# How does your sanctuary compare?
# based on sanctsound data

rm(list=ls()) 
library(xlsx)
library(PAMscapes)
library(lubridate)
library(dplyr)
library(reshape)
library(ggplot2)

dataDir = "F:\\SanctSound\\"
outDir  = "F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\"

# LOAD ONMS Metadata ####
habitat = "shelf"
siteFocus = "MB02"
metaFile = paste0(outDir,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
sites = lookup[lookup[,7] == habitat ,] #oceanographic setting
siteInterest = sites[,5]
# McKenna et al 2021 paper, nearshore shelf sites
#siteInterest = c("hi01", "sb01", "gr01","fk02","oc01","mb01","ci01","oc02","mb02") 
FOI = as.data.frame ( read.xlsx(metaFile, sheetIndex = 3) )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOIs = FOI [ FOI$Sanctuary == substr(tolower( siteFocus), 1,2), ]

## SanctSound FILES - ERDAP ####
# NOTE - might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles = list.files(path = dataDir, pattern = "TOL_1h", full.names = T, recursive = T)
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
inFiles = filesTOL[!grepl("/analysis/", filesTOL)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[sapply(inFiles, function(x) any(grepl(paste(toupper( siteInterest ), collapse = "|"), x)))]

sData = NULL
yoi = 2019
for (ii in 1:length(inFiles)) { # ii = 1
  tmpFile = inFiles[ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  site = sapply( strsplit(basename(tmpFile), "[_]"), "[[",2)
  tmp = loadSoundscapeData( inFiles[ii], extension = typ)
  tmp$site = site
  tmp$yr = year(tmp$UTC)
  tmp = tmp[tmp$yr == yoi, ]
  
  if (ii > 1) {
    col_mismatched = setdiff(colnames(tmp), colnames(sData))
    tmpc = tmp[, !colnames(tmp) %in% col_mismatched]
  } else {
    tmpc = tmp
  }

  if(nrow(tmpc)>0 ) {
    cat( inFiles[ii], "Start = ", as.character( as.Date( min(tmpc$UTC) ) ),"\n")
    sData = rbind(sData, tmpc)
  }
  
}
sData$mth= month(sData$UTC)

#Find month with most data
month_counts = ( ( sData %>% count(mth) ))
moi = month_counts %>% filter(n == max(n))
sDatat = sData[sData$mth == moi$mth,]
unique(sDatat$site)

#Percentiles for each site
tol_columns = grep("TOL", colnames(sDatat))
site_split = split(sDatat, sDatat$site) # Calculate quantiles for each site
season_quantiles = lapply(site_split, function(sDatat) {
  apply(sDatat[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
})

seasonAll = NULL
for (ii in 1: length(season_quantiles) ) {
  tmp = as.data.frame ( season_quantiles[ii] ) 
  colnames(tmp) = colnames(sDatat)[tol_columns]
  tmp$Quantile = rownames(tmp)
  tmp$Season = names(season_quantiles)[ii]
  rownames(tmp) = NULL
  seasonAll = rbind(seasonAll,tmp)
}

tol_columns = grep("TOL", colnames(seasonAll))
mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
colnames(mallData) = c("Quantile", "Site", "Frequency" , "SoundLevel" )
mallDataS = mallData[ mallData$Site == siteFocus, ]
names(seasonAll)

start_points = mallData %>%
  group_by(Site, Quantile) %>%
  slice_min(order_by = Frequency, n = 1)
start_points = start_points[start_points$Quantile == "50%", ]

ggplot() +
  #median TOL values
  geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Site), linewidth = 1) +
  geom_line(data = mallDataS[mallDataS$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 3) +
  #scale_color_manual(values = c("Winter" = "#56B4E9", "Spring" = "#009E73", "Summer" = "#CC79A7", "Fall" = "#E69F00")) +
  # Add vertical lines at FQstart
  geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  # Add labels at the bottom of each line
  geom_text(data = FOIs, aes(x = FQstart, y = 65, label = Label), angle = 90, vjust = 1, hjust = -.2, size = 3) +
  scale_x_log10() +  
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 16, face = "bold", hjust = 0)) +  # This line removes the legend
  labs(
    title    = paste0("How does my sanctuary compare? "),
    subtitle = paste0( siteFocus, " compared to other ", habitat, " sites in ",month.abb[moi$mth],"-", yoi ),
    caption  = "Representative month from SanctSound Project", 
    x = "Frequency Hz",
    y = expression(paste("Sound Levels (dB re 1", mu, " Pa third-octave bands)" ) )
  ) +
geom_text(data = start_points, aes(x = Frequency, y = SoundLevel, label = Site), 
          vjust = 0, hjust = 1, size = 3, color = "black")

#month with max samples for each site
category_counts <- sData %>% count(mth, site)
category_counts %>% 
  group_by(site) %>% 
  slice_max(n, n = 1, with_ties = FALSE) %>% 
  ungroup()


