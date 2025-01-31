# How does your sanctuary compare?
# based on sanctsound data

rm(list=ls()) 

dataDir = "F:\\SanctSound\\"
outDir  = "F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\"

# LOAD ONMS Metadata ####
metaFile = paste0(outDir,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
sites = lookup[lookup[,7] == "shelf" ,] #oceanographic setting
sites[,5]

# McKenna et al 2021 paper, nearshore shelf sites
siteInterest = c("hi01", "sb01", "gr01","fk02","oc01","mb01","ci01","oc02","mb02") 