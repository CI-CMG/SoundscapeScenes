
library(xlsx)

site  = c("sb01","sb03")
inDir = "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\context\\"
metaFile = paste0(inDir,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = "Summary") )
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )

siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]

siteDetails = as.data.frame ( read.xlsx(metaFile, sheetIndex = "SB") )
siteDetails = siteDetails[rowSums(is.na(siteDetails)) != ncol(siteDetails), ]
siteDetails = siteDetails[, colSums(is.na(siteDetails)) != nrow(siteDetails)]

siteText = as.data.frame ( read.xlsx(metaFile, sheetIndex = "Standard Report Text") )
siteText = siteText[rowSums(is.na(siteText)) != ncol(siteText), ]
siteText = siteText[, colSums(is.na(siteText)) != nrow(siteText)]

