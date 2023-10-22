# check for files
SOI = "SB03"

dirTop = "F:\\SanctSound" # ?? create loop through all manta directories ??
detFiles = list.files(path = dirTop, pattern =SOI, full.names = T, recursive = T)
detFiles = detFiles[grepl("ships.csv", detFiles)] #remove metadata
detFilesB = unique( basename(detFiles ) )
order( as.numeric ( sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", detFilesB), "_"), "[[", 3)  ) )
