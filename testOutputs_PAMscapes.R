# testing converted files
rm(list=ls()) 
#one file test
#chu = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\ncConversion\\data\\Chuck - NRS01_H5R6B.1.5000_20180831_DAILY_MILLIDEC_MinRes_v3.nc"
#kan = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\ncConversion\\data\\Kanishka 11-11 NRS01_H5R6B.1.5000_20180831_DAILY_MILLIDEC_MinRes_v3.nc"
#new = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\ncConversion\\data\\output file.nc"


# NRS01_H5R6B.1.5000_20180831_DAILY_MILLIDEC_MinRes_v3-2.nc- good
# NRS01_H5R6B.1.5000_20180901_DAILY_MILLIDEC_MinRes_v3.nc - good
# NRS01_2022_H5R6.1.5000_20200916_DAILY_MILLIDEC_MinRes_v3.nc -  corrupt data
# NRS01_2022_H5R6.1.5000_20200917_DAILY_MILLIDEC_MinRes_v3.nc -  corrupt data


chu = "G:\\My Drive\\ActiveProjects\\CIRES\\Manta Conversion Project\\output - chuck\\NRS01_2022_H5R6.1.5000_20200917_DAILY_MILLIDEC_MinRes_v3.nc"
kan = "G:\\My Drive\\ActiveProjects\\CIRES\\Manta Conversion Project\\output - test\\NRS01_2022_H5R6.1.5000_20200917_DAILY_MILLIDEC_MinRes_v3.nc"
#new = "G:\\.shortcut-targets-by-id\\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\\SoundCoop_AcousticScene\\ncConversion\\data\\output file.nc"

#devtools::install_github('TaikiSan21/PAMscapes')
library(PAMscapes)
Inchu = as.data.frame( loadSoundscapeData( chu, extension = typ)  )
#Innew   = as.data.frame( loadSoundscapeData( new, extension = typ) )
Inkan   = as.data.frame( loadSoundscapeData( kan, extension = typ) )

cDataC = binSoundscapeData(Inchu, bin = "1hour", method = c("median") )
#cDataN = binSoundscapeData(Innew, bin = "1hour", method = c("median") )
cDataK = binSoundscapeData(Inkan, bin = "1hour", method = c("median") )

cDataC$HMD_12
#cDataI$HMD_12
cDataK$HMD_12

