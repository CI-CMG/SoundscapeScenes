# OVERVIEW ACOUSTIC SCENE ANALYSIS 
# SITES: c( "CI04_02", "GR01_01", "HI04_02", "MB02_02", "OC02_02", "PM02_01","SB01_02", "SB03_08")

# PURPOSE:
""
# input: 
# output:

inDir   = "F:\\SanctSound\\" 
outDIR  = "F:\SanctSound\analysis\combineFiles_AcousticScene"
codeDir = "F:\CODE\GitHub\SoundscapeScenes"

# (0) PURPOSE: check detection files available for each site
"F:/CODE/GitHub/SoundscapeScenes/check4files.R"
# input: \\detections\\
# output: list of files

# (1) PURPOSE: label and Integrate 1-min hybrid-milli-decade (HMD) data with event based detections
"F:/CODE/GitHub/SoundscapeScenes/1_HMDwithLowFDets_generic_DayOut.R"
# input: "MinRes.csv", \\detections\\
# output: HMDdetLF_, HMDcheck

# (1.2) PURPOSE: plots of the spectra for each acoustic scene category
"F:/CODE/GitHub/SoundscapeScenes/plot_HMDdets.R"
# input: HMDdetLF
# output: HMDdetLF_Spectra_, ASspectraPer_

# (1.3) PURPOSE: plots of spectra by sites
"F:/CODE/GitHub/SoundscapeScenes/plot_HMDdets_allSites.R" 
# input: "HMDdetLF_Spectra"
# output: HMDdetLF_Spectra_, ASspectraPer_

# (not used) PURPOSE: de-noising methods for ambient samples, selects only Ambient samples, and runs RRPCA
"F:/CODE/GitHub/SoundscapeScenes/2_HMDwithLFAS_RRPCA.R"
# input: HMDdetLF
# output: Ambient_HMD_allSites_, "\\RRPCA_HMD_allSites_"

# (2) PURPOSE: de-noising methods for ambient samples, selects only Ambient samples, and runs RRPCA
"F:/CODE/GitHub/SoundscapeScenes/2_HMDwithLFAS_RRPCA_allData.R"
# input: HMDdetLF
# output: HMDdets_RpcaSite_, RRPCAsum_bySite
# THIS CODE PLOTS THESE OUTPUTS-
"F:/CODE/GitHub/SoundscapeScenes/plot_HMDdets_allSites_rrpcaBySite.R"

# (2.1) PURPOSE: plots of the spectra for each acoustic scene category with rrpca
"F:/CODE/GitHub/SoundscapeScenes/plot_HMDdets_allSites_rrpca.R"
# input: "HMDdetsRpca_"
# output: ASspectra_Anthro_, \\ASspectra_Bio_, \\ASspectra_BioAnthro_

# (3) PURPOSE: (not working) build training data sets for 
"F:/CODE/GitHub/SoundscapeScenes/3_subSampleHMD.R"
# input: "HMDdetsRpca"
# output: NOT COMPLETE!!

# SITES- AU_CHO1
# 1_HMD_withDETs-CH01 
# PURPOSE: add columns with detections types to the HDM netCDF files.. including season
# output: csv files for each day
# 1_HMD_withDETsAS-CH01 
# output: csv file with all data labeled 
