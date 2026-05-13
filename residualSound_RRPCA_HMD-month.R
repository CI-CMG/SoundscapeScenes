# De-noising methods HMD files for a site
# Soundscape tool - 
# (1) residual soundscape is the predictable part of the soundscape so the sensory conditions for a species/species group

#devtools::install_github('TaikiSan21/PAMscapes')
#library(devtools)

rm(list=ls())
library(PAMscapes)
library(lubridate)
library(data.table)
library(dplyr)
library(rsvd)
library(reshape2)
library(ggplot2)

# PARAMS 
fqr = 1001.2  # only process low frequency part of spectra
exten = ".nc" # file extension
DC = Sys.Date()

# MM drive local
gdrive  = "C:/Users/megan/Documents/Data/" # "C:/Users/mckenna/Documents/Data/" # C:\Users\megan\Documents\Data
siteIn  = "PMEL_AK_202009_NRS01" # site to process, change to the folder name
dirIn   = paste0( gdrive, siteIn )
MOI = "2021-07"

# SH drive MAKARA
#siteIn = "PMEL_AK"
#gdrive = paste0( "W:/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/",siteIn,"/") #nmfs GCP HMD netCDFs

#GET DATA ####
# e.g.NEFSC_SBNMS_201811_SB03_20181112.nc PMEL_CHNMS_202311_NRS13_20231109.nc
inFiles = list.files(dirIn, pattern = exten, recursive = T, full.names = T )
dirOut  = dirIn

# truncate "inFiles" list to unique year-months
uDays = as.Date(sapply( strsplit(basename(inFiles), "_"), "[[", 5), format = "%Y%m%d")
uMonths = as.Date(format(uDays, "%Y-%m-01"))
files_by_month = split(inFiles, uMonths) 

# Samara- does this work if you have more than one month in this file structure?
# files_by_month = files_by_month[1] 

#check column names
all_files <- unlist(files_by_month, use.names = FALSE)
col_list <- lapply(all_files, function(f) {
  dat <- loadSoundscapeData(f, keepQuals = c(1,2))
  names(dat)
})
reference_cols <- col_list[[1]]
same_cols <- sapply(col_list, function(x)
  identical(x, reference_cols)
)
same_cols
problem_files <- all_files[!same_cols]

lapply(which(!same_cols), function(i) {
  list(
    file = all_files[i],
    missing = setdiff(reference_cols, col_list[[i]]),
    extra   = setdiff(col_list[[i]], reference_cols)
  )
})

required_cols <- c("HMD_100", "HMD_1001")
missing_required <- sapply(col_list, function(x)
  !all(required_cols %in% x)
)
all_files[missing_required]

#combine monthly data
all_monthly_data <- rbindlist(
  
  lapply(names(files_by_month), function(m) {
    
    rbindlist(
      
      lapply(files_by_month[[m]], function(f) {
        
        dat <- loadSoundscapeData(f, keepQuals = c(1,2))
        
        hmd_cols <- grep("^HMD_", names(dat), value = TRUE)
        freqs <- as.numeric(sub("HMD_", "", hmd_cols))
        
        dat <- dat[, hmd_cols[freqs >= 100 & freqs <= 1001], drop = FALSE]
        
        # add metadata
        dat$month <- m
        dat$file  <- basename(f)
        
        dat
      }),
      
      use.names = TRUE,
      fill = TRUE
    )
    
  }),
  
  use.names = TRUE,
  fill = TRUE
)


#all_monthly_data
df = as.data.frame(all_monthly_data)
num_dat = df[, sapply(df, is.numeric)] #numeric data
freqs = as.numeric(sub("HMD_", "", colnames(num_dat)))
valid = !is.na(freqs)
trunc_dat = num_dat[, valid & freqs <= fqr]
pressure_dat = 10^(trunc_dat / 20)

#RRPCA METHOD------------------------------------------------------------------
#after you run the RRPCA once save out the results and then uncomment the rrpca because it takes for ever
monthly_rrpca = rrpca(pressure_dat)
setwd(dirIn)
saveRDS(monthly_rrpca, file = paste0(dirOut, "/RRPCA_monthly_", siteIn, "_", DC, ".rds"))
monthly_rrpca = readRDS( paste0(dirIn, "/RRPCA_monthly_PMEL_AK_202009_NRS01_2026-04-23.rds") )
# separate output of the RRPCA results
L   = monthly_rrpca$L
S   = monthly_rrpca$S
err = monthly_rrpca$err
L = as.data.frame(L)

#PERCENTILES LOW RANK ----------------------------------------------------------------------
percentiles = as.data.frame( apply(L, 2, quantile, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) )
L50low = 20 * log10(percentiles) # convert back to dB 
L50low2 = L50low %>% as.data.frame() %>%
  rownames_to_column("percentile") #add column with percentile value


colnames(L50low2)[-1] <- freqs # rename ONLY frequency columns
df_longL <- L50low2 %>%
  pivot_longer(
    cols = -percentile,
    names_to = "frequency",
    values_to = "value"
  )

df_longL$frequency <- as.numeric(sub("HMD_", "", df_longL$frequency) )


# PERCENTILES OF LOW RANK
pL = ggplot(df_longL,
       aes(x = frequency,
           y = value,
           color = percentile,
           group = percentile)) +
  geom_line(linewidth = 1.2) +
  scale_x_log10() +
  labs(
    title = "LOW RANK PERCENTILES",
    x = "Frequency (Hz)",
    y = "Sound Level",
    color = "Percentile"
  ) +
  theme_minimal()
#PERCENTILES END ----------------------------------------------------------------------

#SPECTROGRAM ----------------------------------------------------------------------
#df_long = melt(L)
#colnames(df_long) = c("time_idx", "freq_idx", "value")
#df_long$value = 20 * log10(df_long$value) # convert back to dB 
#need to convert the index for time and frequency to actual values
#df_long$UTC   = seq(as.POSIXct("2020-07-01 00:00:00", tz = "UTC"),
                  # by = "1 min", length.out = length( unique(df_long$time_idx) ))
#df_long$freq  = freqs[df_long$freq_idx]

#Make a spectrogram of monthly low rank matrix
# ggplot(df_long, aes(x = UTC, y = freq, fill = value)) +
#   geom_tile() +
#   scale_y_log10() + 
#   scale_fill_viridis_c(limits = c(60, 120), oob = scales::squish) + # control dynamic range
#   labs(title = paste0("Low-Rank Spectrogram (L)-", MOI),
#        x = "Time", y = "Frequency (Hz)", fill = "Pressure") +
#   theme_minimal()
# # weird banding.... need to blend tiles?
#SPECTROGRAM END ----------------------------------------------------------------------

# PERCENTILES OF SOUND LEVELS ----------------------------------------------------------------------
#plot with percentiles and median Low rank result - 
trunc_datP = 10^(trunc_dat / 20)
percentiles = as.data.frame( apply(trunc_datP, 2, quantile, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) )
df2 = 20 * log10(percentiles) # convert back to dB 
df <- df2 %>%
  as.data.frame() %>%
  rownames_to_column("percentile")
colnames(df)[-1] <- freqs
df_long <- df %>%
  pivot_longer(
    cols = -percentile,
    names_to = "frequency",
    values_to = "value"
  )
df_long$frequency <- as.numeric(sub("HMD_", "", df_long$frequency) ) #as.numeric(df_long$frequency)
pP = ggplot(df_long,
       aes(x = frequency,
           y = value,
           color = percentile,
           group = percentile)) +
  geom_line(linewidth = 1.2) +
  scale_x_log10() +
  labs(
    title = "SOUND LEVEL PERCENTILES",
    x = "Frequency (Hz)",
    y = "Sound Level",
    color = "Percentile"
  )+
  theme_minimal()

saveRDS(df_long, file = paste0(dirOut, "/RRPCA_OrigPercentilesMonthly_", siteIn, "_", DC, ".rds"))

library(gridExtra)
grid.arrange(pL, pP, nrow = 1)


library(patchwork)
(pP + pL) +
  plot_annotation(title = "Soundscape Percentile Comparison")


df_long$type  <- "Sound Level Percentiles"
df_longL$type <- "Low Rank Percentiles"
df_all <- bind_rows(df_long, df_longL)
scale_color_grey()
ggplot(df_all,
       aes(x = frequency,
           y = value,
           color = percentile,
           linetype = type,
           group = interaction(percentile, type))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "25%" = "grey60",
      "50%" = "black",
      "75%" = "grey60"
    )
  ) + 
  # scale_color_grey(start = 0.1, end = 0.7) +
  scale_x_log10() +
  labs(
    title = "Sound Level Percentile Comparison",
    x = "Frequency (Hz)",
    y = "Sound Level"
  ) +
  theme_minimal()

# PERCENTILES OF SOUND LEVELS ----------------------------------------------------------------------
# INTERPRET: going from qualitative to quantitative interpretations
#Narrative: Rather than interpreting the proportion of energy explained by the low-rank component, 
# we focus on how the low-rank structure evolves over time and where the sparse component 
# introduces meaningful deviations, particularly at higher percentiles and during transient events.
# RRPCA: What portion of the soundscape is persistent and structured versus transient and event-driven?
# low rank -- anything predictable, repeating, or slowly varying
# sparse -- anything rare, abrupt, or high-intensity relative to background

#(1) How does the background (L) evolve over time?
# How does the distribution of the original data compare to the “background” (low-rank L) for each month?
# “background soundscape trajectory”
L_db <- 20 * log10(x$L)
L_mean_time <- rowMeans(L_db, na.rm = TRUE)
#INTERPRET: tracking the mean for each month

#(2) When do deviations (S) occur and how strong are they?
deviation_ratio <- abs(x$S) / (x$L + 1e-10) #relative to S
mean_deviation <- mean(deviation_ratio, na.rm = TRUE)
#INTERPRET: small = mostly structured soundscape, large = lots of transient disruption

#(3) Where do L and original diverge? How much of the loud end of the soundscape is event-driven?
p95_orig <- quantile(x$orig, 0.95)
p95_L    <- quantile(x$L, 0.95)
event_contribution <- p95_orig - p95_L
# INTERPRET: 

#(4) How often do events dominate?
E_L_t <- rowSums(x$L^2)
E_S_t <- rowSums(x$S^2)
event_dominant <- E_S_t > E_L_t
mean(event_dominant) 
#INTERPRET: 0.1, events dominate 10% of the time
# does this vary by frequency range? shipping in low? spikes in high?

# How do I beark it into less arbirtray 

#rolling mean of L trend over 6 hours



# previous  code #### -----------------------------------------

# Run RRPCA on each month (only if lots of data) 
monthly_rrpca = lapply(df, function(dat) {
  
  # numeric only
  num_dat = dat[, sapply(dat, is.numeric)]
  
  # truncate by frequency
  freqs = as.numeric(colnames(num_dat))
  valid = !is.na(freqs)
  trunc_dat = num_dat[, valid & freqs <= fqr]
  
  # convert to pressure
  pressure_dat = 10^(trunc_dat / 20)
  
  # run rrpca
  rrpca(pressure_dat)
})

RRPCAsumOUT = NULL # summary of percentiles for each site
# LOOP through sites ####
for (f in 1: length(inFiles)) { # f = 1
  
  load( inFiles[f])
  
  AS = read.csv(paste0(dirIn,"\\", siteN, "_HmdDetsAS.csv"))
  AS = mutate(AS, season = ifelse(season == "notFilled", "no label", season))
  AS$season <- factor(AS$season, ordered = TRUE, levels = c("form", "ice", "break", "open","no label"))
  
  load( inFiles[f])
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  HMDdet$Site = st
  
  idNA = ( which(is.na(HMDdet))) # check for NAs, as.data.frame( colnames( Ambient )[1:10] )
  idx  = grep("^X", colnames(HMDdet))
  hix  = as.numeric( gsub("X","", names(HMDdet)[idx]) )
  Nv   =  HMDdet[, idx]  #dB values
  NvP  = 10^(Nv/20)     #pressure values
  nvDate = HMDdet$dateTime
  
  ## truncate to 100-1kHz ####
  fe =  which(hix == 1001.2)
  NvPt = NvP[,1:fe]
  Nv   = Nv[ ,1:fe]
  hix  = hix[1:fe]
  
  ## RRPCA ####
  # Robust principal components analysis separates a matrix into a low-rank plus sparse component
  #a method for the robust separation of a rectangular (m, n) matrix A into a low-rank component L and a sparse component S
  # input = ( NvP ) 
  lamd = max(NvPt)^-0.5 #default settings
  nvpcaTOL = rrpca(NvPt)
  sampleHours = nrow(NvP)
  save(nvpcaTOL, file = paste0(dirOut, "\\RRPCA_HMD_allSites_", st, "_", DC, ".Rda") )
  
  ## (option to load rrpca results here) ####
  
  ## RRPCA results ####
  #low rank
  Lr = as.data.frame(nvpcaTOL$L) 
  colnames(Lr) = hix
  LrDB = 10*log10( Lr^2 )  #CHECK: min(LrDB$`63`), no negative values, just values without transients
  colnames(LrDB) = hix
  #sparse matrix
  Sp = as.data.frame(nvpcaTOL$S) 
  colnames(Sp) = hix
  SpDB = 10*log10( (Sp)^2 ) # negative and zero values-- does not make sense to convert back to dB
  colnames(SpDB) = hix
  
  ## RRPCA thresholds ####
  # sum of difference across frequencies for each minute
  LRdiff = as.data.frame ( rowSums( abs ( (LrDB - Nv) ) ) )
  colnames(LRdiff) = 'LRdiff'
  LRfq   = as.data.frame ( as.numeric ( colnames(LrDB) [apply(LrDB, 1, (which.max) )] ) )
  colnames(LRfq) = 'LRfq'
  # sum of sparce across frequencies for each minute
  SPsum = as.data.frame  ( rowSums( abs ( Sp ) ) )
  colnames(SPsum) = 'SPsum'
  
  ## label files ####
  HMDdet$LowRanK = as.numeric( as.character(LRdiff$LRdiff ) )
  HMDdet$Sparce  = as.numeric( as.character(SPsum$SPsum  ) )
  HMDdet$LRfq = LRfq$LRfq
  
  ## percentile for thresholds ####
  RRPCAsum = as.data.frame ( rbind ( quantile(HMDdet$LowRanK, na.rm = T),
                                     quantile(HMDdet$LRfq, na.rm = T),
                                     quantile(HMDdet$Sparce, na.rm = T) ) )
  RRPCAsum$Site = st
  RRPCAsum$RRPCAmetric = c("LR-sum","LR-freq","SP-sum")
  RRPCAsumOUT = rbind(RRPCAsumOUT, RRPCAsum )
  
  ## write out new HMD+ ####
  save(HMDdet, file = paste0(dirOut, "\\HMDdets_RpcaSite_", st, "_", DC, ".Rda") )
 
} # end site loop

save(RRPCAsum, file = paste0(dirOut, "\\RRPCAsum_bySite", "_", DC, ".Rda") )
