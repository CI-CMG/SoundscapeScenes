
rm(list=ls())

library(ggplot2)
library(tidyverse)

dirIn = choose.dir()
setwd(dirIn)
inFiles = list.files(pattern = ".rds")

parts = NULL
for (f in seq_along(inFiles)) {
  parts[[f]] <- strsplit(inFiles[f], "_")[[1]] }


f = 1
f2 = 2

monthly_rrpca = readRDS( inFiles [f])
df_long = readRDS( inFiles [f2])
df_long$frequency = as.numeric( df_long$frequency )

parts <- strsplit(inFiles [f], "_")[[1]]
site = parts[6]
mth = ( parts[5])
L   = monthly_rrpca$L
S   = monthly_rrpca$S
err = monthly_rrpca$err
L = as.data.frame(L)

#PERCENTILES LOW RANK ----------------------------------------------------------------------
percentiles = as.data.frame( apply(L, 2, quantile, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) )
L50low = 20 * log10(percentiles) # convert back to dB 
L50low2 = L50low %>%
  as.data.frame() %>%
  rownames_to_column("percentile") #add column with percentile value


# rename ONLY frequency columns
freqs = df_long$frequency
colnames(L50low2)[-1] <- freqs
df_longL <- L50low2 %>%
  pivot_longer(
    cols = -percentile,
    names_to = "frequency",
    values_to = "value"
  )
df_longL$frequency = as.numeric( df_longL$frequency )

#PLOT______________________________________________________________________
df_longL$type <- "Low Rank Percentiles"
df_long$type  <- "Sound Level Percentiles"

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
    title = "", # "Sound Level Percentile Comparison",
    x = "Frequency (Hz)",
    y = "Sound Level dB re 1 µPa²/Hz",
    caption = paste0(site, " ", mth )
  ) +
  theme_minimal((base_size = 16))+ 
  theme(legend.position = "none")
 



#difference between SL - LR---------------------------------------------------------------
# positive means that SL is higher
df_cbind <- bind_cols(df_long, df_longL)
df_cbind$df_diff =  df_cbind$value...3 - df_cbind$value...7
df50 = df_cbind[ df_cbind$percentile...5 == "75%",]
ggplot(df50, aes(x = df_diff))+
  geom_histogram(bins = 30) +
  xlim(-1,1)+
  labs(
    x = "dB above residual soundscape (median)",
    y = "",
    caption = paste0(site, " ", mth )
  ) +
  theme_minimal((base_size = 16))

AK01 = df_cbind
