
rm(list=ls())

library(ggplot2)
library(tidyverse)

freqs = 100:797

dirIn = choose.dir()
setwd(dirIn)
inFiles = list.files(pattern = ".rds")

f = 1
monthly_rrpca = readRDS( inFiles [f])
parts <- strsplit(inFiles [f], "_")[[1]]
site = parts[4]
month = as.Date( gsub(".rds", "", parts[5]) )
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
colnames(L50low2)[-1] <- freqs
df_longL <- L50low2 %>%
  pivot_longer(
    cols = -percentile,
    names_to = "frequency",
    values_to = "value"
  )

df_longL$frequency <- as.numeric(df_longL$frequency)
                                     
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
pL


df_long = readRDS( inFiles [2])
df_longL$frequency = df_long$frequency

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
    title = "Sound Level Percentile Comparison",
    x = "Frequency (Hz)",
    y = "Sound Level"
  ) +
  theme_minimal()
