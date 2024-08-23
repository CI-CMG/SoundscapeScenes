rm(list=ls()) 
library(ncdf4)
library(ggplot2)
library(reshape2)  # For reshaping the data for ggplot2
library(lubridate)

frq_range = c(100, 1997.6)

## READ in HMD netCDF files ####
fileM = "F:\\SoundCoop\\hmd_downloadedGCP\\test\\NRS11_H5R6.1.5000_20200518_DAILY_MILLIDEC_MinRes_v2.nc"
ncM = nc_open(fileM)
fM =  t( as.data.frame( ncvar_get(ncM, "frequency")   ) )
fM <- round(fM,  1)
psdM = t ( as.data.frame( ncvar_get(ncM, "psd")   ) )
timeM = as.data.frame( as.POSIXct( ncvar_get(ncM, "time"), origin = "1970-01-01", tz = "UTC") )
dataM = cbind(timeM,psdM)
colnames(dataM) = c("dateTime",fM)
# qualityMat <- ncvar_get(nc, "quality_flag")   
# target_value <- target_valueIn  
# indices <- which(qualityMat == target_value, arr.ind = TRUE)
# row_indices <- indices[, 1]
# col_indices <- indices[, 2]
# myPSD[row_indices, col_indices] <- NA

fileP = "F:\\SoundCoop\\hmd_downloadedGCP\\test\\NRS11_20200518.nc"
ncP = nc_open(fileP)
fP =  t( as.data.frame( ncvar_get(ncP, "frequency")   ) )
fP <- round(fP, 1)
psdP = t ( as.data.frame( ncvar_get(ncP, "psd")   ) )
timeP = as.data.frame( as.POSIXct( ncvar_get(ncP, "time"), origin = "1970-01-01", tz = "UTC") )
dataP = cbind(timeM,psdP)
colnames(dataP) = c("dateTime",fP)


# compare frequency
# need to round to the nearest 10th for PyPAM
fM = round(fM,  1)
fP = round(fP, 1)
rmovef = setdiff(fM, fP) # these are in M but not P... has lower and higher frequencies included
setdiff(fP, fM) # these are in P but not M

# TRIM DATA
# Check matching column names
common_cols = intersect(colnames(dataM), colnames(dataP))
M_filtered = dataM[, common_cols, drop = FALSE]
P_filtered = dataP[, common_cols, drop = FALSE] # round this to 1
P_filtered[, 2:ncol(P_filtered)] =  round( P_filtered[, 2:ncol(P_filtered) ] , 1)

# compare psd
diff_df = ( M_filtered - P_filtered )
diff_df$dateTime = M_filtered$dateTime
melted_df <- melt(diff_df, id.vars = "dateTime", variable.name = "Column", value.name = "Value")
melted_df$fq= as.numeric(as.character( melted_df$Column ))
melted_df$ValueR =  round(melted_df$Value, 3)
melted_df$Hr = hour(melted_df$dateTime)

library(plotly)
p = ggplot(melted_df, aes(x = fq, y = dateTime, fill = ValueR)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",        # Color for the lowest values
    mid = "white",       # Color for the midpoint value
    high = "red",        # Color for the highest values
    midpoint = 0,        # Midpoint of the gradient
    limits = c(-1.32, 0.608)  # Range of values to cover
  ) +
  labs(title = "Heatmap of Differences", x = "Columns", y = "Dates") +
  facet_wrap(~Hr,ncol = 5) +
  theme_minimal()

#p
#ggplotly(p)


p = ggplot(melted_df, aes(x = ValueR, y = ..density..)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ Hr, ncol = 3) +  # Facet by hour and column
  labs(title = "Smoothed Distribution of differnces by Hour", x = "dB", y = "") +
  theme_minimal()
ggplotly(p)

#results- difference in dB
# over all quantile difference
quantile(melted_df$ValueR, c(1, 0.99, 0.75, .5, .25, 0.05,0))
# quantile differences by hour
quantiles_df <- melted_df %>%
  group_by(Hr) %>%
  summarise(
    q100 = quantile(ValueR, .1, na.rm = TRUE),
    q99 = quantile(ValueR, 0.99, na.rm = TRUE),
    q75 = quantile(ValueR, 0.75, na.rm = TRUE),
    q50 = quantile(ValueR, 0.50, na.rm = TRUE),
    q25 = quantile(ValueR, 0.25, na.rm = TRUE),
    q05 = quantile(ValueR, 0.05, na.rm = TRUE),
    q00 = quantile(ValueR, 0, na.rm = TRUE)
  )
quantiles_df

#results- median difference by frequency
median_df <- melted_df %>%
  group_by(fq) %>%
  summarise(
    q99 = quantile(ValueR, 0.99, na.rm = TRUE),
    q50 = quantile(ValueR, 0.50, na.rm = TRUE),
    q01 = quantile(ValueR, 0.01, na.rm = TRUE)
    
  )

# View the resulting data frame with medians for each hour and `fq` group
print(median_df)
# Conclusion - most of error is at 00 hour, across all frequencies

#results- median values by frequency
quantiles <- c(0, 0.25, 0.5, 0.75, 0.99, 1)
M_filteredpsd = M_filtered[, 2:ncol(M_filtered)]
M_quantiles = as.data.frame( apply(M_filteredpsd, 2, function(x) quantile(x, probs = quantiles, na.rm = TRUE)) )
M_quantiles$Quantile=rownames(M_quantiles)
M_quantilesM=melt(M_quantiles, id.vars = "Quantile", variable.name = "Column", value.name = "Value")
M_quantilesM$fq = as.numeric(as.character( M_quantilesM$Column) )
M_quantilesM$method = "manta"

P_filteredpsd = P_filtered[, 2:ncol(M_filtered)]
P_quantiles = as.data.frame( apply(P_filteredpsd, 2, function(x) quantile(x, probs = quantiles, na.rm = TRUE)) )
P_quantiles$Quantile <- rownames(P_quantiles)
P_quantilesM=melt(P_quantiles, id.vars = "Quantile", variable.name = "Column", value.name = "Value")
P_quantilesM$fq = as.numeric(as.character( P_quantilesM$Column) )
P_quantilesM$method = "pypam"

p = ggplot()+
  geom_line(data = P_quantilesM[P_quantilesM$Quantile == "50%",], aes(x = fq, y = Value), color = "blue", linewidth = .3 ) +
  geom_line(data = P_quantilesM[P_quantilesM$Quantile == "75%",], aes(x = fq, y = Value), color = "blue", linewidth = .3, alpha = .5, linetype = "dashed") +
  geom_line(data = P_quantilesM[P_quantilesM$Quantile == "25%",], aes(x = fq, y = Value), color = "blue", linewidth = .3, alpha = .5, linetype = "dashed" ) +
  geom_line(data = M_quantilesM[M_quantilesM$Quantile == "50%",], aes(x = fq, y = Value), color = "orange", linewidth = .3 ) +
  geom_line(data = M_quantilesM[M_quantilesM$Quantile == "75%",], aes(x = fq, y = Value), color = "orange", linewidth = .3, alpha = .5, linetype = "dashed") +
  geom_line(data = M_quantilesM[M_quantilesM$Quantile == "25%",], aes(x = fq, y = Value), color = "orange", linewidth = .3, alpha = .5, linetype = "dashed" ) +
  scale_x_log10() +
  theme_minimal()
  
ggplotly(p)

