# Define the ERDDAP base URL and dataset name
erddap_base_url <- 'https://erddap.sensors.ioos.us/erddap'
erddap_dataset <- 'wmo_44013'
# Define the variables you want to retrieve
wind_var <- 'wind_speed'
swt_var <- 'sea_water_temperature'
wave_var <- 'sea_surface_wave_significant_height'
anomaly_var <- 'swt_anomaly'

# Get the time range from your sound data
time_start <- as.character(min(sound_ds$time))
time_end   <- as.character(max(sound_ds$time))

erddap_dataset_url <- paste0(
  erddap_base_url, '/tabledap/', erddap_dataset, '.csv',
  '?time,', wind_var, ',', swt_var, ',', wave_var, ',', anomaly_var,
  '&time>=', time_start, '&time<=', time_end
)
