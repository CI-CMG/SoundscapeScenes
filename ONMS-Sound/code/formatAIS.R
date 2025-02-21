rm(list=ls()) 

#AIS combine data
rm(list=ls()) 
outDir =  "F:\\CODE\\GitHub\\SoundscapeScenes\\ONMS-Sound\\" 
outDirC = paste0( outDir,"context\\") #context

# only run the first time #### 
## ONMS data ####
AIStran = read.csv(paste0(outDirC, "smp_v2_transits_data.csv") )
aisONMS  = do.call(rbind, strsplit(AIStran$loc_id.transit_id.segment.mmsi.imo.name.callsign.type.loa.recs.avg_sog_dw.min_sog.max_sog.dist_nm.op_hrs.start_time_utc.end_time_utc, "\\|"))
colnames(aisONMS) <- c("loc_id", "transit_id", "segment", "mmsi", "imo", "name", "callsign", "type", 
                     "loa", "recs", "avg_sog_dw", "min_sog", "max_sog", "dist_nm", "op_hrs", 
                     "start_time_utc", "end_time_utc")
aisONMS <- as.data.frame(aisONMS, stringsAsFactors = FALSE)
unique( aisONMS$loc_id )
aisONMS$Start = as.POSIXct( gsub("[+]00", "", aisONMS$start_time_utc), tz = "GMT" ) 
aisONMS$End   = as.POSIXct( gsub("[+]00", "", aisONMS$end_time_utc), tz = "GMT" )
rm(AIStran)

##SanctSound data ####
AIStran = read.csv(paste0(outDirC, "SanctSound_smp_transit_data_updateFormat.csv") )
aiSS <- as.data.frame(AIStran, stringsAsFactors = FALSE)
aiSS$Start = as.POSIXct( gsub("[+]00", "", aiSS$start_time_utc), tz = "GMT" ) 
aiSS$End   = as.POSIXct( gsub("[+]00", "", aiSS$end_time_utc), tz = "GMT" )
aisONMS = rbind(aisONMS,aiSS)
aisONMS$Buffer = 10
rm(AIStran, aiSS)

##NRS data - ####
#not same formate!!!
aisNRS = read.csv(paste0(outDirC, "nrs_transit_dataf.csv") )
aisNRS <- as.data.frame(aisNRS, stringsAsFactors = FALSE)
aisNRS$Start = as.POSIXct( gsub("[+]00", "", aisNRS$start_time_utc), tz = "GMT" ) 
aisNRS$End   = as.POSIXct( gsub("[+]00", "", aisNRS$end_time_utc), tz = "GMT" )
aisNRS$Buffer = 20
aisONMS = rbind(aisONMS,aisNRS)
save(aisONMS, file = paste0(outDirC, "Combine_ONMS_AIStransits_dataF.Rda") )

# run for new data #### 
## new dataa ####
newONMS = read.csv(paste0(outDirC, "smp_v2_transits_data.csv") )
newONMS  = do.call(rbind, strsplit(newONMS$loc_id.transit_id.segment.mmsi.imo.name.callsign.type.loa.recs.avg_sog_dw.min_sog.max_sog.dist_nm.op_hrs.start_time_utc.end_time_utc, "\\|"))
colnames(newONMS) <- c("loc_id", "transit_id", "segment", "mmsi", "imo", "name", "callsign", "type", 
                       "loa", "recs", "avg_sog_dw", "min_sog", "max_sog", "dist_nm", "op_hrs", 
                       "start_time_utc", "end_time_utc")
newONMS <- as.data.frame(newONMS, stringsAsFactors = FALSE)
newONMS$Start = as.POSIXct( gsub("[+]00", "", newONMS$start_time_utc), tz = "GMT" ) 
newONMS$End   = as.POSIXct( gsub("[+]00", "", newONMS$end_time_utc), tz = "GMT" )
newONMS$Buffer = 10

## previously combined data ####
AIStran = load(paste0(outDirC, "Combine_ONMS_AIStransits_dataF.Rda") )
