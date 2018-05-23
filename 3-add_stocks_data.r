source("2-clean_merge_tow_data.r")

nstocks <- length(adios.data[,1])

for (istock in 1:nstocks){
  adios <- read.csv(paste0("data/",adios.data$adios_file_name[istock]), header = TRUE)
  adios <- filter(adios, 
                  YEAR >= 2009,
                  SURVEY %in% c("NMFS spring BTS", "NMFS fall BTS")) %>%
    mutate(CatchWt = ifelse(is.na(CATCH_WT_CAL), 0, CATCH_WT_CAL)) %>%
    select(Cruise = CRUISE6,
           Station = STATION,
           Strata = STRATUM,
           Tow = TOW,
           DEPTH,
           CatchWt) %>%
    mutate(ID = adios.data$ID[istock],
           Stock_short = adios.data$Stock_short[istock])
  
  merged_adios_final_tow <- merge(adios, final_tow, by=c("Cruise", "Strata", "Tow", "Station"))
  
  if (istock == 1) withcatch <- merged_adios_final_tow
  if (istock > 1) withcatch <- rbind(withcatch, merged_adios_final_tow)
}
dim(withcatch)

# create variables for catch per area using standard wing area and tow specific wing area
withcatch <- withcatch %>%
  mutate(Cstandard = CatchWt / standard_wing_swept_area,
         Cwingarea = CatchWt / wing_area_swept)

