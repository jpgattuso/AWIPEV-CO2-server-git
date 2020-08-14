if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
library("lubridate")
if (!require("hms")) install.packages("hms")
library(hms)
if (!require("oce")) install.packages("oce")
library(oce)
if (!require("xts")) install.packages("xts")
library(xts)
if (!require("curl")) install.packages("curl")
library(curl)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("tsibble")) install.packages("tsibble")
library(tsibble)
if (!require("stringr")) install.packages("stringr")
library(stringr)

####define who is the user and define path
rm(list = ls())

if (Sys.getenv("LOGNAME") == "gattuso") path = "../../pCloud\ Sync/Documents/experiments/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
if (Sys.getenv("LOGNAME") == "samir") path = "../../pCloud\ Sync/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
if (system('echo "$USER"', intern = TRUE) == "awipev") {
  setwd("/home/awipev/ny-alesund/") #to run on server
 path = "data/NRT_data/"
}

####Set environmental variables####
Sys.setenv(TZ="UTC")
##Provide in this section the parameters to be downloaded
#*********************************
#Use the following aggregation parameters
agg_time = "MINUTE" #SECOND, MINUTE, HOUR, DAY, MONTH, YEAR
#Calculate the following statistical values. 
agg_fun_1 = "MEAN" #MEDIAN
agg_fun_2 = "STDDEV"
agg_fun_3 = "N"
#*********************************

# # read past data 
# previous_NRT_data <- readRDS(file = paste0(path, "previous_NRT_data.rds")) %>% 
#   dplyr::mutate(datetime = ymd_hms(datetime)
#   )

# # This is to process the whole data set, from 2015
# if (file.exists(paste0(path, "all_nydata_minute.rds")) == FALSE) {
#   data <- previous_NRT_data
# } else {

#### Download all data, year by year ####
  start_date <- c(ymd_hms("2015-07-22 00:00:00"), ymd_hms("2016-01-01 00:00:00"), ymd_hms("2017-01-01 00:00:00"), 
                  ymd_hms("2018-01-01 00:00:00"), ymd_hms("2019-01-01 00:00:00"), ymd_hms("2020-01-01 00:00:00"))
  startdate <- format(start_date, "%Y-%m-%dT%H:%M:%S")
  end_date <- c(ymd_hms("2015-12-31 23:59:59"), ymd_hms("2016-12-31 23:59:59"), ymd_hms("2017-12-31 23:59:59"), 
                  ymd_hms("2018-12-31 23:59:59"), ymd_hms("2019-12-31 23:59:59"), ymd_hms("2020-12-31 23:59:59"))
  enddate <- format(end_date, "%Y-%m-%dT%H:%M:%S")
  file_name <- c("NRT_data_2015.rds","NRT_data_2016.rds","NRT_data_2017.rds","NRT_data_2018.rds","NRT_data_2019.rds","NRT_data_2020.rds")

for(i in 1:6) {
  if(agg_time=="MINUTE"){
    aggregate_string=paste0("&aggregate=",agg_time)
  } else {
    aggregate_string=paste0("&aggregate=",agg_time,"&aggregateFunctions=",agg_fun_1,"&aggregateFunctions=",agg_fun_2,"&aggregateFunctions=",agg_fun_3)
  }
  code1 <- paste0("https://dashboard.awi.de/data-xxl/rest/data?beginDate=",startdate[i],"&endDate=",enddate[i],"&format=text/tab-separated-values",aggregate_string,
                  "&sensors=station:svluwobs:fb_731101:sbe45_awi_0403:salinity",
                  "&sensors=station:svluwobs:fb_731101:sbe45_awi_0403:temperature",
                  "&sensors=station:svluwobs:svluw2:ctd_183:conductivity_awi_01:salinity", 
                  "&sensors=station:svluwobs:svluw2:ctd_181:conductivity_awi_02:salinity", 
                  "&sensors=station:svluwobs:svluw2:ctd_578:conductivity_awi_03:salinity",
                  "&sensors=station:svluwobs:svluw2:ctd_964:conductivity_awi_04:salinity",
                  "&sensors=station:svluwobs:svluw2:ctd_awi_964:salinity_004",
                  "&sensors=station:svluwobs:svluw2:ctd_103:conductivity_awi_01:salinity",
                  "&sensors=station:svluwobs:svluw2:ctd_103:pressure_awi_01:pressure",
                  "&sensors=station:svluwobs:svluw2:ctd_181:pressure_awi_02:pressure",
                  "&sensors=station:svluwobs:svluw2:ctd_183:pressure_awi_01:pressure",
                  "&sensors=station:svluwobs:svluw2:ctd_578:pressure_awi_03:depth",
                  "&sensors=station:svluwobs:svluw2:ctd_964:pressure_awi_04:depth",
                  "&sensors=station:svluwobs:svluw2:ctd_awi_964:pressure_002",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_0317001:ph",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_0317001:total_alcalinity",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_0317001:invalid_salinity",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_0317001:invalid_ph",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_0317001:invalid_ta",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_1215000:ph",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_1215000:total_alcalinity",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_1215000:invalid_salinity",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_1215000:invalid_ph",
                  "&sensors=station:svluwobs:fb_731101:hydrofia_awi_1215000:invalid_ta",
                  "&sensors=station:svluwobs:svluw2:sbe38_awi_0657:temperature",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:ph_internal",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:ph_external",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:voltage_internal",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:voltage_external",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:ph_temperature",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_007:internal_relative_humidity",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:ph_internal",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:ph_external",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:volt_internal",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:volt_external",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:ph_temperature",
                  "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:internal_relative_humidity")
  
  code2 <- paste0("https://dashboard.awi.de/data-xxl/rest/data?beginDate=",startdate[i],"&endDate=",enddate[i],"&format=text/tab-separated-values",aggregate_string,
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:zero",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:signal_proc",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:signal_raw",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:signal_ref",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:flush",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:p_in",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:p_ndir",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:t_gas",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:pco2_corr",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:pco2_corr_flush",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0215_obsvlfr_01:pco2_corr_zero",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:zero",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:signal_proc",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:signal_raw",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:signal_ref",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:flush",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:p_in",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:p_ndir",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:t_gas",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:pco2_corr",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:pco2_corr_flush",
                  "&sensors=station:svluwobs:fb_731101:co2ft_0515_obsvlfr_01:pco2_corr_zero",
                  "&sensors=station:svluwobs:fb_731101:durafet_obsvlfr_01:hw_ph",
                  "&sensors=station:svluwobs:fb_731101:durafet_obsvlfr_01:hw_temperature",
                  "&sensors=station:svluwobs:svluw2:par_awi_401:par",
                  "&sensors=station:svluwobs:svluw2:par_awi_493:par",
                  "&sensors=station:svluwobs:fb_731101:par_awi_495:par",
                  "&sensors=station:svluwobs:fb_731101:turbidity_awi_01:turbidity",
                  "&sensors=station:svluwobs:svluw2:ctd_183:temperature_awi_01:temperature",
                  "&sensors=station:svluwobs:svluw2:ctd_181:temperature_awi_02:temperature",
                  "&sensors=station:svluwobs:svluw2:ctd_578:temperature_awi_03:temperature",
                  "&sensors=station:svluwobs:svluw2:ctd_964:temperature_awi_04:temperature",
                  "&sensors=station:svluwobs:svluw2:ctd_awi_964:temperature_002",
                  "&sensors=station:svluwobs:svluw2:ctd_103:temperature_awi_01:temperature"
  )
  data1 <- data.table::fread(code1, encoding = "UTF-8", showProgress	= TRUE)
  data2 <- data.table::fread(code2, encoding = "UTF-8", showProgress	= TRUE)
  colnames(data1) <- c("datetime", "sal_fb", "temp_fb", "sal_insitu_183", "sal_insitu_181", "sal_insitu_578", "sal_insitu_964", "sal_insitu_964b","sal_insitu_103", "pressure_insitu_103", "pressure_insitu_181" ,"pressure_insitu_183", "pressure_insitu_578", "pressure_insitu_964" ,"pressure_insitu_964b" ,"pH_AT_0317", "AT_0317", "InvSal_0317", "InvpH_0317", "InvAT_0317", "pH_AT_1215", "AT_1215", "InvSal_1215", "InvpH_1215", "InvAT_1215","temp_insitu_11m", "phINT_007","phEXT_007","voltINT_007","voltEXT_007", "T_seaF_007", "Humidity_007","phINT_1005","phEXT_1005","voltINT_1005","voltEXT_1005", "T_seaF_1005", "Humidity_1005")
  colnames(data2) <- c("datetime", "State_Zero_0215", "Signal_Proc_0215", "Signal_Raw_0215", "Signal_Ref_0215", "State_Flush_0215", "P_In_0215", "P_NDIR_0215", "T_Gas_0215", "pco2_raw_0215", "pco2_raw_Flush_0215","pco2_raw_Zero_0215","State_Zero_0515", "Signal_Proc_0515", "Signal_Raw_0515", "Signal_Ref_0515", "State_Flush_0515", "P_In_0515", "P_NDIR_0515", "T_Gas_0515", "pco2_raw_0515", "pco2_raw_Flush_0515","pco2_raw_Zero_0515", "ph_dur", "temp_dur","par_insitu_profile", "par_insitu_10m", "par_air", "turb_fb", "temp_insitu_183", "temp_insitu_181", "temp_insitu_578", "temp_insitu_964", "temp_insitu_964b","temp_insitu_103" )
  data1$datetime <- ymd_hms(data1$datetime)
  data2$datetime <- ymd_hms(data2$datetime)
  tmp <- left_join(data1, data2, by = "datetime")
  saveRDS(tmp, file = paste0(path,file_name[i]), version = 2)
  if (exists("dat") == FALSE) {
    dat <- tmp
  } else {
    dat <- dplyr::bind_rows(dat, tmp)
  }
  saveRDS(dat, file = paste0(path, "NRT_data_all"), version = 2)
} 


