if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
library("lubridate")
if (!require("oce")) install.packages("oce")
library(oce)
if (!require("dygraphs")) install.packages("dygraphs")
library(dygraphs)
if (!require("tidyverse")) install.packages("tidyverse")
library(xts)
if (!require("curl")) install.packages("curl")
library(curl)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("tsibble")) install.packages("tsibble")
library(tsibble)
if (!require("cowplot")) install.packages("cowplot")
library(cowplot)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
if (!require("xts")) install.packages("xts")
if (!require("stringr")) install.packages("stringr")
library(stringr)
if (!require("devtools")) install.packages("devtools")
library(devtools)
if (!require("feasts")) install.packages("feasts")
library(feasts)
if (!require("knitr")) install.packages("knitr")
library(knitr)
if (!require("kableExtra")) install.packages("kableExtra")
library(kableExtra)


####define who is the user and define path
rm(list = ls())

if (Sys.getenv("LOGNAME") == "gattuso") path = "../../pCloud\ Sync/Documents/experiments/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
if (Sys.getenv("LOGNAME") == "samir") path = "../../pCloud\ Sync/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
if (system('echo "$USER"') == "awipev") {
 setwd("/home/awipev/ny-alesund/") #to run on server
 path = "data/NRT_data/"
}

# # sur serveur
# setwd("/home/awipev/ny-alesund/") #to run on server
# path = "data/NRT_data/"

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

#### Download current data ####
# Generally data from yesterday
# Create end_date (data until yesterday 23:59:59) and start_date (last data from previous data minute).

# start_date = Open the last "year" file (minute fomat) and take the last line date - 1 day. like that we avoid NA to interpolate Sprim2beamZ (pCO2) later.
# start_date <- ymd_hms("2015-07-25 00:00:00")
# start_date <- ymd_hms("2018-01-01 00:00:00")

# # if enddate - startdate < 1 d one skips everything until the end of this script
# if (end_date-start_date <= days(1)) {
#  stop()
#  }

# read past data
previous_NRT_data <- NULL
for (i in c(2015:2020)) {
  print(i)
  fil <- paste0(path, "NRT_data_", as.character(i), ".rds")
  if (file.exists(fil) == TRUE) {
    tmp <- as_tibble(readRDS(fil))
    #### Binding data (previous + new) ####
    previous_NRT_data <- bind_rows(previous_NRT_data, tmp)
  }}
#previous_NRT_data <- as_tibble(previous_NRT_data)
#previous_NRT_data$datetime <- ymd_hms(previous_NRT_data$datetime)
summary(previous_NRT_data$datetime)
  
read_nrt <- function(agg_time = agg_time) {
 data <- NULL
 tmp <- NULL
 
# selected_data_minute <- readRDS(file = paste0(path, "all_nydata_minute.rds"))
# start_date <- ymd_hms(selected_data_minute$datetime[nrow(selected_data_minute)-1]) - days(60)
# startdate <- format(start_date, "%Y-%m-%dT%H:%M:%S")
# 
# # end_date <- ymd_hms("2017-12-31 23:59:59")
# # end_date <- ymd_hms("2020-02-15 23:59:59")
# end_date <- ymd_hms(paste0(Sys.Date(), " 00:00:00 UTC"))
# enddate <- format(end_date,"%Y-%m-%dT%H:%M:%S")

 for (i in c(2019:2020)) {
  print(i)
  startdate <- ymd_hms(paste0(as.character(i), "-01-01 00:00:00"))
  startdate <- format(startdate, "%Y-%m-%dT%H:%M:%S")
  enddate <- ymd_hms(paste0(as.character(i), "-12-31 23:59:59"))
  enddate <- format(enddate, "%Y-%m-%dT%H:%M:%S")

if(agg_time=="MINUTE"){
 aggregate_string=paste0("&aggregate=",agg_time)
} else {
 aggregate_string=paste0("&aggregate=",agg_time,"&aggregateFunctions=",agg_fun_1,"&aggregateFunctions=",agg_fun_2,"&aggregateFunctions=",agg_fun_3)
}
code <- paste0("https://dashboard.awi.de/data-xxl/rest/data?beginDate=",startdate,"&endDate=",enddate,"&format=text/tab-separated-values",aggregate_string,
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
        "&sensors=station:svluwobs:svluw2:seafet_obsvlfr_1005:internal_relative_humidity",
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

tmp <- data.table::fread(code, encoding = "UTF-8", showProgress	= TRUE)
colnames(tmp) <- c("datetime", "sal_fb", "temp_fb", "sal_insitu_183", "sal_insitu_181", "sal_insitu_578", "sal_insitu_964", "sal_insitu_964b","sal_insitu_103", "pressure_insitu_103", "pressure_insitu_181" ,"pressure_insitu_183", "pressure_insitu_578", "pressure_insitu_964" ,"pressure_insitu_964b" ,"pH_AT_0317", "AT_0317", "InvSal_0317", "InvpH_0317", "InvAT_0317", "pH_AT_1215", "AT_1215", "InvSal_1215", "InvpH_1215", "InvAT_1215","temp_insitu_11m", "phINT_007","phEXT_007","voltINT_007","voltEXT_007", "T_seaF_007", "Humidity_007","phINT_1005","phEXT_1005","voltINT_1005","voltEXT_1005", "T_seaF_1005", "Humidity_1005", "State_Zero_0215", "Signal_Proc_0215", "Signal_Raw_0215", "Signal_Ref_0215", "State_Flush_0215", "P_In_0215", "P_NDIR_0215", "T_Gas_0215", "pco2_raw_0215", "pco2_raw_Flush_0215","pco2_raw_Zero_0215","State_Zero_0515", "Signal_Proc_0515", "Signal_Raw_0515", "Signal_Ref_0515", "State_Flush_0515", "P_In_0515", "P_NDIR_0515", "T_Gas_0515", "pco2_raw_0515", "pco2_raw_Flush_0515","pco2_raw_Zero_0515", "ph_dur", "temp_dur","par_insitu_profile", "par_insitu_10m", "par_air", "turb_fb", "temp_insitu_183", "temp_insitu_181", "temp_insitu_578", "temp_insitu_964", "temp_insitu_964b","temp_insitu_103" )
if(i == 2015) {
 data <- tmp 
 } else {
  data <- dplyr::bind_rows(data, tmp)
 }
saveRDS(tmp, file = paste0(path, "NRT_data_", as.character(i), ".rds"))
 }
return(data)
}

#data <- as_tibble(read_nrt(agg_time = "MINUTE"))
data <- previous_NRT_data
data$datetime <- ymd_hms(data$datetime)
#data2 <- data
# Create instrument column as flag
data <- data %>%
 dplyr::mutate(
  pco2_inst = ifelse(
   datetime >= "2015-07-19 00:00:00" &
    datetime <= "2016-02-23 12:00:00",
   "0215",
   ifelse(
    datetime >= "2016-02-23 23:00:00" &
     datetime <= "2017-02-04 23:00:00",
    "0515" ,
    ifelse(
     datetime >= "2017-02-09 00:00:00" &
      datetime <= "2018-02-09 00:00:00",
     "0215" ,
     ifelse(
      datetime >= "2018-04-14 00:00:00" &
       datetime <= "2018-10-31 00:00:00",
      "0515" ,
      ifelse(
       datetime >= "2018-10-31 15:00:00" &
        datetime <= "2019-09-03 12:00:00",
       "0215" ,
       ifelse(
        datetime >= "2019-09-03 17:00:00" &
         datetime <= "2099-12-02 23:59:59",
        "0515" ,
        NA
       )
      )
     )
    )
   )
  ),
  
  seafet_inst = ifelse(
   datetime >= "2017-08-24 12:00:00" &
    datetime <= "2018-04-17 12:00:00",
   "1005",
   ifelse(
    datetime >= "2018-04-17 12:00:00" &
     datetime <= "2099-12-02 23:59:59",
    "007" ,
    NA
   )
  ),
  
  ta_inst = ifelse(
   datetime >= "2016-02-26 00:00:00" &
    datetime <= "2017-03-21 12:00:00",
   "1215",
   ifelse(
    datetime >= "2018-01-08 12:55:00" &
     datetime <= "2018-06-20 00:00:00",
    "0317",
    ifelse(
     datetime >= "2018-07-31 00:00:00" &
      datetime <= "2018-10-30 18:00:00",
     "1215",
     ifelse(
      datetime >= "2018-10-30 18:00:00" &
       datetime <= "2099-12-02 23:59:59",
      "0317" ,
      NA
     )
    )
   )
  )
 )

########### Binding different insitu salinity in one column ########### 
data <- data %>%
 dplyr::mutate(sal_insitu_ctd = ifelse(
  !is.na(sal_insitu_183),
  sal_insitu_183,
  ifelse(
   !is.na(sal_insitu_181),
   sal_insitu_181,
   ifelse(
    !is.na(sal_insitu_578),
    sal_insitu_578,
    ifelse(
     !is.na(sal_insitu_964b),
     sal_insitu_964b,
     ifelse(
      !is.na(sal_insitu_103),
      sal_insitu_103,
      ifelse(!is.na(sal_insitu_964), sal_insitu_964, NA)
     )
    )
   )
  )
 ))

########### Binding different insitu pressure in one column ########### 
data <- data %>%
 dplyr::mutate(pressure_insitu_ctd = ifelse(
  !is.na(pressure_insitu_183),
  pressure_insitu_183,
  ifelse(
   !is.na(pressure_insitu_181),
   pressure_insitu_181,
   ifelse(
    !is.na(pressure_insitu_578),
    pressure_insitu_578,
    ifelse(
     !is.na(pressure_insitu_964b),
     pressure_insitu_964b,
     ifelse(
      !is.na(pressure_insitu_103),
      pressure_insitu_103,
      ifelse(!is.na(pressure_insitu_964), pressure_insitu_964, NA)
     )
    )
   )
  )
 ))
########### Binding different insitu temp in one column ###########
data <- data %>%
 dplyr::mutate(temp_insitu_ctd = ifelse(
  !is.na(temp_insitu_183),
  temp_insitu_183,
  ifelse(
   !is.na(temp_insitu_181),
   temp_insitu_181,
   ifelse(
    !is.na(temp_insitu_578),
    temp_insitu_578,
    ifelse(
     !is.na(temp_insitu_964b),
     temp_insitu_964b,
     ifelse(
      !is.na(temp_insitu_103),
      temp_insitu_103,
      ifelse(!is.na(temp_insitu_964), temp_insitu_964, NA)
     )
    )
   )
  )
 ))

########### Correction pCO2 Contros ########### 
# Bind different pCO2 sensors (column with S/N) in the same column (without S/N): 0215 + 0515
data <- data %>%
 dplyr::mutate(
  State_Zero = ifelse(
   pco2_inst == "0215",
   State_Zero_0215,
   ifelse(pco2_inst == "0515", State_Zero_0515, NA)
  ),
  Signal_Proc = ifelse(
   pco2_inst == "0215",
   Signal_Proc_0215,
   ifelse(pco2_inst == "0515", Signal_Proc_0515, NA)
  ),
  Signal_Raw = ifelse(
   pco2_inst == "0215",
   Signal_Raw_0215,
   ifelse(pco2_inst == "0515", Signal_Raw_0515, NA)
  ),
  Signal_Ref = ifelse(
   pco2_inst == "0215",
   Signal_Ref_0215,
   ifelse(pco2_inst == "0515", Signal_Ref_0515, NA)
  ),
  State_Flush = ifelse(
   pco2_inst == "0215",
   State_Flush_0215,
   ifelse(pco2_inst == "0515", State_Flush_0515, NA)
  ),
  P_In =    ifelse(
   pco2_inst == "0215",
   P_In_0215,
   ifelse(pco2_inst == "0515", P_In_0515, NA)
  ),
  P_NDIR =   ifelse(
   pco2_inst == "0215",
   P_NDIR_0215,
   ifelse(pco2_inst == "0515", P_NDIR_0515, NA)
  ),
  T_Gas =    ifelse(
   pco2_inst == "0215",
   T_Gas_0215,
   ifelse(pco2_inst == "0515", T_Gas_0515, NA)
  ),
  pco2_raw = ifelse(
   pco2_inst == "0215",
   pco2_raw_0215,
   # USE TO BE: PCO2_Corr, before April 2020
   ifelse(pco2_inst == "0515", pco2_raw_0515, NA)
  ),
  pco2_raw_Flush = ifelse(
   pco2_inst == "0215",
   pco2_raw_Flush_0215,
   ifelse(pco2_inst == "0515", pco2_raw_Flush_0515, NA)
  ),
  pco2_raw_Zero = ifelse(
   pco2_inst == "0215",
   pco2_raw_Zero_0215,
   ifelse(pco2_inst == "0515", pco2_raw_Zero_0515, NA)
  )
 )

##### Binding several TA instruments (different Serial Number) in one for each parameter ####
# bind TA_0317 and TA_1215 data = AT
data <- data %>%
 dplyr::mutate(AT= ifelse(!is.na(AT_1215), AT_1215,
              ifelse(!is.na(AT_0317), AT_0317,NA)))
## seaFET ##
# phINT et ph_EXT _007 et _1005
data <- data %>%
  dplyr::mutate(
    phINT = ifelse(!is.na(phINT_007), phINT_007,
                   ifelse(!is.na(phINT_1005), phINT_1005, NA)),
    phEXT = ifelse(!is.na(phEXT_007), phEXT_007,
                   ifelse(!is.na(phEXT_1005), phEXT_1005, NA))
  )

# voltINT et voltEXT _007 et _1005
data <- data %>%
  dplyr::mutate(
    voltINT = ifelse(
      !is.na(voltINT_007),
      voltINT_007,
      ifelse(!is.na(voltINT_1005), voltINT_1005, NA)
    ),
    voltEXT = ifelse(
      !is.na(voltEXT_007),
      voltEXT_007,
      ifelse(!is.na(voltEXT_1005), voltEXT_1005, NA)
    )
  )

# temp_sf _007 et _1005
data <- data %>%
  dplyr::mutate(temp_sf = ifelse(
    !is.na(T_seaF_007),
    T_seaF_007,
    ifelse(!is.na(T_seaF_1005), T_seaF_1005, NA)
  ))

 # Data processing for pCO2 HydroC CO2 Contros in Ny-Alesund.
 # Documentation received by Steffen Assmann on the 2016-09-15/19 and 
 # stored on /fb_doc/Calibration/pCO2_#0215_#0515
 # Data from the documentation is stored on 
 # awipev/ny_alesund/data/Calibration_pCO2/Data_Processing_sheet_pCO2.txt
 
 # Adding manualy data into Data_Processing_sheet_pCO2 file.
 # Open the data calibration sheet and convert the dates. 
data_process <- read_xlsx(range="A1:AF8", na = "NA", paste0(path, "Data_Processing_sheet_pCO2.xlsx"), col_types = c("text","text","text","text", "date","date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "date", "date", "numeric",  "date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

# data_process$DateCalibration <- dmy(data_process$DateCalibration)
# data_process$DateCalibrationPost <- dmy(data_process$DateCalibrationPost)
# data_process$DateDelivery <- dmy(data_process$DateDelivery)
# data_process$StartDeployment <- ymd_hms(data_process$StartDeployment)
# data_process$EndDeployment <- ymd_hms(data_process$EndDeployment)


 # Adding the PeriodDeplpCO2 column to z first.
 # Adding the others parameters according to the Contros formula PDF
 # Adding the new period ech time we receive the pCO2 sensor from the calibration
data <- data %>%
  dplyr::mutate(
    PeriodDeplpCO2 = ifelse(
      datetime >= "2015-07-23 00:00:00" &
        datetime <= "2016-02-23 22:00:00",
      1,
      ifelse(
        datetime >= "2016-02-23 23:00:00" &
          datetime <= "2017-02-04 23:00:00",
        2 ,
        ifelse(
          datetime >= "2017-02-09 00:00:00" &
            datetime <= "2018-02-09 00:00:00",
          3 ,
          ifelse(
            datetime >= "2018-04-14 00:00:00" &
              datetime <= "2018-10-31 00:00:00",
            4 ,
            ifelse(
              datetime >= "2018-10-31 15:00:00" &
                datetime <= "2019-09-03 12:00:00",
              5 ,
              ifelse(
                datetime >= "2019-09-03 17:00:00" &
                  datetime <= "2099-12-02 23:59:59",
                6 ,
                NA
              )
            )
          )
        )
      )
    )
  )

# Adding the fTsensor, F, P0, T0, k1, k2,k3 (Pre calibration) + F_Post, P0_Post...(Post calibration) columns to z 
data <-
  left_join(
    data,
    data_process %>% dplyr::select(
      PeriodDeplpCO2,
      T0,
      P0,
      F,
      fTsensor,
      k1,
      k2,
      k3,
      T0_post,
      P0_post,
      F_post,
      fTsensor_post,
      k1_post,
      k2_post,
      k3_post
    ),
    by = "PeriodDeplpCO2"
  )

data <- data %>%
  dplyr::mutate(
    Signal_RawZ = ifelse(State_Zero == 1 , Signal_Raw, NA),
    Signal_RefZ = ifelse(State_Zero == 1 , Signal_Ref, NA),
    Signal_ProcZ = ifelse(State_Zero == 1 , Signal_Proc, NA),
    S2beam = Signal_Raw / Signal_Ref,
    S2beamZ = Signal_RawZ / Signal_RefZ,
    Sprim2beam = S2beam * fTsensor,
    Sprim2beamZ = S2beamZ * fTsensor
  )

# We remove the 3rd value of Sprim2beamZ during the zeroing (at 00:03:00 and 12:03:00)
# We keep in Sprim2beamZ the 2nd value of the zeroing (at 00:02:00 and 12:02:00) when it exists 
# if it does not, we keep the 1st value (at 00:01:00 and 12:01:00) of the zeroing 
# see Steffen's email on the 2016-12-13 11:11
data$Time <- strftime(data$datetime, "%H:%M:%S")
data$Sprim2beamZ[which(data$Time == "00:02:00" &
                         !is.na(data$Sprim2beamZ)) - 1] <- NA
data$Sprim2beamZ[which(data$Time == "12:02:00" &
                         !is.na(data$Sprim2beamZ)) - 1] <- NA
#z$Sprim2beamZ[which(z$Time == "00:03:00" | z$Time == "12:03:00")] <- NA
data$Sprim2beamZ[which(
  data$Time != "00:02:00" &
    data$Time != "12:02:00" &
    data$Time != "12:01:00" & data$Time != "00:01:00"
)] <- NA

# We interpolate Sprim2beamZ and we convert it from "list" to "dataframe"
# because approx returns to a list.
# data$Sprim2beamZ_interp <- NULL
data$Sprim2beamZ_interp <-
  as.data.frame(approx(
    x = data$datetime,
    y = data$Sprim2beamZ ,
    xout = data$datetime,
    rule = 2
  ))[, 2]
# We remove interpolated data that matche a zeroing / flush period
data <- data %>%
  dplyr::mutate(Sprim2beamZ_interp = ifelse(State_Zero == 1 |
                                              State_Flush == 1, NA, Sprim2beamZ_interp))
########### pCO2 SPAN DRIFT CORRECTION
# To calculate the drift-corrected polynomial coefficients from the pre and post calibration
# Calculate the new k1 k2 k3
# Extract the first and last S'2beamZ of each periode of deployment.
# the s dataframe needs to be created again with the new pco2 period when the period will be known.
s <- read.table(file= paste0(path, "SpreambeamZ_extraction.csv"), header = TRUE, dec = ".", sep=",")
data <- data %>%
 dplyr::mutate(k1t = ifelse(PeriodDeplpCO2 == 1, k1 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3] - s[2,3] )) * k1_post,
           ifelse(PeriodDeplpCO2 == 2, k1 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3] - s[4,3] )) * k1_post,
           ifelse(PeriodDeplpCO2 == 3, k1 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3] - s[6,3] )) * k1_post, 
           ifelse(PeriodDeplpCO2 == 4, k1 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3] - s[8,3] )) * k1_post,
           ifelse(PeriodDeplpCO2 == 5, k1 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3] - s[10,3] )) * k1_post,
           ifelse(PeriodDeplpCO2 == 6, k1 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3] - s[12,3] )) * k1_post,NA)
                         )))))) %>%
 dplyr::mutate(k2t = ifelse(PeriodDeplpCO2 == 1, k2 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3] - s[2,3] )) * k2_post,
           ifelse(PeriodDeplpCO2 == 2, k2 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3] - s[4,3] )) * k2_post,
           ifelse(PeriodDeplpCO2 == 3, k2 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3] - s[6,3] )) * k2_post, 
           ifelse(PeriodDeplpCO2 == 4, k2 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3] - s[8,3] )) * k2_post, 
           ifelse(PeriodDeplpCO2 == 5, k2 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3] - s[10,3] )) * k2_post,
           ifelse(PeriodDeplpCO2 == 6, k2 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3] - s[12,3] )) * k2_post,NA)
                         )))))) %>%
 dplyr::mutate(k3t = ifelse(PeriodDeplpCO2 == 1, k3 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3] - s[2,3] )) * k3_post,
           ifelse(PeriodDeplpCO2 == 2, k3 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3] - s[4,3] )) * k3_post,
           ifelse(PeriodDeplpCO2 == 3, k3 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3] - s[6,3] )) * k3_post, 
           ifelse(PeriodDeplpCO2 == 4, k3 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3] - s[8,3] )) * k3_post, 
           ifelse(PeriodDeplpCO2 == 5, k3 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3] - s[10,3] )) * k3_post,
           ifelse(PeriodDeplpCO2 == 6, k3 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3] - s[12,3] )) * k3_post, NA)
                         ))))))

data <- data %>%
 dplyr::mutate(k1t = ifelse(!is.na(k1t), k1t, k1)) %>%
 dplyr::mutate(k2t = ifelse(!is.na(k2t), k2t, k2)) %>%
 dplyr::mutate(k3t = ifelse(!is.na(k3t), k3t, k3)) 

# Continue to calculate parameters and FINAL PCO2 CORRECTED (pco2_corr, use to be PCO2_corr_contros before April 2020)
data <- data %>%
  dplyr::mutate(
    SprimDCt = Sprim2beam / Sprim2beamZ_interp,
    Sproct = F * (1 - SprimDCt),
    xco2wet = (k3t * Sproct ^ 3 + k2t * Sproct ^ 2 + k1t * Sproct) * (P0 *
                                                                        (T_Gas + 273.15)) / (T0 * P_NDIR),
    pco2_corr = xco2wet * (P_In / 1013.25)
  )

##### First cleaning

# quality flags
# 1: good data
# 3: impossible date and time test
# 4: data not usable according to manufacturer (flush mode, zero mode, calibration mode...) 
# 6: failing the manufacturer range test (not used)
# 7: failing the regional range test 
# 12: failing the spike test (NAs from despike)
# 13: failing the gradient test
# 15: the instrument was not deployed or operated
# 99: failing the final visual inspection of the data. 

#### Argo impossible date test
data <- data %>%
  dplyr::mutate(
    dt_qf = ifelse(
      year(data$datetime) < 2010 | year(data$datetime) > 2022 |
        month(data$datetime) < 1 | month(data$datetime) > 12 |
        day(data$datetime) < 1 | day(data$datetime) > 31 |
        hour(data$datetime) < 0 | hour(data$datetime) > 23 |
        minute(data$datetime) < 0 | minute(data$datetime) > 59,
      3,
      1
    )
  ) %>%
  dplyr::filter(dt_qf == 1) # removes rows with wrong dates

## pCO2 

# Argo Global Range Test. If QF = 4 replace value by NA
# Removing outliers due to acid flush in the FB at 12:00 and 00:00 
# pco2_raw = Raw data from the sensor (use to be PCO2_Corr before April 2020)
# pco2_corr = Final corrected data from Contros "data processing document", (use to be PCO2_corr_contros before April 2020)
data <- data %>%
  dplyr::mutate(
    pco2_raw_qf = ifelse(
      State_Zero >= 1 | State_Flush >= 1,
      4,
      ifelse(pco2_raw < 100 | pco2_raw > 450 , 7,
             ifelse(
               is.na(pco2_raw),
               15,
               ifelse(
                 Time >= "00:00:00" &
                   Time <= "01:30:00" |
                   Time >= "12:00:00" &
                   Time <= "13:00:00" |
                   datetime >= "2017-03-10 08:00:00" &
                   datetime < "2017-03-21 20:00:00",
                 99,
                 1
               )
             ))
    ),
    pco2_raw = ifelse(pco2_raw_qf != 1 , NA, pco2_raw),
    pco2_corr_qf = ifelse(
      State_Zero >= 1 | State_Flush >= 1,
      4,
      ifelse(pco2_corr < 100 |
               pco2_corr > 450 , 7,
             ifelse(
               is.na(pco2_corr),
               15,
               ifelse(
                 Time >= "00:00:00" &
                   Time <= "01:30:00" |
                   Time >= "12:00:00" &
                   Time <= "13:00:00" |
                   datetime >= "2017-03-10 08:00:00" &
                   datetime < "2017-03-21 20:00:00",
                 99,
                 1
               )
             ))
    ),
    pco2_corr = ifelse(pco2_corr_qf != 1 , NA, pco2_corr)
  )

## filter salinities above 28
data <- data %>%
  dplyr::mutate(
    sal_fb_qf = ifelse(sal_fb < 28 | sal_fb > 37, 7,
                       ifelse(is.na(sal_fb), 15,
                              1)),
    sal_fb = ifelse(sal_fb_qf != 1 , NA, sal_fb),
    sal_insitu_ctd_qf = ifelse(sal_insitu_ctd < 28 | sal_fb > 37, 7,
                               ifelse(
                                 is.na(sal_insitu_ctd),
                                 15,
                                 ifelse(datetime > "2019-12-26 00:00:00", 99, 1)
                               )),
    sal_insitu_ctd = ifelse(sal_insitu_ctd_qf != 1 , NA, sal_insitu_ctd),
  )

## filter temperatures above 10
data <- data %>%
  dplyr::mutate(
    temp_dur_qf = ifelse(temp_dur > 10 | temp_dur < -2 , 7,
                         ifelse(is.na(temp_dur), 15,
                                1)),
    temp_dur = ifelse(temp_dur_qf != 1 , NA, temp_dur),
    temp_fb_qf = ifelse(temp_fb > 10 | temp_fb < -2 , 7,
                        ifelse(is.na(temp_fb), 15,
                               1)),
    temp_fb = ifelse(temp_fb_qf != 1 , NA, temp_fb),
    temp_insitu_11m_qf = ifelse(
      temp_insitu_11m > 10 | temp_insitu_11m < -2 ,
      7,
      ifelse(is.na(temp_insitu_11m), 15,
             1)
    ),
    temp_insitu_11m = ifelse(temp_insitu_11m_qf != 1 , NA, temp_insitu_11m),
  )

## filter Voltages seaFET to remove outliers when calculating final corrected pH.
data <- data %>%
  dplyr::mutate(
    voltINT_qf = ifelse(voltINT > 0 , 99,
                        ifelse(is.na(voltINT), 15,
                               1)),
    voltINT = ifelse(voltINT_qf != 1 , NA, voltINT),
    voltEXT_qf = ifelse(voltEXT > -0.4 , 99,
                        ifelse(is.na(voltEXT), 15,
                               1)),
    voltEXT = ifelse(voltEXT_qf != 1 , NA, voltEXT)
  )
## seaFET
data <- data %>%
  dplyr::mutate(
    phINT_qf = ifelse(phINT < 7.5 | phINT > 8.5 , 7,
                      ifelse(is.na(phINT), 15,
                             1)),
    phINT = ifelse(phINT_qf != 1 , NA, phINT),
    phEXT_qf = ifelse(phEXT < 7.5 | phEXT > 8.5 , 7,
                      ifelse(is.na(phEXT), 15,
                             1)),
    phEXT = ifelse(phEXT_qf != 1 , NA, phEXT)
  )
## durafet 
data <- data %>%
  dplyr::mutate(
    ph_dur_qf = ifelse(ph_dur < 7.5 | ph_dur > 8.5 , 7,
                       ifelse(is.na(ph_dur), 15,
                              1)),
    ph_dur = ifelse(ph_dur_qf != 1 , NA, ph_dur)
  )
##########  first data cleaning : despike() ###########
data <- data %>%   
  dplyr::mutate(pco2_raw_filtered= despike(data$pco2_raw, reference= "median", n=2, k=5761, replace="NA"),
         pco2_corr_filtered= despike(data$pco2_corr, reference= "median", n=2, k=5761, replace="NA"),
         sal_fb_filtered= despike(data$sal_fb, reference= "median", n=2, k=5761, replace="NA"),
         temp_fb_filtered= despike(data$temp_fb, reference= "median", n=2, k=5761, replace="NA"),
         ph_dur_filtered= despike(data$ph_dur, reference= "median", n=2, k=5761, replace="NA"),
         temp_dur_filtered= despike(data$temp_dur, reference= "median", n=2, k=5761, replace="NA"),
         temp_insitu_11m_filtered= despike(data$temp_insitu_11m, reference= "median", n=2, k=5761, replace="NA"),
         date = as.Date(data$datetime),
         hour = hour(data$datetime)
 ) %>% #Now one adds flag 12 when new NAs were introduced by despike
 dplyr::mutate(pco2_raw_qf = ifelse(!is.na(pco2_raw) & is.na(pco2_raw_filtered), 12, pco2_raw_qf),
        pco2_corr_qf = ifelse(!is.na(pco2_corr) & is.na(pco2_corr_filtered), 12, pco2_corr_qf),
        sal_fb_qf = ifelse(!is.na(sal_fb) & is.na(sal_fb_filtered), 12, sal_fb_qf),
        temp_fb_qf = ifelse(!is.na(temp_fb) & is.na(temp_fb_filtered), 12, temp_fb_qf),
        ph_dur_qf = ifelse(!is.na(ph_dur) & is.na(ph_dur_filtered), 12, ph_dur_qf),
        temp_dur_qf = ifelse(!is.na(temp_dur) & is.na(temp_dur_filtered), 12, temp_dur_qf),
        temp_insitu_11m_qf = ifelse(!is.na(temp_insitu_11m) & is.na(temp_insitu_11m_filtered), 12, temp_insitu_11m_qf))

#MINUTE: save all parameters in long format (not selected data)
if (file.exists(paste0(path, "all_nydata_minute.rds")) == TRUE) {
 previous_all_nydata_minute <- readRDS(paste0(path, "all_nydata_minute.rds")) %>%
  dplyr::filter(datetime - days(60) ) # remove 60 days to avoid duplicate with despike newly done and old despike in the RDS
 #### Binding data (previous + new) ####
 data <- rbind(previous_all_nydata_minute, data)
 #Remove duplicate due to binding
 data <- data %>%
 distinct(datetime, .keep_all = T)
saveRDS(file= paste0(path, "all_nydata_minute.rds"), data)
} else {
 saveRDS(file= paste0(path, "all_nydata_minute.rds"), data)
}

# HOUR
data_hour <- data%>%
 dplyr::group_by(date, hour) %>%
 dplyr::summarise( State_Zero = mean(State_Zero, na.rm = TRUE),
          State_Flush = mean(State_Flush, na.rm = TRUE),
          Signal_Ref= mean(Signal_Ref, na.rm = TRUE),
          Signal_Raw = mean(Signal_Raw, na.rm = TRUE),
          Signal_Proc = mean(Signal_Proc, na.rm = TRUE),
          P_In= mean(P_In, na.rm = TRUE),
          P_NDIR= mean(P_NDIR, na.rm = TRUE),
          T_Gas= mean(T_Gas, na.rm = TRUE),
          pco2_raw= mean(pco2_raw, na.rm = TRUE),
          pco2_raw_Flush= mean(pco2_raw_Flush, na.rm = TRUE),
          pco2_raw_Zero= mean(pco2_raw_Zero, na.rm = TRUE),
          Signal_RefZ= mean(Signal_RefZ, na.rm = TRUE),
          Signal_RawZ= mean(Signal_RawZ, na.rm = TRUE),
          Signal_ProcZ= mean(Signal_ProcZ, na.rm = TRUE),
          S2beam= mean(S2beam, na.rm = TRUE),
          S2beamZ= mean(S2beamZ, na.rm = TRUE),
          Sprim2beam= mean(Sprim2beamZ, na.rm = TRUE),
          Sprim2beamZ= mean(Sprim2beam, na.rm = TRUE),
          Sprim2beamZ_interp= mean(Sprim2beamZ_interp, na.rm = TRUE),
          k1t= mean(k1t, na.rm = TRUE),
          k2t= mean(k2t, na.rm = TRUE) ,
          k3t= mean(k3t, na.rm = TRUE) ,
          SprimDCt= mean(SprimDCt, na.rm = TRUE) ,
          Sproct= mean(Sproct, na.rm = TRUE) ,
          xco2wet= mean(xco2wet, na.rm = TRUE) ,
          pco2_corr= mean(pco2_corr, na.rm = TRUE) ) %>%
 dplyr::mutate(datetime = ymd_h(paste(date, hour, sep=" ", tz = "UTC"))) %>%
 dplyr::ungroup() %>% # this is to be able to perform the following changes
 dplyr::select(datetime, everything()) %>%
 dplyr::arrange(desc(datetime))

#### Download previous data ####
if (file.exists(paste0(path, "all_nydata_hour.rds")) == TRUE) {
 previous_all_nydata_hour <- readRDS(paste0(path, "all_nydata_hour.rds"))
 #### Binding data (previous + new) ####
 data_hour <- rbind(previous_all_nydata_hour, data_hour)
 #Remove duplicate due to binding
 data_hour <- data_hour %>%
  distinct(datetime, .keep_all = T)
 saveRDS(file= paste0(path, "all_nydata_hour.rds"), data_hour)
} else {
 saveRDS(file= paste0(path, "all_nydata_hour.rds"), data_hour)
}

#### MINUTE format (small format) ####
selected_nydata_minute <- data %>%
  dplyr::select(
    datetime,
    dt_qf,
    pco2_raw,
    pco2_raw_filtered,
    pco2_raw_qf,
    pco2_corr,
    pco2_corr_filtered,
    pco2_corr_qf,
    PeriodDeplpCO2,
    pressure_insitu_ctd,
    sal_fb,
    sal_fb_filtered,
    sal_fb_qf,
    sal_insitu_ctd,
    sal_insitu_ctd_qf,
    temp_fb,
    temp_fb_filtered,
    temp_fb_qf,
    temp_insitu_11m,
    temp_insitu_11m_filtered,
    temp_insitu_11m_qf,
    temp_dur,
    temp_dur_filtered,
    temp_dur_qf,
    ph_dur,
    ph_dur_filtered,
    voltINT,
    voltINT_qf,
    voltEXT,
    voltEXT_qf,
    phINT,
    phINT_qf,
    phEXT,
    phEXT_qf,
    temp_sf,
    pco2_inst,
    ta_inst,
    seafet_inst,
    par_insitu_profile,
    par_insitu_10m,
    par_air,
    turb_fb,
    temp_insitu_ctd,
    date,
    hour
  )
if (file.exists(paste0(path, "nydata_minute.rds")) == TRUE) {
  previous_nydata_minute <- readRDS(paste0(path, "nydata_minute.rds"))
  #### Binding data (previous + new) ####
  selected_nydata_minute <-
    rbind(previous_nydata_minute, selected_nydata_minute)
  #Remove duplicate due to binding
  selected_nydata_minute <- selected_nydata_minute %>%
    distinct(datetime, .keep_all = T)
  saveRDS(file = paste0(path, "nydata_minute.rds"), selected_nydata_minute)
} else {
  saveRDS(file = paste0(path, "nydata_minute.rds"), selected_nydata_minute)
}
#### HOUR format ####
# Make groups for data on the profiler
selected_nydata_minute <- selected_nydata_minute %>%
  dplyr::mutate(
    depth = ifelse(
      pressure_insitu_ctd <= 2,
      1,
      ifelse(
        pressure_insitu_ctd > 2 & pressure_insitu_ctd <= 4,
        3,
        ifelse(
          pressure_insitu_ctd > 4 & pressure_insitu_ctd <= 6,
          5,
          ifelse(
            pressure_insitu_ctd > 6 & pressure_insitu_ctd <= 8,
            7,
            ifelse(pressure_insitu_ctd > 8, 9, NA)
          )
        )
      )
    ),
    sal_insitu_ctd_1m = ifelse(depth == 1, sal_insitu_ctd, NA),
    sal_insitu_ctd_3m = ifelse(depth == 3, sal_insitu_ctd, NA),
    sal_insitu_ctd_5m = ifelse(depth == 5, sal_insitu_ctd, NA),
    sal_insitu_ctd_7m = ifelse(depth == 7, sal_insitu_ctd, NA),
    sal_insitu_ctd_9m = ifelse(depth == 9, sal_insitu_ctd, NA),
    temp_insitu_ctd_1m = ifelse(depth == 1, temp_insitu_ctd, NA),
    temp_insitu_ctd_3m = ifelse(depth == 3, temp_insitu_ctd, NA),
    temp_insitu_ctd_5m = ifelse(depth == 5, temp_insitu_ctd, NA),
    temp_insitu_ctd_7m = ifelse(depth == 7, temp_insitu_ctd, NA),
    temp_insitu_ctd_9m = ifelse(depth == 9, temp_insitu_ctd, NA),
    par_insitu_profile_1m = ifelse(depth == 1, par_insitu_profile, NA),
    par_insitu_profile_3m = ifelse(depth == 3, par_insitu_profile, NA),
    par_insitu_profile_5m = ifelse(depth == 5, par_insitu_profile, NA),
    par_insitu_profile_7m = ifelse(depth == 7, par_insitu_profile, NA),
    par_insitu_profile_9m = ifelse(depth == 9, par_insitu_profile, NA)
  )
        
# to add instrument S/N columns
selected_nydata_minute$pco2_inst <-
  as.numeric(selected_nydata_minute$pco2_inst)
selected_nydata_minute$ta_inst <-
  as.numeric(selected_nydata_minute$ta_inst)
selected_nydata_minute$seafet_inst <-
  as.numeric(selected_nydata_minute$seafet_inst)

selected_nydata_hour <- selected_nydata_minute %>%
  dplyr::group_by(date, hour) %>%
  dplyr::summarise(
    sal_insitu_ctd = mean(sal_insitu_ctd, na.rm = TRUE),
    sal_insitu_ctd_1m = mean(sal_insitu_ctd_1m, na.rm = TRUE),
    sal_insitu_ctd_3m = mean(sal_insitu_ctd_3m, na.rm = TRUE),
    sal_insitu_ctd_5m = mean(sal_insitu_ctd_5m, na.rm = TRUE),
    sal_insitu_ctd_7m = mean(sal_insitu_ctd_7m, na.rm = TRUE),
    sal_insitu_ctd_9m = mean(sal_insitu_ctd_9m, na.rm = TRUE),
    sal_fb_filtered = mean(sal_fb_filtered, na.rm = TRUE),
    temp_fb_filtered = mean(temp_fb_filtered, na.rm = TRUE),
    temp_insitu_11m_filtered = mean(temp_insitu_11m_filtered, na.rm = TRUE),
    pco2_raw_filtered = mean(pco2_raw_filtered, na.rm = TRUE),
    PeriodDeplpCO2 = mean(PeriodDeplpCO2, na.rm = TRUE),
    pco2_corr_filtered = mean(pco2_corr_filtered, na.rm = TRUE),
    ph_dur_filtered = mean(ph_dur_filtered, na.rm = TRUE),
    temp_dur_filtered = mean(temp_dur_filtered, na.rm = TRUE),
    temp_sf = mean(temp_sf, na.rm = TRUE),
    phINT = mean(phINT, na.rm = TRUE),
    phEXT = mean(phEXT, na.rm = TRUE),
    voltINT = mean(voltINT, na.rm = TRUE),
    voltEXT = mean(voltEXT, na.rm = TRUE),
    pco2_inst = mean(pco2_inst, na.rm = TRUE),
    ta_inst = mean(ta_inst, na.rm = TRUE),
    seafet_inst = mean(seafet_inst, na.rm = TRUE) ,
    par_insitu_profile_1m = mean(par_insitu_profile_1m, na.rm = TRUE),
    par_insitu_profile_3m = mean(par_insitu_profile_3m, na.rm = TRUE),
    par_insitu_profile_5m = mean(par_insitu_profile_5m, na.rm = TRUE),
    par_insitu_profile_7m = mean(par_insitu_profile_7m, na.rm = TRUE),
    par_insitu_profile_9m = mean(par_insitu_profile_9m, na.rm = TRUE),
    par_insitu_10m = mean(par_insitu_10m, na.rm = TRUE),
    par_air = mean(par_air, na.rm = TRUE),
    turb_fb = mean(turb_fb, na.rm = TRUE),
    temp_insitu_ctd = mean(sal_insitu_ctd, na.rm = TRUE),
    temp_insitu_ctd_1m = mean(temp_insitu_ctd_1m, na.rm = TRUE),
    temp_insitu_ctd_3m = mean(temp_insitu_ctd_3m, na.rm = TRUE),
    temp_insitu_ctd_5m = mean(temp_insitu_ctd_5m, na.rm = TRUE),
    temp_insitu_ctd_7m = mean(temp_insitu_ctd_7m, na.rm = TRUE),
    temp_insitu_ctd_9m = mean(temp_insitu_ctd_9m, na.rm = TRUE)
  ) %>%
  dplyr::mutate(datetime = ymd_h(paste(date, hour, sep = " ", tz = "UTC"))) %>%
  dplyr::ungroup() %>% # this is to be able to perform the following changes
  dplyr::select(datetime, everything()) %>%
  dplyr::select(-c(hour)) %>% # not needed in the shiny display
  dplyr::arrange(desc(datetime))
# HOUR format
d_hour <- selected_nydata_hour

if (file.exists(paste0(path, "nydata_hour.rds")) == TRUE) {
  previous_nydata_hour <- readRDS(paste0(path, "nydata_hour.rds"))
  #### Binding data (previous + new) ####
  d_hour <- rbind(previous_nydata_hour, d_hour)
  #Remove duplicate due to binding
  d_hour <- d_hour %>%
    distinct(datetime, .keep_all = T)
  saveRDS(file = paste0(path, "nydata_hour.rds"), d_hour)
} else {
  saveRDS(file = paste0(path, "nydata_hour.rds"), d_hour)
}
