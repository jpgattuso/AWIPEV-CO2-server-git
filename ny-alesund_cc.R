library("dplyr")
library("plyr")
library("lubridate")
library(oce)
library(ggplot2)
library(dygraphs)
library(xts)
library("stringr")
library(data.table)
library(lubridate) 
library(curl)
library(readxl)

####define who is the user and define path
rm(list = ls())

if (Sys.getenv("LOGNAME") == "gattuso") path = "../../pCloud\ Sync/Documents/experiments/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
if (Sys.getenv("LOGNAME") == "samir") path = "../../pCloud\ Sync/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
# if (system('echo "$USER"') == "awipev") {
#   setwd("/home/awipev/ny-alesund/") #to run on server
#   path = "data/NRT_data/"
# }

# sur serveur
setwd("/home/awipev/ny-alesund/") #to run on server
path = "data/NRT_data/"

####Set environmental variables####
Sys.setenv(TZ="UTC")
if (!require("data.table")) install.packages("data.table")
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
#list_last_year <- list.files(path  = path), pattern = "*all_nydata_minute.*")
#list_last_year <- list_last_year[length(list_last_year)]
# start_date <- (end_date - (days_back *(3600*24)))
# start_date <- ymd_hms("2015-07-25 00:00:00")
# start_date <- ymd_hms("2018-01-01 00:00:00")
selected_data_minute <- readRDS(file = paste0(path, "all_nydata_minute.rds"))
start_date <- ymd_hms(selected_data_minute$datetime[nrow(selected_data_minute)-1]) - days(1)
startdate <- format(start_date, "%Y-%m-%dT%H:%M:%S") 

#end_date <- ymd_hms("2017-12-31 23:59:59") 
#end_date <- ymd_hms("2020-02-15 23:59:59") 
# days_back <- 2
end_date <- ymd_hms(paste0(Sys.Date(), " 00:00:00 UTC"))
enddate <- format(end_date,"%Y-%m-%dT%H:%M:%S")

# if enddate - startdate < 1 d one skips everything until the end of this script
if (end_date-start_date <= days(1)) {
  stop()
  }

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
               "&sensors=station:svluwobs:fb_731101:durafet_obsvlfr_01:hw_temperature"
               
               )

data <- data.table::fread(code, encoding = "UTF-8", showProgress	= TRUE)
colnames(data) <- c("datetime", "sal_fb", "temp_fb", "sal_insitu_183", "sal_insitu_181", "sal_insitu_578", "sal_insitu_964", "sal_insitu_964b","sal_insitu_103", "pH_AT_0317", "AT_0317", "InvSal_0317", "InvpH_0317",  "InvAT_0317",  "pH_AT_1215", "AT_1215", "InvSal_1215", "InvpH_1215",  "InvAT_1215","Temp_SBE38", "phINT_007","phEXT_007","voltINT_007","voltEXT_007", "T_seaF_007", "Humidity_007","phINT_1005","phEXT_1005","voltINT_1005","voltEXT_1005", "T_seaF_1005", "Humidity_1005", "State_Zero_0215", "Signal_Proc_0215", "Signal_Raw_0215", "Signal_Ref_0215", "State_Flush_0215", "P_In_0215", "P_NDIR_0215", "T_Gas_0215", "PCO2_Corr_0215", "PCO2_Corr_Flush_0215","PCO2_Corr_Zero_0215","State_Zero_0515", "Signal_Proc_0515", "Signal_Raw_0515", "Signal_Ref_0515", "State_Flush_0515", "P_In_0515", "P_NDIR_0515", "T_Gas_0515", "PCO2_Corr_0515", "PCO2_Corr_Flush_0515","PCO2_Corr_Zero_0515", "HW_pH1", "HW_Temperature1" )
data$datetime <- ymd_hms(data$datetime)
data2 <- data
# Create instrument column as flag
data <- data %>%
  dplyr::mutate( pco2_inst = ifelse(datetime >= "2015-07-19 00:00:00" & datetime <= "2016-02-23 12:00:00", "0215", 
                               ifelse(datetime >= "2016-02-23 23:00:00" & datetime <= "2017-02-04 23:00:00", "0515" ,
                               ifelse(datetime >= "2017-02-09 00:00:00" & datetime <= "2018-02-09 00:00:00", "0215" ,
                                ifelse(datetime >= "2018-04-14 00:00:00" & datetime <= "2018-10-31 00:00:00", "0515" , 
                                ifelse(datetime >= "2018-10-31 15:00:00" & datetime <= "2019-09-03 12:00:00", "0215" ,
                                ifelse(datetime >= "2019-09-03 17:00:00" & datetime <= "2099-12-02 23:59:59", "0515" ,NA )))))),
                 
                 seafet_inst = ifelse(datetime >= "2017-08-24 12:00:00" & datetime <= "2018-04-17 12:00:00", "1005", 
                               ifelse(datetime >= "2018-04-17 12:00:00" & datetime <= "2099-12-02 23:59:59", "007" ,NA )),
                 
                 ta_inst = ifelse(datetime >= "2016-02-26 00:00:00" & datetime <= "2017-03-21 12:00:00", "1215", 
                          ifelse(datetime >= "2018-01-08 12:55:00" & datetime <= "2018-06-20 00:00:00", "0317",
                           ifelse(datetime >= "2018-07-31 00:00:00" & datetime <= "2018-10-30 18:00:00", "1215",
                           ifelse(datetime >= "2018-10-30 18:00:00" & datetime <= "2099-12-02 23:59:59", "0317" ,NA ))))
                 )


#datarows <- nrow(data)
# if (datarows == 0){
#       cat("No observations in downloaded dataset from ",startdate," to ",enddate,"\n","\n")
#      }  else {
#       cat("Number of datarows downloaded from ",startdate," to ",enddate,": ",datarows,sep="","\n","\n")
#      }

########### Binding different insitu salinity in one column ########### 
data <- data %>%
  dplyr::mutate(sal_insitu = ifelse(!is.na(sal_insitu_183), sal_insitu_183,
                           ifelse(!is.na(sal_insitu_181), sal_insitu_181,
                           ifelse(!is.na(sal_insitu_578), sal_insitu_578, 
                            ifelse(!is.na(sal_insitu_964b), sal_insitu_964b,
                            ifelse(!is.na(sal_insitu_103), sal_insitu_103, 
                          ifelse(!is.na(sal_insitu_964), sal_insitu_964,NA)))))))

########### Binding sal_fb INTO sal_insitu if we get gaps in sal_insitu = czll this new column = sal ########### 
data <- data %>%
  dplyr::mutate(sal = ifelse(!is.na(sal_fb), sal_fb, sal_insitu))

#data <- data%>%
      #  dplyr::filter(sal > 28)
                
 ########### Correction pCO2 Contros ########### 

# Bind different pCO2 sensors (column with S/N) in the same column (without S/N): 0215 + 0515
data <- data %>%
  dplyr::mutate(State_Zero=  ifelse(pco2_inst == "0215", State_Zero_0215,
                             ifelse(pco2_inst == "0515", State_Zero_0515,NA)),
                Signal_Proc=ifelse(pco2_inst == "0215", Signal_Proc_0215,
                             ifelse(pco2_inst == "0515", Signal_Proc_0515,NA)),
                Signal_Raw=ifelse(pco2_inst == "0215", Signal_Raw_0215,
                             ifelse(pco2_inst == "0515", Signal_Raw_0515,NA)),
                Signal_Ref=  ifelse(pco2_inst == "0215", Signal_Ref_0215,
                             ifelse(pco2_inst == "0515", Signal_Ref_0515,NA)),
                State_Flush= ifelse(pco2_inst == "0215", State_Flush_0215,
                             ifelse(pco2_inst == "0515", State_Flush_0515,NA)),
                P_In=        ifelse(pco2_inst == "0215", P_In_0215,
                             ifelse(pco2_inst == "0515", P_In_0515,NA)),
                P_NDIR=      ifelse(pco2_inst == "0215", P_NDIR_0215,
                             ifelse(pco2_inst == "0515", P_NDIR_0515,NA)),
                T_Gas=       ifelse(pco2_inst == "0215", T_Gas_0215,
                             ifelse(pco2_inst == "0515", T_Gas_0515,NA)),
                PCO2_Corr=   ifelse(pco2_inst == "0215", PCO2_Corr_0215,
                             ifelse(pco2_inst == "0515", PCO2_Corr_0515,NA)),
                PCO2_Corr_Flush= ifelse(pco2_inst == "0215", PCO2_Corr_Flush_0215,
                                 ifelse(pco2_inst == "0515", PCO2_Corr_Flush_0515,NA)),
                PCO2_Corr_Zero= ifelse(pco2_inst == "0215", PCO2_Corr_Zero_0215,
                                ifelse(pco2_inst == "0515", PCO2_Corr_Zero_0515,NA)))

 # Data processing for pCO2 HydroC CO2 Contros in Ny-Alesund.
 # Documentation received by Steffen Assmann on the 2016-09-15/19 and 
 # stored on /fb_doc/Calibration/pCO2_#0215_#0515
 # Data from the documentation is stored on 
 # awipev/ny_alesund/data/Calibration_pCO2/Data_Processing_sheet_pCO2.txt
 
 # Adding manualy data into Data_Processing_sheet_pCO2 file.
 # Open the data calibration sheet and convert the dates. 

 data_process <- read_excel(paste0(path, "Data_Processing_sheet_pCO2.xlsx"), col_types = c("text","text","text","text", "date","date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "date", "date", "numeric",  "date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
 # data_process$DateCalibration <- dmy(data_process$DateCalibration)
 # data_process$DateCalibrationPost <- dmy(data_process$DateCalibrationPost)
 # data_process$DateDelivery <- dmy(data_process$DateDelivery)
 # data_process$StartDeployment <- ymd_hms(data_process$StartDeployment)
 # data_process$EndDeployment <- ymd_hms(data_process$EndDeployment)
 # Adding the PeriodDeplpCO2 column to z first.
 # Adding the others parameters according to the Contros formula PDF
 # Adding the new period ech time we receive the pCO2 sensor from the calibration
 data <- data %>%
   dplyr::mutate(PeriodDeplpCO2 = ifelse(datetime >= "2015-07-23 00:00:00" & datetime <= "2016-02-23 22:00:00", 1, 
                                  ifelse(datetime >= "2016-02-23 23:00:00" & datetime <= "2017-02-04 23:00:00", 2 ,
                                  ifelse(datetime >= "2017-02-09 00:00:00" & datetime <= "2018-02-09 00:00:00", 3 ,
                                  ifelse(datetime >= "2018-04-14 00:00:00" & datetime <= "2018-10-31 00:00:00", 4 , 
                                  ifelse(datetime >= "2018-10-31 15:00:00" & datetime <= "2019-09-03 12:00:00", 5 ,
                                  ifelse(datetime >= "2019-09-03 17:00:00" & datetime <= "2099-12-02 23:59:59", 6 ,NA )))))))
                 
# Adding the fTsensor, F, P0, T0, k1, k2,k3 (Pre calibration)  + F_Post, P0_Post...(Post calibration) columns to z 
data <- left_join(data,data_process%>%dplyr::select(PeriodDeplpCO2,T0,P0,F, fTsensor,k1,k2,k3, T0_post, P0_post, F_post, fTsensor_post, k1_post,  k2_post, k3_post), by = "PeriodDeplpCO2")

data <- data %>%
  dplyr::mutate(Signal_RawZ = ifelse(State_Zero == 1 ,Signal_Raw, NA),
                Signal_RefZ = ifelse(State_Zero == 1 ,Signal_Ref, NA),
                Signal_ProcZ = ifelse(State_Zero == 1 ,Signal_Proc, NA),
                S2beam = Signal_Raw / Signal_Ref,
                S2beamZ = Signal_RawZ / Signal_RefZ,
                Sprim2beam = S2beam * fTsensor,
                Sprim2beamZ = S2beamZ * fTsensor)

# We remove the 3rd value of Sprim2beamZ during the zeroing (at 00:03:00 and 12:03:00)
# We keep in Sprim2beamZ the 2nd value of the zeroing (at 00:02:00 and 12:02:00) when it exists 
# if it does not, we keep the 1st value (at 00:01:00 and 12:01:00) of the zeroing 
# see Steffen's email on the 2016-12-13 11:11
data$Time <- strftime(data$datetime, "%H:%M:%S")
data$Sprim2beamZ[which(data$Time == "00:02:00" & !is.na(data$Sprim2beamZ)) - 1] <- NA
data$Sprim2beamZ[which(data$Time == "12:02:00" & !is.na(data$Sprim2beamZ)) - 1] <- NA
#z$Sprim2beamZ[which(z$Time == "00:03:00" | z$Time == "12:03:00")] <- NA
data$Sprim2beamZ[which(data$Time != "00:02:00" & data$Time != "12:02:00" & data$Time != "12:01:00" & data$Time != "00:01:00") ] <- NA

# We interpolate Sprim2beamZ and we convert it from "list" to "dataframe"
# because approx returns to a list.
# data$Sprim2beamZ_interp <- NULL
data$Sprim2beamZ_interp <- as.data.frame(approx(x = data$datetime, y = data$Sprim2beamZ ,xout = data$datetime, rule=2))[,2]
# We remove interpolated data that matche a zeroing / flush period
data <- data %>%
  dplyr::mutate(Sprim2beamZ_interp = ifelse( State_Zero == 1 | State_Flush == 1, NA, Sprim2beamZ_interp)
  )

########### pCO2 SPAN DRIFT CORRECTION
# To calculate the drift-corrected polynomial coefficients from the pre and post calibration
# Calculate the new k1 k2 k3
# Extract the first and last S'2beamZ of each periode of deployment.
# the s dataframe needs to be created again with the new pco2 period when the periode will be known.


# s <- ddply(data[,c("datetime","PeriodDeplpCO2","Sprim2beamZ_interp")], .(PeriodDeplpCO2), function(x) x[c(1, nrow(x)), ])
# write.table(s,file= paste0(path, "fb_data/NRT_data/" ),  sep=",", dec=".")

s <- read.table(file= paste0(path, "SpreambeamZ_extraction.csv"), header = TRUE, dec = ".", sep=",")
#s <- s[-c(nrow(s), nrow(s)-1),]
#s$datetime <- ymd_hms(s$datetime)
#ss <- ddply(data[,c("datetime","PeriodDeplpCO2","Sprim2beamZ_interp")], .(PeriodDeplpCO2), function(x) x[c(1, nrow(x)), ])
#s <- rbind(s, ss)
#write.table(s,file= paste0(path, "SpreambeamZ_extraction.csv" ),  sep=",", dec=".", row.names = F)

data <- data %>%
  dplyr::mutate(k1t = ifelse(PeriodDeplpCO2 == 1, k1 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3]  - s[2,3] )) * k1_post,
                      ifelse(PeriodDeplpCO2 == 2, k1 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3]  - s[4,3] )) * k1_post,
                      ifelse(PeriodDeplpCO2 == 3, k1 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3]  - s[6,3] )) * k1_post, 
                      ifelse(PeriodDeplpCO2 == 4, k1 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3]  - s[8,3] )) * k1_post,
                      ifelse(PeriodDeplpCO2 == 5, k1 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3]  - s[10,3] )) * k1_post,
                      ifelse(PeriodDeplpCO2 == 6, k1 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3]  - s[12,3] )) * k1_post,NA)
                                                  )))))) %>%
  dplyr::mutate(k2t = ifelse(PeriodDeplpCO2 == 1, k2 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3]  - s[2,3] )) * k2_post,
                      ifelse(PeriodDeplpCO2 == 2, k2 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3]  - s[4,3] )) * k2_post,
                      ifelse(PeriodDeplpCO2 == 3, k2 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3]  - s[6,3] )) * k2_post, 
                      ifelse(PeriodDeplpCO2 == 4, k2 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3]  - s[8,3] )) * k2_post, 
                      ifelse(PeriodDeplpCO2 == 5, k2 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3]  - s[10,3] )) * k2_post,
                      ifelse(PeriodDeplpCO2 == 6, k2 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3]  - s[12,3] )) * k2_post,NA)
                                                  )))))) %>%
  dplyr::mutate(k3t = ifelse(PeriodDeplpCO2 == 1, k3 + ((s[1,3] - Sprim2beamZ_interp) / (s[1,3]  - s[2,3] )) * k3_post,
                      ifelse(PeriodDeplpCO2 == 2, k3 + ((s[3,3] - Sprim2beamZ_interp) / (s[3,3]  - s[4,3] )) * k3_post,
                      ifelse(PeriodDeplpCO2 == 3, k3 + ((s[5,3] - Sprim2beamZ_interp) / (s[5,3]  - s[6,3] )) * k3_post, 
                      ifelse(PeriodDeplpCO2 == 4, k3 + ((s[7,3] - Sprim2beamZ_interp) / (s[7,3]  - s[8,3] )) * k3_post, 
                      ifelse(PeriodDeplpCO2 == 5, k3 + ((s[9,3] - Sprim2beamZ_interp) / (s[9,3]  - s[10,3] )) * k3_post,
                      ifelse(PeriodDeplpCO2 == 6, k3 + ((s[11,3] - Sprim2beamZ_interp) / (s[11,3]  - s[12,3] )) * k3_post, NA)
                                                  ))))))

data <- data %>%
  dplyr::mutate(k1t = ifelse(!is.na(k1t), k1t, k1)) %>%
  dplyr::mutate(k2t = ifelse(!is.na(k2t), k2t, k2)) %>%
  dplyr::mutate(k3t = ifelse(!is.na(k3t), k3t, k3)) 

# Continue to calculate parameters and FINAL PCO2 CORRECTED (PCO2_corr_contros)
data <- data %>%
  dplyr::mutate(SprimDCt = Sprim2beam / Sprim2beamZ_interp,
                Sproct  = F*(1-SprimDCt),
                xco2wet = (k3t*Sproct^3 + k2t*Sproct^2 + k1t*Sproct)* (P0*(T_Gas + 273.15))/(T0 * P_NDIR),
                PCO2_corr_contros = xco2wet *( P_In / 1013.25)
  )

##### First cleaning
##### Binding several S/N data in one for each parameter ####
## pCO2 
# Removing outliers due to acid flush in the FB at 12:00 and 00:00 
# PCO2_Corr = Raw data from the sensor
# PCO2_corr_contros = Final corrected data from Contros "data processing document"
data <- data %>%
  dplyr::mutate(PCO2_Corr = ifelse(State_Zero >= 1 | State_Flush >= 1 | PCO2_Corr >= 600 | PCO2_Corr <= 50 ,NA, PCO2_Corr),
                PCO2_Corr = ifelse(Time >= "01:30:00" & Time < "12:00:00" | Time >= "13:00:00", PCO2_Corr, NA),
                PCO2_corr_contros = ifelse(State_Zero >= 1 | State_Flush >= 1 | PCO2_corr_contros >= 600 | PCO2_corr_contros <= 50,NA, PCO2_corr_contros),
                PCO2_corr_contros = ifelse(Time >= "01:30:00" & Time < "12:00:00" | Time >= "13:00:00", PCO2_corr_contros, NA),
                Sproct2 = ifelse(Sproct > 1000 | Sproct < -1 , NA, Sproct),
                PCO2_Corr_Zero2 = ifelse(PCO2_Corr_Zero > 20000, NA, PCO2_Corr_Zero))
## TA ##

# TA
data <- data %>%
  dplyr::mutate(AT= ifelse(datetime >= "2016-02-26 12:00:00" & AT_1215 > 100 & InvSal_1215 == 0 & InvpH_1215 == 0 & InvAT_1215 ==0 , AT_1215, NA))
data <- data %>%
  dplyr::mutate(AT= ifelse(datetime >= "2016-02-26 12:00:00" & AT_0317 > 100 & InvSal_0317 == 0 & InvpH_0317 == 0 & InvAT_0317 ==0 , AT_0317, NA))
data <- data %>%
  dplyr::mutate(AT = ifelse(datetime == "2017-05-23 21:07:00", NA, AT)) 
# bind TA_0317 and TA_1215 data = AT
data <- data %>%
  dplyr::mutate(AT= ifelse(!is.na(AT_1215), AT_1215,
                           ifelse(!is.na(AT_0317), AT_0317,NA)))
## seaFET ##
# phINT et ph_EXT _007 et _1005
data <- data %>%
  dplyr::mutate(phINT = ifelse(!is.na(phINT_007),phINT_007,
                               ifelse(!is.na(phINT_1005),phINT_1005, NA)),
                phEXT = ifelse(!is.na(phEXT_007),phEXT_007,
                               ifelse(!is.na(phEXT_1005),phEXT_1005, NA))
  )
data <- data %>%
  dplyr::mutate(phINT = ifelse(phINT >7.5  & phINT <8.5, phINT, NA), 
                phEXT = ifelse(phEXT >7.5  & phEXT <8.5, phEXT, NA))
# voltINT et voltEXT _007 et _1005
data <- data %>%
  dplyr::mutate(voltINT = ifelse(!is.na(voltINT_007),voltINT_007,
                                 ifelse(!is.na(voltINT_1005),voltINT_1005, NA)),
                voltEXT = ifelse(!is.na(voltEXT_007),voltEXT_007,
                                 ifelse(!is.na(voltEXT_1005),voltEXT_1005, NA))
  )

# T_seaF _007 et _1005
data <- data %>%
  dplyr::mutate(T_seaF = ifelse(!is.na(T_seaF_007),T_seaF_007,
                                 ifelse(!is.na(T_seaF_1005),T_seaF_1005, NA))
  )         

##########   first data cleaning : despike()  ###########
#PCO2_Corr_filtered = raw pco2 data from sensor + despike.
#PCO2_corr_contros_filtered = final pco2 data from sensor corrected by "contros" document correction + despike.

data <- data %>%     
   dplyr::mutate(PCO2_Corr_filtered= despike(data$PCO2_Corr, reference= "median", n=0.5, k=121, replace="NA"),
                 PCO2_corr_contros_filtered= despike(data$PCO2_corr_contros, reference= "median", n=1.8, k=155, replace="NA"),
                 sal_filtered= despike(data$sal, reference= "median", n=1, k=65, replace="NA"),
                 sal_fb_filtered= despike(data$sal_fb, reference= "median", n=1, k=65, replace="NA"),
                 sal_insitu_filtered= despike(data$sal_insitu, reference= "median", n=1, k=65, replace="NA"),
                 temp_fb_filtered= despike(data$temp_fb, reference= "median", n=1, k=65, replace="NA"),
                 AT_filtered= despike(data$AT, reference= "median", n=0.5, k=121, replace="NA"),
                 phINT_filtered= despike(data$phINT, reference= "median", n=8, k=241, replace="NA"),
                 phEXT_filtered= despike(data$phEXT, reference= "median", n=8, k=241, replace="NA"),
                 HW_pH1_filtered= despike(data$HW_pH1, reference= "median", n=8, k=241, replace="NA"),
                 HW_Temperature1_filtered= despike(data$HW_Temperature1, reference= "median", n=1, k=65, replace="NA"),
                 Temp_SBE38_filtered= despike(data$Temp_SBE38, reference= "median", n=0.5, k=65, replace="NA"),
                 date = as.Date(data$datetime),
                 hour = hour(data$datetime)
  )

# # for 2015 because special year not complete with NA. 
# data <- data %>%     
#   dplyr::mutate(PCO2_Corr_filtered= PCO2_Corr,
#                 PCO2_corr_contros_filtered= PCO2_corr_contros,
#                 Salinity_filtered= despike(data$Salinity, reference= "median", n=1, k=65, replace="NA"),
#                 Temperature_filtered= despike(data$Temperature, reference= "median", n=1, k=65, replace="NA"),
#                 Temp_SBE38_filtered= despike(data$Temp_SBE38, reference= "median", n=0.5, k=65, replace="NA"),
#                 HW_Temperature1_filtered= "NA",
#                 HW_pH1_filtered= "NA",
#                 AT_filtered="NA",
#                 phINT_filtered= "NA",
#                 phEXT_filtered= "NA"
#               )

#MINUTE
# save all parameters in long format (not selected data)
if (file.exists(paste0(path, "all_parameters_nydata_minute.rds")) == TRUE) {
  previous_all_parameters_data_minute <- readRDS(paste0(path, "all_parameters_nydata_minute.rds"))
  #### Binding data (previous + new) ####
  data <- rbind(previous_all_parameters_data_minute, data)
  #Remove duplicate due to binding
  data <-  data %>%
  distinct(datetime, .keep_all = T)
saveRDS(file= paste0(path, "all_parameters_nydata_minute.rds"), data)
} else {
  saveRDS(file= paste0(path, "all_parameters_nydata_minute.rds"), data)
}

# HOUR
data_hour <- data%>%
  dplyr::group_by( date, hour) %>%
  dplyr::summarise( State_Zero = mean(State_Zero, na.rm = TRUE),
                    State_Flush = mean(State_Flush, na.rm = TRUE),
                    Signal_Ref= mean(Signal_Ref, na.rm = TRUE),
                    Signal_Raw = mean(Signal_Raw, na.rm = TRUE),
                    Signal_Proc = mean(Signal_Proc, na.rm = TRUE),
                    P_In= mean(P_In, na.rm = TRUE),
                    P_NDIR= mean(P_NDIR, na.rm = TRUE),
                    T_Gas= mean(T_Gas, na.rm = TRUE),
                    PCO2_Corr= mean(PCO2_Corr, na.rm = TRUE),
                    PCO2_Corr_Flush= mean(PCO2_Corr_Flush, na.rm = TRUE),
                    PCO2_Corr_Zero= mean(PCO2_Corr_Zero, na.rm = TRUE),
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
                    PCO2_corr_contros= mean(PCO2_corr_contros, na.rm = TRUE) ) %>%
  dplyr::mutate(datetime = ymd_h(paste(date, hour, sep=" ", tz = "UTC"))) %>%
  dplyr::ungroup() %>% # this is to be able to perform the following changes
  dplyr::select(datetime, everything()) %>%
  dplyr::arrange(desc(datetime))

#### Download previous data ####
if (file.exists(paste0(path, "all_parameters_nydata_hour.rds")) == TRUE) {
  previous_all_parameters_data_hour <- readRDS(paste0(path, "all_parameters_nydata_hour.rds"))
  #### Binding data (previous + new) ####
  data_hour <- rbind(previous_all_parameters_data_hour, data_hour)
  #Remove duplicate due to binding
  data_hour <-  data_hour %>%
    distinct(datetime, .keep_all = T)
  saveRDS(file= paste0(path, "all_parameters_nydata_hour.rds"), data_hour)
} else {
  saveRDS(file= paste0(path, "all_parameters_nydata_hour.rds"), data_hour)
}

#### MINUTE format (small format) ####
selected_data_minute <- data  %>%
    dplyr::select( datetime,
                   PCO2_Corr_filtered,
                   PCO2_corr_contros_filtered,
                   PeriodDeplpCO2, 
                   sal_filtered,
                   sal_fb_filtered,
                   sal_insitu_filtered,
                   temp_fb_filtered,
                   Temp_SBE38_filtered,
                   HW_Temperature1_filtered,
                   HW_pH1_filtered,
                   AT_filtered,
                   voltINT,
                   voltEXT,
                   phINT_filtered,
                   phEXT_filtered,
                   T_seaF,
                   pco2_inst,
                   ta_inst,
                   seafet_inst,
                   date,
                   hour)

if (file.exists(paste0(path, "all_nydata_minute.rds")) == TRUE) {
  previous_data_minute <- readRDS(paste0(path, "all_nydata_minute.rds"))
  #### Binding data (previous + new) ####
  selected_data_minute <- rbind(previous_data_minute, selected_data_minute)
  #Remove duplicate due to binding
  selected_data_minute <-  selected_data_minute %>%
    distinct(datetime, .keep_all = T)
  saveRDS(file= paste0(path, "all_nydata_minute.rds"), selected_data_minute)
} else {
  saveRDS(file= paste0(path, "all_nydata_minute.rds"), selected_data_minute)
}

#### HOUR format ####
# to add instrument S/N columns
selected_data_minute$pco2_inst <- as.numeric(selected_data_minute$pco2_inst)
selected_data_minute$ta_inst <- as.numeric(selected_data_minute$ta_inst)
selected_data_minute$seafet_inst <- as.numeric(selected_data_minute$seafet_inst)

selected_data_hour <- selected_data_minute%>%
  dplyr::group_by( date, hour) %>%
  dplyr::summarise(sal_filtered = mean(sal_filtered, na.rm = TRUE),
                   sal_insitu_filtered = mean(sal_insitu_filtered, na.rm = TRUE),
                   sal_fb_filtered = mean(sal_fb_filtered, na.rm = TRUE),
                   temp_fb_filtered = mean(temp_fb_filtered, na.rm = TRUE),
                   Temp_SBE38_filtered= mean(Temp_SBE38_filtered, na.rm = TRUE),
                   PCO2_Corr_filtered = mean(PCO2_Corr_filtered, na.rm = TRUE),
                   PeriodDeplpCO2 = mean(PeriodDeplpCO2, na.rm = TRUE),
                   PCO2_corr_contros_filtered= mean(PCO2_corr_contros_filtered, na.rm = TRUE),
                   AT_filtered= mean(AT_filtered, na.rm = TRUE),
                   HW_pH1_filtered= mean(HW_pH1_filtered, na.rm = TRUE),
                   HW_Temperature1_filtered= mean(HW_Temperature1_filtered, na.rm = TRUE),
                   T_seaF= mean(T_seaF, na.rm = TRUE),
                   phINT_filtered= mean(phINT_filtered, na.rm = TRUE),
                   phEXT_filtered= mean(phEXT_filtered, na.rm = TRUE),
                   voltINT= mean(voltINT, na.rm = TRUE),
                   voltEXT= mean(voltEXT, na.rm = TRUE),
                   pco2_inst= mean(pco2_inst, na.rm = TRUE),
                   ta_inst= mean(ta_inst, na.rm = TRUE),
                   seafet_inst= mean(seafet_inst, na.rm = TRUE) ) %>%
  dplyr::mutate(datetime = ymd_h(paste(date, hour, sep=" ", tz = "UTC"))) %>%
  dplyr::ungroup() %>% # this is to be able to perform the following changes
  dplyr::select(datetime, everything()) %>%
  dplyr::select(-c( hour)) %>% # not needed in the shiny display
  dplyr::arrange(desc(datetime))

# despike is run a 2nd time ON HOUR FORMAT DATA for TA in order to remove wrong flush measurements
selected_data_hour <- selected_data_hour %>%     
  dplyr::mutate(AT_filtered = despike(selected_data_hour$AT_filtered, reference= "median", n=0.3, k=217, replace="NA")) 

# HOUR format
d_hour <- selected_data_hour

if (file.exists(paste0(path, "all_nydata_hour.rds")) == TRUE) {
  previous_data_hour <- readRDS(paste0(path, "all_nydata_hour.rds"))
  #### Binding data (previous + new) ####
  d_hour <- rbind(previous_data_hour, d_hour)
  #Remove duplicate due to binding
  d_hour <-  d_hour %>%
    distinct(datetime, .keep_all = T)
  saveRDS(file= paste0(path, "all_nydata_hour.rds"), d_hour)
} else {
  saveRDS(file= paste0(path, "all_nydata_hour.rds"), d_hour)
}

#save(file= paste0(path, "fb_awipev-co2_server/ny-alesund/data/processed/all_nydata_hour.Rdata"), d_hour)

#@@@@@@@@@@@@@@@@@@@@@@@@
#load(file = paste0(path, "all_nydata_hour.Rdata"))


# # PLOT TEST
at_contros_cleaned_xts <- dplyr::select(d_hour,datetime,sal_insitu_filtered, sal_fb_filtered, sal_filtered )
at_contros_cleaned_xts <- as.xts(at_contros_cleaned_xts, order.by = d_hour$datetime)
dygraph(at_contros_cleaned_xts, group = "awipev", main=" ", ylab="pco2") %>%
#dySeries("Sproct",  label = "Sproct", color = "red", strokeWidth = 0, pointSize=2) %>%
  dySeries("sal_filtered", label="sal_filtered", color = "black", strokeWidth = 0, pointSize=4) %>%
dySeries( "sal_fb_filtered", label = "sal_fb_filtered",  color = "red", strokeWidth = 0, pointSize=2) %>%
  dySeries("sal_insitu_filtered", label = " sal_insitu_filtered",color = "green", strokeWidth = 0, pointSize=0.5) %>%
  #dySeries("Sprim2beamZ_interp", label = "Sprim2beamZ_interp",color =" grey", strokeWidth = 0, pointSize=1) %>%
 #dyAxis("y",valueRange = c(-2, 2)) %>%
  dyLimit(0,color = "black", strokePattern ="dashed") %>%
  dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) %>%
  dyOptions(useDataTimezone = TRUE,drawGrid = TRUE, drawPoints = TRUE, strokeWidth= 0, digitsAfterDecimal = 5) %>%
  dyRangeSelector(height = 30)

# # Bad period for pCO2 (N°4) - look at period 5 - 6
# tmp <- data_hour %>%
#   dplyr::mutate(PeriodDeplpCO2 = ifelse(datetime >= "2015-07-23 00:00:00" & datetime <= "2016-02-23 22:00:00", 1, 
#                                         ifelse(datetime >= "2016-02-23 23:00:00" & datetime <= "2017-02-04 23:00:00", 2 ,
#                                                ifelse(datetime >= "2017-02-09 00:00:00" & datetime <= "2018-02-09 00:00:00", 3 ,
#                                                       ifelse(datetime >= "2018-04-14 00:00:00" & datetime <= "2018-10-31 00:00:00", 4 , 
#                                                              ifelse(datetime >= "2018-10-31 15:00:00" & datetime <= "2019-09-03 12:00:00", 5 ,
#                                                                     ifelse(datetime >= "2019-09-03 17:00:00" & datetime <= "2099-12-02 23:59:59", 6 ,NA )))))))
# 
# tmp <- tmp %>%
#   dplyr::filter(PeriodDeplpCO2 <= 4)%>%
# dplyr::select(-c(date, hour, PeriodDeplpCO2))
# #Plot les parametres en facette
# 
# tmp_melt<- melt(tmp, id.vars="datetime")
# 
# several_pco2_parameters <- ggplot(tmp_melt) + geom_point(aes(x=datetime, y=value, color=variable),size=0.5) +
#   facet_grid(variable~., scales="free_y") + 
#   theme_bw()+ xlab("Time")+ylab("")+
#   scale_colour_discrete(guide="none")
# 
# ggsave("../../pCloud Sync/exp168_AWIPEV-CO2/fb_figures/pCO2/Bad_period/several_pco2_parameters.png",several_pco2_parameters, height= 108,width = 24, units="cm")
