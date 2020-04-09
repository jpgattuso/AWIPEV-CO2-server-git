library(dplyr)
library(dygraphs)
library(xts)
path = "../../pCloud\ Sync/Documents/experiments/exp168_AWIPEV-CO2/fb_awipev-co2_server/ny-alesund/data/NRT_data/"
tmp <- as_tibble(readRDS(file= paste0(path, "all_parameters_nydata_minute.rds")))
tmp2 <- tmp %>%
  dplyr::select(datetime, PCO2_corr_contros_filtered) %>%
  dplyr::filter(datetime < "2017-01-01")
tmp_xts <- as.xts(tmp2, order.by = tmp2$datetime)
dygraph(tmp_xts, group = "awipev", main=" ", ylab="pCO2") %>%
#dySeries("Sproct",  label = "Sproct", color = "red", strokeWidth = 0, pointSize=2) %>%
  dySeries("PCO2_corr_contros_filtered", label="pCO2", color = "black", strokeWidth = 0, pointSize=4) %>%
#dySeries( "sal_fb_filtered", label = "sal_fb_filtered",  color = "red", strokeWidth = 0, pointSize=2) %>%
 # dySeries("sal_insitu_filtered", label = " sal_insitu_filtered",color = "green", strokeWidth = 0, pointSize=0.5) %>%
  #dySeries("Sprim2beamZ_interp", label = "Sprim2beamZ_interp",color =" grey", strokeWidth = 0, pointSize=1) %>%
 #dyAxis("y",valueRange = c(-3, 8)) %>%
  dyLimit(0,color = "black", strokePattern ="dashed") %>%
  dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = TRUE) %>%
  dyOptions(useDataTimezone = TRUE,drawGrid = TRUE, drawPoints = TRUE, strokeWidth= 0, digitsAfterDecimal = 5) %>%
  dyRangeSelector(height = 30)
