library(tidyverse)
library(fmi2)
library(dygraphs)

get_pressure <- function(start, end) {
  data <- obs_weather_hourly(starttime = start,
                             endtime = end,
                             fmisid = 100971) %>% 
    dplyr::filter(variable == "PA_PT1H_AVG" & lubridate::hour(time) == "10" & !is.na(value)) %>% 
    mutate(date = lubridate::date(time))
  
  return(data)
}

startdates <- seq(ymd('2021-01-01'), ymd('2023-12-01'), by = '1 month')
enddates <- seq(ymd('2021-01-23'), ymd('2023-12-23'), by = '1 month')

res <- purrr::map2(startdates, enddates, get_pressure)
res_df <- list_rbind(res)

# xts object
time <- res_df$date
pres <- res_df %>% 
  xts::xts(x = res_df$value, order.by = time)

g <- dygraph(pres, main="Air pressure in Kaisaniemi at 10 am <br> Data: FMI | graph: @tts") %>% 
  dySeries("V1", label = "Pressure") %>%
  dyAxis("y", label = "hPa") %>%
  dyLegend(show = "onmouseover", hideOnMouseOut = TRUE) %>% 
  dyRangeSelector() %>% 
  dyUnzoom()

htmlwidgets::saveWidget(g, file = "press.html")
