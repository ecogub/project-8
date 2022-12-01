library(macrosheds)
library(tidyverse)
ms_catalog <- ms_catalog()

ms_vars <- ms_catalog()

target_sites <- ms_catalog %>%
    filter(variable_code == 'Si',
           unit == 'mg/L',
           first_record_utc < '1989-09-30',
           #last_record_utc > "2000-9-30",
           domain != 'arctic',
           domain != 'luquillo')
ggplot(., aes(x = mean_obs_per_day))+
    geom_histogram()
