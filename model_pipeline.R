library(tidyverse)
library(tidymodels)
library(here)

data1 <- read_csv(here('out/GREEN4.csv')) %>%
    select(wy, month, load, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'Colorado')
data2 <- read_csv(here('out/w3.csv')) %>%
    select(wy, month, load, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'Vermont')
data3 <- read_csv(here('out/GSMACK.csv')) %>%
    select(wy, month, load, yield, pH_mean, gpp_mean, precip_mean) %>%
    mutate(site = 'California')

data <- rbind(data1, data2, data3)

pre_caa <- data %>%
    filter(wy < 1990,
           wy > 1987) %>%
    select(-wy)

pre_caa %>%
    select(-site, -month) %>%
    pairs()

post_caa <- data %>%
    filter(wy < 2013,
           wy > 2010) %>%
    select(-wy)

post_caa %>%
    select(-site, -month) %>%
    pairs()


