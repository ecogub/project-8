library(macrosheds)
library(tidyverse)
library(here)
library(feather)
library(RiverLoad)
library(lubridate)
library(lfstat)
library(imputeTS)

my_ms_dir = 'C:/Users/gubbins/Dropbox/RSFME/data/ms'

ms_catalog <- ms_catalog()

ms_vars <- ms_catalog()

ms_sites <- ms_download_site_data()

target_sites <- ms_catalog %>%
    filter(variable_code == 'Si',
           unit == 'mg/L',
           first_record_utc < '1989-09-30',
           #last_record_utc > "2000-9-30",
           domain != 'arctic',
           domain != 'luquillo')

target_site = 'GREEN4'

# selena use 'w3' from 'HBEF' as your site instead of EB

aggegrate_dataset <- function(target_site, my_ms_dir){

# data reading magic #####
site_info <- ms_sites %>%
    filter(site_code == target_site)

area <- site_info$ws_area_ha[[1]]

ws_trait_file_path <- here('src', 'ws_traits.csv')

trait_df <- read_csv(ws_trait_file_path) %>%
    filter(site_name == target_site) %>%
    pivot_wider(names_from = var, values_from = val, id_cols = c(year),values_fn = mean) %>%
    rename(wy = year) %>%
    mutate(wy = as.factor(wy))


chem_file_path <- file.path(my_ms_dir, site_info$domain[[1]], 'stream_chemistry', paste0(target_site,'.feather'))

if(site_info$domain[[1]] == 'hbef'){
    chem_df <- read_feather(chem_file_path) %>%
        filter(var == 'GN_SiO2_Si' |
                   var == 'GN_pH') %>%
        pivot_wider(names_from = var, values_from = val, id_cols = c(datetime, site_code)) %>%
        rename(con = GN_SiO2_Si,
               pH = GN_pH)
}

if(site_info$domain[[1]] != 'hbef'){
chem_df <- read_feather(chem_file_path) %>%
    filter(var == 'GN_Si' |
           var == 'GN_pH') %>%
    pivot_wider(names_from = var, values_from = val, id_cols = c(datetime, site_code)) %>%
    rename(con = GN_Si,
           pH = GN_pH)
}

q_file_path <- file.path(my_ms_dir, site_info$domain[[1]], 'discharge', paste0(target_site,'.feather'))

q_df <- read_feather(q_file_path) %>%
    rename(q_lps = val) %>%
    select(datetime, site_code, q_lps)

precip_file_path <- here('src', 'ws_traits_subannual', site_info$domain[[1]],'ws_traits','cc_precip', paste0('raw_',target_site,'.feather'))

precip_df <- read_feather(precip_file_path) %>%
    pivot_wider(names_from = var, values_from = val, id_cols = c(datetime))

gpp_file_path <- here('src', 'ws_traits_subannual', site_info$domain[[1]],'ws_traits','gpp', paste0('raw_',target_site,'.feather'))

gpp_df <- read_feather(gpp_file_path) %>%
    pivot_wider(names_from = var, values_from = val, id_cols = c(datetime))

comb_df <- full_join(chem_df, q_df, by = c('site_code', 'datetime')) %>%
    full_join(., gpp_df, by = 'datetime') %>%
    full_join(., precip_df, by = 'datetime') %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))

# check there is enough chem data
good_years <- comb_df %>%
    group_by(wy) %>%
    filter(!is.na(con)) %>%
    count() %>%
    filter(n > 10) %>%
    pull(wy)
length(good_years)

# put everything together and output#####
out_df <- comb_df %>%
    filter(wy %in% good_years) %>%
    mutate(con = na_interpolation(con, option = 'linear'),
           q_lps = na_interpolation(q_lps, option = 'linear'),
           cc_precip_median = na_interpolation(cc_precip_median, option = 'linear'),
           va_gpp_median = na_interpolation(va_gpp_median, option = 'linear'),
           pH = na_interpolation(pH, option = 'linear'),
           q_day = q_lps*86400,
           flux_kg_day = q_day*con*1e6,
           load_day = flux_kg_day/area,
           month = month(datetime)) %>%
    group_by(wy, month) %>% #grouping by water year
    # ADD MORE ANNUAL STUFF BELOW
    summarize(load = sum(load_day),
              yield = sum(q_day),
              load_yield_adj = sum(load/yield),
              pH_mean = mean(na.omit(pH)),
              gpp_mean = mean(va_gpp_median),
              precip_mean = mean(cc_precip_median)) %>%
    left_join(., trait_df, by = 'wy')

return(out_df)
}


#load_yield_adj is what we are trying to predict
# then run random forest on years prior to 1990 and current years

write_csv(aggegrate_dataset('w3', my_ms_dir), file = here('out/w3.csv'))
write_csv(aggegrate_dataset('GREEN4', my_ms_dir), file = here('out/GREEN4.csv'))
write_csv(aggegrate_dataset('GSMACK', my_ms_dir), file = here('out/GSMACK.csv'))
