library(pryr)
library(tidyverse)
library(sapfluxnetr)
library(profvis)
library(future)

options('future.globals.maxSize' = 2*1014*1024^2)

init_mem <- mem_used()
profvis({
  # This will need at least 5GB of memory during the process
  folder <- '../sapfluxnet_db/0.0.3/sapwood'
  sfn_metadata <- read_sfn_metadata(folder)
  sites <- list.files(folder, '.RData') %>%
    str_remove('.RData')
  
  daily_results <- read_sfn_data(sites[1:100], folder) %>%
    daily_metrics(tidy = TRUE, metadata = sfn_metadata)
}) %>%
  htmlwidgets::saveWidget('~/Descargas/pv_100_sites.html')
browseURL('~/Descargas/pv_100_sites.html')
end_mem <- mem_used()
end_mem - init_mem

init_mem <- mem_used()
profvis({
  folder <- '../sapfluxnet_db/0.0.3/sapwood'
  sfn_metadata <- read_sfn_metadata(folder)
  sites <- sites <- list.files(folder, '.RData') %>%
    str_remove('.RData')
  
  daily_results_1 <- read_sfn_data(sites[1:25], folder) %>%
    daily_metrics(tidy = TRUE, metadata = sfn_metadata)
  daily_results_2 <- read_sfn_data(sites[26:50], folder) %>%
    daily_metrics(tidy = TRUE, metadata = sfn_metadata)
  daily_results_3 <- read_sfn_data(sites[51:75], folder) %>%
    daily_metrics(tidy = TRUE, metadata = sfn_metadata)
  daily_results_4 <- read_sfn_data(sites[76:100], folder) %>%
    daily_metrics(tidy = TRUE, metadata = sfn_metadata)
  
  daily_results <- bind_rows(
    daily_results_1, daily_results_2,
    daily_results_3, daily_results_4
  )
  
  rm(daily_results_1, daily_results_2, daily_results_3, daily_results_4)
}) %>%
  htmlwidgets::saveWidget('~/Descargas/pv_100_sites_spl.html')
browseURL('~/Descargas/pv_100_sites_spl.html')
end_mem <- mem_used()
end_mem - init_mem

