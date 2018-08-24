library(future)
library(pryr)
library(tidyverse)
library(profvis)
library(microbenchmark)
library(sapfluxnetr)

#####
# folder <- 'tests/testthat/big_test/plant/'
# sites <- list.files(folder, pattern = '.RData')[1:10] %>%
#   stringr::str_remove('.RData')
# metadata <- read_sfn_metadata(folder)
# site_objects <- read_sfn_data(sites, folder)
# metrics_res <- read_sfn_data(sites, folder) %>%
#   daily_metrics()
# interval <- 'general'
# 
# 
# 
# mbm <- microbenchmark(
#   tidyfier_default = metrics_tidyfier(metrics_res, metadata, interval),
#   tidyfier_alt = metrics_tidyfier_alt(metrics_res, metadata, interval),
#   check = function(values) {all_equal(values[[1]], values[[2]])},
#   times = 10
# )
# 
# mbm
# autoplot.microbenchmark(mbm)
# 
# pv_default <- profvis({
#   metrics_tidyfier(metrics_res, sfn_metadata, interval)
# })
# 
# 
# pv_sapf_alt <- profvis({
#   metrics_tidyfier_alt(metrics_res, sfn_metadata, interval)
# })
# 
# pv_daily_metrics <- profvis({
#   daily_metrics(read_sfn_data(sites, folder))
# })
# 
# profvis({
#   site_objects <- read_sfn_data(sites, folder)
#   pause(2)
#   tururu <- function(site_objects) {
#     inherits(site_objects, 'sfn_data_multi')
#     class(site_objects) == 'sfn_data_multi'
#     is(site_objects, 'sfn_data_multi')
#     class(sfn_data) %in% c('sfn_data', 'sfn_data_multi')
#   }
#   
#   tururu(site_objects)
# })


options('future.globals.maxSize' = 4*1014*1024^2)
plan('sequential')
profvis({
  
  init_mem <- mem_used()
  init_mem
  
  folder <- '../sapfluxnet_db/0.0.3/sapwood/'
  sites <- list.files(folder, pattern = '.RData') %>%
    stringr::str_remove('.RData')
  metadata <- read_sfn_metadata(folder, .write_cache = FALSE)
  res <- read_sfn_data(sites, folder) %>%
    daily_metrics(tidy = TRUE, metadata = metadata)
  
  end_mem <- mem_used()
  end_mem
  print(end_mem - init_mem)
  
}) %>%
  htmlwidgets::saveWidget('~/Descargas/pv_complete_seq.html')
browseURL('~/Descargas/pv_complete_seq.html')

options('future.globals.maxSize' = 1014*1024^2)
plan('multicore')
# profvis({
  
  init_mem <- mem_used()
  init_mem
  
  tictoc::tic()
  folder <- '../sapfluxnet_db/0.0.3/sapwood/'
  sites <- list.files(folder, pattern = '.RData') %>%
    stringr::str_remove('.RData')
  metadata <- read_sfn_metadata(folder, .write_cache = FALSE)
  res <- read_sfn_data(sites, folder) %>%
    daily_metrics(tidy = TRUE, metadata = metadata)
  tictoc::toc()
  
  end_mem <- mem_used()
  end_mem
  print(end_mem - init_mem)
  
# }) %>%
  # htmlwidgets::saveWidget('~/Descargas/pv_complete_mc.html')
# browseURL('~/Descargas/pv_complete_mc.html')
