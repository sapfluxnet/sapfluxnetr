sfn_data_fixes <- function(db_folder) {
  
  data_levels_folders <- file.path(db_folder, 'RData', c('plant', 'sapwood', 'leaf'))
  sites <- c('AUS_MAR_HSW_HIG', 'COL_MAC_SAF_RAD', 'ESP_YUN_T3_THI')
  
  for (folder in data_levels_folders) {
    AUS_MAR_HSW_HIG_fixes(folder)
    COL_MAC_SAF_RAD_fixes(folder)
    ESP_YUN_T3_THI_fixes(folder)
  }
}

ESP_YUN_T3_THI_fixes <- function(folder) {
  
  if (!file.exists(file.path(folder, 'ESP_YUN_T3_THI.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('ESP_YUN_T3_THI', folder)
  
  good_sapf_data <- bad_site %>%
    get_sapf_data() %>%
    select(
      ESP_YUN_T3_THI_Api_Js_1 = ESP_YUN_T3_Api_Js_1,
      ESP_YUN_T3_THI_Api_Js_2 = ESP_YUN_T3_Api_Js_2,
      ESP_YUN_T3_THI_Api_Js_3 = ESP_YUN_T3_Api_Js_3,
      ESP_YUN_T3_THI_Api_Js_4 = ESP_YUN_T3_Api_Js_4,
      ESP_YUN_T3_THI_Api_Js_5 = ESP_YUN_T3_Api_Js_5,
      ESP_YUN_T3_THI_Api_Js_6 = ESP_YUN_T3_Api_Js_6
    )
  
  good_sapf_flags <- bad_site %>%
    get_sapf_flags() %>%
    select(
      ESP_YUN_T3_THI_Api_Js_1 = ESP_YUN_T3_Api_Js_1,
      ESP_YUN_T3_THI_Api_Js_2 = ESP_YUN_T3_Api_Js_2,
      ESP_YUN_T3_THI_Api_Js_3 = ESP_YUN_T3_Api_Js_3,
      ESP_YUN_T3_THI_Api_Js_4 = ESP_YUN_T3_Api_Js_4,
      ESP_YUN_T3_THI_Api_Js_5 = ESP_YUN_T3_Api_Js_5,
      ESP_YUN_T3_THI_Api_Js_6 = ESP_YUN_T3_Api_Js_6
    )
  
  good_plant_md <- bad_site %>%
    get_plant_md() %>%
    mutate(
      pl_code = stringr::str_replace(pl_code, 'ESP_YUN_T3_', 'ESP_YUN_T3_THI_')
    )
  
  ESP_YUN_T3_THI <- bad_site
  get_sapf_data(ESP_YUN_T3_THI) <- good_sapf_data
  get_sapf_flags(ESP_YUN_T3_THI) <- good_sapf_flags
  get_plant_md(ESP_YUN_T3_THI) <- good_plant_md
  
  save(ESP_YUN_T3_THI, file = file.path(folder, 'ESP_YUN_T3_THI.RData'))
}

COL_MAC_SAF_RAD_fixes <- function(folder) {
  
  if (!file.exists(file.path(folder, 'COL_MAC_SAF_RAD.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('COL_MAC_SAF_RAD', folder)
  
  good_sapf_data <- bad_site %>%
    get_sapf_data() %>%
    select(
      COL_MAC_SAF_RAD_Tca_Js_1 = COL_MAC_SAF_Tca_Js_1,
      COL_MAC_SAF_RAD_Tca_Js_2 = COL_MAC_SAF_Tca_Js_2,
      COL_MAC_SAF_RAD_Tca_Js_3 = COL_MAC_SAF_Tca_Js_3
    )
  
  good_sapf_flags <- bad_site %>%
    get_sapf_flags() %>%
    select(
      COL_MAC_SAF_RAD_Tca_Js_1 = COL_MAC_SAF_Tca_Js_1,
      COL_MAC_SAF_RAD_Tca_Js_2 = COL_MAC_SAF_Tca_Js_2,
      COL_MAC_SAF_RAD_Tca_Js_3 = COL_MAC_SAF_Tca_Js_3
    )
  
  good_plant_md <- bad_site %>%
    get_plant_md() %>%
    mutate(
      pl_code = stringr::str_replace(pl_code, 'COL_MAC_SAF_', 'COL_MAC_SAF_RAD_')
    )
  
  COL_MAC_SAF_RAD <- bad_site
  get_sapf_data(COL_MAC_SAF_RAD) <- good_sapf_data
  get_sapf_flags(COL_MAC_SAF_RAD) <- good_sapf_flags
  get_plant_md(COL_MAC_SAF_RAD) <- good_plant_md
  
  save(COL_MAC_SAF_RAD, file = file.path(folder, 'COL_MAC_SAF_RAD.RData'))
}

AUS_MAR_HSW_HIG_fixes <- function(folder) {
  
  if (!file.exists(file.path(folder, 'AUS_MAR_HSW_HIG.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('AUS_MAR_HSW_HIG', folder)
  
  good_sapf_data <- bad_site %>%
    get_sapf_data() %>%
    select(
      AUS_MAR_HSW_HIG_Eob_Js_1 = AUS_MAR_HSW_Eob_Js_1,
      AUS_MAR_HSW_HIG_Eob_Js_2 = AUS_MAR_HSW_Eob_Js_2,
      AUS_MAR_HSW_HIG_Eob_Js_3 = AUS_MAR_HSW_Eob_Js_3,
      AUS_MAR_HSW_HIG_Ecy_Js_4 = AUS_MAR_HSW_Ecy_Js_4
    )
  
  good_sapf_flags <- bad_site %>%
    get_sapf_flags() %>%
    select(
      AUS_MAR_HSW_HIG_Eob_Js_1 = AUS_MAR_HSW_Eob_Js_1,
      AUS_MAR_HSW_HIG_Eob_Js_2 = AUS_MAR_HSW_Eob_Js_2,
      AUS_MAR_HSW_HIG_Eob_Js_3 = AUS_MAR_HSW_Eob_Js_3,
      AUS_MAR_HSW_HIG_Ecy_Js_4 = AUS_MAR_HSW_Ecy_Js_4
    )
  
  good_plant_md <- bad_site %>%
    get_plant_md() %>%
    mutate(
      pl_code = stringr::str_replace(pl_code, 'AUS_MAR_HSW_', 'AUS_MAR_HSW_HIG_')
    )
  
  AUS_MAR_HSW_HIG <- bad_site
  get_sapf_data(AUS_MAR_HSW_HIG) <- good_sapf_data
  get_sapf_flags(AUS_MAR_HSW_HIG) <- good_sapf_flags
  get_plant_md(AUS_MAR_HSW_HIG) <- good_plant_md
  
  save(AUS_MAR_HSW_HIG, file = file.path(folder, 'AUS_MAR_HSW_HIG.RData'))
}

