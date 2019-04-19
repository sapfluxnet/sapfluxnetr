sfn_data_fixes <- function(db_folder, csv = FALSE) {
  
  data_levels_folders <- file.path(db_folder, 'RData', c('plant', 'sapwood', 'leaf'))
  sites <- c('AUS_MAR_HSW_HIG', 'COL_MAC_SAF_RAD', 'ESP_YUN_T3_THI')
  
  for (folder in data_levels_folders) {
    AUS_MAR_HSW_HIG_fixes(folder, csv)
    COL_MAC_SAF_RAD_fixes(folder, csv)
    ESP_YUN_T3_THI_fixes(folder, csv)
    .write_metadata_cache(file.path(folder))
  }
}

ESP_YUN_T3_THI_fixes <- function(folder, csv = FALSE) {
  
  if (!file.exists(file.path(folder, 'ESP_YUN_T3_THI.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('ESP_YUN_T3_THI', folder)
  
  # check sapflow
  if ('ESP_YUN_T3_THI_Api_Js_1' %in% names(get_sapf_data(bad_site))) {
    good_sapf_data <- bad_site %>%
      get_sapf_data()
  } else {
    good_sapf_data <- bad_site %>%
      get_sapf_data() %>%
      dplyr::select(
        ESP_YUN_T3_THI_Api_Js_1 = ESP_YUN_T3_Api_Js_1,
        ESP_YUN_T3_THI_Api_Js_2 = ESP_YUN_T3_Api_Js_2,
        ESP_YUN_T3_THI_Api_Js_3 = ESP_YUN_T3_Api_Js_3,
        ESP_YUN_T3_THI_Api_Js_4 = ESP_YUN_T3_Api_Js_4,
        ESP_YUN_T3_THI_Api_Js_5 = ESP_YUN_T3_Api_Js_5,
        ESP_YUN_T3_THI_Api_Js_6 = ESP_YUN_T3_Api_Js_6
      )
  }
  # check flags
  if ('ESP_YUN_T3_THI_Api_Js_1' %in% names(get_sapf_flags(bad_site))) {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags()
  } else {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags() %>%
      dplyr::select(
        ESP_YUN_T3_THI_Api_Js_1 = ESP_YUN_T3_Api_Js_1,
        ESP_YUN_T3_THI_Api_Js_2 = ESP_YUN_T3_Api_Js_2,
        ESP_YUN_T3_THI_Api_Js_3 = ESP_YUN_T3_Api_Js_3,
        ESP_YUN_T3_THI_Api_Js_4 = ESP_YUN_T3_Api_Js_4,
        ESP_YUN_T3_THI_Api_Js_5 = ESP_YUN_T3_Api_Js_5,
        ESP_YUN_T3_THI_Api_Js_6 = ESP_YUN_T3_Api_Js_6
      )
  }
  # check plant names in plant metadata
  is_plant_md_ok <- bad_site %>%
    get_plant_md() %>%
    pull(pl_code) %>%
    str_detect(pattern = '_THI_') %>%
    all()
  
  if (is_plant_md_ok) {
    good_plant_md <- bad_site %>%
      get_plant_md()
  } else {
    good_plant_md <- bad_site %>%
      get_plant_md() %>%
      dplyr::mutate(
        pl_code = stringr::str_replace(pl_code, 'ESP_YUN_T3_', 'ESP_YUN_T3_THI_')
      )
  }
  
  # finaly build the fixed site
  ESP_YUN_T3_THI <- bad_site
  get_sapf_data(ESP_YUN_T3_THI) <- good_sapf_data
  get_sapf_flags(ESP_YUN_T3_THI) <- good_sapf_flags
  get_plant_md(ESP_YUN_T3_THI) <- good_plant_md
  # and save it
  save(ESP_YUN_T3_THI, file = file.path(folder, 'ESP_YUN_T3_THI.RData'))
  
  # csv files
  if (isTRUE(csv)) {
    # sapf_data
    get_sapf_data(ESP_YUN_T3_THI) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(ESP_YUN_T3_THI)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'ESP_YUN_T3_THI_sapf_data.csv'
      ))
    
    # sapf_flags
    get_sapf_flags(ESP_YUN_T3_THI) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(ESP_YUN_T3_THI)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'ESP_YUN_T3_THI_sapf_flags.csv'
      ))
    
    # plant_md
    get_plant_md(ESP_YUN_T3_THI) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'ESP_YUN_T3_THI_plant_md.csv'
      ))
  }
}

COL_MAC_SAF_RAD_fixes <- function(folder, csv = FALSE) {
  
  if (!file.exists(file.path(folder, 'COL_MAC_SAF_RAD.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('COL_MAC_SAF_RAD', folder)
  # check sapflow
  if ('COL_MAC_SAF_RAD_Tca_Js_1' %in% names(get_sapf_data(bad_site))) {
    good_sapf_data <- bad_site %>%
      get_sapf_data()
  } else {
    good_sapf_data <- bad_site %>%
      get_sapf_data() %>%
      dplyr::select(
        COL_MAC_SAF_RAD_Tca_Js_1 = COL_MAC_SAF_Tca_Js_1,
        COL_MAC_SAF_RAD_Tca_Js_2 = COL_MAC_SAF_Tca_Js_2,
        COL_MAC_SAF_RAD_Tca_Js_3 = COL_MAC_SAF_Tca_Js_3
      )
  }
  # check flags
  if ('COL_MAC_SAF_RAD_Tca_Js_1' %in% names(get_sapf_flags(bad_site))) {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags()
  } else {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags() %>%
      dplyr::select(
        COL_MAC_SAF_RAD_Tca_Js_1 = COL_MAC_SAF_Tca_Js_1,
        COL_MAC_SAF_RAD_Tca_Js_2 = COL_MAC_SAF_Tca_Js_2,
        COL_MAC_SAF_RAD_Tca_Js_3 = COL_MAC_SAF_Tca_Js_3
      )
  }
  # check plant names in plant metadata
  is_plant_md_ok <- bad_site %>%
    get_plant_md() %>%
    pull(pl_code) %>%
    str_detect(pattern = '_RAD_') %>%
    all()
  
  if (is_plant_md_ok) {
    good_plant_md <- bad_site %>%
      get_plant_md()
  } else {
    good_plant_md <- bad_site %>%
      get_plant_md() %>%
      dplyr::mutate(
        pl_code = stringr::str_replace(pl_code, 'COL_MAC_SAF_', 'COL_MAC_SAF_RAD_')
      )
  }
  
  # finaly build the fixed site
  COL_MAC_SAF_RAD <- bad_site
  get_sapf_data(COL_MAC_SAF_RAD) <- good_sapf_data
  get_sapf_flags(COL_MAC_SAF_RAD) <- good_sapf_flags
  get_plant_md(COL_MAC_SAF_RAD) <- good_plant_md
  # and save it
  save(COL_MAC_SAF_RAD, file = file.path(folder, 'COL_MAC_SAF_RAD.RData'))
  
  # csv files
  if (isTRUE(csv)) {
    # sapf_data
    get_sapf_data(COL_MAC_SAF_RAD) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(COL_MAC_SAF_RAD)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'COL_MAC_SAF_RAD_sapf_data.csv'
      ))
    
    # sapf_flags
    get_sapf_flags(COL_MAC_SAF_RAD) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(COL_MAC_SAF_RAD)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'COL_MAC_SAF_RAD_sapf_flags.csv'
      ))
    
    # plant_md
    get_plant_md(COL_MAC_SAF_RAD) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'COL_MAC_SAF_RAD_plant_md.csv'
      ))
  }
}

AUS_MAR_HSW_HIG_fixes <- function(folder, csv = FALSE) {
  
  if (!file.exists(file.path(folder, 'AUS_MAR_HSW_HIG.RData'))) {
    return()
  }
  
  bad_site <- read_sfn_data('AUS_MAR_HSW_HIG', folder)
  # check sapflow
  if ('AUS_MAR_HSW_HIG_Eob_Js_1' %in% names(get_sapf_data(bad_site))) {
    good_sapf_data <- bad_site %>%
      get_sapf_data()
  } else {
    good_sapf_data <- bad_site %>%
      get_sapf_data() %>%
      dplyr::select(
        AUS_MAR_HSW_HIG_Eob_Js_1 = AUS_MAR_HSW_Eob_Js_1,
        AUS_MAR_HSW_HIG_Eob_Js_2 = AUS_MAR_HSW_Eob_Js_2,
        AUS_MAR_HSW_HIG_Eob_Js_3 = AUS_MAR_HSW_Eob_Js_3,
        AUS_MAR_HSW_HIG_Ecy_Js_4 = AUS_MAR_HSW_Ecy_Js_4
      )
  }
  # check flags
  if ('AUS_MAR_HSW_HIG_Eob_Js_1' %in% names(get_sapf_flags(bad_site))) {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags()
  } else {
    good_sapf_flags <- bad_site %>%
      get_sapf_flags() %>%
      dplyr::select(
        AUS_MAR_HSW_HIG_Eob_Js_1 = AUS_MAR_HSW_Eob_Js_1,
        AUS_MAR_HSW_HIG_Eob_Js_2 = AUS_MAR_HSW_Eob_Js_2,
        AUS_MAR_HSW_HIG_Eob_Js_3 = AUS_MAR_HSW_Eob_Js_3,
        AUS_MAR_HSW_HIG_Ecy_Js_4 = AUS_MAR_HSW_Ecy_Js_4
      )
  }
  # check plant names in plant metadata
  is_plant_md_ok <- bad_site %>%
    get_plant_md() %>%
    pull(pl_code) %>%
    str_detect(pattern = '_HIG_') %>%
    all()
  
  if (is_plant_md_ok) {
    good_plant_md <- bad_site %>%
      get_plant_md()
  } else {
    good_plant_md <- bad_site %>%
      get_plant_md() %>%
      dplyr::mutate(
        pl_code = stringr::str_replace(pl_code, 'AUS_MAR_HSW_', 'AUS_MAR_HSW_HIG_')
      )
  }
  
  # finaly build the fixed site
  AUS_MAR_HSW_HIG <- bad_site
  get_sapf_data(AUS_MAR_HSW_HIG) <- good_sapf_data
  get_sapf_flags(AUS_MAR_HSW_HIG) <- good_sapf_flags
  get_plant_md(AUS_MAR_HSW_HIG) <- good_plant_md
  # and save it
  save(AUS_MAR_HSW_HIG, file = file.path(folder, 'AUS_MAR_HSW_HIG.RData'))
  
  # csv files
  if (isTRUE(csv)) {
    # sapf_data
    get_sapf_data(AUS_MAR_HSW_HIG) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(AUS_MAR_HSW_HIG)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'AUS_MAR_HSW_HIG_sapf_data.csv'
      ))
    
    # sapf_flags
    get_sapf_flags(AUS_MAR_HSW_HIG) %>%
      dplyr::mutate(TIMESTAMP_solar = get_solar_timestamp(AUS_MAR_HSW_HIG)) %>%
      dplyr::select(dplyr::starts_with('TIMESTAMP'), everything()) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'AUS_MAR_HSW_HIG_sapf_flags.csv'
      ))
    
    # plant_md
    get_plant_md(AUS_MAR_HSW_HIG) %>%
      readr::write_csv(path = file.path(
        stringr::str_replace(folder, 'RData', 'csv'),
        'AUS_MAR_HSW_HIG_plant_md.csv'
      ))
  }
}

