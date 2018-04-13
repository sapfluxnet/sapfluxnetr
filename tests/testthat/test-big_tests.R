context("big_tests")

skip_big <- function() {
  skip('Not run big_test')
}

test_that('each site can be loaded, completly summarised and plotted', {

  skip_big()

  # sites for each unit level
  sites_names_plant <- stringr::str_remove(
    list.files('big_test/plant', pattern = '.RData'), '.RData'
  )

  sites_names_sapwood <- stringr::str_remove(
    list.files('big_test/sapwood', pattern = '.RData'), '.RData'
  )

  sites_names_leaf <- stringr::str_remove(
    list.files('big_test/leaf', pattern = '.RData'), '.RData'
  )

  # metadata indicator
  indicator <- 1

  # outer loop
  for (unit_level in list(
    plant = sites_names_plant,
    sapwood = sites_names_sapwood,
    leaf = sites_names_leaf
  )) {

    # inner loop
    for (site in unit_level) {

      if (indicator == 1) {folder <- 'big_test/plant'}
      if (indicator == 2) {folder <- 'big_test/sapwood'}
      if (indicator == 3) {folder <- 'big_test/leaf'}

      sfn_data <- read_sfn_data(site, folder = folder)

      sfn_filtered <- suppressWarnings(
        sfn_filter(
          sfn_data,
          dplyr::between(ta, 0, 10)
        )
      )

      sfn_mutated <- sfn_mutate(
        sfn_data,
        ta = ta + 273.15
      )

      env_vars <- c('ta', 'rh', 'vpd', 'sw_in', 'ppfd_in', 'netrad', 'ext_rad',
                    'swc_shallow', 'swc_deep', 'ws', 'precip')

      sfn_mutated_at <- sfn_mutate_at(
        sfn_data,
        .vars = dplyr::vars(-TIMESTAMP, -dplyr::one_of(env_vars)),
        .funs = dplyr::funs(dplyr::case_when(
          ta > 10 ~ NA_real_,
          ta < 0 ~ NA_real_,
          TRUE ~ .
        ))
      )

      metrics <- sfn_metrics(
        sfn_data, period = 'daily',
        .funs = dplyr::funs(
          mean = mean(., na.rm = TRUE),
          n = n(),
          coverage = data_coverage(.),
          q_95 = stats::quantile(., 0.95, na.rm = TRUE),
          q_99 = stats::quantile(., 0.99, na.rm = TRUE),
          max = max(., na.rm = TRUE),
          max_time = max_time(., .data$TIMESTAMP_coll),
          min = min(., na.rm = TRUE),
          min_time = min_time(., .data$TIMESTAMP_coll),
          centroid = diurnal_centroid(.)
        ),
        solar = TRUE, general = TRUE,
        predawn = TRUE, midday = TRUE, nighttime = TRUE,
        pd_start = 4, pd_end = 6, md_start = 13, md_end = 15,
        night_start = 21, night_end = 6
      )

      sapf_names <- c('sapf_gen', 'sapf_pd', 'sapf_md', 'sapf_day', 'sapf_night')
      env_names <- c('env_gen', 'env_pd', 'env_md', 'env_day', 'env_night')

      metrics_tidy <- metrics_tidyfier(metrics, read_sfn_metadata(folder))

      # test if load
      expect_s4_class(sfn_data, 'sfn_data')

      # test if dplyr_methods work
      expect_s4_class(sfn_mutated, 'sfn_data')
      expect_s4_class(sfn_mutated_at, 'sfn_data')

      if (!is.null(sfn_filtered)) {
        expect_s4_class(sfn_filtered, 'sfn_data')
        expect_false(
          any(get_env_data(sfn_filtered)[['ta']] > 10)
        )
        expect_false(
          any(get_env_data(sfn_filtered)[['ta']] < 0)
        )
      }

      expect_true(
        all(
          (get_env_data(sfn_mutated)[['ta']] - get_env_data(sfn_data)[['ta']]) == 273.15,
          na.rm = TRUE
        )
      )

      expect_true(
        all(
          is.na(
            get_sapf_data(sfn_mutated_at)[get_env_data(sfn_mutated_at)[['ta']] > 10, 2]
          )
        )
      )

      # test if plot
      expect_s3_class(sfn_plot(sfn_data), 'gg')
      expect_s3_class(sfn_plot(sfn_data, type = 'sapf', solar = FALSE), 'gg')
      expect_s3_class(sfn_plot(sfn_data, formula_env = ~ta), 'gg')

      # test if summarise
      expect_true(is.list(metrics))
      expect_length(metrics, 2)
      expect_length(metrics[['sapf']], 5)
      expect_length(metrics[['env']], 5)
      expect_identical(names(metrics[['sapf']]), sapf_names)
      expect_identical(names(metrics[['env']]), env_names)
      expect_s3_class(metrics[['sapf']][['sapf_day']], 'tbl')
      expect_s3_class(metrics[['env']][['env_day']], 'tbl')

      # test if tidyfy
      expect_s3_class(metrics_tidy, 'tbl')
      expect_true(nrow(metrics_tidy) > 0)

    }

    # up indicator
    indicator <- indicator + 1

  }



})

test_that('processes works in a more complex environment', {

  skip_big()

  filter_by_var(
    pl_sens_meth %in% c('HR', 'HD'),
    si_biome == 'Mediterranean',
    folder = 'big_test/plant',
    join = 'and',
    .use_cache = TRUE
  )

  filter_by_var(
    pl_sens_meth %in% c('HR', 'HD'),
    si_biome == 'Mediterranean',
    folder = 'big_test/plant',
    join = 'or',
    .use_cache = TRUE
  )

  filter_by_var(
    pl_sens_meth %in% c('HR', 'HD'),
    si_biome == 'Mediterranean',
    folder = 'big_test/plant',
    join = 'and',
    .use_cache = TRUE
  ) %>%
    read_sfn_data(folder = 'big_test/plant') %>%
    daily_metrics(solar = 'FALSE')

  filter_by_var(
    pl_sens_meth %in% c('HR', 'HD'),
    si_biome == 'Mediterranean',
    folder = 'big_test/plant',
    join = 'and',
    .use_cache = TRUE
  ) %>%
    read_sfn_data(folder = 'big_test/plant') %>%
    monthly_metrics(solar = 'FALSE')

  filter_by_var(
    # pl_sens_meth %in% c('HR', 'HD'),
    si_biome %in% c('Mediterranean', 'Temperate forest'),
    folder = 'big_test/plant',
    join = 'and',
    .use_cache = TRUE
  ) %>%
    read_sfn_data(folder = 'big_test/plant') %>%
    monthly_metrics(solar = 'FALSE')

  ## plot flow after metrics:
  foo$ARG_MAZ$env$env %>%
    select(TIMESTAMP, ends_with('_coverage')) %>%
    gather(Env_var, Value, -TIMESTAMP) %>%
    ggplot(aes(x = TIMESTAMP, y = Value, colour = Env_var)) +
    geom_line() +
    facet_wrap(~ Env_var, ncol = 3, scales = 'free_y')

})
