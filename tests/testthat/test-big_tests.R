context("big_tests")

skip_big <- function() {
  skip('Not run big_test')
}

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
