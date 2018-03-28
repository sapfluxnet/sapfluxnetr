library(tidyverse)
library(sapfluxnetr)

load(file = 'tests/testthat/big_test/plant/ARG_TRE.RData')

FOO <- ARG_TRE

# sapf
FOO_sapf_data <- get_sapf(FOO)[,-1]

# sapf_flags
FOO_sapf_flags <- get_sapf_flags(FOO)[,-1]

# env_data
FOO_env_data <- get_env(FOO)[,-1]

# env_flags
FOO_env_flags <- get_env_flags(FOO)[,-1]

# si code
FOO_si_code <- 'FOO'

# timestamps, nothing to see here
FOO_solar_timestamp <- get_solar_timestamp(FOO)
FOO_timestamp <- get_timestamp(FOO)

# site md
FOO_site_md <- get_site_md(FOO) %>%
  mutate_at(vars(contains('email')),
            funs(if_else(is.character(.), NA_character_, NA_character_, NA_character_))) %>%
  mutate(si_code = 'FOO',
         )

FOO_stand_md <- get_stand_md(FOO) %>%
  mutate(si_code = 'FOO',
  )
FOO_species_md <- get_species_md(FOO) %>%
  mutate(si_code = 'FOO',
  )
FOO_plant_md <- get_plant_md(FOO) %>%
  mutate(si_code = 'FOO',
  )
FOO_env_md <- get_env_md(FOO) %>%
  mutate(si_code = 'FOO',
  )

FOO_sapf_data_bad <- FOO_sapf_data[-1,]
FOO_sapf_flags_bad <- FOO_sapf_flags[-1,]
FOO_env_data_bad <- FOO_env_data[-1,]
FOO_env_flags_bad <- FOO_env_flags[-1,]
FOO_solar_timestamp_bad <- FOO_solar_timestamp[-1]
FOO_timestamp_bad <- FOO_timestamp[-1]

get_si_code(FOO) <- FOO_si_code
get_site_md(FOO) <- FOO_site_md
get_stand_md(FOO) <- FOO_stand_md
get_species_md(FOO) <- FOO_species_md
get_plant_md(FOO) <- FOO_plant_md
get_env_md(FOO) <- FOO_env_md

save(FOO, file = 'tests/testthat/Data/FOO.RData')
devtools::use_data(FOO, overwrite = TRUE)
rm(list = c('FOO', 'ARG_TRE'))
save.image(file = 'tests/testthat/FOO.RData')

## REMOVE ALL
rm(list = ls())
##

load(file = 'tests/testthat/big_test/plant/ARG_MAZ.RData')

BAR <- ARG_MAZ

# sapf
BAR_sapf_data <- get_sapf(BAR)[,-1]

# sapf_flags
BAR_sapf_flags <- get_sapf_flags(BAR)[,-1]

# env_data
BAR_env_data <- get_env(BAR)[,-1]

# env_flags
BAR_env_flags <- get_env_flags(BAR)[,-1]

# si code
BAR_si_code <- 'BAR'

# timestamps, nothing to see here
BAR_solar_timestamp <- get_solar_timestamp(BAR)
BAR_timestamp <- get_timestamp(BAR)

# site md
BAR_site_md <- get_site_md(BAR) %>%
  mutate_at(vars(contains('email')),
            funs(if_else(is.character(.), NA_character_, NA_character_, NA_character_))) %>%
  mutate(si_code = 'BAR',
  )

BAR_stand_md <- get_stand_md(BAR) %>%
  mutate(si_code = 'BAR',
  )
BAR_species_md <- get_species_md(BAR) %>%
  mutate(si_code = 'BAR',
  )
BAR_plant_md <- get_plant_md(BAR) %>%
  mutate(si_code = 'BAR',
  )
BAR_env_md <- get_env_md(BAR) %>%
  mutate(si_code = 'BAR',
  )

BAR_sapf_data_bad <- BAR_sapf_data[-1,]
BAR_sapf_flags_bad <- BAR_sapf_flags[-1,]
BAR_env_data_bad <- BAR_env_data[-1,]
BAR_env_flags_bad <- BAR_env_flags[-1,]
BAR_solar_timestamp_bad <- BAR_solar_timestamp[-1]
BAR_timestamp_bad <- BAR_timestamp[-1]

get_si_code(BAR) <- BAR_si_code
get_site_md(BAR) <- BAR_site_md
get_stand_md(BAR) <- BAR_stand_md
get_species_md(BAR) <- BAR_species_md
get_plant_md(BAR) <- BAR_plant_md
get_env_md(BAR) <- BAR_env_md

save(BAR, file = 'tests/testthat/Data/BAR.RData')
devtools::use_data(BAR, overwrite = TRUE)
rm(list = c('BAR', 'ARG_MAZ'))
save.image(file = 'tests/testthat/BAR.RData')

## REMOVE ALL
rm(list = ls())
##

load(file = 'tests/testthat/big_test/plant/AUS_CAN_ST2_MIX.RData')

BAZ <- AUS_CAN_ST2_MIX

# sapf
BAZ_sapf_data <- get_sapf(BAZ)[,-1]

# sapf_flags
BAZ_sapf_flags <- get_sapf_flags(BAZ)[,-1]

# env_data
BAZ_env_data <- get_env(BAZ)[,-1]

# env_flags
BAZ_env_flags <- get_env_flags(BAZ)[,-1]

# si code
BAZ_si_code <- 'BAZ'

# timestamps, nothing to see here
BAZ_solar_timestamp <- get_solar_timestamp(BAZ)
BAZ_timestamp <- get_timestamp(BAZ)

# site md
BAZ_site_md <- get_site_md(BAZ) %>%
  mutate_at(vars(contains('email')),
            funs(if_else(is.character(.), NA_character_, NA_character_, NA_character_))) %>%
  mutate(si_code = 'BAZ',
  )

BAZ_stand_md <- get_stand_md(BAZ) %>%
  mutate(si_code = 'BAZ',
  )
BAZ_species_md <- get_species_md(BAZ) %>%
  mutate(si_code = 'BAZ',
  )
BAZ_plant_md <- get_plant_md(BAZ) %>%
  mutate(si_code = 'BAZ',
  )
BAZ_env_md <- get_env_md(BAZ) %>%
  mutate(si_code = 'BAZ',
  )

BAZ_sapf_data_bad <- BAZ_sapf_data[-1,]
BAZ_sapf_flags_bad <- BAZ_sapf_flags[-1,]
BAZ_env_data_bad <- BAZ_env_data[-1,]
BAZ_env_flags_bad <- BAZ_env_flags[-1,]
BAZ_solar_timestamp_bad <- BAZ_solar_timestamp[-1]
BAZ_timestamp_bad <- BAZ_timestamp[-1]

get_si_code(BAZ) <- BAZ_si_code
get_site_md(BAZ) <- BAZ_site_md
get_stand_md(BAZ) <- BAZ_stand_md
get_species_md(BAZ) <- BAZ_species_md
get_plant_md(BAZ) <- BAZ_plant_md
get_env_md(BAZ) <- BAZ_env_md

save(BAZ, file = 'tests/testthat/Data/BAZ.RData')
devtools::use_data(BAZ, overwrite = TRUE)
rm(list = c('BAZ', 'AUS_CAN_ST2_MIX'))
save.image(file = 'tests/testthat/BAZ.RData')
