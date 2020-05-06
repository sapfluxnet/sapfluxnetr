library(dplyr)
library(sapfluxnetr)

load(file = 'tests/testthat/big_test/plant/ARG_TRE.RData')

# sapf
ARG_TRE_sapf_data <- get_sapf_data(ARG_TRE)[,-1]

# sapf_flags
ARG_TRE_sapf_flags <- get_sapf_flags(ARG_TRE)[,-1]

# env_data
ARG_TRE_env_data <- get_env_data(ARG_TRE)[,-1]

# env_flags
ARG_TRE_env_flags <- get_env_flags(ARG_TRE)[,-1]

# si code
ARG_TRE_si_code <- 'ARG_TRE'

# timestamps, nothing to see here
ARG_TRE_solar_timestamp <- get_solar_timestamp(ARG_TRE)
ARG_TRE_timestamp <- get_timestamp(ARG_TRE)

# site md
ARG_TRE_site_md <- get_site_md(ARG_TRE) %>%
  mutate_at(vars(contains('email')),
            list(~ if_else(is.character(.), NA_character_, NA_character_, NA_character_)))

ARG_TRE_stand_md <- get_stand_md(ARG_TRE)
ARG_TRE_species_md <- get_species_md(ARG_TRE)
ARG_TRE_plant_md <- get_plant_md(ARG_TRE)
ARG_TRE_env_md <- get_env_md(ARG_TRE)

ARG_TRE_sapf_data_bad <- ARG_TRE_sapf_data[-1,]
ARG_TRE_sapf_flags_bad <- ARG_TRE_sapf_flags[-1,]
ARG_TRE_env_data_bad <- ARG_TRE_env_data[-1,]
ARG_TRE_env_flags_bad <- ARG_TRE_env_flags[-1,]
ARG_TRE_solar_timestamp_bad <- ARG_TRE_solar_timestamp[-1]
ARG_TRE_timestamp_bad <- ARG_TRE_timestamp[-1]

get_site_md(ARG_TRE) <- ARG_TRE_site_md


save(ARG_TRE, file = 'tests/testthat/Data/ARG_TRE.RData')
usethis::use_data(ARG_TRE, overwrite = TRUE)
rm(list = c('ARG_TRE'))
save.image(file = 'tests/testthat/ARG_TRE.RData')

## REMOVE ALL
rm(list = ls())
##

load(file = 'tests/testthat/big_test/plant/ARG_MAZ.RData')

# sapf
ARG_MAZ_sapf_data <- get_sapf_data(ARG_MAZ)[,-1]

# sapf_flags
ARG_MAZ_sapf_flags <- get_sapf_flags(ARG_MAZ)[,-1]

# env_data
ARG_MAZ_env_data <- get_env_data(ARG_MAZ)[,-1]

# env_flags
ARG_MAZ_env_flags <- get_env_flags(ARG_MAZ)[,-1]

# si code
ARG_MAZ_si_code <- 'ARG_MAZ'

# timestamps, nothing to see here
ARG_MAZ_solar_timestamp <- get_solar_timestamp(ARG_MAZ)
ARG_MAZ_timestamp <- get_timestamp(ARG_MAZ)

# site md
ARG_MAZ_site_md <- get_site_md(ARG_MAZ) %>%
  mutate_at(vars(contains('email')),
            list(~ if_else(is.character(.), NA_character_, NA_character_, NA_character_)))

ARG_MAZ_stand_md <- get_stand_md(ARG_MAZ)
ARG_MAZ_species_md <- get_species_md(ARG_MAZ)
ARG_MAZ_plant_md <- get_plant_md(ARG_MAZ)
ARG_MAZ_env_md <- get_env_md(ARG_MAZ)

ARG_MAZ_sapf_data_bad <- ARG_MAZ_sapf_data[-1,]
ARG_MAZ_sapf_flags_bad <- ARG_MAZ_sapf_flags[-1,]
ARG_MAZ_env_data_bad <- ARG_MAZ_env_data[-1,]
ARG_MAZ_env_flags_bad <- ARG_MAZ_env_flags[-1,]
ARG_MAZ_solar_timestamp_bad <- ARG_MAZ_solar_timestamp[-1]
ARG_MAZ_timestamp_bad <- ARG_MAZ_timestamp[-1]

get_site_md(ARG_MAZ) <- ARG_MAZ_site_md


save(ARG_MAZ, file = 'tests/testthat/Data/ARG_MAZ.RData')
usethis::use_data(ARG_MAZ, overwrite = TRUE)
rm(list = c('ARG_MAZ'))
save.image(file = 'tests/testthat/ARG_MAZ.RData')

## REMOVE ALL
rm(list = ls())
##

load(file = 'tests/testthat/big_test/plant/AUS_CAN_ST2_MIX.RData')

# sapf
AUS_CAN_ST2_MIX_sapf_data <- get_sapf_data(AUS_CAN_ST2_MIX)[,-1]

# sapf_flags
AUS_CAN_ST2_MIX_sapf_flags <- get_sapf_flags(AUS_CAN_ST2_MIX)[,-1]

# env_data
AUS_CAN_ST2_MIX_env_data <- get_env_data(AUS_CAN_ST2_MIX)[,-1]

# env_flags
AUS_CAN_ST2_MIX_env_flags <- get_env_flags(AUS_CAN_ST2_MIX)[,-1]

# si code
AUS_CAN_ST2_MIX_si_code <- 'AUS_CAN_ST2_MIX'

# timestamps, nothing to see here
AUS_CAN_ST2_MIX_solar_timestamp <- get_solar_timestamp(AUS_CAN_ST2_MIX)
AUS_CAN_ST2_MIX_timestamp <- get_timestamp(AUS_CAN_ST2_MIX)

# site md
AUS_CAN_ST2_MIX_site_md <- get_site_md(AUS_CAN_ST2_MIX) %>%
  mutate_at(vars(contains('email')),
            list(~ if_else(is.character(.), NA_character_, NA_character_, NA_character_)))

AUS_CAN_ST2_MIX_stand_md <- get_stand_md(AUS_CAN_ST2_MIX)
AUS_CAN_ST2_MIX_species_md <- get_species_md(AUS_CAN_ST2_MIX)
AUS_CAN_ST2_MIX_plant_md <- get_plant_md(AUS_CAN_ST2_MIX)
AUS_CAN_ST2_MIX_env_md <- get_env_md(AUS_CAN_ST2_MIX)

AUS_CAN_ST2_MIX_sapf_data_bad <- AUS_CAN_ST2_MIX_sapf_data[-1,]
AUS_CAN_ST2_MIX_sapf_flags_bad <- AUS_CAN_ST2_MIX_sapf_flags[-1,]
AUS_CAN_ST2_MIX_env_data_bad <- AUS_CAN_ST2_MIX_env_data[-1,]
AUS_CAN_ST2_MIX_env_flags_bad <- AUS_CAN_ST2_MIX_env_flags[-1,]
AUS_CAN_ST2_MIX_solar_timestamp_bad <- AUS_CAN_ST2_MIX_solar_timestamp[-1]
AUS_CAN_ST2_MIX_timestamp_bad <- AUS_CAN_ST2_MIX_timestamp[-1]

get_site_md(AUS_CAN_ST2_MIX) <- AUS_CAN_ST2_MIX_site_md


save(AUS_CAN_ST2_MIX, file = 'tests/testthat/Data/AUS_CAN_ST2_MIX.RData')
usethis::use_data(AUS_CAN_ST2_MIX, overwrite = TRUE)
rm(list = c('AUS_CAN_ST2_MIX'))
save.image(file = 'tests/testthat/AUS_CAN_ST2_MIX.RData')

## REMOVE ALL
rm(list = ls())
##

sfn_metadata_ex <- sapfluxnetr:::.write_metadata_cache('tests/testthat/Data',
                                                    .dry = TRUE)

usethis::use_data(sfn_metadata_ex, overwrite = TRUE)
