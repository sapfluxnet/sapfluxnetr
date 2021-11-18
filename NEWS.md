# sapfluxnetr 0.1.3

* Fixed tests to comply with next version of dplyr dependency (1.0.8)

# sapfluxnetr 0.1.2

* Improved docs (sub-daily aggregations, default metrics...)
* Fix use of `setdiff` to check differences between data frames, not working on devel R versions

# sapfluxnetr 0.1.1

* Remove `tidyr::gather` and `tidyr::spread` calls, replacing them with the now
  recommended `tidyr::pivot_longer` and `tidyr::pivot_wide` calls.
* Following the new database available, *"Mediterranean"* biome is replaced
  by *"Woodland/Shrubland"* in all vignettes and function examples where it was
  used.

# sapfluxnetr 0.1.0

* Removed `tidyverse` dependency from *suggests*.
* Updated internal `.fixed_metrics_funs` to use formulas instead of quosures
  on quantile metrics in the functions list, as the use of quosures here is
  deprecated by newer dplyr versions.
* First minor version, open data released

# sapfluxnetr 0.0.7

* Replaced calls of dplyr::funs with list (or rlang::list2 if splicing is needed)
  as the former is soft deprecated in dplyr 0.8.0
* One word periods ('daily', 'monthly'...) in `sfn_metrics` function are
  **soft deprecated**. Please use '1 day', '1 month'... instead. 
* Removed all dependencies on 'tibbletime' package
* Added utils internal functions: .collapse_timestamp and .parse_period
* Added installation from CRAN to vignette and readme

# sapfluxnetr 0.0.6

* Improved examples in the functions help
* Updated license file to comply with CRAN policy
* Cleaning to comply with CRAN checks in windows builds and travis CI
* Added .accumulated_posix_aware helper function to avoid summing posix objects
* Added pkgdown support
* Added Travis CI support
* Updated sapfluxnetr Not So Quick Guide vignette and README file

# sapfluxnetr 0.0.5

* Cleaning to comply with CRAN checks.
* Added the *"Metadata and data units"* vignette.
* Fixes for the new `purrr` version.
* Exported the pipe operator (`%>%`) from `magrittr` package. Now loading
  `sapfluxnetr` the pipe can be used.
* Added `acummulated_precip` as new metric for the default `*_metrics` functions.
* Fixed `data_coverage` function. Now it returns the real coverage based on the
  timestep, the period to summarise for and the timestamp values. This only will
  work within the wrapper metrics functions (using internally the 
  `.fixed_metrics_funtions` function).
* Fixed a bug in sfn_metrics that for special intervals (md, pd, daylight) the
  filtering step was collecting timestamps above the int_end
* Fixed bug in metrics function that created min and max time variables as double
  instead of POSIXct in sites with NAs in the first day of measures
* Now species metadata variables are returned individually instead of a list
* Added helper function `sfn_sites_in_folder` to list the site codes in a folder
* Deprecated `filter_by_var`, substituted by `filter_sites_by_md`
* **Implementation of furrr::future_map** in `sfn_metrics`. After some
  benchmarking, the benefits in time are solid, so now the user has the ability
  to perform the metrics in parallel.
* New logic with performance improvements for metrics_tidyfier (introducing the
  .sapflow_tidy internal helper function)
* Added get methods for sfn_data_multi class objects
* Fixed bug in metrics function that created min and max time variables as double
  instead of POSIXct in sites with NAs in the first day of measures
* Now species metadata variables are returned individually instead of in a list

# sapfluxnetr 0.0.4

* Added geom control to `sfn_plot` function.
* Refactored `sfn_metrics` to uniformize interval start and interval end
* Modified `nightly_metrics` to return only night interval
* Created `predawn_metrics`, `midday_metrics` and `daylight_metrics` functions
  taking leverage in the refactored `sfn_metrics` functions.
* Modified `daily_metrics` and `monthly_metrics` to return only the general
  interval metrics, avoiding this way the creation of very big objects.
* Added `tidy` argument to *_metrics functions, to skip one step when creating
  tidy metrics.
* Updated README file
* Added bug report link to DESCRIPTION file
* Updated documentation and vignettes accordingly with the changes made

# sapfluxnetr 0.0.3

* Added metrics_tidyfier function to convert to tidy the metrics results.
* Added sfn_metadata_ex to Data.
* Changed all example data names to the original site name.
* Improved install explanation in quick guide vignette.
* Added a `NEWS.md` file to track changes to the package.
* Added `README.md` file for new users.

# sapfluxnetr 0.0.2

* Code and Docs cleaning.
* Added vignettes for flags, classes, quick guide and custom aggregation .
* Updated big-tests for dplyr-like functions.
* Refactored metadata cache.
* Modification of sfn_data show method to include site paper.
* Added documentation for dplyr-like methods sfn_filter, sfn_mutate and
  sfn_mutate_at.
* Added .flag internal function to flag mutated sfn_data objects.

# sapfluxnetr 0.0.1

* Initial version of the package.
