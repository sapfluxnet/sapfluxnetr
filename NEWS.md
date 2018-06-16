# sapfluxnetr 0.0.4.9000
* Fixed bug in metrics function that created min and max time variables as double
  instead of POSIXct in sites with NAs in the first day of measures
* Now species metadata variables are returned individually isntead of a list

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
