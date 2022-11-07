# mpathsenser 1.1.1
## Major changes
* `identify_gaps()` now allows multiple sensors to be used. This is particularly useful when there 
are no sensors with high frequency sampling (like accelerometer and gyroscope) or to ensure there 
can be no measurements within the gaps from any sensor.
* Changed the arguments names of `copy_db()` `from_db` and `to_db` to `source_db` and `target_db`
respectively.
* Set `activity_duration()`, `screen_duration()`, `n_screen_on()`, `n_screen_unlocks()`, and 
`step_count()` to internal until it is clear how these functions should behave and, more 
importantly, what their output should be.
* Reworked `moving_average()` to work correctly on multiple participants.

## Deprecations
* Deprecated functionality for on-the-fly database creation in several functions. This disentangles 
the functionalities of `create_db()` and the other functions, where the latter implicitly depended 
on the former. The following arguments are thereby rendered disabled:
  - `dbname` and `overwrite_db` arguments in `import()`
  - `path` and `db_name` in `copy_db()`
* Deprecated the `parallel` argument in several functions. If you wish to process in parallel, you
must now specify this beforehand using a [future plan](https://rdrr.io/cran/future/), e.g.
`future::plan("multisession")`. As a consequence, the package `future` is no longer a dependency 
(but `furrr` is).
* Deprecated the `plot` argument in `coverage`. To plot a coverage chart, you can now use the 
default `plot()` function with the output from `coverage`.

## Minor changes
* All functions gained basic argument checking, ensuring that at least the proper type has been 
input.
* The package now provides more nicely formatted errors, warnings, and messages through 
[`rlang::abort`, `rlang::warn`, and `rlang::inform`](https://rlang.r-lib.org/reference/abort.html).
* Partially rewrote `import()` to be more manageable in code. As a consequence, the dependency on 
`rjson` and `dbx` can be dropped in favour of jsonlite and native SQL. 
* Added `lifecycle` as a dependency for deprecating arguments.
* Added a warning section in `identify_gaps()` and friends to inform the user of a possible 
inconsistency when identifying gaps. 
* Switched `identify_gaps()` from using the lag of each measurements towards using the lead. This
makes no difference in the output but is a little easier to read.

## Bug fixes
* Fixed a note when first running `link()` or `link_gaps()` in a session, stating that using 
external vectors `dplyr::select()` is ambiguous.
* `bin_data()` now correctly includes measurements in bins that do not have a stop time. This was in 
particular a problem with the last measurement of a series.
* Fixed a non-working example in `bin_data()`.
* Fixed a bug in `add_gaps()` where multiple gaps in succession (i.e. without other data in between)
were incorrectly handled.
* Fixed `app_category()` not  being able to find the exact app name in the search results, thereby 
defaulting to the `n`th result (default 1).

# mpathsenser 1.1.0
## Major changes
* Added several functions:
  - `link_gaps`: For linking gap data to other data, i.e. how many gaps occur within an interval.
  - `add_gaps`: To interleave gaps with other data.
  - `bin_data`: To subdivide data into bins, e.g. all measurements within an hour or day.
* Added lifecycle badge to signal the state of functions.
* `link` has been revised and expanded:
  - Replaced `offset` with `offset_before` and `offset_after`, allowing both to be specified at the 
  same time (#3).
  - Added new `add_before` and `add_after` argument to allow the last row before the measurement 
  and first row after the measurement respectively to be added to the data.
  - Added a new `split` argument, allowing computation to be split among many parts thereby
  lowering computational burden.
* `app_category` is now case insensitive and gained the new argument "exact" to be able to match the
package name exactly based on a partial match.
* Added a (start of a) vignette to further highlight the package use.

## Minor changes
* Changed `get_activity` to `activity_duration`.
* Changed `link2` to `link_db`.

## Bug fixes
* Fixed issue where `link` runs out of memory when there are too many matches (#2). `link` is now 
much more memory efficient and slightly faster.
* Fixed issue in `get_data` which allowed multiple sensors to be requested from one function call, 
sometimes leading to crashes (#4).
* Fixed issue in `link` where column `original_time` is missing if no records before or after the 
interval are found (#6).
* Fixed a bug in import where sensor data not present in first file of the batch are dropped for the
other files well.
* Fixed app_category to work with the updated Google Play website.

# mpathsenser 1.0.3
* Fix final tests not yet using TMPDIR

# mpathsenser 1.0.2
* Changed some unit to use TMPDIR

# mpathsenser 1.0.1
* Fixed floating point differences when testing on MacOS

# mpathsenser 1.0.0
* Initial release on CRAN
* Changed the name to mpathsenser
