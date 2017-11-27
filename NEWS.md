# evaluator 0.1.1.900 (unreleased)

* new sample_tef function
* new sample_lm function
* new sample_tc function
* Tests and code coverage reporting added

# evaluator 0.1.1

* Replaced dependency on `modeest` with a slimmer `statip` dependency
* Removed dependency on `magrittr`
* Default (overridable) locations of input and results directories now consistently set to "~/data" and "~/results" respectively
* `generate_report` now takes an optional `focus_scenario_ids` parameter to override the scenarios on which special emphasis (usually executive interest) is desired.
* Improve user experience for optional packages. User is now prompted to install optional dependencies (shiny, DT, flexdashboard, statip, rmarkdown, etc.) when running reporting functionality which requires them.
* Substantial improvements in the sample analysis flow detailed in the usage vignette. You can now actually run all the commands as-is and have them work, which was previously "challenging".
* `summarize_all` renamed to the more descriptive `summarize_to_disk` to avoid dplyr conflict
* Add requirement for at least pander v0.6.1 for `tibble` compatability
* Substantial refactoring on vignette
  * Added missing save steps
  * Corrected package name for `Viewer` to `rstudioapi` 
  * Fixed a few incorrect placeholders
  * Properly committed compiled files to package for distribution and installation
* Update all tidyverse calls to account for deprecations and split out of `purrrlyr`
* Windows CI builds added via Appveyor
* Use `annotate_logticks` over manual breaks on risk_dashboard

# evaluator 0.1.0

* Initial submission to CRAN
