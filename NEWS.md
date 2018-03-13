# evaluator 0.2.0

## New Functionality
* New sample dataset: `mappings` contains sample qualitative to quantitative 
  parameters.
* risk_dashboard expects a mandatory output parameter to the desired rendered 
  output.
* New `pkgdown` generated web documentation at https://evaluator.severski.net.
* Refactored `generate_report` function.
    * Now accepts `format` parameter to specify HTML, PDF, or Word
    * Optional `styles` parameter allows user to supply custom CSS or Word
    reference document to customize styles and fonts
    * RMarkdown 1.8.2 or greater (currently GitHub only) required to work 
    around the temporary file deletion issue specified in 
    https://github.com/rstudio/rmarkdown/issues/1184. Use
    `devtools::install_github("rstudio/rmarkdown", "b84f706")` or greater.
* Expose OpenFAIR model selection in `run_simulation` call
    * Provide default TEF/TC/DIFF/LM OpenFAIR model
* New `create_templates` function for populating starter/sample files, making 
  starting a fresh analysis easier than ever!
* Experimental quick start script, run_analysis.R, supplied with `create_templates()`.
* All default directories normallized to ~/evaluator/[inputs|results]
* New OpenFAIR primitives:
    * sample_tef
    * sample_lm
    * sample_tc
    * sample_diff
    * sample_vuln
* New composition functions:
    * compare_tef_vuln
    * select_loss_opportunities
* New difficulty composition functions:
    * get_mean_control_strength
    
## Bug Fixes
* Improved help documentation on many functions
* Update of usage vignette
* Auto loads `extrafont` database for better font detection
    * Falls back to standard `sans` family when none of the preferred options 
      are available
* Drop use of `tcltk` progress bar in favor of console-compatible 
    `dplyr::progress_estimated`. Also enables reduced package dependencies.
* Tests and code coverage reporting added
    * Improve faulty `capabilities` validation
* Removed dependency on `purrrlyr`

## Miscellaneous Changes
* `generate_report` defaults to creating a MS Word document as the output type

# evaluator 0.1.1

* Replaced dependency on `modeest` with a slimmer `statip` dependency
* Removed dependency on `magrittr`
* Default (overridable) locations of input and results directories now consistently set to "~/data" and "~/results" respectively
* `generate_report` now takes an optional `focus_scenario_ids` parameter to override the scenarios on which special emphasis (usually executive interest) is desired.
* Improve user experience for optional packages. User is now prompted to install optional dependencies (shiny, DT, flexdashboard, statip, rmarkdown, etc.) when running reporting functionality which requires them.
* Substantial improvements in the sample analysis flow detailed in the usage vignette. You can now actually run all the commands as-is and have them work, which was previously "challenging".
* `summarize_all` renamed to the more descriptive `summarize_to_disk` to avoid dplyr conflict
* Add requirement for at least pander v0.6.1 for `tibble` compatibility
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
