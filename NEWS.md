# evaluator 0.4.2

* Internal updates for recent `tibble` and `tidyr` changes.

# evaluator 0.4.1

* Update for compatibility with `vctrs` 0.2.0 CRAN release.
* `summarize_to_disk()` and `create_templates()` no longer default to the 
  user's home directory for writing to disk. A path to place the generated 
  files must be provided.
* General documentation improvements. Clarifying language and correcting 
  formatting.
* Removed inadvertent dependency on R 3.5 `tempdir()` syntax.

# evaluator 0.4.0

This release includes a greatly improved flow when starting directly from 
quantitative inputs. In addition to the spreadsheet importing flow, 
users have the option to start with quantitative inputs and leverage the new 
`tidyrisk_scenario` objects to move directly to simulation. Users with 
quantitative flows may also use [collector](https://collector.tidyrisk.org) to 
work with their quantitative data.

## New Feature
* `read_qualitative_inputs()` pulls in all qualitative inputs.
* `tidyrisk_scenario` object for containing the elements of a risk scenario 
  for simulation.
  - A RStudio Add-in for creating a skeleton `tidyrisk_scenario` object is 
  available.
* `summarize_scenario()` function creates summary for a single scenario's results. 
  - `summarize_scenarios()` is now a wrapper around `summarize_scenario()`
* An alternative model `openfair_tef_tc_diff_plm_sr()` is now available for 
  simulating scenarios with secondary loss risk.
* `loss_scatterplot()` generates a scatter plot of total loss exposure vs. 
  number of loss events for a single scenario.
* `exposure_histogram()` generates a histogram of losses for a single scenario, 
  optionally displaying the 95% value at risk level.
* `loss_exceedance_curve()` generates a loss exceedance curve for one or 
  more simulations summarized at the iteration level.

## Improvements
* Massive speed fix for a slow down that has been present since the v0.2.x 
  series. Initial testing shows improvements of up to 700% when running 
  simulations!
* Summary statistics for mean TC and DIFF exceedance now properly handle 
  extreme situations where 100% of threat events are either resisted or 
  become loss events.
* All percentages are consistently imported, stored, and processed as 
  decimal values (from 0 - 1). This makes the TC and DIFF parameters use the 
  same scale as VULN.
  * As a side benefit, the restrictions on the format of the mappings CSV are 
  relaxed. Doubles (decimals) are now permitted in the low, most likely, and 
  high columns.
  * The custom percent format function in the default Risk Report is removed 
  and functionality is now provided via `scales::percent()`.
* Scenario Explorer application now can be used when skipping qualitative 
  data and starting directly from quantitative data.
* Risk Report can now be used when skipping qualitative data and starting 
  directly from quantitative data.
* The risk report and risk dashboard now use the same metric for minimum 
  expected losses. The previous value reported on the dashboard was incorrect.
* `select_loss_events()` now returns zeros when there are zero threat events in 
  a given period. Previously this function returned NA which could potentially 
  cause issues in reporting.
* Updated the default sample mapping files to reference the current maximum 
  OCR fine (SLE), from the 2015 Anthem breach.
* All save and load functions consistently use rds formatted files instead 
  of a mix of rda/rds.
* Removed all remaining uses to SE forms of dplyr verbs.
* Removed use of soft deprecated ggplot `aes_()` functions.
* Clean up CSS for HTML reports, improving style consistency, particularly 
  around font families.
* Increase test coverage, including moving more shinytest tests to run on CI 
  instances and package spelling tests.
* The internal contract between `run_simulation()` and modeling functions 
  such as `openfair_tef_tc_diff_lm()` is established. `run_simulation()` 
  will always confirm a scenario object has all of the OpenFAIR components 
  needed by the specified modeling function. This enables easier extension of 
  Evaluator to new models.
* `openfair_example()` uses (at last!) the same simulation engine as 
  `run_simulation()`. This provides more consistent results, less code to 
  maintain, and opens this demo app to displaying more metrics that are native 
  to evaluator. A very initial pass to this last point has implemented.
  
## Other Changes
* `generate_scatterplot()` is deprecated in favor of the new 
  `loss_scatterplot()` function.
* Switched the base font for the Risk Report to Open Sans, retaining the 
  Condensed version for headers.
* Renamed the default sample dataset to the hypothetical MetroCare Hospital. 
  The default sets now all consistently use the `mc_` prefix to distinguish 
  them from parameter names.
* Deprecated `load_data()`. Use `read_qualitative_inputs()` or 
  `read_quantitative_inputs()` as appropriate.
* Re-export the pipe operator `%>%`
* Rename `simulation` column to `iteration` to be more consistent with general 
  MC uses.
* Move from soft-deprecated `purrr::invoke()` to `rlang::eval()`.

# evaluator 0.3.2

## Improvements

* Removed dependency on `purrrlyr`
* Improve checks on summary function to avoid floating point comparisons
generating false failures.

# evaluator 0.3.1

* Correct ORCID identifier

## New Feature

* `read_quantitative_inputs()` added to allow easier skipping over qualitative 
inputs and going straight to quantitative inputs, such as generated by 
`collector`.

## Bug fix

* `summarize_domains()` was still referencing an ALE column which did not 
exist on the summary roll up (aggregating ALE is possible as a strict sum 
across scenarios). 

# evaluator 0.3.0

* Data structure changes 
    * IDs for simulations and capabilities no longer need to be numeric. ID 
    styles in the format of "FOO-123" and "MY_Scenario" are now supported.
    * Quantified scenarios now store parameters for TEF, TC, LM, and DIFF 
    values as list columns. This allows non L/ML/H/CONF distributions to be 
    more easily sampled. Qualitative scenario structure is unchanged so this 
    should have no impact on most users.
    * `capabilities` table has renamed the `id` column to `capability_id` to 
    be consistent with other ID columns throughout the schema. Survey (Excel) 
    users are not impacted by this change.
* Model interface change - With the unification on list columns for OpenFAIR 
parameters, the top level model objects no longer take a dedicated `diff_estimates` 
option. The `run_simulations()` function accounts for this change. Users using 
the standard flow will not be impacted.
* `summarize_domains()` - Incorporates the now removed 
`calculate_domain_impact()`  and `calculate_weak_domains()` functions. As part 
of this consolidation, the `mean_tc_exceedance` and  `mean_diff_exceedance` 
calculations are improved by handling NAs in some simulations without zeroing 
out the entire calculation.
* `summarize_domains()` - Properly calculates `mean_diff_exceedance` when there 
all threat events are successfully avoided.
* `generate_heatmap()` - Takes a `domain_summary` input rather than the 
deprecated `domain_impact` structure.

## Bug Fixes
* Using distributions not in the `base` or `stats` namespaces was practically 
impossible. All atomic OpenFAIR functions have been refactored to take a 
fully qualified function (i.e. `EnvStats::rnormTrunc`).
* `explore_scenarios()` was trying to assign a mappings variable to the global 
context, which rightly failed. Scaled back the assignment to the current 
scope.
* `select_loss_opportunities()` properly returns an NA for the threat & difficulty 
exceedance calculations when there are no threat events in a given simulated 
period.
* `summarize_scenarios()` - correctly handles scenarios in which no threat 
events occur in a given simulation. This bug was limited to `mean_tc_exceedance`. 
For previously run simulations, re-summarizing the `scenario_results` will 
generate corrected values.
* `calculate_max_losses()` - no longer returns a duplicate set of results if 
not passed any outliers.

## Improvements
* `run_simulations()` - New `ale_maximum` parameter allows an absolute cap on 
per simulation annual losses to be set. This is an interim step in lieu of 
full hierarchical interaction modeling.
* `run_simulations()` - Errors encountered during runs are now reported better.
* `run_simulations()` - Implements parallel execution via the `furrr` package. 
To run simulations across all cores of a local machine, load `furrr` and 
run `plan(multicore)` before launching an analysis. For more information, 
see the `furrr::future_map()` documentation.
* `sample_lm()` and `sample_tc()` check if they are asked to generate zero 
requested samples, bypassing calling the underlying generation function. This 
avoids problems with generating functions which do not gracefully handle being 
asked to sample a non positive number (zero) of events.
* `load_data()` now fully specifies the expected CSV file formats, avoiding 
possible surprises and making invocations less noisy on the console.
* Removed all deprecated standard-evaluation tidyverse verbs in favor of 
`rlang::.data` constructs, making CRAN checks much simpler.
* Minor documentation cleanup.

# evaluator 0.2.3

## Bug Fixes
* Additional logic in tests to verify that supplemental packages are available, 
skipping the test if said packages are not installed.
* Update tests to convert tibbles to data.frames prior to comparison as a work 
around for [dplyr/2751](https://github.com/tidyverse/dplyr/issues/2751).

# evaluator 0.2.2

## Analysis Change
* Previous versions sampled threat frequency (TEF) as a continuous distribution.
    Threat event counts are discrete (they either happen or don't in a given 
    simulated period), so this previous method was incorrect and inflated threat 
    counts. The differences are small in the standard 10,000 evaluation range, 
    but users should note the change.
* Part of the discretization step for TEF has a positive side effect of making 
    the threat_event column in data objects an integer rather than a double-
    long. As this is still a numeric type, there should be no impact on 
    users for this change apart from a slightly tidier data output.

## Bug Fixes
* Update risk_dashboard to not cache any chunks. This keeps `render` from 
    from trying to write to the package install directory.
* Updated tests to use four significant digits where long reference values are 
    passed, avoiding precision issues on machines without long doubles.


# evaluator 0.2.1

## Bug Fixes
* Document optional dependency on pandoc and add test skip logic to avoid 
    Rmarkdown tests on systems where pandoc is not available.
    + Add testing to Travis matrix for Linux builds with and without pandoc.
* run_analysis script was missing namespace calls for `readr::cols`.
* All Rmarkdown calls now specify an `intermediates_dir` value of `tempdir()`. 
    This can be overwritten on the function call if needed.
* Report generation is now much quieter by default.

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
* Expose OpenFAIR model selection in `run_simulation()` call
    * Provide default TEF/TC/DIFF/LM OpenFAIR model
* New `create_templates()` function for populating starter/sample files, making 
  starting a fresh analysis easier than ever!
* Experimental quick start script, run_analysis.R, supplied with `create_templates()`.
* All default directories normalized to ~/evaluator/[inputs|results]
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
    `dplyr::progress_estimated()`. Also enables reduced package dependencies.
* Tests and code coverage reporting added
    * Improve faulty `capabilities` validation
* Removed dependency on `purrrlyr`

## Miscellaneous Changes
* `generate_report` defaults to creating a MS Word document as the output type

# evaluator 0.1.1

* Replaced dependency on `modeest` with a slimmer `statip` dependency
* Removed dependency on `magrittr`
* Default (overridable) locations of input and results directories now consistently set to "~/data" and "~/results" respectively
* `generate_report()` now takes an optional `focus_scenario_ids` parameter to override the scenarios on which special emphasis (usually executive interest) is desired.
* Improve user experience for optional packages. User is now prompted to install optional dependencies (shiny, DT, flexdashboard, statip, rmarkdown, etc.) when running reporting functionality which requires them.
* Substantial improvements in the sample analysis flow detailed in the usage vignette. You can now actually run all the commands as-is and have them work, which was previously "challenging".
* `summarize_all()` renamed to the more descriptive `summarize_to_disk()` to avoid dplyr conflict
* Add requirement for at least pander v0.6.1 for `tibble` compatibility
* Substantial refactoring on vignette
  * Added missing save steps
  * Corrected package name for `Viewer` to `rstudioapi` 
  * Fixed a few incorrect placeholders
  * Properly committed compiled files to package for distribution and installation
* Update all tidyverse calls to account for deprecations and split out of `purrrlyr`
* Windows CI builds added via AppVeyor
* Use `annotate_logticks()` over manual breaks on risk_dashboard

# evaluator 0.1.0

* Initial submission to CRAN
