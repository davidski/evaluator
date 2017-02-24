## Test environments
* local Windows 64 install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking R code for possible problems ... NOTE
  calculate_domain_impact: no visible binding for global variable 'ale'
  calculate_domain_impact: no visible binding for global variable '.'
  Undefined global functions or variables:
    . ale

  This is a side-effect of using dplyr's summarize_each functionality with
  a function (quantile) that requires .dot support. Standard evaluation
  has been used extensively to ensure this is not a problem for users. While
  only a cosmetic error, future iterations will explore using purrr or
  other techniques to see if avoiding this note completely is possible.

## Downstream dependencies

There are currently no downstream dependencies on this package.
