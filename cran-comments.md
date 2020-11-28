## Update

This is a hotfix to address recent breaking changes in `tibble`.

## Test environments

* local Windows 10 64 install, R 4.0.3
* local OS X, R 4.0.3
* OS X (via GitHub Actions), R-release
* win-builder (R-devel and R-release)
* R-Hub
  * "devian-gcc-devel-nold" - R-devel --enable-long-double=no
  * Windows (R-release and R-devel)
  * Linux (R-release and R-devel)

## R CMD check results

* There were no NOTEs, ERRORs or WARNINGs.

## Downstream dependencies

The `collector` package (also authored by me) depends on this package. This 
release does not impact collector, apart from fixing CRAN checks on 
collector that fail due to `evaluator`.
