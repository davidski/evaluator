## Update

This is a minor update, addressing stale links, some deprecated functions 
in dependencies, and internal code maintenance. No user-facing changes exist.

## Test environments

* local Windows 10 64 install, R 4.0.3
* local OS X, R 4.0.5
* OS X (via GitHub Actions), R-release
* win-builder (R-devel and R-release)
* R-Hub
  * "devian-gcc-devel-nold" - R-devel --enable-long-double=no
  * Windows (R-release and R-devel)
  * Linux (R-release and R-devel)

## R CMD check results

* There were no NOTEs, ERRORs or WARNINGs.

## Downstream dependencies

The `collector` package (also authored by me) depends on this package. 
`collector` is currently archived on CRAN, but a resubmission of this 
package will take place once `evaluator` is updated on CRAN.
