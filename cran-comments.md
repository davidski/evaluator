## Update

This is a minor update, removing some dependencies and substituting others 
with SUGGESTS when full removals could not easily be supported. This is in 
response to the potential archival of `extrafont` (a hard dependency in 
prior versions of this package) per a notice from CRAN.

## Test environments

* local Windows 10 64 install, R 4.0.5
* local OS X, R 4.0.5
* OS X (via GitHub Actions), R-release
* win-builder (R-devel and R-release)
* R-Hub
  * "devian-gcc-devel-nold" - R-devel --enable-long-double=no
  * Windows (R-release and R-devel)
  * Linux (R-release and R-devel)

## R CMD check results

* There are no NOTES, ERRORS, or WARNINGS.

## Downstream dependencies

The `collector` package (also authored by me) depends on this package. 
`collector` is currently archived on CRAN, but a resubmission of this 
package is in process.
