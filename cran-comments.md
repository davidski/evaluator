## Update

This is a bug-fix update for an existing package. Primary fixes include
correcting newly detected CRAN policy violations regarding undocumented `pandoc` 
dependencies and some `rmarkdown` calls inappropriately writing intermediate 
files to a package directory instead of a temporary directory. These have been 
corrected, test coverage has been updated, and the CI build matrix expanded to 
try to address all of these issues.

## Test environments

* local Windows 10 64 install, R 3.4.3
* local OS X, R 3.4.4
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.4
* Ubuntu 14.04 LTS (on travis-ci)
  * R-release (both with and without pandoc)
  * R-devel   (both with and without pandoc)
* OS X 10.12 (on travis-ci), R-release
* win-builder (devel and release)

## R CMD check results

* Win-builder R-Release (not R-Devel) issues a false positive NOTE on a 
  potential mis-spelling.
* There were no ERRORs or WARNINGs.

## Downstream dependencies

There are currently no downstream dependencies on this package.
