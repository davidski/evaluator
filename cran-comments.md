## Update

This is a bug-fix update for an existing package that was just archived today 
(March 28th) due to a failure on systems without long-double support which 
has just been fixed.  Other fixes include files being written to the package 
directory, traced to an inadvertant `cache` option in a supplied Rmd document.

## Test environments

* local Windows 10 64 install, R 3.4.4
* local OS X, R 3.4.4
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.4
* Ubuntu 14.04 LTS (on travis-ci)
  * R-release (both with and without pandoc)
  * R-devel  (both with and without pandoc)
* OS X 10.12 (on travis-ci), R-release
* win-builder (devel and release)

## R CMD check results

* Win-builder R-Release (not R-Devel) issues a false positive NOTE on a 
  potential mis-spelling.
* Win-builder R-Devel intermittently fails due to an Rcpp error with dplyr. 
  The error does not consistently generate and there is no Rcpp code in this 
  pacakge.
* There were no ERRORs or WARNINGs.

## Downstream dependencies

There are currently no downstream dependencies on this package.
