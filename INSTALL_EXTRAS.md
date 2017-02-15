# Installation Extras

This is a work-in-progress set of notes for installing a minimum functioning 
R environment for working with the Evaluator toolkit. While the official 
instructions from the R Project and RStudio sites are cannon, these notes 
may be of use to speed the process.

# MacOS

For Mac users running homebrew, the following terminal commands can be used to 
set up a functional environment. When running Evaluator, other pacakges will 
be installed on initial use when necessary.


```
# R and RStudio under homebrew derived from @hrbrmstr's post
# https://rud.is/b/2015/10/22/installing-r-on-os-x-100-homebrew-edition/

# Core R and RStudio
brew install homebrew/science/R
brew install Caskroom/cask/rstudio

# Java, it's a thing
brew cask install java 

# tell R about Java
R CMD javareconf JAVA_CPPFLAGS=-I/System/Library/Frameworks/JavaVM.framework/Headers
 
# Latex required for PDF knitting
brew cask install mactex
 
# extra libraries to make other packages easier to work woth 
brew install libsvg curl libxml2 gdal geos boost
 
# font installation
brew tap caskroom/fonts
brew cask install font-fira-code font-iosevka font-inconsolata font-open-sans-condensed font-open-sans font-roboto-condensed
R -e 'install.package("extrafont", repos="http://cran.cnr.berkeley.edu")'
R -e 'extrafont::font_import(prompt = FALSE)'

# ensure pacman is available
R -e 'install.package("pacman", repos="http://cran.cnr.berkeley.edu")'
```

# Windows

- Install R

```
choco install --yes r.studio miktex
```
