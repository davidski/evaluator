# Installation Extras

While using the full power of Evaluator is easier with some basic proficency with the R langauge, making quantitative risk analysis available to a broader audience has always been a core goal of this project. Users with less R experience may appreciate these notes on setting up a R environment.

# Docker

The simplest method applicable to most users is the corresponding [evaluator-docker](https://github.com/davidski/evaluator-docker) project. This Dockerfile (and the corresponding pre-built image on [Docker Hub](https://hub.docker.com/r/davidski/evaluator-docker/)) is the fastest and surest means of getting started with Evaluator.

# MacOS

For Mac users running homebrew, the following terminal commands can be used to 
set up a functional environment.

```
# R and RStudio under homebrew derived from @hrbrmstr's post
# https://rud.is/b/2015/10/22/installing-r-on-os-x-100-homebrew-edition/

# Core R and RStudio
brew install homebrew/science/R
brew install Caskroom/cask/rstudio

# Latex required for PDF knitting
brew cask install mactex
 
# extra libraries to make other packages easier to work with 
brew install libsvg curl libxml2 gdal geos boost
 
# font installation
brew tap caskroom/fonts
brew cask install font-fira-code font-iosevka font-inconsolata font-open-sans-condensed font-open-sans font-roboto-condensed
R -e 'install.package("extrafont", repos="http://cran.cnr.berkeley.edu")'
R -e 'extrafont::font_import(prompt = FALSE)'
```

Then install the Evaluator library either directly from CRAN via `install.packages('evaluator')` or from GitHub via `devtools::install_github('davidski/evaluator')`.

# Windows

Windows users should use [chocolately](https://chocolatey.org/install) for a simple `homebrew`-like package manager experience.

- Install R

    -  `choco install --yes r`

- Install miktex for PDF knitting

    - `choco install --yes miktex`

- Install RStudio (not available on Chocolately)

    - https://www.rstudio.com/products/rstudio/download/#download