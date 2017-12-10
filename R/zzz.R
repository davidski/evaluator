.onAttach <- function(libname, pkgname) {

  # ensure fonts are available for use/detection
  # technique borrowed from r-pkg and hrbrthemes

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    windowsFonts <- grDevices::windowsFonts # work around for https://github.com/wch/extrafont/issues/44
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
  pdfFonts <- grDevices::pdfFonts #work around for https://github.com/wch/extrafont/issues/44
  extrafont::loadfonts("pdf", quiet = TRUE)
  postscriptFonts <- grDevices::postscriptFonts #work around for https://github.com/wch/extrafont/issues/44
  extrafont::loadfonts("postscript", quiet = TRUE)
  #nocov end
}
