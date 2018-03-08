
.onAttach <- function(libname, pkgname) {

  # if (interactive()) {
  #   packageStartupMessage("This package is under active development.")
  # }
  # See https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r and
  # https://github.com/wch/extrafont/issues/44.
  if (.Platform$OS.type == "windows")  {
    # if (interactive()) {
    #   packageStartupMessage("Registered Windows fonts with R.")
    # }
    fnt_windows <- grDevices::windowsFonts()
    extrafont::loadfonts("win", quiet = TRUE)
  }
  # fnt_windows <- grDevices::windowsFonts()
  # extrafont::loadfonts("win", quiet = TRUE)
  invisible()
}
