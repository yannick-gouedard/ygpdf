#' A ygpdf Function
#'
#' updates (installs) the ygpdf library
#'
#' @importFrom devtools install
#' @keywords process
#' @export
#'

update_ygpdf <- function() {
  old_wd <- getwd()
  setwd(get_r_ygpdf_root_folder())
  if (Sys.getenv('USERNAME') == "DELL" &
      Sys.getenv('COMPUTERNAME') == "PRODUCTIVITY-YG") {
    library(roxygen2)
    devtools::document()
  }
  devtools::install()
  library(ygpdf)
  setwd(old_wd)
}
