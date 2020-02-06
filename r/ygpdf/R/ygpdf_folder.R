#' A ygpdf Function
#'
#' Gives the current folder
#'
#' @keywords Current folder
#' @export
#' @examples
#' current_dir <- get_current_folder()
#' current_dir
#' [1] "D:/github.com/yannick-gouedard/ygpdf/r/ygpdf"
#' Warning message:
#'   In get_current_folder() : current_dir from getwd()
#'
#' NB: A warning message appeared as I directly called the function,
#' without being in a script
#'

get_current_folder <- function() {
  if (length(sys.parents()) > 1) {
    # script is called
    current_dir <- dirname(sys.frame(1)$ofile)
  } else {
    warning("current_dir from getwd()")
    current_dir <- getwd()
  }
  return(current_dir)
}

#' A ygpdf Function
#'
#' Gives the R ygpdf root folder
#'
#' @keywords R ygpdf root folder
#' @export
#' @examples
#'

get_r_ygpdf_root_folder <- function() {
  if (Sys.getenv('GITROOT_YGPDF') == '') {
    set_GITROOT_ygpdf_folder()
  }
  ygpdf_dir <- file.path(Sys.getenv('GITROOT_YGPDF'),
                         'r',
                         'ygpdf',
                         'R')
  ygpdf_dir <- normalizePath(ygpdf_dir, winslash = "/", mustWork = NA)
  return(ygpdf_dir)
}

#' A ygpdf Function
#'
#' sets the R ygpdf root folder
#'
#' @importFrom utils choose.dir
#' @keywords R ygpdf root folder
#' @export
#' @examples
#'

set_GITROOT_ygpdf_folder <- function() {
  if (Sys.getenv('GITROOT_YGPDF') != '') {
    warning(paste0('GITROOT_YGPDF already set to ',
                   Sys.getenv('GITROOT_YGPDF'),
                   '\n Resetting it now.'))
  }
  git_dir <- choose.dir(default = "", caption = "Select the GitHub root folder")
  git_dir <- normalizePath(git_dir, winslash = "/", mustWork = NA)
  Sys.setenv('GITROOT_YGPDF' = git_dir)
}
