
#' A ygpdf Function
#'
#' stamps an existing pdf with an image or a text
#'
#' @keywords pdf, adding image, adding text
#' @export
#' @importFrom pdftools pdf_pagesize
#' @import knitr
#' @examples
#'
#' stamp_file <- system.file("images", "Rlogo.pdf", package = "ygpdf")
#'
#' stamp_list <- data.frame(what = c(stamp_file, stamp_file),  # what?
#'                          page = c(1, 2),                    # on wich page?
#'                          type = c('image', 'image'),        # image or text?
#'                          position_x = c(0.1, 0.5),          # ratio of page width
#'                          position_y = c(0.2, 0.5),          # ratio of page height
#'                          size = c(0.1, 0.5),                # ratio of page width
#'                          angle = c(0, 0),                   # rotation angle
#'                          alpha = c(0.5, 0.5),               # transparency
#'                          stringsAsFactors = FALSE)
#' stamp_pdf(original_pdf, stamp_list, NULL)
#'
#' Note that:   if (is.null(out_file)) { out_file <- gsub('.pdf', ' - stamped.pdf', in_file) }
#'

stamp_pdf <- function(in_file, in_list = NULL, out_file = NULL) {
  # check for NULL entries and put some default
  if (is.null(in_list)) {
    in_list <- data.frame(what = c(system.file("images",
                                               "Rlogo.pdf",
                                               package = "ygpdf"),
                                    'R logo'),
                          page = c(1, 1),
                          type = c('image', 'text'),
                          position_x = c(0.5, 0.5),
                          position_y = c(0.5, 0.75),
                          size = c(0.5, 0.5),
                          angle = c(15, 15),
                          alpha = c(0.5, 0.5),
                          stringsAsFactors = FALSE)
  }
  if (is.null(out_file)) {
    out_file <- gsub('.pdf', ' - stamped.pdf', in_file)
  }
  #
  # dealing with spaces in file names
  #
  in_file_no_spaces <- gsub(' ', '', in_file)
  out_file_no_spaces <- gsub(' ', '', out_file)
  remove_in_file_no_spaces_afterwards <- FALSE
  remove_out_file_no_spaces_afterwards <- FALSE
  if (in_file_no_spaces != in_file) {
    file.copy(in_file, in_file_no_spaces)
    remove_in_file_no_spaces_afterwards <- TRUE
  }
  if (out_file_no_spaces != out_file) {
    remove_out_file_no_spaces_afterwards <- TRUE
  }

  pagesize <- pdf_pagesize(in_file_no_spaces)
  tex_file <- gsub('.pdf', '.tex', out_file_no_spaces)

  ##### create the rnw file
  #
  # I put an inline rnw as I do not want to have a second file (the .rnw)
  # perhaps there is a way to knit inline without the rnw file ...
  #
  # I use \\documentclass{minimal}
  # and define custom paper size as the one pdftools told me
  # also defining \\paperwidth and \\paperheight
  #
  stamped_rnw <- paste0(
    '\\documentclass{minimal}\n',
    '\\special{papersize=', pagesize[1, 6], 'px,', pagesize[1, 5], 'px}',
    '\\setlength{\\paperwidth}{', pagesize[1, 6], 'px}',
    '\\setlength{\\paperheight}{', pagesize[1, 5], 'px}',
    '\\usepackage{eso-pic}',
    '\\usepackage{graphicx}',
    '\\usepackage{changepage}',
    '\\usepackage{tikz}\n',
    '\\pagestyle{empty}\n\n',
    '\\begin{document}\n')
  #
  # for each page of the input PDF
  #
  for (this_page in seq_len(nrow(pagesize))) {
    #
    # put the original page as background
    #
    stamped_rnw <- paste0(stamped_rnw,
      '\\AddToShipoutPictureBG*{\\AtStockLowerLeft{\\includegraphics[page=',
      this_page, ']{', in_file_no_spaces, '}}}\n')
    #
    # put the stamps if there is one (or more) on this page
    # one tikzpicture per page
    #
    # [TODO]: see if we can have one tikzpicture for all images on all pages...
    #
    if (this_page %in% unique(in_list$page)) {
      stamped_rnw <- paste0(stamped_rnw,
                            '\\begin{tikzpicture}[remember picture,overlay]\n')
      for (this_stamp_pos in seq_along(in_list$what[in_list$page == this_page])) {
        what <- in_list$what[in_list$page == this_page][this_stamp_pos]
        this_position_x <- in_list$position_x[in_list$page == this_page][this_stamp_pos]
        this_position_y <- in_list$position_y[in_list$page == this_page][this_stamp_pos]
        this_size <- in_list$size[in_list$page == this_page][this_stamp_pos]
        this_angle <- in_list$angle[in_list$page == this_page][this_stamp_pos]
        this_alpha <- in_list$alpha[in_list$page == this_page][this_stamp_pos]
        if (in_list$type[in_list$page == this_page][this_stamp_pos] == 'image') {
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\node [xshift=', this_position_x, '\\paperwidth,',
                                      'yshift=-', this_position_y, '\\paperheight,',
                                      'rotate=', this_angle, ',opacity=', this_alpha, ']',
                                      'at (current page.north west) ',
                                      '{\\includegraphics[width=', this_size,
                                      '\\paperwidth]{', what, '}} ;\n'))
        } else {
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\node [align=center,xshift=', this_position_x, '\\paperwidth,',
                                      'yshift=-', this_position_y, '\\paperheight,',
                                      'rotate=', this_angle, ',scale=', this_size/30, '\\paperwidth,',
                                      'text opacity=', this_alpha, '] ',
                                      'at (current page.north west) ',
                                      '{', what, '} ;\n'))
        }
      }
      stamped_rnw <- paste0(stamped_rnw, '\\end{tikzpicture}\n')
    }
    if (this_page != nrow(pagesize)) {
      stamped_rnw <- paste0(stamped_rnw, '~\\clearpage\n')
    }
  }
  stamped_rnw <- paste0(stamped_rnw, '\\end{document}')
  #
  # Create temporary file
  #
  rnw_file <- 'stamp_pdf.Rnw'
  cat(stamped_rnw, file = rnw_file)
  #
  # knit
  #
  knit2pdf(input = rnw_file, output = tex_file)
  #
  # remove temporary files
  #
  file.remove(rnw_file)
  file.remove(tex_file)
  if (remove_in_file_no_spaces_afterwards) {
    file.remove(in_file_no_spaces)
  }
  if (remove_out_file_no_spaces_afterwards) {
    file.rename(out_file_no_spaces, out_file)
  }

}

