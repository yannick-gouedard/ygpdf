
#' A ygpdf Function
#'
#' stamps an existing pdf with an image or a text
#'
#' @keywords pdf, adding image, adding text
#' @export
#' @importFrom pdftools pdf_pagesize
#' @importFrom knitr knit2pdf
#' @examples
#'
#' stamp_file <- 'logo_conspel.png'
#' original_pdf_no_spaces <- gsub(' ', '', original_pdf)           # I do not want any spaces in pdf name
#'                                                                 # so beware if original_pdf has got a path
#' stamp_list <- data.frame(what = c(stamp_file, stamp_file),      # what?
#'                          page = c(1, 2),                        # on wich page?
#'                          type = c('image', 'image'),            # image or text?
#'                          position_x = c(0.1, 0.5),              # ratio of page width
#'                          position_y = c(0.2, 0.5),              # ratio of page height
#'                          size = c(0.1, 0.5),                    # ratio of page width
#'                          angle = c(0, 0),
#'                          stringsAsFactors = FALSE)
#' stamp_pdf(original_pdf_no_spaces, stamp_list, NULL)
#'
#' Note that:   if (is.null(out_file)) { out_file <- gsub('.pdf', '-stamped.pdf', in_file) }
#'

stamp_pdf <- function(in_file, this_list = NULL, out_file = NULL) {
  # check for NULL entries and put some default
  if (is.null(this_list)) {
    this_list <- data.frame(what = c(system.file("images",
                                                 "Rlogo.pdf",
                                                 package = "ygpdf"),
                                     'R logo'),
                            page = c(1, 1),
                            type = c('image', 'text'),
                            position_x = c(0.5, 0.5),
                            position_y = c(0.5, 0.75),
                            size = c(0.5, 0.5),
                            angle = c(15, 15),
                            stringsAsFactors = FALSE)
  }
  if (is.null(out_file)) {
    out_file <- gsub('.pdf', '-stamped.pdf', in_file)
  }
  pagesize <- pdf_pagesize(in_file)
  tex_file <- gsub('.pdf', '.tex', out_file)
  this_paper <- 'unknown_paper'
  #
  # check page size and orientation of the input PDF
  # works only if all pages of the PDF have same papersize and orientation
  #
  # [TODO]: see if there is a generic way to do this
  #
  if (pagesize[1, 5] == '841.89' & pagesize[1, 6] == '595.276') {
    this_paper <- 'a4paper,landscape'
  } else {
    if (pagesize[1, 5] == '595.276' & pagesize[1, 6] == '841.89') {
      this_paper <- 'a4paper'
    }
  }

  ##### create the rnw file
  #
  # I put an inline rnw as I do not want to have a second file (the .rnw)
  # perhaps there is a way to knit inline without the rnw file ...
  #
  stamped_rnw <- paste0(
    '\\documentclass[', this_paper, ']{article}\n',
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
      this_page, ']{', in_file, '}}}\n')
    #
    # put the stamps if there is one (or more) on this page
    # one tikzpicture per page
    #
    # [TODO]: see if we can have one tikzpicture for all images on all pages...
    #
    if (this_page %in% unique(this_list$page)) {
      stamped_rnw <- paste0(stamped_rnw,
                            '\\begin{tikzpicture}[remember picture,overlay]\n')
      for (this_stamp_pos in seq_along(this_list$what[this_list$page == this_page])) {
        what <- this_list$what[this_list$page == this_page][this_stamp_pos]
        this_position_x <- this_list$position_x[this_list$page == this_page][this_stamp_pos]
        this_position_y <- this_list$position_y[this_list$page == this_page][this_stamp_pos]
        this_size <- this_list$size[this_list$page == this_page][this_stamp_pos]
        this_angle <- this_list$angle[this_list$page == this_page][this_stamp_pos]
        if (this_list$type[this_list$page == this_page][this_stamp_pos] == 'image') {
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\node [xshift=', this_position_x, '\\paperwidth,',
                                      'yshift=-', this_position_y, '\\paperheight,',
                                      'rotate=', this_angle, ']',
                                      'at (current page.north west) ',
                                      '{\\includegraphics[width=', this_size,
                                      '\\paperwidth]{', what, '}} ;\n'))
        } else {
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\node [align=center,xshift=', this_position_x, '\\paperwidth,',
                                      'yshift=-', this_position_y, '\\paperheight,',
                                      'rotate=', this_angle, ',scale=', this_size/30, '\\paperwidth,text opacity=0.4] ',
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
  # remove temporary rnw file
  #
  file.remove(rnw_file)
}

