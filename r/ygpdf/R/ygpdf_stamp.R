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
#'
#' ####
#'
#' Example 1:
#' For an A4 portrait PDF file
#' ygpdf_stamp(in_file = "<define here the PDF file>", is_landscape = FALSE)
#'
#' ####
#'
#' Example 2:
#' stamp_file <- system.file("images", "Rlogo.pdf", package = "ygpdf")
#' stamp_list <- data.frame(what = c(stamp_file, stamp_file),       # what?
#'                          page = c(1, 2),                         # on wich page?
#'                          type = c('image', 'image'),             # image or text?
#'                          position_x = c(0.1, 0.5),               # ratio of page width
#'                          position_y = c(0.2, 0.5),               # ratio of page height
#'                          size = c(0.1, 0.5),                     # ratio of page width
#'                          angle = c(0, 0),                        # rotation angle
#'                          alpha = c(0.5, 0.5),                    # transparency
#'                          col_right = c('#FF0000', '#0000FF'),    # color right, for text only
#'                          col_leftt = c('#0000FF', '#FF0000'),    # color left, for text only
#'                          stringsAsFactors = FALSE)
#' ygpdf_stamp(original_pdf, stamp_list, NULL, is_landscape = TRUE)
#'
#' Note that:   if (is.null(out_file)) { out_file <- gsub('.pdf', ' - stamped.pdf', in_file) }
#'
#' ####
#'
#' Example 3:
#' pdf_stamped_file <- gsub('.pdf', ' - stamped.pdf', pdf_file)
#' nb_pages <- pdftools::pdf_length(pdf_file)
#' colours <- viridis::viridis_pal(option = 'B')(nb_pages + 1)
#' watermark <- "--- draft ---"
#' stamp_list <- data.frame(what = rep(watermark, nb_pages),        # what?
#'                          page = 1:nb_pages,                      # on wich page?
#'                          type = rep('text', nb_pages),           # image or text?
#'                          position_x = rep(0.37, nb_pages),       # ratio of page width
#'                          position_y = rep(0.37, nb_pages),       # ratio of page height
#'                          size = rep(0.05, nb_pages),             # ratio of page width
#'                          angle = rep(52.6, nb_pages),            # rotation angle
#'                          alpha = rep(.8, nb_pages),              # transparency
#'                          col_right = colours[1:nb_pages],        # color right, for text only
#'                          col_leftt = colours[2:(nb_pages + 1)],  # color left, for text only
#'                          stringsAsFactors = FALSE)
#' ygpdf_stamp(in_file = pdf_file, in_list = stamp_list,
#'             out_file = pdf_stamped_file, is_landscape = FALSE)
#'
#' ####
#'

ygpdf_stamp <- function(in_file, in_list = NULL, out_file = NULL,
                        is_landscape = TRUE) {
  # check for NULL entries and put some default
  if (is.null(in_list)) {
    in_list <- data.frame(what = c(system.file("images",
                                               "Rlogo.pdf",
                                               package = "ygpdf"),
                                    'R logo'),
                          page = c(1, 1),
                          type = c('image', 'text'),
                          position_x = c(0.74, 0.37),
                          position_y = c(0.74, 0.37),
                          size = c(0.5, 0.04),
                          angle = c(52.6, 52.6),
                          alpha = c(0.5, 0.8),
                          col_right = c('#FF0000', '#0000FF'),
                          col_leftt = c('#0000FF', '#FF0000'),
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

  paper_height <- round(max(pagesize[1, 5:6]), 0)
  paper_width <- round(min(pagesize[1, 5:6]), 0)
  if (is_landscape) {
    this_max <- paper_height
    paper_height <- paper_width
    paper_width <- this_max
  }
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
    '\\special{papersize=', paper_width, 'px,', paper_height, 'px}\n',
    if_else(!is_landscape, '\\special{landscape}\n', ''),
    '\\setlength{\\paperwidth}{', paper_width, 'px}\n',
    '\\setlength{\\paperheight}{', paper_height, 'px}\n',
    '\\usepackage{eso-pic}',
    '\\usepackage{graphicx}',
    '\\usepackage{changepage}',
    '\\usepackage{tikz}\n',
    '\\usetikzlibrary{fadings}\n',
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
      for (this_stamp_pos in seq_along(in_list$what[in_list$page == this_page])) {
        what <- in_list$what[in_list$page == this_page][this_stamp_pos]
        this_position_x <- in_list$position_x[in_list$page == this_page][this_stamp_pos]
        this_position_y <- in_list$position_y[in_list$page == this_page][this_stamp_pos]
        this_size <- in_list$size[in_list$page == this_page][this_stamp_pos]
        this_angle <- in_list$angle[in_list$page == this_page][this_stamp_pos]
        this_alpha <- in_list$alpha[in_list$page == this_page][this_stamp_pos]
        this_col_right <- grDevices::col2rgb(in_list$col_right[in_list$page == this_page][this_stamp_pos]) / 255
        this_col_left <- grDevices::col2rgb(in_list$col_left[in_list$page == this_page][this_stamp_pos]) / 255
        if (in_list$type[in_list$page == this_page][this_stamp_pos] == 'image') {
          stamped_rnw <- paste0(stamped_rnw,
                                '\\begin{tikzpicture}[remember picture,overlay]\n')
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\node [xshift=', this_position_x, '\\paperwidth,',
                                      'yshift=-', this_position_y, '\\paperheight,',
                                      'rotate=', this_angle, ',opacity=', this_alpha, ']',
                                      'at (current page.north west) ',
                                      '{\\includegraphics[width=', this_size,
                                      '\\paperwidth]{', what, '}} ;\n'))
          stamped_rnw <- paste0(stamped_rnw, '\\end{tikzpicture}\n')
        } else {
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\begin{tikzfadingfrompicture}[name=thisText]\n',
                                      '\\node [align=center,rotate=', this_angle,
                                      ',scale=', this_size, '\\paperwidth,',
                                      'text=transparent!', 100 * this_alpha, '] ',
                                      '{', what, '} ;\n',
                                      '\\end{tikzfadingfrompicture}\n'))
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\definecolor{thisTextRight}{rgb}{',
                                      this_col_right[1], ',', this_col_right[2], ',', this_col_right[3], '}\n',
                                      '\\definecolor{thisTextLeft}{rgb}{',
                                      this_col_left[1], ',', this_col_left[2], ',', this_col_left[3], '}\n'))
          stamped_rnw <- paste0(stamped_rnw,
                                '\\begin{tikzpicture}[remember picture,overlay]\n')
          stamped_rnw <- paste(stamped_rnw,
                               paste0('\\shade[path fading=thisText,fit fading=false,',
                                      'left color=thisTextLeft,right color=thisTextRight,',
                                      'fading transform={xshift=', this_position_x, '\\paperwidth,yshift=-', this_position_y, '\\paperheight}] ',
                                      '(-\\paperwidth,-\\paperheight) rectangle (\\paperwidth,\\paperheight) ;\n'))
          stamped_rnw <- paste0(stamped_rnw, '\\end{tikzpicture}\n')
        }
      }
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
  knit2pdf(input = rnw_file, output = tex_file, quiet = TRUE)
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

