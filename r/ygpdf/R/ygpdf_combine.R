#' A ygpdf Function
#'
#' converts 2 PDF files into one
#' page(n) = page_pdf1(n) + page_pdf2(n)
#'
#' @keywords pdf, combine
#' @export
#' @importFrom pdftools pdf_pagesize
#' @importFrom dplyr if_else
#' @import knitr
#' @examples
#'
#' ygpdf_combine(first_pdf, second_pdf, out_pdf, keep_paper_size = TRUE, is_landscape = FALSE)
#'
#' [ TODO ]: manage portrait / landscape orientation
#'


ygpdf_combine <- function(first_pdf, second_pdf, out_pdf,
                          keep_paper_size = TRUE,
                          is_landscape = FALSE) {
  first_pdf_no_spaces <- gsub(' ', '', first_pdf)
  second_pdf_no_spaces <- gsub(' ', '', second_pdf)
  out_pdf_no_spaces <- gsub(' ', '', out_pdf)
  remove_first_pdf_no_spaces_afterwards <- FALSE
  remove_second_pdf_no_spaces_afterwards <- FALSE
  remove_out_pdf_no_spaces_afterwards <- FALSE
  if (first_pdf_no_spaces != first_pdf) {
    file.copy(first_pdf, first_pdf_no_spaces)
    remove_first_pdf_no_spaces_afterwards <- TRUE
  }
  if (second_pdf_no_spaces != second_pdf) {
    file.copy(second_pdf, second_pdf_no_spaces)
    remove_second_pdf_no_spaces_afterwards <- TRUE
  }
  if (out_pdf_no_spaces != out_pdf) {
    remove_out_pdf_no_spaces_afterwards <- TRUE
  }

  pagesize <- pdf_pagesize(first_pdf_no_spaces)
  pagesize_second <- pdf_pagesize(second_pdf_no_spaces)
  if (!((round(max(pagesize_second[1, 5:6]), 0) == round(max(pagesize_second[1, 5:6]), 0)) &
        (round(min(pagesize_second[1, 5:6]), 0) == round(min(pagesize_second[1, 5:6]), 0)))) {
    stop('Two pdfs shall have save page size')
  }
  paper_height <- round(max(pagesize[1, 5:6]), 0)
  paper_width <- round(min(pagesize[1, 5:6]), 0)
  #
  # if original is not landscape, then the output is
  #
  if (!is_landscape) {
    this_max <- paper_height
    paper_height <- paper_width
    paper_width <- this_max
  }
  tex_file <- gsub('.pdf', '.tex', out_pdf_no_spaces)
  ##### create the rnw file
  #
  # I put an inline rnw as I do not want to have a second file (the .rnw)
  # perhaps there is a way to knit inline without the rnw file ...
  #
  # I use \\documentclass{minimal}
  # and define custom paper size as the one pdftools told me
  # also defining \\paperwidth and \\paperheight
  #
  combined_pdf <- paste0(
    '\\documentclass{minimal}\n',
    '\\special{papersize=', paper_width, 'px,', paper_height, 'px}\n',
    if_else(!is_landscape, '\\special{landscape}\n', ''),
    '\\setlength{\\paperwidth}{', paper_width, 'px}\n',
    '\\setlength{\\paperheight}{', paper_height, 'px}\n',
    '\\usepackage{pdfpages}\n',
    '\\pagestyle{empty}\n\n',
    '\\begin{document}\n',
    '\\includepdfmerge[nup=',
    if_else(!is_landscape, '2x1', '1x2'), ',landscape=',
    if_else(!is_landscape, 'false', 'false'), ']{\n')
  #
  # for each page of the input PDF
  #
  for (this_page in seq_len(nrow(pagesize))) {
    #
    # combine two PDFs page per page in 2 columns, rotated
    #
    combined_pdf <-
      paste0(combined_pdf,
             first_pdf_no_spaces, ',', this_page,',\n',
             second_pdf_no_spaces, ',', this_page
             )

    if (this_page != nrow(pagesize)) {
      combined_pdf <- paste0(combined_pdf,',\n')
    }
  }
  combined_pdf <- paste0(combined_pdf, '}\n\\end{document}')
  #
  # Create temporary file
  #
  rnw_file <- 'combined_pdf.Rnw'
  cat(combined_pdf, file = rnw_file)
  #
  # knit
  #
  knit2pdf(input = rnw_file, output = tex_file, quiet = TRUE)
  #
  # remove temporary files
  #
  file.remove(rnw_file)
  file.remove(tex_file)
  if (remove_first_pdf_no_spaces_afterwards) {
    file.remove(first_pdf_no_spaces)
  }
  if (remove_second_pdf_no_spaces_afterwards) {
    file.remove(second_pdf_no_spaces)
  }
  if (remove_out_pdf_no_spaces_afterwards) {
    file.rename(out_pdf_no_spaces, out_pdf)
  }
}
