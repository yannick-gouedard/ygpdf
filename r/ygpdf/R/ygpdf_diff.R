#' A ygpdf Function
#'
#' shows differences between 2 pdfs
#' uses code from https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
#'
#' @keywords pdf, differences
#' @export
#' @importFrom imager magick2cimg grayscale imhessian threshold label draw_circle save.image imlist imappend as.cimg
#' @importFrom magick image_read image_read_pdf image_quantize
#' @importFrom dplyr summarise group_by
#' @importFrom pdftools pdf_render_page pdf_length
#' @import magrittr
#' @examples
#'
#' ygpdf_diff(first_pdf, second_pdf, density, combine_results = TRUE, is_landscape = TRUE)
#'
#' [ TODO ]: manage portrait / landscape orientation
#'

ygpdf_diff <- function(first_pdf, second_pdf,
                       density = 72, combine_results = TRUE,
                       is_landscape = TRUE) {
  # check for fonts in the PDfs
  #pdftools::pdf_fonts(first_pdf)
  #pdftools::pdf_fonts(second_pdf)
  #pdf.options(useDingbats=FALSE)

  nb_pages <- max(pdf_length(first_pdf), pdf_length(second_pdf))

  stamp_list <- data.frame(what = '',               # what?
                           page = '',               # on wich page?
                           type = '',               # image or text?
                           position_x = '',         # ratio of page width
                           position_y = '',         # ratio of page height
                           size = '',               # ratio of page width
                           angle = '',              # angle
                           alpha = '',              # transparency (opacity)
                           stringsAsFactors = FALSE)
  out_difference <- c()

  for (this_page in 1:nb_pages) {
    # turn PDFs into Hi-res images
    #first_image <- image_read_pdf(first_pdf, density = density)
    #second_image <- image_read_pdf(second_pdf, density = density)
    first_image <- pdf_render_page(first_pdf, page = this_page, dpi = density)
    second_image <- pdf_render_page(second_pdf, page = this_page, dpi = density)

    # convert images to greyscale
    first_grey <- first_image %>% image_read() %>% image_quantize(colorspace = 'gray')
    second_grey <- second_image %>% image_read() %>% image_quantize(colorspace = 'gray')

    #
    # get.centers code taken from
    # https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
    #
    get.centers <- function(im, thr = "99%")
    {
      dt <- imhessian(im) %$% { xx * yy - xy^2 } %>% threshold(thr) %>% label
      as.data.frame(dt) %>% subset(value > 0) %>% dplyr::group_by(value) %>%
        dplyr::summarise(mx = mean(x), my = mean(y))
    }

    # compute difference between images
    first_cimg <- magick2cimg(first_grey)
    second_cimg <- magick2cimg(second_grey)
    cimg_diff <- 255 - abs(first_cimg - second_cimg) / 2
    graph <- cimg_diff %>%
      get.centers() %$%
      draw_circle(cimg_diff, mx, my, radius = 2, color = 'black',
                  opacity = 1, filled = TRUE)
    #
    # Add alpha channel
    #
    alpha <- grayscale(graph, drop = FALSE)
    alpha[alpha < 1] <- 0
    alpha[alpha > 1] <- 1
    out_difference <- c(out_difference,
                        sum(alpha == 0) / sum(alpha >= 0))

    alpha[, , 1:2] <- 1
    alpha <- imlist(alpha, as.cimg(1 - alpha[, , 3])) %>% imappend("c")

    stamp_file <- paste0('mask', this_page, '.png')
    save.image(alpha, stamp_file, quality = 1)

    stamp_list <- rbind(stamp_list,
                        data.frame(what = stamp_file,      # what?
                                   page = this_page,       # on wich page?
                                   type = 'image',         # image or text?
                                   position_x = 0.5,       # ratio of page width
                                   position_y = 0.5,       # ratio of page height
                                   size = 1,               # ratio of page width
                                   angle = 0,              # angle
                                   alpha = 0.3,            # transparency (opacity)
                                   stringsAsFactors = FALSE))
  }

  first_diff <- gsub('.pdf', ' - differences.pdf', first_pdf)
  second_diff <- gsub('.pdf', ' - differences.pdf', second_pdf)
  ygpdf_stamp(first_pdf, stamp_list, first_diff,
              is_landscape = is_landscape)
  ygpdf_stamp(second_pdf, stamp_list, second_diff,
              is_landscape = is_landscape)

  file.remove(stamp_list$what[stamp_list$type == 'image'])

  if (combine_results) {
    combined_pdf <- gsub('.pdf', ' - differences combined.pdf', first_pdf)
    ygpdf_combine(first_diff,
                  second_diff,
                  combined_pdf,
                  keep_paper_size = TRUE,
                  is_landscape = is_landscape)
    file.remove(first_diff)
    if (first_diff != second_diff) {
      file.remove(second_diff)
    }
  }

  return(out_difference)
}
