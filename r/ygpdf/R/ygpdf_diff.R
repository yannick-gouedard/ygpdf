#' A ygpdf Function
#'
#' shows differences between 2 pdfs
#' uses code from https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
#'
#' @keywords pdf, differences
#' @export
#' @importFrom imager magick2cimg grayscale imhessian threshold label draw_circle save.image
#' @importFrom magick image_read_pdf image_quantize
#' @importFrom dplyr summarise group_by
#' @import magrittr
#' @examples
#'
#' ygpdf_diff(first_pdf, second_pdf, density, combine_results = TRUE, is_landscape_orientation = TRUE)
#'
#' [ TODO ]: manage portrait / landscape orientation
#'

ygpdf_diff <- function(first_pdf, second_pdf,
                       density = 72, combine_results = TRUE,
                       is_landscape_orientation = TRUE) {
  # turn PDFs into Hi-res images
  first_image <- image_read_pdf(first_pdf, density = density)
  second_image <- image_read_pdf(second_pdf, density = density)

  # convert images to greyscale
  first_grey <- first_image %>% image_quantize(colorspace = 'gray')
  second_grey <- second_image %>% image_quantize(colorspace = 'gray')

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
    get.centers("99.9%") %$%
    draw_circle(cimg_diff, mx, my, radius = 10, color = 'black',
                opacity = 1, filled = TRUE)
  #
  # Add alpha channel
  #
  alpha <- grayscale(graph, drop = FALSE)
  alpha[alpha < 1] <- 0
  alpha[alpha > 1] <- 1
  out_difference <- sum(alpha == 0) / sum(alpha >= 0)

  alpha[, , 1:2] <- 1

  stamp_file <- 'mask.png'
  save.image(alpha, stamp_file, quality = 0.7)

  stamp_list <- data.frame(what = stamp_file,      # what?
                           page = 1,               # on wich page?
                           type = 'image',         # image or text?
                           position_x = 0.5,       # ratio of page width
                           position_y = 0.5,       # ratio of page height
                           size = 1,               # ratio of page width
                           angle = 0,              # angle
                           alpha = 0.3,            # transparency (opacity)
                           stringsAsFactors = FALSE)

  first_diff <- gsub('.pdf', ' - differences.pdf', first_pdf)
  second_diff <- gsub('.pdf', ' - differences.pdf', second_pdf)
  ygpdf_stamp(first_pdf, stamp_list, first_diff)
  ygpdf_stamp(second_pdf, stamp_list, second_diff)

  file.remove(stamp_file)

  if (combine_results) {
    combined_pdf <- gsub('.pdf', ' - differences combined.pdf', first_pdf)
    ygpdf_combine(first_diff,
                  second_diff,
                  combined_pdf,
                  keep_paper_size = TRUE,
                  is_landscape_orientation = is_landscape_orientation)
    file.remove(first_diff)
    file.remove(second_diff)
  }


  return(out_difference)
}
