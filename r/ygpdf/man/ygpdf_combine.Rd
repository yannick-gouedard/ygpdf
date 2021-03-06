% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ygpdf_combine.R
\name{ygpdf_combine}
\alias{ygpdf_combine}
\title{A ygpdf Function}
\usage{
ygpdf_combine(
  first_pdf,
  second_pdf,
  out_pdf,
  keep_paper_size = TRUE,
  is_landscape = FALSE
)
}
\description{
converts 2 PDF files into one
page(n) = page_pdf1(n) + page_pdf2(n)
}
\examples{

ygpdf_combine(first_pdf, second_pdf, out_pdf, keep_paper_size = TRUE, is_landscape = FALSE)

[ TODO ]: manage portrait / landscape orientation

}
\keyword{combine}
\keyword{pdf,}
