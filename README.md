# ygpdf
Tools to work on PDF files

## Content
features / functions list:
 1. `ygpdf_stamp()` 
  - takes as inputs:
	- input PDF filename, 
	- stamp list (data.frame - can be `NULL`),
	- and output PDF filename (can be `NULL`).
  - stamps the input PDF with images and text as defined by the stamp list.
  - saves the resulting pdf as output PDF filename.
  - more than one page of PDF.
  - more than one image or text per page.
  - handles position, size, rotation, transparency.
  - R + LaTeX + Tikz.
  - white spaces in filenames: I handle them (by copying into _nowhitespace_ filenames and copying back afterwards). 
  - Just beware if the filename includes a path as this path shall exist without white spaces.
  - [ TO DO ]: separate filename from path 
 2. `ygpdf_combine()`
  - combines 2 pdfs in one, page per page and 2 pages per side.
  - output has same size than original, rotated 90 degrees: portrait <--> landscape
  - up to now, needs to know if input PDFs are landscape or porttrait oriented.
  - [ TO DO ]: find a way to know if input PDF orientation
 3. `ygpdf_diff()`
  - takes 2 PDFs as input.
  - checks differences between PDFs.
  - creates a mask, page per page, circling the differences using `ygpdf::ygpdf_stamp`
  - gives as output either 2 PDFs same age size or 1 PDF with the 2 inputs, same page size, two pages per side using `ygpdf::ygpdf_combine()`.
  - returns difference percentage.