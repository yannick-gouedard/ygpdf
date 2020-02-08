# ygpdf
Tools to work on PDF files

## Content
features / functions list:
 1. `stamp_pdf()` 
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
  - white spaces in filenames: I handle them (by copying into _nowhitespace_ filenames and copying back afterwards). Just beware if the filename includes a path as this path shall exist without white spaces.
 2. `pdf_difference()`
  - coming soon
  - takes 2 PDFs as input + parameters.
  - checks difference between PDFs (application .
  - creates a mask, page per page circling the differences.
  - gives as output either 2 PDFs same age size or 1 PDF with the 2 inputs, same page size, two pages per side.
  - returns difference percentage.