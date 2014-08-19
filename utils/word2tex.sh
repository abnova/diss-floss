# Reproducible process to convert a Microsoft Word document into a TEX document
#
# (Currently under development; partially based on information in the following discussion:
# http://tex.stackexchange.com/questions/46015/converting-ms-word-doc-to-latex-by-command-line)
#
# Step 1. Save Word file as ODT
#
# Step 2. Install or verify installation of Writer2LaTeX (TODO: automate)
# (http://writer2latex.sourceforge.net/)
#
sudo apt-get install writer2latex
#
# Step 3. Convert ODT document to TEX document, using Writer2LaTeX ('ultraclean' option!)
#
w2l -ultraclean <input_filename>.odt <output_filename>.tex
#
# Step 4. Convert generated TEX document to PDF document and review the results
#
# Option 1. Manual convertion by opening TEX document in RStudio and clicking "Compile PDF" button
# Option 2. Automatic conversion by using script behind Option 1.
#
grDevices::pdf.options(useDingbats = FALSE) # optional
require(knitr)
opts_knit$set(concordance = TRUE)
knit('filename.rnw', encoding='UTF-8')
pdflatex filename.tex
#
# Step 5. Review generated PDF document and note issues
# Step 6. Fix noted issues in the source TEX document
# Step 7. Repeat steps 4-6 until reaching the desired quality