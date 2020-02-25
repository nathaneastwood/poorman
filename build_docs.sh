Rscript -e "devtools::document()"
Rscript -e "rmarkdown::render('README.Rmd', output_format = 'md_document')"
