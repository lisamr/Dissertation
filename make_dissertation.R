library(rmarkdown)

rmarkdown::render("dissertation_mahood.Rmd")
rmarkdown::render("dissertation_title.Rmd")

system(paste("pdftk dissertation_title.pdf taf.pdf dissertation_mahood.pdf cat output full_dissertation.pdf"))

