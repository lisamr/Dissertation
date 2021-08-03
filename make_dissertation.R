library(pdftools)

#first knit the dissertation 

#combine with title page
pdf_combine(c("title_page.pdf", "dissertation_Lrosenthal.pdf"), output = "full_dissertation_LRosenthal.pdf")

