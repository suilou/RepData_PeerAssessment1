# generate_MD_HTML.R

# 4/10/2016
# for Coursera Data Scientist Series Course#5 Project 1

install.packages("knitr")
install.packages("markdown")
library(knitr)
library(markdown)

#transform the .Rmd to a markdown (.md) file.
knit('./RepData_PeerAssessment1/PA1_template.Rmd')

#transform the .md to HTML format
markdownToHTML("PA1_template.md", "PA1_template.html",fragment.only = TRUE)