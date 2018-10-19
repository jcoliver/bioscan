# Render all .Rmd reports
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-10-19

rm(list = ls())

################################################################################

report.files <- list.files(path = ".", pattern = "*.Rmd", full.names = TRUE)

for (report.file in report.files) {
  rmarkdown::render(input = report.file)
}