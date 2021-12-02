## code to prepare `my_dataset` dataset goes here
m1=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))
m6=readxl::read_excel(paste0(getwd(),"/data-raw/before2000.xlsx"))

m1tcars=mtcars
m3=m1[1:5,c(1,3)]

usethis::use_data(m1,m1tcars,m3, overwrite = FALSE)

m4=m1[1:10,c(1,3)]
usethis::use_data(m4,m6,overwrite = TRUE,internal = TRUE)
