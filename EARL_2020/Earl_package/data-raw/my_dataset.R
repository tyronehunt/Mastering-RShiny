## code to prepare `my_dataset` dataset goes here

# my_dataset <- mtcars[1:10, 1:6]
# usethis::use_data(my_dataset, overwrite = TRUE)

my_dataset <- readr::read_csv("https://raw.githubusercontent.com/ColinFay/earlworkshop2020/main/data/dataset.csv")
usethis::use_data(my_dataset, overwrite = TRUE)
