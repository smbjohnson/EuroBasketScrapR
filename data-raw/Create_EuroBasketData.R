## code to prepare `DATASET` dataset goes here

EuroBasketData <- read.table("data-raw\\european_df.csv", header = TRUE, sep = ",")

usethis::use_data(EuroBasketData, overwrite = TRUE, compress = "xz")
