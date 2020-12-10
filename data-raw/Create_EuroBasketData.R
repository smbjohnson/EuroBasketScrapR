## code to prepare `DATASET` dataset goes here

EuroBasketData <- read.table("C:\\Users\\Samuel Johnson\\OneDrive\\College\\Fall 2020\\Sports\\european_df.csv", header = TRUE, sep = ",")

usethis::use_data(EuroBasketData, overwrite = TRUE, compress = "xz")
