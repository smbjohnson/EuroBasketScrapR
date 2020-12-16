## code to prepare `DATASET` dataset goes here

library(maps)

CountryNames <- map("world", plot = FALSE)$names

usethis::use_data(CountryNames, overwrite = TRUE, compress = "xz")
