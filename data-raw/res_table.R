## code to prepare `res_table` dataset goes here

tabla <- readr::read_csv("data-raw/tabla.csv", 
                         locale = readr::locale(encoding = "UTF-8"), readr::cols(
                           Nutriente = readr::col_character(),
                           Concentración = readr::col_character(),
                           Dosis = readr::col_character(),
                           `Total de Nutriente` = readr::col_number(),
                           `Volumen (ml)` = readr::col_number()
                           ), col_names = TRUE)


usethis::use_data(tabla, overwrite = TRUE)
