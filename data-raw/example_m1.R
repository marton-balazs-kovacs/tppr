file <- curl::curl_download("https://raw.githubusercontent.com/kekecsz/Transparent_psi_RR_materials/master/TPP_example_data_M1.zip", tempfile(fileext = ".zip"))
example_m1 <- vroom::vroom(file)

example_m1 <-
  example_m1 %>% 
  dplyr::mutate(row_counter = dplyr::row_number())
  
usethis::use_data(example_m1, overwrite = TRUE)
