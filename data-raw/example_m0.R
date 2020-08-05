example_m0 <- vroom::vroom("https://raw.githubusercontent.com/kekecsz/Transparent_psi_RR_materials/master/TPP_example_data_M0.csv")

example_m0 <-
  example_m0 %>% 
  dplyr::mutate(row_counter = dplyr::row_number())

usethis::use_data(example_m0, overwrite = TRUE)
