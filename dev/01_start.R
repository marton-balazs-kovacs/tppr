golem::fill_desc(
  pkg_name = "tppr",
  pkg_title = "Shiny app to show real-time results of the TPP project",
  pkg_description = "The package contains a shiny app that shows the real-time results of the TPP project.",
  author_first_name = "Marton",
  author_last_name = "Kovacs",
  author_email = "marton.balazs.kovacs@gmail.com",
  repo_url = NULL
)     
golem::set_golem_options()
usethis::use_mit_license( name = "Golem User" )
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )
usethis::use_git()
usethis::use_github(private = TRUE)
golem::use_recommended_tests()
golem::use_recommended_deps()
golem::remove_favicon()
golem::use_favicon()
golem::use_utils_ui()
golem::use_utils_server()
rstudioapi::navigateToFile( "dev/02_dev.R" )
