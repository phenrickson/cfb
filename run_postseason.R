rmarkdown::render(here::here("make_postseason_matchups.Rmd"),
                  params = list(input_season = input_season,
                                nsims = 1000),
                  output_file =  "postseason_matchups",
                  output_dir = here::here("simulations", "postseason", 2022))