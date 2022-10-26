# load necessary pieces
source(here::here("scripts/load_packages.R"))

# load teams
load(here::here("data", "season_conference_divisions.Rdata"))

# function
run_weekly_sims = function(input_season,
                           input_week) {
        # run through
        foreach(i=1:length(input_week)) %do% {
                
                rmarkdown::render(here::here("run_season_simulations.Rmd"),
                                  params = list(sim_week = input_week[i],
                                                input_season = input_season,
                                                nsims = 1000),
                                  output_file =  paste("team_predictions_week", input_week[i], input_season, sep="_"),
                                  output_dir = here::here("simulations", "season", input_season))
        }
}

# function
run_weekly_report = function(input_season,
                           input_week) {
        # run through
        foreach(i=1:length(input_week)) %do% {
                
                rmarkdown::render(here::here("make_season_simulations_report.Rmd"),
                                  params = list(sim_week = input_week[i],
                                                input_season = input_season,
                                                nsims = 1000),
                                  output_file =  paste("team_predictions_week", input_week[i], input_season, sep="_"),
                                  output_dir = here::here("simulations", "season", input_season))
        }
        
}

# function
run_team_profiles= function(input_team,
                            input_season,
                             input_week) {
        # run through
        foreach(i=1:length(input_team)) %do% {
                
                rmarkdown::render(here::here("make_team_profiles.Rmd"),
                                  params = list(team = input_team[i],
                                                sim_week = input_week,
                                                input_season = input_season,
                                                nsims = 1000),
                                  output_file =  input_team[i],
                                  output_dir = here::here("team_profiles", "season", input_season))
        }
}

# function
run_weekly_matchups = function(input_season,
                              input_week) {
        # run through
        foreach(i=1:length(input_week)) %do% {
                
                rmarkdown::render(here::here("make_season_weekly_matchups.Rmd"),
                                  params = list(sim_week = input_week[i],
                                                input_season = input_season,
                                                nsims = 1000),
                                  output_file =  paste("matchups", input_week[i], input_season, sep="_"),
                                  output_dir = here::here("simulations", "season", input_season))
        }
}

# set week
weeks = 8
year = 2022

# set teams
teams = season_conference_divisions %>%
        filter(SEASON == year) %>%
       # filter(CONFERENCE == 'ACC') %>%
     #   filter(TEAM %in% c("Virginia", "Virginia Tech", "Wake Forest")) %>%
        #filter(TEAM == 'Duke') %>%
        # filter(CONFERENCE %in% c("American Athletic",
        #                     "Mountain West",
        #                     "FBS Independents")) %>%
        select(TEAM) %>%
        distinct() %>%
        pull(TEAM)

# run sims
run_weekly_sims(input_season = year,
                input_week = weeks)

# make report for predictions
run_weekly_report(input_season = year,
                  input_week = weeks)

# make matchups report
run_weekly_matchups(input_season = year,
                  input_week = weeks)

# make team profiles
run_team_profiles(input_team = teams,
                  input_season = year,
                  input_week = weeks)
