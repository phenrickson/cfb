# what: load cfb data to gcp

# load packages
library(tidyverse)
library(purrr)
library(dplyr)
library(data.table)
library(magrittr)
library(odbc)
library(bigrquery)
library(DBI)
library(keyring)

# cfb packages
library(cfbfastR)

# function to take cfbfastR type and timestamp into features
cfb_attributes = function(x) {
        
        # return nothing if empty
        if(length(x) == 0) {out = data.frame()}
        else {
        out = x %>%
                        mutate(cfbfastR_timestamp = attributes(.) %$%
                                       cfbfastR_timestamp,
                               cfbfastR_type = attributes(.) %$% 
                                       cfbfastR_type) %>%
                        as_tibble
        }
        return(out)
}

# seeasons to search
historical_seasons = 1869:cfbfastR:::most_recent_cfb_season()

# get seasons
recent_seasons <- 2000:cfbfastR:::most_recent_cfb_season()

### active tables

# conferences
conferences = cfbd_conferences() %>%
        cfb_attributes()

# teams
teams = cfbd_team_info() %>%
        cfb_attributes()

# coaches
coaches = cfbd_coaches() %>%
        mutate(srs = as.numeric(srs),
               sp_overall = as.numeric(sp_overall),
               sp_offense = as.numeric(sp_offense),
               sp_defense = as.numeric(sp_defense)) %>%
        # attributes to features
        cfb_attributes()

### historical season tables

# get calendar for each season
calendars = map(historical_seasons, ~ cfbd_calendar(year = .) %>%
                        cfb_attributes()) %>%
        rbindlist %>%
        as_tibble %>%
        mutate(season = as.integer(season))

# get games for each season
games = map(historical_seasons, ~ cfbd_game_info(year = ., 
                                                 season_type = "both") %>%
                    cfb_attributes()) %>%
        rbindlist %>%
        as_tibble %>%
        mutate(start_date = as_datetime(start_date),
               home_post_win_prob = as.numeric(home_post_win_prob),
               away_post_win_prob = as.numeric(away_post_win_prob),
               excitement_index = as.numeric(excitement_index)) %>%
        mutate(home_post_win_prob = round(home_post_win_prob, 4),
               away_post_win_prob = round(away_post_win_prob, 4))

# team_rankings
# goes back to 1936
team_rankings = map(historical_seasons[historical_seasons > 1935], 
                    ~ cfbd_rankings(year = .) %>%
                            cfb_attributes()) %>%
        rbindlist %>%
        as_tibble

### recent season tables
# drives
drives = map(recent_seasons, ~ cfbd_drives(year = ., 
                                                 season_type = "both") %>%
                     cfb_attributes() %>%
                     mutate(season = .x)) %>%
        rbindlist(., fill = T) %>%
        as_tibble %>%
        mutate(drive_id = as.numeric(drive_id)) %>%
        select(-elapsed.hours)

# play types
play_types = cfbd_play_types() %>%
        cfb_attributes()

# plays
# available starting in 2001
plays = map(recent_seasons[recent_seasons > 2000], ~ cfbd_pbp_data(year = ., 
                                          season_type = "both") %>%
                    cfb_attributes() %>%
                    mutate(season = .x)) %>%
        rbindlist(use.names = T) %>%
        as_tibble %>%
        mutate(id_play = as.numeric(id_play),
               ppa = as.numeric(ppa)) %>%
        select(season, everything()) %>%
        rename(clock_minutes = clock.minutes,
               clock_seconds = clock.seconds)

# # player stats for player
# # only available after 2013
# # # need to enter game ids to retrieve the play by play data
# game_ids_for_play_stats = games %>%
#         filter(home_division == 'fbs' | away_division == 'fbs') %>%
#         filter(season == 2014) %>%
#         pull(game_id) %>%
#         unique()
# 
# # now get player stats at the play level
# play_stats_player  = map(game_ids_for_play_stats, ~ cfbd_play_stats_player(game_id = .)) %>%
#         rbindlist(use.names = T) %>%
#         as_tibble

# player usage
player_usage = map(recent_seasons[recent_seasons > 2013], ~ cfbd_player_usage(year = .) %>%
                           cfb_attributes()) %>%
        rbindlist(use.names = T) %>%
        as_tibble %>%
        mutate(athlete_id = as.numeric(athlete_id))

# players returning
player_returning = map(recent_seasons[recent_seasons > 2013], ~ cfbd_player_returning(year = .) %>%
                               cfb_attributes()) %>%
        rbindlist(use.names = T) %>%
        as_tibble

### recruiting
# recruiting team
recruiting_team = map(recent_seasons, ~ cfbd_recruiting_team(year = .) %>%
                              cfb_attributes()) %>%
        rbindlist %>%
        as_tibble %>%
        mutate(points = as.numeric(points))

# transfer portal
# only looks to be available starting in 2020
recruiting_transfer_portal = map(recent_seasons[recent_seasons > 2019],
                                 ~ cfbd_recruiting_transfer_portal(year = .) %>% 
                                         cfb_attributes()) %>%
        rbindlist %>%
        as_tibble

# recruiting position groups
recruiting_positions = map(recent_seasons, ~ cfbd_recruiting_position(start_year = .,
                                                                      end_year = .)  %>% 
                                   cfb_attributes() %>%
                                   mutate(season = .x)) %>%
        rbindlist %>%
        as_tibble %>%
        select(season, everything())

# recruiting players
recruiting_players = map(recent_seasons[recent_seasons > 2000], 
                         ~ cfbd_recruiting_player(year = .) %>%
                                 cfb_attributes()) %>%
        rbindlist %>%
        as_tibble %>%
        mutate(id = as.numeric(id),
               athlete_id = as.numeric(id))

### load to gcp
# get project credentials
PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "test-bucket"

# auth to bq

# establish connection
bigquerycon<-dbConnect(
        bigrquery::bigquery(),
        project = PROJECT_ID,
        dataset = "cfb"
)

## initial load
# teams
dbWriteTable(bigquerycon,
             name = "teams",
             overwrite = T,
             value = teams)

# conferences
dbWriteTable(bigquerycon,
             name = "conferences",
             overwrite = T,
             value = conferences)

# calendars
dbWriteTable(bigquerycon,
             name = "calendars",
             overwrite = T,
             value = calendars)

# games
dbWriteTable(bigquerycon,
             name = "games",
             overwrite = T,
             value = games)

# drives
dbWriteTable(bigquerycon,
             name = "drives",
             overwrite = T,
             value = drives)

# play_types
dbWriteTable(bigquerycon,
             name = "play_types",
             overwrite = T,
             value = play_types)

# plays
dbWriteTable(bigquerycon,
             name = "plays",
             overwrite = T,
             value = plays)

# player usage
dbWriteTable(bigquerycon,
             name = "player_usage",
             overwrite = T,
             value = player_usage)

# player returning
dbWriteTable(bigquerycon,
             name = "player_returning",
             overwrite = T,
             value = player_returning)

# recruiting by team
dbWriteTable(bigquerycon,
             name = "recruiting_team",
             overwrite = T,
             value = recruiting_team)

# recruiting by team
dbWriteTable(bigquerycon,
             name = "recruiting_transfer_portal",
             overwrite = T,
             value = recruiting_transfer_portal)

# transfer portal
dbWriteTable(bigquerycon,
             name = "recruiting_positions",
             overwrite = T,
             value = recruiting_positions)

# recruitw
dbWriteTable(bigquerycon,
             name = "recruiting_players",
             overwrite = T,
             value = recruiting_players)
