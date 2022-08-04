### write elo to snowflake

### packages
source(here::here("scripts/load_packages.R"))
library(jsonlite)
library(forcats)
library(DBI)
library(odbc)
library(RODBC)
library(keyring)

### connect to DBs
# connect to snowflake
myconn <- DBI::dbConnect(odbc::odbc(),
                         "SnowflakeDSII",
                         Database = "CFB_DEMO",
                         warehouse = "DEMO_WH",
                         uid="phil.henrickson",
                         pwd=keyring::key_get("AE_Snowflake"))

# set conflict preferences
conflict_prefer("lag", "dplyr")

### functions
source(here::here("functions/get_expected_score.R"))
source(here::here("functions/get_new_elos.R"))
source(here::here("functions/calc_elo_ratings.R"))

### teams
teams_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.TEAMS')) %>%
        as_tibble() %>%
        rename(TEAM = SCHOOL,
               TEAM_ID = ID,
               TEAM_MASCOT = MASCOT,
               TEAM_ABBREVIATION = ABBREVIATION)

### games
games_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_GAMES')) %>%
        as_tibble() %>%
        mutate(CONFERENCE_GAME = case_when(CONFERENCE_GAME == 'true' ~ T,
                                           CONFERENCE_GAME == 'false' ~ F)) %>%
        mutate(GAME_DATE = as.Date(START_DATE)) %>%
        mutate(START_DATE = as_datetime(START_DATE)) %>%
        arrange(START_DATE) %>%
        group_by(SEASON, SEASON_TYPE, HOME_CONFERENCE) %>%
        mutate(LAST_GAME = START_DATE == max(START_DATE)) %>% 
        ungroup() %>%
        # logic for flagging conference championship games
        mutate(CONFERENCE_CHAMPIONSHIP = case_when(CONFERENCE_GAME == T & 
                                                           (HOME_CONFERENCE != 'FBS Independents') & 
                                                           (HOME_CONFERENCE != 'FCS Independents') &
                                                           !(HOME_CONFERENCE %in% 'Big Ten' & SEASON < 2014) &
                                                           !(HOME_CONFERENCE %in% 'Western Athletic') &
                                                           (NEUTRAL_SITE == T | 
                                                                    is.na(VENUE) |
                                                                    VENUE == 'Glass Bowl' |
                                                                    VENUE == 'Bright House Networks Stadium' |
                                                                    VENUE == 'Alamodome' |
                                                                    VENUE == 'Georgia Dome' |
                                                                    HOME_CONFERENCE == 'Sun Belt' |
                                                                    HOME_CONFERENCE == 'American Athletic') &
                                                           SEASON > 2000 &
                                                           SEASON_TYPE == 'regular' &
                                                           WEEK > 13 &
                                                           LAST_GAME == T ~ T,
                                                   TRUE ~ F)) %>%
        mutate(HOME_WIN = factor(case_when(HOME_POINTS > AWAY_POINTS ~ 'yes',
                                           HOME_POINTS < AWAY_POINTS ~ 'no',
                                           HOME_POINTS == AWAY_POINTS ~ 'no'),
                                 levels = c("no", "yes"))) %>%
        mutate(HOME_DIVISION  = replace_na(HOME_DIVISION, "missing"),
               AWAY_DIVISION = replace_na(AWAY_DIVISION, "missing")) %>%
        left_join(., teams_raw %>%
                          select(TEAM, TEAM_ABBREVIATION) %>%
                          rename(HOME_TEAM = TEAM,
                                 HOME_TEAM_ABBR = TEAM_ABBREVIATION),
                  by = c("HOME_TEAM")) %>%
        left_join(., teams_raw %>%
                          select(TEAM, TEAM_ABBREVIATION) %>%
                          rename(AWAY_TEAM = TEAM,
                                 AWAY_TEAM_ABBR = TEAM_ABBREVIATION),
                  by = c("AWAY_TEAM"))


## define games for elo function
games = games_raw %>%
        mutate(HOME_DIVISION = case_when(is.na(HOME_DIVISION) & SEASON < 1900 ~ 'fbs',
                                         TRUE ~ HOME_DIVISION)) %>%
        mutate(AWAY_DIVISION = case_when(is.na(AWAY_DIVISION) & SEASON < 1900 ~ 'fbs',
                                         TRUE ~ AWAY_DIVISION)) %>%
        mutate(FBS = HOME_DIVISION == 'fbs' | AWAY_DIVISION == 'fbs') %>%
        filter((FBS == T) | (FBS==F & SEASON < 1981)) %>%
        arrange(GAME_DATE) %>%
        select(GAME_ID, 
               SEASON, 
               WEEK,
               SEASON_TYPE,
               START_DATE,
               GAME_DATE,
               NEUTRAL_SITE, 
               HOME_TEAM,
               AWAY_TEAM,
               CONFERENCE_GAME,
               CONFERENCE_CHAMPIONSHIP,
               HOME_CONFERENCE,
               AWAY_CONFERENCE,
               HOME_DIVISION,
               AWAY_DIVISION,
               HOME_POINTS,
               AWAY_POINTS) 

# set pars
selected_pars = tibble(k = 35,
                       v = 400,
                       reversion = 0.2,
                       home_field_advantage = 75)

# now get the elo ratings up to present
elo_games= calc_elo_ratings(games %>%
                                    filter(!is.na(HOME_POINTS) | !is.na(AWAY_POINTS)),
                            home_field_advantage = selected_pars$home_field_advantage,
                            reversion = selected_pars$reversion,
                            k = selected_pars$k,
                            v = selected_pars$v,
                            verbose = T)

# game
game_elo_ratings = elo_games$game_outcomes

# team
team_elo_ratings = 
        elo_games$team_outcomes %>%
        left_join(., game_elo_ratings %>%
                          select(GAME_ID, HOME_TEAM, AWAY_TEAM,
                                 HOME_PROB, AWAY_PROB,
                                 HOME_PREGAME_ELO, HOME_POSTGAME_ELO,
                                 AWAY_PREGAME_ELO, AWAY_POSTGAME_ELO),
                  by = c("GAME_ID")) %>%
        mutate(HOME = HOME_TEAM == TEAM) %>%
        mutate(WIN_PROB = case_when(TEAM == HOME_TEAM ~ HOME_PROB,
                                                  TEAM == AWAY_TEAM ~ AWAY_PROB)) %>%
        mutate(OPPONENT_PREGAME_ELO = case_when(HOME_TEAM == TEAM ~ AWAY_PREGAME_ELO,
                                                HOME_TEAM == OPPONENT ~ HOME_PREGAME_ELO),
               OPPONENT_POSTGAME_ELO = case_when(HOME_TEAM == TEAM ~ AWAY_POSTGAME_ELO,
                                                 HOME_TEAM == OPPONENT ~ HOME_POSTGAME_ELO)) %>%
        select(GAME_ID,
               SEASON,
               SEASON_TYPE,
               WEEK,
               GAME_DATE,
               HOME,
               TEAM,
               CONFERENCE,
               DIVISION,
               POINTS,
               MARGIN, 
               OUTCOME,
               WIN_PROB,
               PREGAME_ELO,
               POSTGAME_ELO,
               OPPONENT,
               OPPONENT_POINTS,
               OPPONENT_PREGAME_ELO,
               OPPONENT_POSTGAME_ELO)

### write to snowflake
# write elo game outcomes
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.ELO_GAME_OUTCOMES"),
                  value = game_elo_ratings,
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

# write elo team outcomes
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.ELO_TEAM_OUTCOMES"),
                  value = team_elo_ratings %>%
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

rm(list=ls())
gc()
