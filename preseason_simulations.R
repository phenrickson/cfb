### packages
source(here::here("scripts/load_packages.R"))
library(TTR)
library(jsonlite)
library(forcats)
library(DBI)
library(odbc)
library(RODBC)
library(keyring)

### connect to DBS
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
source(here::here("functions", "simulateX.R"))
source(here::here("functions", "sim_game_margin.R"))
source(here::here("functions", "calc_elo_ratings.R"))
source(here::here("functions", "sim_elo_ratings.R"))
source(here::here("functions", "sim_rest_of_season.R"))

# color palettes
load(here::here("data", "team_colors.Rdata"))
custom_color_teams <- scale_colour_manual(name = "TEAM", values = team_colors)
custom_fill_teams <- scale_fill_manual(name = "TEAM", values = team_colors)

### get data sources
# teams raw
teams_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.TEAMS')) %>%
        as_tibble() %>%
        rename(TEAM = SCHOOL,
               TEAM_ID = ID,
               TEAM_MASCOT = MASCOT,
               TEAM_ABBREVIATION = ABBREVIATION)

# get games
games_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_GAMES')) %>%
        as_tibble() %>%
        mutate(CONFERENCE_GAME = case_when(CONFERENCE_GAME == 'true' ~ T,
                                           CONFERENCE_GAME == 'false' ~ F)) %>%
        mutate(GAME_DATE = as.Date(START_DATE)) %>%
        mutate(START_DATE = as_datetime(START_DATE)) %>%
        arrange(START_DATE) %>%
        # logic for flagging conference championship games
        group_by(SEASON, SEASON_TYPE, HOME_CONFERENCE) %>%
        mutate(LAST_GAME = START_DATE == max(START_DATE)) %>% 
        ungroup() %>%
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

# get team conference mapping
team_conference_season = DBI::dbGetQuery(myconn,
                                         paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_TEAM_CONFERENCE_SEASON')) %>%
        as_tibble()

# team mapping function
team_mapping = teams_raw %>%
        mutate(TEAM_ABBR = gsub(" ", "\\.", gsub("[[:punct:]]", "\\.", TEAM))) %>%
        select(TEAM, TEAM_ABBR, TEAM_ABBREVIATION, COLOR, ALT_COLOR) %>%
        mutate(COLOR = case_when(TEAM == 'Toledo' ~ '#003E7E',
                                 TRUE ~ COLOR))

### get historical elo
# games
elo_games = DBI::dbGetQuery(myconn,
                                       paste('SELECT * FROM CFB_DEMO.CFD_ANALYTICS.ACTIVE_ELO_GAMES')) %>%
        as_tibble()

# teams
elo_teams = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_ANALYTICS.ACTIVE_ELO_TEAMS')) %>%
        as_tibble() %>%
        select(-LOAD_DATE)

### recruiting 
recruiting_teams_raw = DBI::dbGetQuery(myconn,
                                       paste('SELECT * FROM CFB_DEMO.CFD_RAW.RECRUITING_TEAMS ')) %>%
        as_tibble() %>%
        mutate(POINTS = as.numeric(POINTS)) %>%
        rename(RECRUITING_POINTS = POINTS)

# expand
recruiting_teams = expand.grid(TEAM = unique(recruiting_teams_raw$TEAM),
                               YEAR = seq(min(recruiting_teams_raw$YEAR),
                                          max(recruiting_teams_raw$YEAR))) %>%
        left_join(., recruiting_teams_raw,
                  by = c("YEAR", "TEAM")) %>%
        mutate(RECRUITING_POINTS = replace_na(RECRUITING_POINTS, 0)) %>%
        filter(YEAR > 2001) %>%
        left_join(., team_conference_season,
                  by = c("YEAR"="SEASON", "TEAM")) %>%
        filter(DIVISION == 'fbs') %>%
        group_by(TEAM) %>%
        mutate(n = n()) %>%
        filter(n > 4) %>%
        group_by(TEAM) %>%
        select(-n) %>%
        mutate(EMA = EMA(RECRUITING_POINTS, n =4)) %>%
        # now fill with actual in the event that a team doesn't have yet have enough data
        mutate(EMA = case_when(is.na(EMA) & !is.na(RECRUITING_POINTS) ~ RECRUITING_POINTS,
                               TRUE ~ EMA)) %>%
        # mutate_at(c("EMA", "WMA"),
        #           replace_na, 0) %>%
        arrange(TEAM, YEAR)  %>%
        mutate(SEASON = YEAR) %>%
        ungroup() %>%
        select(TEAM, SEASON, CONFERENCE, DIVISION, RANK, RECRUITING_POINTS, EMA)

# get what we need
recruiting = recruiting_teams %>%
        select(TEAM, SEASON, CONFERENCE, RECRUITING_POINTS, EMA) %>%
        rename(RECRUITING_SCORE = EMA)


### efficiency
# season level efficiency rankings
season_team_rankings = DBI::dbGetQuery(myconn,
                paste('SELECT * FROM CFB_DEMO.CFD_ANALYTICS.EFFICIENCY_TEAM_SEASON')) %>%
        as_tibble()

### combine datasets to get games
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
               AWAY_POINTS) %>%
        left_join(.,
                  recruiting %>%
                          select(SEASON,
                                 TEAM,
                                 RECRUITING_SCORE) %>%
                          rename(HOME_TEAM = TEAM,
                                 HOME_RECRUITING_SCORE = RECRUITING_SCORE),
                  by = c("SEASON",
                         "HOME_TEAM")) %>%
        left_join(.,
                  recruiting %>%
                          select(SEASON,
                                 TEAM,
                                 RECRUITING_SCORE) %>%
                          rename(AWAY_TEAM = TEAM,
                                 AWAY_RECRUITING_SCORE = RECRUITING_SCORE),
                  by = c("SEASON",
                         "AWAY_TEAM"))

## functions for converting elo to list from specified year
teams_to_list = function(x) {
        
        setNames(unlist(x$ELO) %>%
                         split(., seq(nrow(x))),
                 rownames(x))
}

team_seasons_to_list = function(x) {
        
        setNames(unlist(x$SEASON) %>%
                         split(., seq(nrow(x))),
                 rownames(x))
}

# 2022 season 
future_seasons = expand.grid(TEAM = season_team_rankings  %>%
                                     select(TEAM) %>%
                                     distinct(TEAM) %>%
                                     pull(),
                             TYPE = c('raw', 'adjusted'),
                             SEASON = 2022) %>%
        as_tibble()

# get efficiency and elo for historical + future season
teams_data = season_team_rankings %>%
        bind_rows(., future_seasons) %>%
        filter(TYPE == 'adjusted') %>%
        select(-TYPE) %>%
        # add in season end elo ratings
        left_join(.,
                  elo_teams %>%
                          group_by(SEASON, TEAM) %>%
                          arrange(GAME_DATE) %>%
                          mutate(WEEK = row_number()) %>%
                          mutate(LAST_GAME = GAME_DATE == max(GAME_DATE)) %>%
                          filter(LAST_GAME ==T) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          rename(ELO = POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        # add in season recruiting data
        left_join(., 
                  recruiting %>%
                          select(TEAM, SEASON, RECRUITING_SCORE),
                  by = c("SEASON", "TEAM")) %>%
        arrange(SEASON, TEAM) %>%
        group_by(TEAM) %>%
        mutate(PREVIOUS_OVERALL = lag(OVERALL, 1),
               PREVIOUS_OFFENSE = lag(OFFENSE, 1),
               PREVIOUS_DEFENSE = lag(DEFENSE, 1),
               PREVIOUS_ELO = lag(ELO, 1)) %>%
        ungroup() %>%
        filter(SEASON > 2007)

# select season
input_season = 2022

# split into training and test
teams_train  = teams_data %>%
        filter(SEASON < input_season)

teams_test = teams_data %>%
        filter(SEASON == input_season)

# create recipe
teams_recipe =      
        recipe(ELO ~.,
               data = teams_train) %>%
        update_role(all_predictors(),
                    new_role = "id") %>%
        update_role(PREVIOUS_OFFENSE,
                    PREVIOUS_DEFENSE,
                    PREVIOUS_ELO,
                    RECRUITING_SCORE,
                    new_role = 'predictor') %>%
        # manual missingness
        step_mutate(RECRUITING_SCORE = replace_na(RECRUITING_SCORE, 0),
                    PREVIOUS_ELO = replace_na(PREVIOUS_ELO, 1200),
                    PREVIOUS_OFFENSE = replace_na(PREVIOUS_OFFENSE,-0.1),
                    PREVIOUS_DEFENSE = replace_na(PREVIOUS_DEFENSE, -0.1)) %>%
        step_normalize(all_predictors())

# linear model
lm_mod = linear_reg(mode = "regression",
                    engine = "lm")

# create workflow
teams_workflow = workflow() %>%
        add_recipe(teams_recipe) %>%
        add_model(lm_mod)

# run workflow
adjusted_teams_test = 
        teams_workflow %>%
        fit(bind_rows(teams_train)) %>%
        augment(bind_rows(teams_test %>%
                                  mutate(dataset = 'test'))) %>%
        select(SEASON, dataset, TEAM, starts_with("PREVIOUS"),
               ELO, .pred) %>%
        mutate_if(is.numeric, round, 3)

# for parallelization
library(future.apply)
plan(multisession, workers = 7)

# set season to simulate
# set initial games
initial_games = games %>%
        filter(SEASON < input_season)

# elo pars
elo_pars = tibble(k = 35,
                  v = 400,
                  reversion = 0,
                  home_field_advantage = 75,
                  recruiting_weight =0)

# train points model
points_model = elo_games %>%
        filter(SEASON >=1970) %>% 
        filter(SEASON < input_season) %>%
        mutate(HOME_ELO_DIFF = case_when(NEUTRAL_SITE == F ~ 
                                                 HOME_PREGAME_ELO + 
                                                 elo_pars$home_field_advantage -
                                                 AWAY_PREGAME_ELO,
                                         TRUE ~ HOME_PREGAME_ELO - AWAY_PREGAME_ELO)) %>%
        mutate(HOME_SCORE_DIFF = HOME_POINTS-AWAY_POINTS) %>%
        nest() %>%
        # now fit linear models, i'll do so using stan
        mutate(lm = map(data, ~ lm(HOME_SCORE_DIFF ~ HOME_ELO_DIFF +0,
                                   data = .x))) %>%
        pluck("lm",1)

# base elo, from historical games
base_teams_elo = elo_teams %>%
        filter(SEASON < input_season) %>%
        group_by(TEAM) %>%
        filter(GAME_DATE == max(GAME_DATE)) %>%
        slice_max(POSTGAME_ELO, n=1, with_ties = F) %>%
        ungroup() %>%
        select(TEAM, POSTGAME_ELO) %>%
        rename(ELO = POSTGAME_ELO) %>%
        column_to_rownames("TEAM")

# now adjusted
# get computed elo as input
adjusted_teams_elo = adjusted_teams_elo %>%
        filter(SEASON == input_season) %>%
        select(TEAM, .pred) %>%
        rename(ELO = .pred) %>%
        column_to_rownames("TEAM")


# get team seasons
team_elo_seasons = elo_teams %>%
        filter(SEASON < input_season) %>%
        group_by(TEAM) %>%
        slice_max(., order_by = SEASON, n=1, with_ties =F) %>%
        ungroup() %>%
        select(TEAM, SEASON) %>%
        column_to_rownames("TEAM")

# convert to list
base_teams = teams_to_list(base_teams_elo)
adjusted_teams = teams_to_list(adjusted_teams_elo)
team_seasons = team_seasons_to_list(team_elo_seasons)

# in format for function
elo_input = list(teams = c(adjusted_teams,
                           base_teams[!(names(base_teams) %in% names(adjusted_teams))]),
                   team_seasons = team_seasons)


# simulate season from the start
set.seed(1999)
season_simulations = sim_rest_of_season(games,
                                                 season = input_season,
                                                 elo_initial = elo_input,
                                                 elo_pars = elo_pars,
                                                 nsims = 2022,
                                                 sim_start_week = 0,
                                                 season_start_only = T,
                                                 aggregate = F,
                                                 points_model = points_model)

season_simulations %$%
        team_outcomes %>% 
        group_by(SEASON, TEAM) %>% 
        filter(GAME_DATE == max(GAME_DATE)) %>% 
        summarize(ELO = mean(POSTGAME_ELO)) %>%
        arrange(desc(ELO))
