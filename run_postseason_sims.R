# sim postseason games 

# input settings ----------------------------------------------------------

temp_params = list()
temp_params$input_season = 2022
temp_params$nsims = 1000



# packages ----------------------------------------------------------------


source(here::here("scripts/load_packages.R"))
library(jsonlite)
library(forcats)
library(TTR)
library(flextable)
conflict_prefer("lag", "dplyr")

# parallelization
library(future.apply)
library(furrr)
plan(multisession, workers = 7)

library(DBI)
library(odbc)
library(RODBC)
library(keyring)
library(cfbfastR)


# functions ---------------------------------------------------------------

### functions
source(here::here("functions/get_expected_score.R"))
source(here::here("functions/get_new_elos.R"))
source(here::here("functions/calc_elo_ratings.R"))
source(here::here("functions", "simulateX.R"))
source(here::here("functions", "sim_game_margin.R"))
source(here::here("functions", "calc_elo_ratings.R"))
source(here::here("functions", "sim_elo_ratings.R"))
source(here::here("functions", "sim_rest_of_season.R"))
source(here::here("functions", "theme_phil.R"))
source(here::here("functions", "span_func.R"))
source(here::here("functions", "get_conference_championships.R"))
source(here::here("functions", "get_playoffs.R"))

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


# color palettes
load(here::here("data", "team_colors.Rdata"))
custom_color_teams <- scale_colour_manual(name = "TEAM", values = team_colors)
custom_fill_teams <- scale_fill_manual(name = "TEAM", values = team_colors)



# data --------------------------------------------------------------------


### connect to DBS
# connect to snowflake
myconn <- DBI::dbConnect(odbc::odbc(),
                         "SnowflakeDSII",
                         Database = "CFB_DEMO",
                         warehouse = "DEMO_WH",
                         uid="phil.henrickson",
                         pwd=keyring::key_get("AE_Snowflake"))

### get data sources
# teams raw
teams_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.TEAMS')) %>%
        as_tibble() %>%
        rename(TEAM = SCHOOL,
               TEAM_ID = ID,
               TEAM_MASCOT = MASCOT,
               TEAM_ABBREVIATION = ABBREVIATION)

# game notes
game_notes = DBI::dbGetQuery(myconn,
                             paste("SELECT SEASON, ID, HOME_TEAM, AWAY_TEAM, NOTES FROM CFB_DEMO.CFD_RAW.GAMES 
                            WHERE HOME_DIVISION = 'fbs' AND AWAY_DIVISION = 'fbs'
                            ")) %>%
        as_tibble() %>%
        filter(SEASON == temp_params$input_season) %>%
        mutate(NOTES = as.character(NOTES)) %>%
        filter(grepl("Championship", NOTES)) %>%
        filter(!grepl("FCS|Division II|Division III", NOTES))

# get games
games_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_GAMES')) %>%
        as_tibble() %>%
        mutate(CONFERENCE_GAME = case_when(CONFERENCE_GAME == 'true' ~ T,
                                           CONFERENCE_GAME == 'false' ~ F)) %>%
        mutate(GAME_DATE = as.Date(START_DATE)) %>%
        mutate(START_DATE = as_datetime(START_DATE)) %>%
        arrange(START_DATE) %>%
        # # get first game
        # group_by(GAME_ID) %>%
        # slice_tail(n =1) %>%
        # logic for flagging conference championship games
        group_by(SEASON, SEASON_TYPE, HOME_CONFERENCE) %>%
        mutate(LAST_GAME = START_DATE == max(START_DATE)) %>% 
        ungroup() %>%
        left_join(., 
                  game_notes %>%
                          transmute(SEASON,
                                    GAME_ID = ID,
                                    CONFERENCE_CHAMPIONSHIP = T),
                  by = c("SEASON", "GAME_ID")) %>%
        mutate(CONFERENCE_CHAMPIONSHIP = case_when(CONFERENCE_CHAMPIONSHIP == T ~ T,
                                                   TRUE ~ F)) %>%
        # mutate(CONFERENCE_CHAMPIONSHIP = case_when(CONFERENCE_GAME == T & 
        #                                                    (HOME_CONFERENCE != 'FBS Independents') & 
        #                                                    (HOME_CONFERENCE != 'FCS Independents') &
        #                                                    !(HOME_CONFERENCE %in% 'Big Ten' & SEASON < 2014) &
        #                                                    !(HOME_CONFERENCE %in% 'Western Athletic') &
        #                                                    (NEUTRAL_SITE == T | 
        #                                                             is.na(VENUE) |
        #                                                             VENUE == 'Glass Bowl' |
        #                                                             VENUE == 'Bright House Networks Stadium' |
        #                                                             VENUE == 'Alamodome' |
        #                                                             VENUE == 'Georgia Dome' |
#                                                             HOME_CONFERENCE == 'Sun Belt' |
#                                                             HOME_CONFERENCE == 'American Athletic') &
#                                                    SEASON > 2000 &
#                                                    SEASON_TYPE == 'regular' &
#                                                    WEEK > 13 &
#                                                    LAST_GAME == T ~ T,
#                                            TRUE ~ F)) %>%
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

# data sources
load(file=here::here("data", "season_team_rankings_type.Rdata"))

# run script to clean games
### combine datasets to get games
games = games_raw %>%
        filter(SEASON_TYPE == 'regular') %>%
        #   filter(CONFERENCE_CHAMPIONSHIP == F) %>%
        filter(SEASON == temp_params$input_season) %>%
        # get most recent
        group_by(GAME_ID) %>%
        arrange(desc(is.na(OUTCOME))) %>%
        #  arrange(desc(HOME_POINTS)) %>%
        #   arrange(desc(START_DATE)) %>%
        slice_tail(n =1) %>%
        ungroup() %>%
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
               HOME_TEAM_ABBR,
               AWAY_TEAM,
               AWAY_TEAM_ABBR,
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

# games that for some damn reason the API swapped the home and away team
problem_ids = games_raw %>%
        filter(SEASON == temp_params$input_season) %>% 
        group_by(GAME_ID) %>%
        summarize(home_teams = n_distinct(HOME_TEAM)) %>% 
        arrange(desc(home_teams)) %>%
        filter(home_teams > 1) %>%
        pull(GAME_ID)

# fix issue with 2022 games
games = games %>%
        filter(SEASON == temp_params$input_season) %>%
        filter(!(GAME_ID %in% problem_ids)) %>%
        bind_rows(.,
                  games %>%
                          filter(GAME_ID %in% problem_ids) %>%
                          select(-starts_with("AWAY_")) %>%
                          set_names(., gsub("HOME_", "AWAY_", names(.))) %>%
                          bind_cols(.,
                                    games %>%
                                            filter(GAME_ID %in% problem_ids) %>%
                                            select(starts_with("AWAY")) %>%
                                            set_names(., gsub("AWAY_", "HOME_", names(.))))) %>%
        # fix missing score on select games
        # mutate(HOME_POINTS = case_when(GAME_ID == 401415214 ~ 14,
        #                                TRUE ~ HOME_POINTS)) %>%
        # mutate(AWAY_POINTS = case_when(GAME_ID == 401415214 ~ 31,
        #                                TRUE ~ AWAY_POINTS)) %>%
        filter(GAME_ID != 401416593) %>%
        arrange(START_DATE)


#calendar = cfbd_calendar(temp_params$input_season)

# # conference championship games
# conference_championship_games = games_raw %>%
#         filter(SEASON_TYPE == 'regular') %>%
#         filter(CONFERENCE_CHAMPIONSHIP == T) %>%
#         filter(SEASON == temp_params$input_season) %>%
#         # get most recent
#         group_by(GAME_ID) %>%
#         arrange(desc(is.na(OUTCOME))) %>%
#         #  arrange(desc(HOME_POINTS)) %>%
#         #   arrange(desc(START_DATE)) %>%
#         slice_tail(n =1) %>%
#         ungroup() %>%
#         mutate(HOME_DIVISION = case_when(is.na(HOME_DIVISION) & SEASON < 1900 ~ 'fbs',
#                                          TRUE ~ HOME_DIVISION)) %>%
#         mutate(AWAY_DIVISION = case_when(is.na(AWAY_DIVISION) & SEASON < 1900 ~ 'fbs',
#                                          TRUE ~ AWAY_DIVISION)) %>%
#         mutate(FBS = HOME_DIVISION == 'fbs' | AWAY_DIVISION == 'fbs') %>%
#         filter((FBS == T) | (FBS==F & SEASON < 1981)) %>%
#         arrange(GAME_DATE) %>%
#         select(GAME_ID, 
#                SEASON, 
#                WEEK,
#                SEASON_TYPE,
#                START_DATE,
#                GAME_DATE,
#                NEUTRAL_SITE, 
#                HOME_TEAM,
#                HOME_TEAM_ABBR,
#                AWAY_TEAM,
#                AWAY_TEAM_ABBR,
#                CONFERENCE_GAME,
#                CONFERENCE_CHAMPIONSHIP,
#                HOME_CONFERENCE,
#                AWAY_CONFERENCE,
#                HOME_DIVISION,
#                AWAY_DIVISION,
#                HOME_POINTS,
#                AWAY_POINTS) %>%
#         left_join(.,
#                   recruiting %>%
#                           select(SEASON,
#                                  TEAM,
#                                  RECRUITING_SCORE) %>%
#                           rename(HOME_TEAM = TEAM,
#                                  HOME_RECRUITING_SCORE = RECRUITING_SCORE),
#                   by = c("SEASON",
#                          "HOME_TEAM")) %>%
#         left_join(.,
#                   recruiting %>%
#                           select(SEASON,
#                                  TEAM,
#                                  RECRUITING_SCORE) %>%
#                           rename(AWAY_TEAM = TEAM,
#                                  AWAY_RECRUITING_SCORE = RECRUITING_SCORE),
#                   by = c("SEASON",
#                          "AWAY_TEAM"))

# postseason games
postseason_games = cfbd_game_info(temp_params$input_season,
                                  season_type = 'postseason') %>%
        rename_all(toupper) %>%
        select(one_of(names(games_raw))) %>%
        mutate(CONFERENCE_GAME = case_when(CONFERENCE_GAME == 'true' ~ T,
                                           CONFERENCE_GAME == 'false' ~ F)) %>%
        mutate(GAME_DATE = as.Date(START_DATE)) %>%
        mutate(START_DATE = as_datetime(START_DATE)) %>%
        arrange(START_DATE)  %>%
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
                  by = c("AWAY_TEAM")) %>%
        mutate(OUTCOME = case_when(HOME_POINTS > AWAY_POINTS ~ 'home win',
                                   AWAY_POINTS > HOME_POINTS ~ 'away win')) %>%
        # get most recent
        group_by(GAME_ID) %>%
        arrange(desc(is.na(OUTCOME))) %>%
        #  arrange(desc(HOME_POINTS)) %>%
        #   arrange(desc(START_DATE)) %>%
        slice_tail(n =1) %>%
        ungroup() %>%
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
               HOME_TEAM_ABBR,
               AWAY_TEAM,
               AWAY_TEAM_ABBR,
               CONFERENCE_GAME,
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

# set up conference divisions
load(here::here("data", "season_conference_divisions.Rdata"))

# for this season
conference_divisions = season_conference_divisions %>%
        filter(SEASON == temp_params$input_season)

# season
future_seasons = expand.grid(TEAM = season_team_rankings  %>%
                                     select(TEAM) %>%
                                     distinct(TEAM) %>%
                                     pull(),
                             TYPE = c('raw', 'adjusted'),
                             SEASON = temp_params$input_season) %>%
        as_tibble()

# get efficiency and elo for historical + future season
teams_data = season_team_rankings %>%
        filter(SEASON < temp_params$input_season) %>%
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


# split into training and test
teams_train  = teams_data %>%
        filter(SEASON < temp_params$input_season)

teams_test = teams_data %>%
        filter(SEASON == temp_params$input_season)

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

# set season to simulate
# set initial games
initial_games = games %>%
        filter(SEASON < temp_params$input_season)

# elo pars
elo_pars = tibble(k = 35,
                  v = 400,
                  reversion = 0,
                  home_field_advantage = 75,
                  recruiting_weight =0)

# train points model
points_model = elo_games %>%
        filter(SEASON >=1970) %>% 
        filter(SEASON < temp_params$input_season) %>%
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
        filter(SEASON < temp_params$input_season) %>%
        group_by(TEAM) %>%
        filter(GAME_DATE == max(GAME_DATE)) %>%
        slice_max(POSTGAME_ELO, n=1, with_ties = F) %>%
        ungroup() %>%
        select(TEAM, POSTGAME_ELO) %>%
        rename(ELO = POSTGAME_ELO) %>%
        column_to_rownames("TEAM")

# now adjusted
# get computed elo as input
adjusted_teams_elo = adjusted_teams_test %>%
        filter(SEASON == temp_params$input_season) %>%
        select(TEAM, .pred) %>%
        rename(ELO = .pred) %>%
        column_to_rownames("TEAM")

# get team seasons
team_elo_seasons = elo_teams %>%
        filter(SEASON < temp_params$input_season) %>%
        group_by(TEAM) %>%
        slice_max(., order_by = SEASON, n=1, with_ties =F) %>%
        ungroup() %>%
        select(TEAM, SEASON) %>%
        column_to_rownames("TEAM")

# convert to list
base_teams = teams_to_list(base_teams_elo)
adjusted_teams = teams_to_list(adjusted_teams_elo)
team_seasons = team_seasons_to_list(team_elo_seasons)

# get elo input
elo_input = list(teams = c(adjusted_teams,
                           base_teams[!(names(base_teams) %in% names(adjusted_teams))]),
                 team_seasons = team_seasons)


# # rm
# rm(base_teams_elo,
#    adjusted_teams_elo,
#    adjusted_teams_test,
#    recruiting_teams_raw,
#    recruiting_teams,
#    recruiting,
#    initial_games,
#    elo_games,
#    elo_teams,
#    teams_test,
#    teams_train,
#    teams_recipe,
#    teams_workflow,
#    teams_raw,
#    future_seasons)


# calculate elo in run up -------------------------------------------------


# calculate elo
elo_input = calc_elo_ratings(games %>%
                                     filter(SEASON == temp_params$input_season) %>%
                                     filter(!is.na(HOME_POINTS)),
                             teams = elo_input$teams,
                             team_seasons = elo_input$teams,
                             reversion = elo_pars$reversion,
                             k = elo_pars$k,
                             v = elo_pars$v,
                             home_field_advantage = elo_pars$home_field_advantage)

# get team ratings
team_elo_so_far =  elo_input$team_outcomes %>%
        rename(RATING = POSTGAME_ELO) %>%
        select(SEASON, WEEK, TEAM, RATING) %>%
        bind_rows(.,
                  do.call(rbind,
                          c(adjusted_teams,
                            base_teams[!(names(base_teams) %in% names(adjusted_teams))])) %>% 
                          as.data.frame() %>% 
                          rownames_to_column("TEAM") %>%
                          rename(RATING = V1) %>% 
                          mutate(SEASON = temp_params$input_season) %>%
                          mutate(WEEK = 0))

# get games
games_so_far = elo_input %$%
        team_outcomes %>%
        left_join(.,
                  conference_divisions,
                  by = c("SEASON", "TEAM", "CONFERENCE")) %>%
        left_join(.,
                  conference_divisions %>%
                          rename(OPPONENT = TEAM,
                                 OPPONENT_CONFERENCE = CONFERENCE,
                                 OPPONENT_CONFERENCE_DIVISION = CONFERENCE_DIVISION),
                  by = c("SEASON", "OPPONENT")) %>%
        mutate(CONFERENCE_GAME = case_when(CONFERENCE == OPPONENT_CONFERENCE ~ T,
                                           TRUE ~ F),
               CONFERENCE_DIVISION_GAME = case_when(CONFERENCE == OPPONENT_CONFERENCE & CONFERENCE_DIVISION == OPPONENT_CONFERENCE_DIVISION ~ T,
                                                    CONFERENCE == OPPONENT_CONFERENCE & 
                                                            ((is.na(CONFERENCE_DIVISION) & is.na(OPPONENT_CONFERENCE_DIVISION))) ~ T,
                                                    TRUE ~ F)) %>%
        select(-home_field_advantage, -reversion, -k, -v)



# sim games ---------------------------------------------------------------

set.seed(52)
sim_conference_championship_games = sim_rest_of_season(games,
                                                       season = temp_params$input_season,
                                                       elo_initial = elo_input,
                                                       elo_pars = elo_pars,
                                                       nsims = temp_params$nsims,
                                                       sim_start_week = 13,
                                                       season_start_only = T,
                                                       aggregate = F,
                                                       points_model = points_model)


conference_championshiop_predictions = sim_conference_championship_games$game_outcomes %>%
        select(-CONFERENCE_CHAMPIONSHIP) %>%
        left_join(., 
                  game_notes %>%
                          transmute(SEASON,
                                    GAME_ID = ID,
                                    CONFERENCE_CHAMPIONSHIP = T),
                  by = c("SEASON", "GAME_ID")) %>%
        filter(CONFERENCE_CHAMPIONSHIP == T) %>%
        group_by(GAME_ID, SIM_FROM_WEEK) %>%
        mutate(HOME_SIM_PRED = case_when(HOME_SIM_MARGIN >0 ~ 1,
                                         TRUE ~ 0)) %>%
        summarize(HOME_PREGAME_ELO = mean(HOME_PREGAME_ELO),
                  AWAY_PREGAME_ELO = mean(AWAY_PREGAME_ELO),
                  HOME_POSTGAME_ELO = mean(HOME_POSTGAME_ELO),
                  AWAY_POSTGAME_ELO = mean(AWAY_POSTGAME_ELO),
                  home_wins = sum(HOME_SIM_PRED),
                  home_margin = median(HOME_SIM_MARGIN),
                  n = n(),
                  .groups = 'drop') %>%
        mutate(HOME_SIM_PROB = home_wins / n) %>%
        mutate(HOME_SIM_MARGIN = home_margin) %>%
        select(SIM_FROM_WEEK, GAME_ID,
               HOME_PREGAME_ELO,
               AWAY_PREGAME_ELO,
               HOME_POSTGAME_ELO,
               AWAY_POSTGAME_ELO,
               HOME_SIM_PROB,
               HOME_SIM_MARGIN)

# now sim postseason games
set.seed(52)
sim_postseason_games = sim_rest_of_season(games %>%
                                                  bind_rows(., postseason_games %>%
                                                                    mutate(WEEK = 16)),
                                          season = temp_params$input_season,
                                          elo_initial = elo_input,
                                          elo_pars = elo_pars,
                                          nsims = temp_params$nsims,
                                          sim_start_week = 15,
                                          season_start_only = T,
                                          aggregate = F,
                                          points_model = points_model)


postseason_predictions = 
        sim_postseason_games$game_outcomes %>%
        select(-CONFERENCE_CHAMPIONSHIP) %>%
        left_join(., 
                  game_notes %>%
                          transmute(SEASON,
                                    GAME_ID = ID,
                                    CONFERENCE_CHAMPIONSHIP = T),
                  by = c("SEASON", "GAME_ID")) %>%
        filter(SEASON_TYPE == 'postseason') %>%
        group_by(GAME_ID, SIM_FROM_WEEK) %>%
        mutate(HOME_SIM_PRED = case_when(HOME_SIM_MARGIN >0 ~ 1,
                                         TRUE ~ 0)) %>%
        summarize(HOME_PREGAME_ELO = mean(HOME_PREGAME_ELO),
                  AWAY_PREGAME_ELO = mean(AWAY_PREGAME_ELO),
                  HOME_POSTGAME_ELO = mean(HOME_POSTGAME_ELO),
                  AWAY_POSTGAME_ELO = mean(AWAY_POSTGAME_ELO),
                  home_wins = sum(HOME_SIM_PRED),
                  home_margin = median(HOME_SIM_MARGIN),
                  n = n(),
                  .groups = 'drop') %>%
        mutate(HOME_SIM_PROB = home_wins / n) %>%
        mutate(HOME_SIM_MARGIN = home_margin) %>%
        select(SIM_FROM_WEEK, GAME_ID,
               HOME_PREGAME_ELO,
               AWAY_PREGAME_ELO,
               HOME_POSTGAME_ELO,
               AWAY_POSTGAME_ELO,
               HOME_SIM_PROB,
               HOME_SIM_MARGIN)

game_predictions = bind_rows(conference_championshiop_predictions,
                             postseason_predictions) %>%
        mutate(SIM_FROM_WEEK = 15)

# combine into one table
games_and_outcomes = games %>%
        bind_rows(., postseason_games %>%
                          mutate(WEEK = 16)) %>%
        filter(SEASON == temp_params$input_season) %>%
        filter(CONFERENCE_CHAMPIONSHIP == T | SEASON_TYPE == 'postseason') %>%
        left_join(., game_predictions %>%
                  # address issue with the damn api flipping the home and away teams
                  group_by(GAME_ID) %>%
                  filter(SIM_FROM_WEEK == max(SIM_FROM_WEEK)),
          by = c("GAME_ID")) %>%
        # HOME_POINTS, AWAY_POINTS) %>%
        # compute quality, which is harmonic mean of the two team's Elo
        mutate(QUALITY = case_when(NEUTRAL_SITE == T ~ 2 / ((1/(HOME_PREGAME_ELO+75) + (1/AWAY_PREGAME_ELO))),
                                   NEUTRAL_SITE == F ~ 2 / ((1/(HOME_PREGAME_ELO) + (1/AWAY_PREGAME_ELO))))) %>%
        # scaled to 0 and 1
        mutate(QUALITY = round(100*((QUALITY - min(QUALITY)) / ((max(QUALITY) - min(QUALITY)))),0)) %>%
        # compute a competitive rating, which is a parabolic function of the home team's win prob
        # that is maximized at 0.5 and minimized at 0 and 1
        # ranges from 0 to 1
        mutate(COMPETITIVE = -4*(HOME_SIM_PROB-0.5)^2+1) %>%
        mutate(COMPETITIVE = round(100*COMPETITIVE)) %>%
        mutate(INTEREST = (0.4 * COMPETITIVE) + (0.6 * QUALITY)) %>%
        mutate(INTEREST = round(100*((INTEREST - min(INTEREST)) / ((max(INTEREST) - min(INTEREST)))),0)) %>%
        #  mutate(QUALITY = 2 / ((1/HOME_PREGAME_ELO) + (1/AWAY_PREGAME_ELO))) %>%
        rename(HOME = HOME_TEAM,
               AWAY = AWAY_TEAM) %>%
        mutate(HOME_SIM = case_when(HOME_SIM_PROB >=.5 ~ 'win',
                                    HOME_SIM_PROB < .5 ~ 'loss')) %>%
        mutate(HOME_OUTCOME = case_when(HOME_POINTS > AWAY_POINTS ~ 'win',
                                        HOME_POINTS < AWAY_POINTS ~ 'loss'),
               HOME_MARGIN = HOME_POINTS - AWAY_POINTS) %>%
        mutate(PRED = case_when(HOME_SIM == 'win' ~ paste(HOME, "by", abs(HOME_SIM_MARGIN)),
                                HOME_SIM == 'loss' ~paste(AWAY, "by", abs(HOME_SIM_MARGIN)))) %>%
        mutate(OUTCOME = case_when(HOME_OUTCOME == 'win' ~ paste(HOME, "by", abs(HOME_MARGIN)),
                                   HOME_OUTCOME == 'loss' ~ paste(AWAY, "by", abs(HOME_MARGIN)))) %>%
        mutate(HOME_PROB = HOME_SIM_PROB) %>%
        mutate(HOME_WIN = factor(case_when(HOME_POINTS > AWAY_POINTS ~ 'yes',
                                           HOME_POINTS < AWAY_POINTS ~ 'no'),
                                 levels = c('no', 'yes'))) %>%
        mutate(HOME_PRED = factor(case_when(HOME_SIM_PROB >= .5 ~ 'yes',
                                            HOME_SIM_PROB < 5 ~ 'no'),
                                  levels = c("no", "yes"))) %>%
        mutate(yes = HOME_SIM_PROB)

qualityColorRamp = colorRampPalette(c("orange", "white", "dodgerblue4"))
homeColorRamp = colorRampPalette(c("white", "deepskyblue1"))
homeColor = homeColorRamp(5)[3]

save(games_and_outcomes,
     file = here::here("simulations", "postseason", "2022", "postseason_predictions.Rdata"))

# formatStyle('Result',
#             'correct',
#             backgroundColor = styleEqual(levels = c(1, 0),
#                                          c(homeColor,
#                                            'lightcoral')))
# formatStyle('Result',
#             'correct',
#             backgroundColor = styleEqual(levels = c(1, 0),
#                                          c(homeColor,
#                                            'lightcoral')))






