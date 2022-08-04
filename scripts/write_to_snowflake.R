# for parallelization
library(future.apply)
plan(multisession, workers = 7)

# initial season
initial_season = 2022

# set initial games
initial_games = games %>%
        filter(SEASON < initial_season)

# set pars
elo_pars = tibble(k = 35,
                  v = 400,
                  reversion = 0.1,
                  home_field_advantage = 75,
                  recruiting_weight =0)

# now get the elo ratings based on this training set
elo_initial = calc_elo_ratings(
        games = initial_games,
        home_field_advantage = elo_pars$home_field_advantage,
        reversion = elo_pars$reversion,
        k = elo_pars$k,
        v = elo_pars$v,
        verbose = T)

# write elo game outcomes
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.ELO_GAME_OUTCOMES"),
                  value = elo_initial$game_outcomes %>%
                          select(-HOME_RECRUITING_SCORE,
                                 -AWAY_RECRUITING_SCORE) %>%
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

# write elo team outcomes
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.ELO_TEAM_OUTCOMES"),
                  value = elo_initial$team_outcomes %>%
                          select(-home_field_advantage,
                                 -reversion,
                                 -k,
                                 -v) %>%
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

# write elo tames

# get fbs teams
teams = elo_initial$team_outcomes %>%
        filter(SEASON == 2021) %>%
        filter(DIVISION == 'fbs') %>%
        distinct(TEAM) %>%
        arrange(TEAM) %>%

# train points model
points_model = elo_initial$game_outcomes %>%
        filter(SEASON >=1970) %>% 
        filter(SEASON < initial_season) %>%
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


source(here::here("functions", "calc_elo_ratings_with_recruiting.R"))
source(here::here("functions", "sim_elo_ratings_with_recruiting.R"))
source(here::here("functions", "sim_rest_of_season.R"))

# simulate season from the start
set.seed(11)
season_simulations = sim_rest_of_season(games,
                                        season = initial_season,
                                        elo_initial = elo_initial,
                                        elo_pars = elo_pars,
                                        nsims = 1000,
                                        sim_start_week = 0,
                                        season_start_only = T,
                                        aggregate = F,
                                        points_model = points_model)


# get simulations into table form
season_elo = season_simulations %$%
        team_outcomes %>%
        filter(SEASON_TYPE == 'regular') %>%
       # filter(CONFERENCE_CHAMPIONSHIP != T) %>%
        filter(DIVISION == 'fbs') %>%
        group_by(SEASON, TEAM, CONFERENCE) %>%
        mutate(max_week = max(WEEK)) %>%
        filter(WEEK == max_week) %>%
        summarize(ELO = mean(POSTGAME_ELO),
                  sd = sd(POSTGAME_ELO), .groups = 'drop') %>%
        arrange(desc(ELO))

# total games per team
max_games = season_simulations %$%
        team_outcomes %>%
        filter(SEASON_TYPE == 'regular') %>%
        filter(DIVISION == 'fbs') %>%
        #filter(CONFERENCE_CHAMPIONSHIP != T) %>%
        group_by(TEAM) %>%
        summarize(games = n_distinct(GAME_ID)) %>%
        summarize(max_games = max(games)) %>%
        pull(max_games)

# # now get total expected wins
season_totals = season_simulations %$%
        team_outcomes %>%
        filter(SEASON_TYPE == 'regular') %>%
        filter(DIVISION == 'fbs') %>%
    #    filter(CONFERENCE_CHAMPIONSHIP != T) %>%
        mutate(WIN = case_when(SIM_MARGIN > 0 ~ 1,
                               TRUE ~ 0)) %>%
        group_by(.id, SEASON, TEAM) %>%
        summarize(WINS = sum(WIN),
                  GAMES = n_distinct(GAME_ID),
                  .groups = 'drop') %>%
        group_by(SEASON, TEAM, WINS, GAMES) %>%
        count() %>%
        group_by(SEASON, TEAM) %>%
        mutate(perc = round(n / sum(n),2))

# make sequence from zero to max games
win_counts = seq(0, max_games, 1)

# get teams
teams = unique(season_totals$TEAM)

max_wins = max(season_totals$WINS)
min_wins = min(season_totals$WINS)

# make empty grid
table_long = expand.grid(SEASON = initial_season,
                         TEAM = teams,
                         WINS = win_counts) %>%
        left_join(.,
                  season_totals %>%
                          select(SEASON, TEAM, WINS, perc) %>%
                          mutate(perc = perc),
                  by = c("SEASON", "TEAM", "WINS")) %>%
        mutate(perc = replace_na(perc, 0)) 


# make table
table = table_long %>%
        select(SEASON, TEAM, WINS, perc) %>%
        pivot_wider(.,
                    id_cols = c("SEASON", "TEAM"),
                    values_from = c("perc"),
                    names_from = c("WINS")) %>%
        left_join(., season_totals %>%
                          select(SEASON, TEAM, WINS, n, perc) %>%
                          #       mutate(perc = perc *100) %>%
                          group_by(SEASON, TEAM) %>%
                          mutate(points = sum(WINS*n)) %>%
                          distinct(SEASON, TEAM, points) %>%
                          ungroup(),
                  by = c("SEASON", "TEAM")) %>%
        left_join(., season_elo,
                  by = c("SEASON", "TEAM")) %>%
        #     arrange(desc(points)) %>%
        arrange(desc(ELO)) %>%
        mutate(SEASON  = factor(SEASON)) %>%
        select(-points) %>%
        select(SEASON, TEAM, ELO, one_of(paste(win_counts)))


elo_func = 
        function(x) {
                
                breaks = c(0,1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400)
                colorRamp=colorRampPalette(c("white", "grey50"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=T,
                             label = FALSE)
                col_palette[mycut]
                
        }

col_func = 
        function(x) {
                
                breaks = c(0, 5, 10, 15, 20, 40, 60)/100
                colorRamp=colorRampPalette(c("white", "grey50"))
                col_palette <- colorRamp(length(breaks))
                mycut <- cut(x, 
                             breaks = breaks,
                             include.lowest = TRUE, 
                             right=T,
                             label = FALSE)
                col_palette[mycut]
                
        }

table %>%
        rename(Elo = ELO) %>%
        mutate(`End Elo`= round(Elo, 0)) %>%
        mutate(Rank = row_number()) %>%
        select(SEASON, TEAM, Rank, `End Elo`, one_of(paste(win_counts))) %>%
        rename(Season = SEASON,
               Team = TEAM) %>%
        flextable() %>%
        autofit() %>%
        bg(., j = paste(win_counts),
           bg = col_func) %>%
        add_header_row(.,
                       values = c("","", "Simulated", paste("Simulated Win Probabilities for", initial_season, "Regular Season")),
                       colwidths = c(1, 1, 2, 1+max(win_counts))) %>%
        flextable::align(j = c("Rank", "End Elo", paste(seq(0, max(win_counts), 1))),
                         part = "all",
                         align = "center") %>%
        color(part = "all",
              color = "grey20") %>%
        bg(.,
           j = 'End Elo',
           bg = elo_func)


# simulations to snowflake
simulations_table = table_long %>%
        mutate(method = 'Elo') %>%
        rename(METHOD = method,
               PERC = perc) %>%
        select(METHOD, SEASON, TEAM, WINS, PERC) %>%
        mutate(LOAD_DATE = Sys.time()) %>%
        as_tibble()

dbId(con, "ANALYTICS_PRESEASON_WIN_SIMULATIONS", "CFD_ANALYTICS")

# write 
DBI::dbWriteTable(myconn, 
             name = DBI::SQL("CFD_ANALYTICS.PRESEASON_WIN_SIMULATIONS"),
             value = simulations_table,
             append = T)

# write efficiency
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.EFFICIENCY_TEAM_SEASON"),
                  value = season_team_rankings %>%
                          select(-MARGIN) %>%
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

# write efficiency season type
DBI::dbWriteTable(myconn, 
                  name = DBI::SQL("CFD_ANALYTICS.EFFICIENCY_TEAM_GAME"),
                  value = weekly_team_rankings %>%
                          mutate(LOAD_DATE = Sys.time()),
                  append = T)

# wisconsins season
season_simulations %$% 
        team_outcomes %>%
        filter(TEAM == 'Wisconsin')  %>%
      #  select(SIM_FROM_WEEK, GAME_ID, SEASON, WEEK, GAME_DATE, TEAM, OPPONENT, SIM_OUTCOME, SIM_MARGIN, PREGAME_ELO, POSTGAME_ELO) %>%
        select(SIM_FROM_WEEK, GAME_ID, SEASON, WEEK, GAME_DATE, TEAM, OPPONENT, SIM_PROB, SIM_MARGIN, PREGAME_ELO, POSTGAME_ELO) %>%
        mutate_if(is.numeric, round, 4) %>%
        as_tibble()

# season_simulations %$% 
#         game_outcomes %>%
#         filter(HOME_TEAM == 'Georgia' | AWAY_TEAM == 'Georgia') %>%
#         select(GAME_ID, SEASON, WEEK, NEUTRAL_SITE, HOME_TEAM, AWAY_TEAM, HOME_SIM_OUTCOME, HOME_SIM_MARGIN) %>%
#         mutate_if(is.numeric, round, 0)

# overall season
season_simulations %$% 
        team_outcomes %>%
        group_by(SEASON,TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo)) %>%
        head(15) %>%
        mutate_if(is.numeric, round, 0)

# big ten season
season_simulations %$% 
        team_outcomes %>%
        filter(CONFERENCE %in% c("Big Ten")) %>%
        group_by(SEASON, CONFERENCE, TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, CONFERENCE, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo))

# SEC season
season_simulations %$% 
        team_outcomes %>%
        filter(CONFERENCE %in% c("SEC")) %>%
        group_by(SEASON, CONFERENCE, TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, CONFERENCE, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo))

# big 12
season_simulations %$% 
        team_outcomes %>%
        filter(CONFERENCE %in% c("Big 12")) %>%
        group_by(SEASON, CONFERENCE, TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, CONFERENCE, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo))

# acc
season_simulations %$% 
        team_outcomes %>%
        filter(CONFERENCE %in% c("ACC")) %>%
        group_by(SEASON, CONFERENCE, TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, CONFERENCE, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo))

# pac-12
season_simulations %$% 
        team_outcomes %>%
        filter(CONFERENCE %in% c("Pac-12")) %>%
        group_by(SEASON, CONFERENCE, TEAM, SIM_OUTCOME) %>%
        count() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("win", "loss"),
                  ~ replace_na(., 0)) %>%
        ungroup() %>%
        select(SEASON, CONFERENCE, TEAM, win, loss) %>%
        arrange(desc(win)) %>%
        left_join(.,
                  season_simulations %$% 
                          team_outcomes %>%
                          group_by(TEAM) %>%
                          filter(GAME_DATE == max(GAME_DATE)) %>%
                          select(SEASON, TEAM, POSTGAME_ELO) %>%
                          distinct() %>%
                          rename(elo= POSTGAME_ELO),
                  by = c("SEASON", "TEAM")) %>%
        arrange(desc(elo))

