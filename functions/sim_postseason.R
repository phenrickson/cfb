sim_postseason = function(season_simulations) {
        
       # conference_divisions = conference_divisions

        # get simulated outcomes for remaining games
        sim_remaining_games  = 
                season_simulations %$% 
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
                       CONFERENCE_DIVISION_GAME = case_when(
                               CONFERENCE == OPPONENT_CONFERENCE & CONFERENCE_DIVISION == OPPONENT_CONFERENCE_DIVISION ~ T,
                               CONFERENCE == OPPONENT_CONFERENCE & ((is.na(CONFERENCE_DIVISION) & is.na(OPPONENT_CONFERENCE_DIVISION))) ~ T,
                               TRUE ~ F)) %>%
                group_by(SEASON, TEAM, SIM_FROM_WEEK) %>%
                mutate(SIM_FROM_DATE = min(GAME_DATE)) %>%
                ungroup() %>%
                select(-home_field_advantage, -reversion, -k, -v)
        
        # calc elo so far
        if(params$sim_week == 0) {
                elo_so_far = tibble(WEEK = params$sim_week)
                games_so_far = tibble(WEEK = params$sim_week)
        } else {
                
                elo_so_far = calc_elo_ratings(games %>%
                                                      filter(SEASON == params$input_season) %>%
                                                      filter(WEEK <= params$sim_week),
                                              teams = elo_input$teams,
                                              team_seasons = elo_input$teams,
                                              reversion = elo_pars$reversion,
                                              k = elo_pars$k,
                                              v = elo_pars$v,
                                              home_field_advantage = elo_pars$home_field_advantage)
                
                # games so far
                games_so_far = elo_so_far %$%
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
                
                
        }
        
        ### conference championships
        # get conferece championship matchups for each simulation
        conference_championship_matchups = sim_remaining_games %>%
                mutate(type = 'sim') %>%
                nest(-.id, -SIM_FROM_WEEK) %>%
                mutate(data = future_map2(data,
                                          SIM_FROM_WEEK,
                                          ~ .x %>%
                                                  rename(OUTCOME = SIM_OUTCOME,
                                                         MARGIN = SIM_MARGIN) %>%
                                                  bind_rows(.,
                                                            games_so_far %>%
                                                                    mutate(type = 'actual') %>%
                                                                    filter(WEEK <= .y)))) %>%
                mutate(conference_championships = future_map(data,
                                                             ~ get_conference_championships(.x))) %>%
                select(.id, SIM_FROM_WEEK, conference_championships) %>%
                unnest()
        
        # simulate the results of each conference championship matchup
        sim_conference_championships = 
                conference_championship_matchups %>%
                pivot_wider(id_cols = c(".id", "SIM_FROM_WEEK", "SEASON", "CONFERENCE"),
                            names_from = c("MATCHUP"),
                            values_from = c("CONFERENCE_DIVISION", "TEAM", "PREGAME_ELO")) %>%
                mutate(SEASON_TYPE = 'conference_championship') %>%
                nest(-.id, -SIM_FROM_WEEK, -CONFERENCE, -SEASON, -SEASON_TYPE) %>%
                mutate(result =  future_map(data, ~ sim_game_margin(home_rating = .x$PREGAME_ELO_HOME,
                                                                    away_rating = .x$PREGAME_ELO_AWAY,
                                                                    points_model) %>%
                                                    as_tibble() %>%
                                                    rename(HOME_SIM_MARGIN = value) %>%
                                                    bind_cols(.x, .) %>%
                                                    mutate(HOME_SIM_MARGIN = case_when(HOME_SIM_MARGIN == 0 ~ sample(c(-1, 1), 1),
                                                                                       TRUE ~ HOME_SIM_MARGIN)) %>%
                                                    mutate(WINNER= case_when(HOME_SIM_MARGIN > 0 ~ TEAM_HOME,
                                                                             HOME_SIM_MARGIN <0 ~ TEAM_AWAY),
                                                           RUNNER_UP = case_when(HOME_SIM_MARGIN >= 0 ~ TEAM_AWAY,
                                                                                 HOME_SIM_MARGIN <0 ~ TEAM_HOME)))) %>%
                mutate(update = future_map2(data, result, ~ get_new_elos(home_rating = .x$PREGAME_ELO_HOME,
                                                                         away_rating = .x$PREGAME_ELO_AWAY,
                                                                         home_field_advantage = 0,
                                                                         home_margin = .y$HOME_SIM_MARGIN,
                                                                         k = elo_pars$k,
                                                                         v = elo_pars$v) %>%
                                                    t() %>%
                                                    as_tibble() %>%
                                                    set_names(c("HOME_POSTGAME_ELO",
                                                                "AWAY_POSTGAME_ELO",
                                                                "HOME_EXPECTED_SCORE",
                                                                "AWAY_EXPECTED_SCORE")))) %>%
                select(.id, SIM_FROM_WEEK, SEASON, SEASON_TYPE, CONFERENCE, data, result, update) %>%
                unnest() %>%
                ungroup() %>%
                mutate(CONFERENCE_HOME = CONFERENCE,
                       CONFERENCE_AWAY = CONFERENCE) %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       SEASON, 
                       SEASON_TYPE,
                       CONFERENCE_HOME, 
                       CONFERENCE_AWAY, 
                       TEAM_HOME, 
                       TEAM_AWAY, 
                       PREGAME_ELO_HOME,
                       PREGAME_ELO_AWAY,
                       HOME_POSTGAME_ELO, 
                       AWAY_POSTGAME_ELO,
                       HOME_SIM_MARGIN, 
                       WINNER,
                       RUNNER_UP) %>%
                rename(HOME_TEAM = TEAM_HOME,
                       AWAY_TEAM = TEAM_AWAY,
                       HOME_PREGAME_ELO = PREGAME_ELO_HOME,
                       AWAY_PREGAME_ELO = PREGAME_ELO_AWAY,
                       HOME_SIM_MARGIN = HOME_SIM_MARGIN,
                       HOME_CONFERENCE = CONFERENCE_HOME,
                       AWAY_CONFERENCE = CONFERENCE_AWAY) %>%
                mutate(AWAY_SIM_MARGIN = -HOME_SIM_MARGIN) %>%
                mutate(WEEK = max_week + 1) %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       SEASON, HOME_CONFERENCE, AWAY_CONFERENCE, SEASON_TYPE, WEEK, HOME_TEAM, AWAY_TEAM, HOME_SIM_MARGIN, AWAY_SIM_MARGIN, HOME_PREGAME_ELO, AWAY_PREGAME_ELO, HOME_POSTGAME_ELO, AWAY_POSTGAME_ELO)  %>%
                mutate(HOME_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'win',
                                                    HOME_SIM_MARGIN < 0 ~ 'loss'),
                       AWAY_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'loss',
                                                    HOME_SIM_MARGIN < 0 ~ 'win'))
        
        # flip to team level for analysis
        sim_conference_championship_games = sim_conference_championships %>%
                select(.id, 
                       SEASON,
                       SEASON_TYPE,
                       SIM_FROM_WEEK,
                       WEEK,
                       HOME_TEAM,
                       HOME_CONFERENCE,
                       HOME_SIM_MARGIN,
                       HOME_SIM_OUTCOME,
                       HOME_PREGAME_ELO,
                       HOME_POSTGAME_ELO,
                       AWAY_PREGAME_ELO,
                       AWAY_POSTGAME_ELO,
                       AWAY_TEAM) %>%
                rename(OPPONENT = AWAY_TEAM,
                       OPPONENT_PREGAME_ELO = AWAY_PREGAME_ELO,
                       OPPONENT_POSTGAME_ELO = AWAY_POSTGAME_ELO) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          sim_conference_championships %>% 
                                  select(.id,
                                         SEASON,
                                         SEASON_TYPE,
                                         SIM_FROM_WEEK,
                                         WEEK,
                                         AWAY_TEAM,
                                         AWAY_CONFERENCE,
                                         AWAY_SIM_MARGIN,
                                         AWAY_SIM_OUTCOME,
                                         AWAY_PREGAME_ELO,
                                         HOME_PREGAME_ELO,
                                         AWAY_POSTGAME_ELO,
                                         HOME_POSTGAME_ELO,
                                         HOME_TEAM) %>%
                                  rename(OPPONENT = HOME_TEAM,
                                         OPPONENT_PREGAME_ELO = HOME_PREGAME_ELO,
                                         OPPONENT_POSTGAME_ELO = HOME_POSTGAME_ELO) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                arrange(.id, WEEK) %>%
                left_join(., conference_divisions,
                          by = c("SEASON", "TEAM", "CONFERENCE"))
        
        rm(conference_championship_matchups,
           conference_championship_games,
           sim_conference_championships)
        
        playoff_matchups = sim_remaining_games %>%
                mutate(type = 'sim') %>%
                # add in simulated conference championps
                bind_rows(.,
                          sim_conference_championship_games %>%
                                  mutate(DIVISION = 'fbs')) %>%
                nest(-.id, -SIM_FROM_WEEK) %>%
                mutate(data = future_map2(data,
                                          SIM_FROM_WEEK,
                                          ~ .x %>%
                                                  rename(OUTCOME = SIM_OUTCOME,
                                                         MARGIN = SIM_MARGIN) %>%
                                                  bind_rows(.,
                                                            games_so_far %>%
                                                                    mutate(type = 'actual') %>%
                                                                    filter(WEEK <= .y)))) %>%
                mutate(playoffs = future_map(data,
                                             ~ get_playoffs(.x))) %>%
                select(.id, SIM_FROM_WEEK, playoffs) %>%
                unnest()
        
        # sim the playoff matchups
        sim_playoffs = playoff_matchups %>%
                mutate(PREGAME_ELO = RATING) %>%
                select(.id, SIM_FROM_WEEK, SEASON, CONFERENCE, TEAM, PREGAME_ELO, playoff_rank) %>%
                mutate(PLAYOFF_GAME = case_when(playoff_rank %in% c(1,4) ~ '1v4',
                                                playoff_rank %in% c(2,3) ~ '2v3')) %>%
                mutate(MATCHUP = case_when(playoff_rank %in% c(1, 2) ~ 'HOME',
                                           playoff_rank %in% c(3, 4) ~ 'AWAY')) %>%
                pivot_wider(id_cols = c(".id", "SIM_FROM_WEEK", "SEASON", "PLAYOFF_GAME"),
                            names_from = c("MATCHUP"),
                            values_from = c("CONFERENCE", "TEAM", "PREGAME_ELO")) %>%
                mutate(SEASON_TYPE = 'playoff') %>%
                nest(-.id, -SIM_FROM_WEEK, -PLAYOFF_GAME, -SEASON, -SEASON_TYPE) %>%
                mutate(result=  future_map(data, ~ sim_game_margin(home_rating = .x$PREGAME_ELO_HOME,
                                                                   away_rating = .x$PREGAME_ELO_AWAY,
                                                                   points_model) %>%
                                                   as_tibble() %>%
                                                   rename(HOME_SIM_MARGIN = value) %>%
                                                   bind_cols(.x, .) %>%
                                                   mutate(HOME_SIM_MARGIN = case_when(HOME_SIM_MARGIN == 0 ~ sample(c(-1, 1), 1),
                                                                                      TRUE ~ HOME_SIM_MARGIN)) %>%
                                                   mutate(WINNER= case_when(HOME_SIM_MARGIN > 0 ~ TEAM_HOME,
                                                                            HOME_SIM_MARGIN <0 ~ TEAM_AWAY),
                                                          RUNNER_UP = case_when(HOME_SIM_MARGIN >= 0 ~ TEAM_AWAY,
                                                                                HOME_SIM_MARGIN <0 ~ TEAM_HOME)))) %>%
                mutate(update = future_map2(data, result, ~ get_new_elos(home_rating = .x$PREGAME_ELO_HOME,
                                                                         away_rating = .x$PREGAME_ELO_AWAY,
                                                                         home_field_advantage = 0,
                                                                         home_margin = .y$HOME_SIM_MARGIN,
                                                                         k = elo_pars$k,
                                                                         v = elo_pars$v) %>%
                                                    t() %>%
                                                    as_tibble() %>%
                                                    set_names(c("HOME_POSTGAME_ELO",
                                                                "AWAY_POSTGAME_ELO",
                                                                "HOME_EXPECTED_SCORE",
                                                                "AWAY_EXPECTED_SCORE")))) %>%
                select(.id, SIM_FROM_WEEK, PLAYOFF_GAME, SEASON, SEASON_TYPE, data, result, update) %>%
                unnest() %>%
                ungroup() %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       PLAYOFF_GAME,
                       SEASON, 
                       SEASON_TYPE,
                       CONFERENCE_HOME, 
                       CONFERENCE_AWAY, 
                       TEAM_HOME, 
                       TEAM_AWAY, 
                       PREGAME_ELO_HOME,
                       PREGAME_ELO_AWAY,
                       HOME_POSTGAME_ELO, 
                       AWAY_POSTGAME_ELO,
                       HOME_SIM_MARGIN, 
                       WINNER,
                       RUNNER_UP) %>%
                rename(HOME_TEAM = TEAM_HOME,
                       AWAY_TEAM = TEAM_AWAY,
                       HOME_PREGAME_ELO = PREGAME_ELO_HOME,
                       AWAY_PREGAME_ELO = PREGAME_ELO_AWAY,
                       HOME_SIM_MARGIN = HOME_SIM_MARGIN,
                       HOME_CONFERENCE = CONFERENCE_HOME,
                       AWAY_CONFERENCE = CONFERENCE_AWAY) %>%
                mutate(AWAY_SIM_MARGIN = -HOME_SIM_MARGIN) %>%
                mutate(WEEK = max_week + 2) %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       PLAYOFF_GAME,
                       SEASON, HOME_CONFERENCE, AWAY_CONFERENCE, SEASON_TYPE, WEEK, HOME_TEAM, AWAY_TEAM, HOME_SIM_MARGIN, AWAY_SIM_MARGIN, HOME_PREGAME_ELO, AWAY_PREGAME_ELO, HOME_POSTGAME_ELO, AWAY_POSTGAME_ELO)  %>%
                mutate(HOME_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'win',
                                                    HOME_SIM_MARGIN < 0 ~ 'loss'),
                       AWAY_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'loss',
                                                    HOME_SIM_MARGIN < 0 ~ 'win'))
        
        # flip to teams
        sim_playoff_games  = sim_playoffs %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       PLAYOFF_GAME,
                       SEASON,
                       SEASON_TYPE,
                       WEEK,
                       HOME_TEAM,
                       HOME_CONFERENCE,
                       HOME_SIM_MARGIN,
                       HOME_SIM_OUTCOME,
                       HOME_PREGAME_ELO,
                       HOME_POSTGAME_ELO,
                       AWAY_PREGAME_ELO,
                       AWAY_POSTGAME_ELO,
                       AWAY_TEAM) %>%
                rename(OPPONENT = AWAY_TEAM,
                       OPPONENT_PREGAME_ELO = AWAY_PREGAME_ELO,
                       OPPONENT_POSTGAME_ELO = AWAY_POSTGAME_ELO) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          sim_playoffs %>% 
                                  select(.id,
                                         SIM_FROM_WEEK,
                                         PLAYOFF_GAME,
                                         SEASON,
                                         SEASON_TYPE,
                                         WEEK,
                                         AWAY_TEAM,
                                         AWAY_CONFERENCE,
                                         AWAY_SIM_MARGIN,
                                         AWAY_SIM_OUTCOME,
                                         AWAY_PREGAME_ELO,
                                         HOME_PREGAME_ELO,
                                         AWAY_POSTGAME_ELO,
                                         HOME_POSTGAME_ELO,
                                         HOME_TEAM) %>%
                                  rename(OPPONENT = HOME_TEAM,
                                         OPPONENT_PREGAME_ELO = HOME_PREGAME_ELO,
                                         OPPONENT_POSTGAME_ELO = HOME_POSTGAME_ELO) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                arrange(.id, WEEK) %>%
                left_join(., team_conference_season %>%
                                  select(SEASON, TEAM, CONFERENCE, DIVISION),
                          by = c("SEASON", "TEAM", "CONFERENCE")) %>%
                filter(DIVISION == 'fbs')
        
        rm(playoff_matchups,
           sim_playoffs)
        
        # get playoff winners
        nc_matchups = sim_playoff_games %>%
                filter(SIM_OUTCOME == 'win') %>%
                select(.id, SIM_FROM_WEEK, SEASON, PLAYOFF_GAME, TEAM, CONFERENCE, POSTGAME_ELO) %>%
                rename(PREGAME_ELO = POSTGAME_ELO) %>%
                arrange(.id, SIM_FROM_WEEK) %>%
                group_by(.id, SIM_FROM_WEEK, SEASON) %>%
                mutate(ID = row_number()) %>%
                ungroup() %>%
                mutate(MATCHUP = case_when(ID == 1 ~ 'HOME',
                                           ID == 2 ~ 'AWAY')) %>%
                select(-PLAYOFF_GAME)
        
        sim_nc = nc_matchups %>%
                pivot_wider(., id_cols = c(".id", "SIM_FROM_WEEK", "SEASON"),
                            names_from = c("MATCHUP"),
                            values_from = c("TEAM", "PREGAME_ELO","CONFERENCE")) %>%
                mutate(SEASON_TYPE = 'national_championship') %>%
                nest(-.id, -SIM_FROM_WEEK, -SEASON, -SEASON_TYPE) %>%
                mutate(result= future_map(data, ~ sim_game_margin(home_rating = .x$PREGAME_ELO_HOME,
                                                                  away_rating = .x$PREGAME_ELO_AWAY,
                                                                  points_model) %>%
                                                  as_tibble() %>%
                                                  rename(HOME_SIM_MARGIN = value) %>%
                                                  bind_cols(.x, .) %>%
                                                  mutate(HOME_SIM_MARGIN = case_when(HOME_SIM_MARGIN == 0 ~ sample(c(-1, 1), 1),
                                                                                     TRUE ~ HOME_SIM_MARGIN)) %>%
                                                  mutate(WINNER= case_when(HOME_SIM_MARGIN > 0 ~ TEAM_HOME,
                                                                           HOME_SIM_MARGIN <0 ~ TEAM_AWAY),
                                                         RUNNER_UP = case_when(HOME_SIM_MARGIN >= 0 ~ TEAM_AWAY,
                                                                               HOME_SIM_MARGIN <0 ~ TEAM_HOME)))) %>%
                mutate(update = future_map2(data, result, ~ get_new_elos(home_rating = .x$PREGAME_ELO_HOME,
                                                                         away_rating = .x$PREGAME_ELO_AWAY,
                                                                         home_field_advantage = 0,
                                                                         home_margin = .y$HOME_SIM_MARGIN,
                                                                         k = elo_pars$k,
                                                                         v = elo_pars$v) %>%
                                                    t() %>%
                                                    as_tibble() %>%
                                                    set_names(c("HOME_POSTGAME_ELO",
                                                                "AWAY_POSTGAME_ELO",
                                                                "HOME_EXPECTED_SCORE",
                                                                "AWAY_EXPECTED_SCORE")))) %>%
                select(.id, SIM_FROM_WEEK, SEASON, SEASON_TYPE, data, result, update) %>%
                unnest() %>%
                ungroup() %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       SEASON, 
                       SEASON_TYPE,
                       CONFERENCE_HOME, 
                       CONFERENCE_AWAY, 
                       TEAM_HOME, 
                       TEAM_AWAY, 
                       PREGAME_ELO_HOME,
                       PREGAME_ELO_AWAY,
                       HOME_POSTGAME_ELO, 
                       AWAY_POSTGAME_ELO,
                       HOME_SIM_MARGIN, 
                       WINNER,
                       RUNNER_UP) %>%
                rename(HOME_TEAM = TEAM_HOME,
                       AWAY_TEAM = TEAM_AWAY,
                       HOME_PREGAME_ELO = PREGAME_ELO_HOME,
                       AWAY_PREGAME_ELO = PREGAME_ELO_AWAY,
                       HOME_SIM_MARGIN = HOME_SIM_MARGIN,
                       HOME_CONFERENCE = CONFERENCE_HOME,
                       AWAY_CONFERENCE = CONFERENCE_AWAY) %>%
                mutate(AWAY_SIM_MARGIN = -HOME_SIM_MARGIN) %>%
                mutate(WEEK = max_week + 3) %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       SEASON, HOME_CONFERENCE, AWAY_CONFERENCE, SEASON_TYPE, WEEK, HOME_TEAM, AWAY_TEAM, HOME_SIM_MARGIN, AWAY_SIM_MARGIN, HOME_PREGAME_ELO, AWAY_PREGAME_ELO, HOME_POSTGAME_ELO, AWAY_POSTGAME_ELO)  %>%
                mutate(HOME_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'win',
                                                    HOME_SIM_MARGIN < 0 ~ 'loss'),
                       AWAY_SIM_OUTCOME = case_when(HOME_SIM_MARGIN > 0 ~ 'loss',
                                                    HOME_SIM_MARGIN < 0 ~ 'win'))
        
        sim_nc_games  = sim_nc  %>%
                select(.id, 
                       SIM_FROM_WEEK,
                       SEASON,
                       SEASON_TYPE,
                       WEEK,
                       HOME_TEAM,
                       HOME_CONFERENCE,
                       HOME_SIM_MARGIN,
                       HOME_SIM_OUTCOME,
                       HOME_PREGAME_ELO,
                       HOME_POSTGAME_ELO,
                       AWAY_PREGAME_ELO,
                       AWAY_POSTGAME_ELO,
                       AWAY_TEAM) %>%
                rename(OPPONENT = AWAY_TEAM,
                       OPPONENT_PREGAME_ELO = AWAY_PREGAME_ELO,
                       OPPONENT_POSTGAME_ELO = AWAY_POSTGAME_ELO) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          sim_nc  %>% 
                                  select(.id,
                                         SIM_FROM_WEEK,
                                         SEASON,
                                         SEASON_TYPE,
                                         WEEK,
                                         AWAY_TEAM,
                                         AWAY_CONFERENCE,
                                         AWAY_SIM_MARGIN,
                                         AWAY_SIM_OUTCOME,
                                         AWAY_PREGAME_ELO,
                                         HOME_PREGAME_ELO,
                                         AWAY_POSTGAME_ELO,
                                         HOME_POSTGAME_ELO,
                                         HOME_TEAM) %>%
                                  rename(OPPONENT = HOME_TEAM,
                                         OPPONENT_PREGAME_ELO = HOME_PREGAME_ELO,
                                         OPPONENT_POSTGAME_ELO = HOME_POSTGAME_ELO) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                arrange(.id, WEEK) %>%
                left_join(., team_conference_season %>%
                                  select(SEASON, TEAM, CONFERENCE, DIVISION),
                          by = c("SEASON", "TEAM", "CONFERENCE")) %>%
                filter(DIVISION == 'fbs')
        
        rm(sim_nc,
           nc_matchups)
        
        all_simulations = list("sim_from_week" = params$sim_week, 
                               "sim_remaining_games" = sim_remaining_games,
                               "sim_conference_championship_games" = sim_conference_championship_games,
                               "sim_playoff_games" = sim_playoff_games,
                               "sim_nc_games" = sim_nc_games)
        
        return(all_simulations)
        
        
}
