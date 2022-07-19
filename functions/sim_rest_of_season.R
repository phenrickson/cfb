sim_rest_of_season <-
function(games,
                              season,
                              elo_initial,
                              elo_pars,
                              nsims = 100,
                              sim_start_week = 0,
                              season_start_only = F,
                              points_model,
                              aggregate = T) {
        
        # set initial elo using inherited elo settings
        elo_so_far = elo_initial
        
        # get a grid of all season and weeks
        season_weeks = games %>%
                filter(SEASON_TYPE == 'regular') %>%
                filter(CONFERENCE_CHAMPIONSHIP == F) %>%
                filter(SEASON == season) %>%
                arrange(START_DATE) %>%
                distinct(SEASON, SEASON_TYPE, WEEK) %>%
                group_by(SEASON) %>%
                mutate(WEEK_NUM = row_number()) %>%
                mutate(PREVIOUS_WEEK = dplyr::lag(WEEK, 1, 0)) %>%
                ungroup() %>%
                filter(WEEK_NUM > sim_start_week)
        
        # weeks to sim
        if (season_start_only == T) {weeks_to_sim = 1} else
        {weeks_to_sim = nrow(season_weeks)}

        # # loop over weeks
        game_simulations = foreach(i = 1:weeks_to_sim,
                                   .combine = bind_rows) %do% {

                                           # get the rest of the games from that season
                                           games_to_simulate = games %>%
                                                   filter(SEASON_TYPE == 'regular') %>%
                                                   filter(CONFERENCE_CHAMPIONSHIP == F) %>%
                                                   left_join(., season_weeks %>%
                                                                     select(SEASON, WEEK, SEASON_TYPE, WEEK_NUM),
                                                             by = c("SEASON", "WEEK", "SEASON_TYPE")) %>%
                                                   filter(SEASON == season_weeks[i,]$SEASON) %>%
                                                   filter(WEEK_NUM >= season_weeks[i,]$WEEK_NUM)
                                           # simulate that week
                                           # simulate the reast of the season
                                           sims_season =  future_replicate(nsims,
                                                                           sim_elo_ratings_with_recruiting(games_to_simulate,
                                                                                                           teams = elo_so_far$teams,
                                                                                                           team_seasons = elo_so_far$team_seasons,
                                                                                                           home_field_advantage = elo_pars$home_field_advantage,
                                                                                                           recruiting_weight = elo_pars$recruiting_weight,
                                                                                                           reversion = elo_pars$reversion,
                                                                                                           k = elo_pars$k,
                                                                                                           v = elo_pars$v,
                                                                                                           verbose=F,
                                                                                                           ties =F,
                                                                                                           points_model = points_model))

                                           # get simulations
                                           sim_game_outcomes = rbindlist(sims_season['game_outcomes',],
                                                                         idcol = T) %>%
                                                   mutate(SIM_FROM_WEEK = season_weeks[i,]$PREVIOUS_WEEK) %>%
                                                   mutate(home_field_advantage = elo_pars$home_field_advantage,
                                                          recruiting_weight = elo_pars$recruiting_weight,
                                                          reversion = elo_pars$reversion,
                                                          k = elo_pars$k,
                                                          v = elo_pars$v)

                                           # aggregate result of simulations
                                           if(aggregate == T) {
                                                   sim_results = games_to_simulate %>%
                                                           left_join(.,
                                                                     sim_game_outcomes %>%
                                                                             group_by(GAME_ID, SIM_FROM_WEEK,
                                                                                      home_field_advantage, recruiting_weight, reversion,
                                                                                      k, v) %>%
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
                                                                             select(GAME_ID,
                                                                                    HOME_PREGAME_ELO,
                                                                                    AWAY_PREGAME_ELO,
                                                                                    HOME_POSTGAME_ELO,
                                                                                    AWAY_POSTGAME_ELO,
                                                                                    HOME_SIM_PROB,
                                                                                    HOME_SIM_MARGIN,
                                                                                    SIM_FROM_WEEK,
                                                                                    home_field_advantage, recruiting_weight,
                                                                                    reversion, k, v),
                                                                     by = "GAME_ID") %>%
                                                           select(SIM_FROM_WEEK, everything()) %>%
                                                           arrange(START_DATE)
                                           } else 
                                                   if (aggregate == F) {sim_results = sim_game_outcomes %>%
                                                           rename(.id = .id)}
                                           
                                           
                                           # if not start week
                                           # after a week is done, get the games that have finished so far
                                           games_so_far = games %>%
                                                   left_join(., season_weeks %>%
                                                                     select(SEASON, WEEK, SEASON_TYPE, WEEK_NUM),
                                                             by = c("SEASON", "WEEK", "SEASON_TYPE")) %>%
                                                   filter(SEASON == season_weeks[i,]$SEASON) %>%
                                                   filter(WEEK_NUM == season_weeks[i,]$WEEK_NUM) %>%
                                                   filter(!is.na(HOME_POINTS) & !is.na(AWAY_POINTS))
                                           
                                           # then, update the elo with the actual result for next week
                                           if (nrow(games_so_far)==0)  {elo_so_far = elo_so_far} else
                                                   {
                                                           elo_so_far = calc_elo_ratings_with_recruiting(
                                                                   games = games_so_far,
                                                                   teams = elo_so_far$teams,
                                                                   team_seasons = elo_so_far$team_seasons,
                                                                   recruiting_weight = 0,
                                                                   home_field_advantage = elo_pars$home_field_advantage,
                                                                   reversion = elo_pars$reversion,
                                                                   k = elo_pars$k,
                                                                   v = elo_pars$v,
                                                                   verbose = F)
                                                           }
                                           
                     
                                           cat("\r", "season simulations", i, "of", weeks_to_sim, "completed");  flush.console()

                                           # store the simulation results
                                           sim_results

                                  }

        # gather team output based on specified aggregation
        if (aggregate == T) {
                # gather output for teams
                team_simulations = game_simulations %>%
                        select(SIM_FROM_WEEK,
                               GAME_ID,
                               SEASON,
                               SEASON_TYPE,
                               WEEK,
                               GAME_DATE,
                               HOME_TEAM,
                               HOME_CONFERENCE,
                               HOME_DIVISION,
                               HOME_SIM_PROB,
                               HOME_SIM_MARGIN,
                               HOME_PREGAME_ELO,
                               HOME_POSTGAME_ELO,
                               AWAY_TEAM,
                               home_field_advantage,
                               recruiting_weight,
                               reversion,
                               k,
                               v
                        ) %>%
                        rename(OPPONENT = AWAY_TEAM) %>%
                        set_names(., gsub("HOME_", "", names(.))) %>%
                        bind_rows(.,
                                  game_simulations %>%
                                          mutate(AWAY_SIM_PROB = 1-HOME_SIM_PROB,
                                                 AWAY_SIM_MARGIN = -HOME_SIM_MARGIN) %>%
                                          select(SIM_FROM_WEEK,
                                                 GAME_ID,
                                                 SEASON,
                                                 SEASON_TYPE,
                                                 WEEK,
                                                 GAME_DATE,
                                                 AWAY_TEAM,
                                                 AWAY_CONFERENCE,
                                                 AWAY_DIVISION,
                                                 AWAY_SIM_PROB,
                                                 AWAY_SIM_MARGIN,
                                                 AWAY_PREGAME_ELO,
                                                 AWAY_POSTGAME_ELO,
                                                 HOME_TEAM,
                                                 home_field_advantage,
                                                 recruiting_weight,
                                                 reversion,
                                                 k,
                                                 v) %>%
                                          rename(OPPONENT = HOME_TEAM) %>%
                                          set_names(., gsub("AWAY_", "", names(.)))) %>%
                        arrange(GAME_DATE)
        } else {
                team_simulations = game_simulations %>%
                        rename(HOME_PREGAME_ELO = HOME_PREGAME_ELO,
                               AWAY_PREGAME_ELO = AWAY_PREGAME_ELO,
                               HOME_POSTGAME_ELO = HOME_POSTGAME_ELO,
                               AWAY_POSTGAME_ELO = AWAY_POSTGAME_ELO) %>%
                        select(.id, 
                               SIM_FROM_WEEK,
                               GAME_ID,
                               SEASON,
                               SEASON_TYPE,
                               WEEK,
                               GAME_DATE,
                               HOME_TEAM,
                               HOME_CONFERENCE,
                               HOME_DIVISION,
                               HOME_SIM_OUTCOME,
                               HOME_SIM_MARGIN,
                               HOME_PREGAME_ELO,
                               HOME_POSTGAME_ELO,
                               AWAY_TEAM,
                               home_field_advantage,
                               recruiting_weight,
                               reversion,
                               k,
                               v
                        ) %>%
                        rename(OPPONENT = AWAY_TEAM) %>%
                        set_names(., gsub("HOME_", "", names(.))) %>%
                        bind_rows(.,
                                  game_simulations %>%
                                          rename(HOME_PREGAME_ELO = HOME_PREGAME_ELO,
                                                 AWAY_PREGAME_ELO = AWAY_PREGAME_ELO,
                                                 HOME_POSTGAME_ELO = HOME_POSTGAME_ELO,
                                                 AWAY_POSTGAME_ELO = AWAY_POSTGAME_ELO) %>%
                                          select(.id,
                                                 SIM_FROM_WEEK,
                                                 GAME_ID,
                                                 SEASON,
                                                 SEASON_TYPE,
                                                 WEEK,
                                                 GAME_DATE,
                                                 AWAY_TEAM,
                                                 AWAY_CONFERENCE,
                                                 AWAY_DIVISION,
                                                 AWAY_SIM_OUTCOME,
                                                 AWAY_SIM_MARGIN,
                                                 AWAY_PREGAME_ELO,
                                                 AWAY_POSTGAME_ELO,
                                                 HOME_TEAM,
                                                 home_field_advantage,
                                                 recruiting_weight,
                                                 reversion,
                                                 k,
                                                 v) %>%
                                          rename(OPPONENT = HOME_TEAM) %>%
                                          set_names(., gsub("AWAY_", "", names(.)))) %>%
                        arrange(GAME_DATE)
                
        }

        # gather output for teams
        out = list("game_outcomes" = game_simulations,
                   "team_outcomes" = team_simulations)

        return(out)
        
}
