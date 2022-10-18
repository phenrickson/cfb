get_playoffs = function(input_team_season) {
        
        # get resumes
        playoff_resumes =  input_team_season %>%
                filter(DIVISION == 'fbs') %>%
                group_by(SEASON, SEASON_TYPE, TEAM, CONFERENCE, OUTCOME) %>%
                count() %>%
                spread(OUTCOME, n) %>%
                mutate_at(c("loss", "win"),
                          ~ replace_na(., 0)) %>%
                mutate(P5 = case_when(CONFERENCE %in% c("SEC", "Big Ten", "Pac-12", "Big 12", "ACC") ~ 1,
                                      TRUE ~ 0)) %>%
                mutate(UNDEFEATED_REGULAR = case_when(SEASON_TYPE == 'regular' & loss == 0 & win > 1 ~ 1,
                                                      TRUE ~ 0)) %>%
                mutate(CONFERENCE_CHAMPION = case_when(SEASON_TYPE == 'conference_championship' & win == 1 ~ 1,
                                                       TRUE ~ 0)) %>%
                mutate(CONFERENCE_RUNNER_UP = case_when(SEASON_TYPE == 'conference_championship' & win == 0 ~ 1,
                                                        TRUE ~ 0)) %>%
                ungroup() %>%
                group_by(SEASON, TEAM, CONFERENCE, P5) %>%
                summarize(WINS = sum(win),
                          LOSSES = sum(loss),
                          UNDEFEATED_REGULAR = sum(UNDEFEATED_REGULAR),
                          CONFERENCE_CHAMPION = sum(CONFERENCE_CHAMPION),
                          CONFERENCE_RUNNER_UP = sum(CONFERENCE_RUNNER_UP),
                          .groups = 'drop') %>%
                # get each team's final elo in each sim
                left_join(.,
                          input_team_season %>%
                                  group_by(TEAM) %>%
                                  filter(WEEK == max(WEEK)) %>%
                                  ungroup() %>%
                                  arrange(desc(POSTGAME_ELO)) %>%
                                  mutate(ELO_RANK = row_number()) %>%
                                  select(TEAM, POSTGAME_ELO, ELO_RANK),
                          by = c("TEAM")) %>%
                # now get the average of each team's opponent's final Elo
                left_join(.,
                          input_team_season %>%
                                  left_join(., 
                                            input_team_season %>%
                                                    group_by(TEAM) %>%
                                                    filter(WEEK == max(WEEK)) %>%
                                                    mutate(LAST_ELO = POSTGAME_ELO) %>%
                                                    select(SEASON, TEAM, LAST_ELO) %>%
                                                    rename(OPPONENT = TEAM,
                                                           OPPONENT_LAST_ELO = LAST_ELO),
                                            by = c("SEASON", "OPPONENT")) %>%
                                  group_by(SEASON, TEAM) %>%
                                  summarize(SCHEDULE = mean(OPPONENT_LAST_ELO, na.rm=T),
                                            .groups = 'drop') %>%
                                  group_by(SEASON) %>%
                                  arrange(desc(SCHEDULE)) %>%
                                  mutate(DIFFICULTY = row_number()) %>%
                                  ungroup(),
                          by = c("SEASON", "TEAM")) %>%
                arrange(desc(WINS))
        
        # now determine playoff selection
        playoff_priorities = 
                playoff_resumes %>%
                # p5 teams that went undefeated and were conference champions
                filter(UNDEFEATED_REGULAR == 1 & P5 == 1 & CONFERENCE_CHAMPION == 1) %>%
                mutate(playoff_priority = 1) %>%
                # independent P5 teams that went undefeated (ie, notre dame)
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(UNDEFEATED_REGULAR == 1 & (P5 == 1 | (CONFERENCE == 'FBS Independents' & ELO_RANK < 9))) %>% 
                                  mutate(playoff_priority = 2) 
                ) %>%
                # p5 teams that won their conference with less than one loss
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(P5 == 1 & CONFERENCE_CHAMPION == 1 & LOSSES < 2 & ELO_RANK < 9) %>%
                                  mutate(playoff_priority = 3)
                ) %>%
                # p5 teams that won their conference with less than three losses
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(P5 == 1 & LOSSES < 2 & ELO_RANK < 5) %>%
                                  mutate(playoff_priority = 4)
                ) %>%
                # undefeated teams that were also conference champions and have a high elo
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(UNDEFEATED_REGULAR == 1 & CONFERENCE_CHAMPION == 1 & ELO_RANK < 8) %>%
                                  mutate(playoff_priority = 5)
                ) %>%
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(P5 == 1 & LOSSES < 3 & ELO_RANK < 5) %>%
                                  mutate(playoff_priority = 6)
                ) %>%
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(P5 == 1 & LOSSES < 3 & ELO_RANK < 10) %>%
                                  mutate(playoff_priority = 7)
                ) %>%
                bind_rows(.,
                          playoff_resumes %>%
                                  filter(P5 == 1 & LOSSES < 3 & ELO_RANK < 15) %>%
                                  mutate(playoff_priority = 8)
                ) %>%
                rename(RATING = POSTGAME_ELO,
                       RANK = ELO_RANK) %>%
                arrange(desc(RATING)) %>%
                group_by(SEASON, TEAM) %>%
                filter(playoff_priority == min(playoff_priority)) %>%
                select( SEASON, TEAM, CONFERENCE, CONFERENCE_CHAMPION, WINS, LOSSES, DIFFICULTY, RANK, RATING, playoff_priority) %>%
                arrange(playoff_priority, desc(CONFERENCE_CHAMPION), RANK, desc(RATING), DIFFICULTY) %>%
                mutate(RATING = round(RATING, 0))  %>%
                ungroup()
        
        # select final matchups
        playoff_participants =
                playoff_priorities %>%
                mutate(playoff_rank = row_number()) %>%
                filter(playoff_rank <= 4) %>%
                ungroup() %>%
                select(SEASON, TEAM, CONFERENCE, CONFERENCE_CHAMPION, WINS, LOSSES, DIFFICULTY, RANK, RATING, playoff_rank)
        
        return(playoff_participants)
        
        
}

# get_playoff_matchups(input_team_season %>%
#                              filter(.id == 4))
# 
# get_playoff_matchups_1 = function(input_team_season) {
#         
#         # get resumes
#         playoff_resumes =  input_team_season %>%
#                 filter(DIVISION == 'fbs') %>%
#                 group_by(.id, SIM_FROM_WEEK, SEASON, SEASON_TYPE, TEAM, CONFERENCE, OUTCOME) %>%
#                 count() %>%
#                 spread(OUTCOME, n) %>%
#                 mutate_at(c("loss", "win"),
#                           ~ replace_na(., 0)) %>%
#                 mutate(P5 = case_when(CONFERENCE %in% c("SEC", "Big Ten", "Pac-12", "Big 12", "ACC") ~ 1,
#                                       TRUE ~ 0)) %>%
#                 mutate(UNDEFEATED_REGULAR = case_when(SEASON_TYPE == 'regular' & loss == 0 & win > 1 ~ 1,
#                                                       TRUE ~ 0)) %>%
#                 mutate(CONFERENCE_CHAMPION = case_when(SEASON_TYPE == 'conference_championship' & win == 1 ~ 1,
#                                                        TRUE ~ 0)) %>%
#                 mutate(CONFERENCE_RUNNER_UP = case_when(SEASON_TYPE == 'conference_championship' & win == 0 ~ 1,
#                                                         TRUE ~ 0)) %>%
#                 ungroup() %>%
#                 group_by(.id, SIM_FROM_WEEK, SEASON, TEAM, CONFERENCE, P5) %>%
#                 summarize(WINS = sum(win),
#                           LOSSES = sum(loss),
#                           UNDEFEATED_REGULAR = sum(UNDEFEATED_REGULAR),
#                           CONFERENCE_CHAMPION = sum(CONFERENCE_CHAMPION),
#                           CONFERENCE_RUNNER_UP = sum(CONFERENCE_RUNNER_UP),
#                           .groups = 'drop') %>%
#                 # get each team's final elo in each sim
#                 left_join(.,
#                           input_team_season %>%
#                                   group_by(.id, SIM_FROM_WEEK, TEAM) %>%
#                                   filter(WEEK == max(WEEK)) %>%
#                                   select(.id, SIM_FROM_WEEK,  TEAM, POSTGAME_ELO) %>%
#                                   group_by(.id, SIM_FROM_WEEK)  %>%
#                                   arrange(desc(POSTGAME_ELO)) %>%
#                                   mutate(ELO_RANK = row_number()) %>%
#                                   ungroup(),
#                           by = c(".id", "SIM_FROM_WEEK", "TEAM")) %>%
#                 # now get the average of each team's opponent's final Elo
#                 left_join(.,
#                           input_team_season %>%
#                                   left_join(., 
#                                             input_team_season %>%
#                                                     group_by(.id, SIM_FROM_WEEK, TEAM) %>%
#                                                     filter(WEEK == max(WEEK)) %>%
#                                                     mutate(LAST_ELO = POSTGAME_ELO) %>%
#                                                     select(.id, SIM_FROM_WEEK, SEASON, TEAM, LAST_ELO) %>%
#                                                     rename(OPPONENT = TEAM,
#                                                            OPPONENT_LAST_ELO = LAST_ELO),
#                                             by = c(".id", "SIM_FROM_WEEK", "SEASON", "OPPONENT")) %>%
#                                   group_by(.id, SIM_FROM_WEEK,  SEASON, TEAM) %>%
#                                   summarize(SCHEDULE = mean(OPPONENT_LAST_ELO, na.rm=T),
#                                             .groups = 'drop') %>%
#                                   group_by(.id, SIM_FROM_WEEK, SEASON) %>%
#                                   arrange(desc(SCHEDULE)) %>%
#                                   mutate(DIFFICULTY = row_number()) %>%
#                                   ungroup(),
#                           by = c(".id","SIM_FROM_WEEK", "SEASON", "TEAM")) %>%
#                 arrange(desc(WINS))
#         
#         # now determine playoff selection
#         playoff_priorities = 
#                 playoff_resumes %>%
#                 # p5 teams that went undefeated and were conference champions
#                 filter(UNDEFEATED_REGULAR == 1 & P5 == 1 & CONFERENCE_CHAMPION == 1) %>%
#                 mutate(playoff_priority = 1) %>%
#                 # independent P5 teams that went undefeated (ie, notre dame)
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(UNDEFEATED_REGULAR == 1 & P5 == 1 & CONFERENCE == 'FBS Independents') %>% 
#                                   mutate(playoff_priority = 2) 
#                 ) %>%
#                 # p5 teams that won their conference with less than one loss
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(P5 == 1 & CONFERENCE_CHAMPION == 1 & LOSSES < 2 & ELO_RANK < 9) %>%
#                                   mutate(playoff_priority = 3)
#                 ) %>%
#                 # p5 teams that won their conference with less than three losses
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(P5 == 1 & LOSSES < 2 & ELO_RANK < 5) %>%
#                                   mutate(playoff_priority = 4)
#                 ) %>%
#                 # undefeated teams that were also conference champions and have a high elo
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(UNDEFEATED_REGULAR == 1 & CONFERENCE_CHAMPION == 1 & ELO_RANK < 5) %>%
#                                   mutate(playoff_priority = 5)
#                 ) %>%
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(P5 == 1 & LOSSES < 3 & ELO_RANK < 5) %>%
#                                   mutate(playoff_priority = 6)
#                 ) %>%
#                 bind_rows(.,
#                           playoff_resumes %>%
#                                   filter(ELO_RANK < 5) %>%
#                                   mutate(playoff_priority = 7)
#                 ) %>%
#                 rename(RATING = POSTGAME_ELO,
#                        RANK = ELO_RANK) %>%
#                 arrange(desc(RATING)) %>%
#                 group_by(.id, SIM_FROM_WEEK, TEAM) %>%
#                 filter(playoff_priority == min(playoff_priority)) %>%
#                 select(.id, SIM_FROM_WEEK, SEASON, TEAM, CONFERENCE, CONFERENCE_CHAMPION, WINS, LOSSES, DIFFICULTY, RANK, RATING, playoff_priority) %>%
#                 arrange(playoff_priority, desc(CONFERENCE_CHAMPION), RANK, desc(RATING), DIFFICULTY) %>%
#                 mutate(RATING = round(RATING, 0))  %>%
#                 ungroup() %>%
#                 arrange(.id, SIM_FROM_WEEK)
#         
#         # select final matchups
#         playoff_participants =
#                 playoff_priorities %>%
#                 arrange(SEASON, .id,  SIM_FROM_WEEK) %>%
#                 group_by(.id, SEASON, SIM_FROM_WEEK) %>%
#                 mutate(playoff_rank = row_number()) %>%
#                 filter(playoff_rank <= 4) %>%
#                 ungroup()
#         
#         # get matchups
#         playoff_matchups = playoff_participants %>%
#                 rename(PREGAME_ELO = RATING) %>%
#                 select(.id, SIM_FROM_WEEK, SEASON, TEAM, CONFERENCE, PREGAME_ELO, playoff_rank) %>%
#                 mutate(PLAYOFF_GAME = case_when(playoff_rank %in% c(1,4) ~ '1v4',
#                                                 playoff_rank %in% c(2,3) ~ '2v3')) %>%
#                 mutate(MATCHUP = case_when(playoff_rank %in% c(1, 2) ~ 'HOME',
#                                            playoff_rank %in% c(3, 4) ~ 'AWAY')) %>%
#                 #   nest(-.id, -PLAYOFF_GAME)
#                 pivot_wider(., id_cols = c(".id", "SIM_FROM_WEEK", "SEASON", "PLAYOFF_GAME"),
#                             names_from = c("MATCHUP"),
#                             values_from = c("TEAM", "PREGAME_ELO","CONFERENCE"))
#         
#         return(playoff_matchups)
#         
#         
# }
# 
