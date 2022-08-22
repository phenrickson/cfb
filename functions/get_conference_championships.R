get_conference_championships = function(input_team_season,
                                        conference_championship_games,
                                        conferences_with_divisions,
                                        conferences_without_divisions){
        
        # team final regular season elos
        team_elos = input_team_season %>%
                group_by(TEAM) %>%
                filter(GAME_DATE == max(GAME_DATE)) %>%
                ungroup() %>%
                mutate(PREGAME_ELO = POSTGAME_ELO) %>%
                select(SEASON, TEAM, PREGAME_ELO)
        
        # get head to head results for conference opponents
        head_to_head_winners = input_team_season %>%
                filter(CONFERENCE_GAME == T) %>%
                mutate(winner = case_when(OUTCOME == 'win' ~ TEAM,
                                          OUTCOME == 'loss' ~ OPPONENT))
        
        # find division winners for conferences with divisions
        division_winners = input_team_season %>%
                # filter to conferences
                filter(CONFERENCE %in% conferences_with_divisions) %>%
                # get types of wins for each team
                mutate(conference_win = case_when(CONFERENCE_GAME == T & OUTCOME == 'win' ~ 1,
                                                  TRUE ~ 0),
                       division_win = case_when(CONFERENCE_DIVISION_GAME == T & OUTCOME == 'win' ~ 1,
                                                TRUE ~ 0),
                       win = case_when(OUTCOME == 'win' ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(SEASON, TEAM, CONFERENCE, CONFERENCE_DIVISION) %>%
                summarize(wins = sum(win),
                          division_wins = sum(division_win),
                          conference_wins = sum(conference_win),
                          .groups = 'drop') %>%
                # then, if more than one, get top team in each division with most conference wins
                group_by(SEASON, CONFERENCE, CONFERENCE_DIVISION) %>%
                slice_max(order_by = conference_wins, n = 1, with_ties=T) %>%
                # get team in each division with most division wins with ties
                group_by(SEASON, CONFERENCE, CONFERENCE_DIVISION) %>%
                slice_max(order_by = division_wins, n = 1, with_ties=T) %>% 
                group_by(SEASON, CONFERENCE, CONFERENCE_DIVISION) %>% 
                mutate(participants = n()) %>%
                ungroup() 
        
        # get those from each division in which there is one team
        division_booked = division_winners %>%
                filter(participants == 1)
        
        # check if any spots are remaining
        division_remaining = conference_championship_games %>%
                filter(CONFERENCE %in% conferences_with_divisions) %>%
                left_join(., 
                          division_booked %>%
                                  group_by(CONFERENCE, CONFERENCE_DIVISION) %>%
                                  count() %>%
                                  rename(spots_taken = n),
                          by = c("CONFERENCE", "CONFERENCE_DIVISION")) %>%
                mutate(spots_left = 1- replace_na(spots_taken, 0)) %>%
                filter(spots_left > 0) %>%
                select(CONFERENCE, CONFERENCE_DIVISION, spots_left)
        
        # for tiebreakers...
        # get head to head
        # if more, then randommly select
        if(nrow(division_remaining) ==0) {division_tiebreakers = tibble()} else {
                
                division_tiebreakers = division_winners %>%
                        filter(participants > 1) %>%
                        left_join(.,
                                  division_winners %>%
                                          filter(participants > 1) %>%
                                          rename(OPPONENT = TEAM) %>%
                                          select(SEASON, CONFERENCE, CONFERENCE_DIVISION, OPPONENT),
                                  by = c("SEASON", "CONFERENCE", "CONFERENCE_DIVISION")) %>%
                        filter(TEAM != OPPONENT) %>%
                        left_join(.,
                                  head_to_head_winners,
                                  by = c("SEASON", "TEAM", "OPPONENT", "CONFERENCE", "CONFERENCE_DIVISION")) %>%
                        filter(TEAM == winner) %>%
                        select(SEASON, TEAM, CONFERENCE, CONFERENCE_DIVISION) %>%
                        arrange(SEASON, CONFERENCE) %>%
                        group_by(SEASON, CONFERENCE, CONFERENCE_DIVISION) %>%
                        mutate(tiebreaker_priority = 1) %>%
                        slice_min(., tiebreaker_priority, n = 1, with_ties = F) %>%
                        left_join(., division_remaining,
                                  by = c("CONFERENCE", "CONFERENCE_DIVISION")) %>%
                        sample_n(spots_left) %>%
                        select(SEASON, TEAM, CONFERENCE, CONFERENCE_DIVISION) %>%
                        ungroup()
                        
        }
        
        # now get winners for conferences without 
        conference_winners = 
                input_team_season %>%
                filter(CONFERENCE %in% conferences_without_divisions) %>%
                # get top for each division
                mutate(conference_win = case_when(CONFERENCE_GAME == T & OUTCOME == 'win' ~ 1,
                                                  TRUE ~ 0),
                       win = case_when(OUTCOME == 'win' ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(SEASON,TEAM,  CONFERENCE) %>%
                summarize(wins = sum(win),
                          conference_wins = sum(conference_win),
                          .groups = 'drop') %>%
                group_by(SEASON, CONFERENCE) %>%
                # then, if more than one, get top team in each division with most conference wins
                slice_max(order_by = conference_wins, n = 2, with_ties=T) %>%
                group_by(SEASON, CONFERENCE) %>% 
                mutate(participants = n()) %>%
                mutate(most_conference_wins = case_when(conference_wins == max(conference_wins) ~ 1,
                                                        TRUE ~ 0)) %>%
                mutate(rank = rank(desc(conference_wins),
                                   ties.method = 'average')) %>%
                ungroup()
        
        # if two top teams, then they are in
        conference_booked = conference_winners %>%
                filter(participants == 2 | rank ==1)
        
        # number of spots left
        conference_remaining = conference_championship_games %>%
                filter(CONFERENCE %in% conferences_without_divisions) %>%
                distinct(CONFERENCE) %>%
                left_join(., 
                          conference_booked %>%
                                  group_by(CONFERENCE) %>%
                                  count() %>%
                                  rename(spots_taken = n),
                          by = c("CONFERENCE")) %>%
                mutate(spots_left = 2- replace_na(spots_taken, 0)) %>%
                filter(spots_left > 0) %>%
                select(CONFERENCE, spots_left)
        
        
        # get head to head
        # if more, then randommly select
        if(nrow(conference_remaining) ==0) {conference_tiebreakers = tibble()} else {
                
                conference_tiebreakers = conference_winners %>%
                        filter(participants > 2 & rank > 1) %>%
                        left_join(.,
                                  conference_winners %>%
                                          filter(participants > 2 & rank > 1) %>%
                                          rename(OPPONENT = TEAM) %>%
                                          select(SEASON, CONFERENCE, OPPONENT),
                                  by = c("SEASON", "CONFERENCE")) %>%
                        filter(TEAM != OPPONENT) %>%
                        left_join(.,
                                  head_to_head_winners,
                                  by = c("SEASON", "TEAM", "OPPONENT", "CONFERENCE")) %>%
                        filter(TEAM == winner | is.na(winner)) %>%
                        group_by(SEASON, TEAM, CONFERENCE, winner) %>%
                        count() %>%
                        ungroup() %>%
                        rename(head_to_head_wins = n) %>%
                        left_join(., conference_remaining,
                                  by = c("CONFERENCE")) %>%
                        group_by(SEASON, CONFERENCE) %>%
                        mutate(n = n()) %>%
                        filter(rank(-head_to_head_wins, ties='random') <= spots_left) %>%
                        ungroup()
                        
        }
        
        # join up and confirm
        conference_championship_games_out = conference_championship_games %>%
                left_join(.,
                          bind_rows(division_booked,
                                    division_tiebreakers) %>%
                                  select(SEASON, CONFERENCE, CONFERENCE_DIVISION, TEAM) %>%
                                  bind_rows(
                                          bind_rows(conference_booked,
                                            conference_tiebreakers) %>%
                                            select(SEASON, TEAM, CONFERENCE) %>%
                                            arrange(CONFERENCE) %>%
                                            group_by(CONFERENCE) %>%
                                            mutate(CONFERENCE_DIVISION = paste(row_number()))
                          ),
                          by = c("CONFERENCE", "CONFERENCE_DIVISION")) %>%
                left_join(.,
                          team_elos,
                          by = c("TEAM", "SEASON")) %>%
                select(SEASON, CONFERENCE, CONFERENCE_DIVISION, MATCHUP, TEAM, PREGAME_ELO) %>%
                arrange(CONFERENCE, CONFERENCE_DIVISION)
        
        return(conference_championship_games_out)
        
}
