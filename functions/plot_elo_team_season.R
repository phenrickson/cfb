plot_elo_team_season <-
function(sim_team_outcomes,
                                games,
                                team, 
                                season,
                                alpha = 0.5,
                                fbs_average = 1520) {
        
        # fbs elo average
        fbs_average = 1520
        
        # team initial elo
        team_start_elo = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(TEAM == team) %>%
                filter(WEEK == min(WEEK)) %>%
                head(1) %>%
                mutate(PREGAME_ELO = round(PREGAME_ELO, 0)) %>%
                pull(PREGAME_ELO)
                
        
        # team elo rating
        team_temp = 
                sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(TEAM == team) %>%
                filter(SEASON_TYPE == 'regular') %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, HOME_TEAM, AWAY_TEAM, CONFERENCE_CHAMPIONSHIP),
                          by = c("GAME_ID")) %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, HOME_TEAM, HOME_TEAM_ABBR),
                          by = c("GAME_ID", "HOME_TEAM")) %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, AWAY_TEAM, AWAY_TEAM_ABBR),
                          by = c("GAME_ID", "AWAY_TEAM")) %>%
                mutate(TEAM_ABBR = case_when(TEAM == HOME_TEAM ~ HOME_TEAM_ABBR,
                                             TEAM == AWAY_TEAM ~ AWAY_TEAM_ABBR)) %>%
                mutate(OPPONENT_ABBR = case_when(OPPONENT == HOME_TEAM ~ HOME_TEAM_ABBR,
                                                 OPPONENT == AWAY_TEAM ~ AWAY_TEAM_ABBR)) %>%
                filter(CONFERENCE_CHAMPIONSHIP == F) %>%
                mutate(OPPONENT_ABBR = case_when(nchar(OPPONENT) >= 14 ~ OPPONENT_ABBR,
                                            TRUE ~ OPPONENT)) %>%
                mutate(OPPONENT_LABEL = case_when(OPPONENT == HOME_TEAM ~ paste("@","\n", OPPONENT_ABBR, sep=""),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs","\n", OPPONENT_ABBR, sep=""))) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM"))
        
        # team wins based on simulations
        team_wins =   team_temp %>% 
                mutate(win = SIM_MARGIN > 0) %>% 
                group_by(SEASON, GAME, TEAM) %>% 
                summarize(wins = sum(win), games = n(),
                          .groups = 'drop') %>%
                mutate(perc = wins / games) %>%
                mutate(expected_win = case_when(perc > .5 ~ 'W',
                                                TRUE ~ 'L'))
        
        # get team record
        team_record = team_wins %>%
                mutate(wins = case_when(expected_win == 'W' ~ 1,
                                        TRUE ~ 0)) %>%
                summarize(win = sum(wins),
                          games = n()) %>%
                mutate(loss = games - win) %>%
                mutate(record = paste(win, loss, sep="-")) %>%
                pull(record)
                
        # create data for plot
        plot_team_data = team_temp %>%
                bind_rows(.,
                          team_temp %>% 
                                  filter(GAME_DATE == max(GAME_DATE)) %>%
                                  mutate(GAME = max(GAME) +1, 
                                         OPPONENT_LABEL = paste("Season End", "Elo", "\n", sep="\n"),
                                    #     OPPONENT_LABEL = paste("Predicted", "\n", "Record:", "\n", team_record, "\n", "\n"),
                                         PREGAME_ELO = POSTGAME_ELO)) %>%
                left_join(., team_wins, 
                          by = c("SEASON", "GAME", "TEAM")) %>%
                mutate(perc = replace_na(as.character(perc), "")) %>%
                mutate(expected_win = replace_na(as.character(expected_win), "")) %>%
                mutate(PLOT_LABEL = case_when(OPPONENT_LABEL == team_record ~ OPPONENT_LABEL,
                                              TRUE ~ paste(
                                                      #expected_win,
                                              perc, 
                                              OPPONENT_LABEL,
                                              sep = '\n')))
        
        # now make plot
        plot_team_data %>%
                ggplot(., aes(x=GAME,
                              color = name,
                              group = .id,
                              y = PREGAME_ELO)) +
                geom_vline(xintercept = seq(1, max(team_temp$GAME+1)),
                        #   alpha = alpha,
                           linetype = 'dashed',
                           color = 'grey80')+
                # geom_text(aes(label = OPPONENT_LABEL,
                #               x = GAME,
                #               y = 2250),
                #           size = 4)+
                geom_line(alpha = alpha)+
                theme_phil()+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                ggtitle(paste("Simulated Elo Ratings for" , team, season),
                        subtitle = str_wrap("Each line is one result from simulating a team's regular season. Win probabilities displayed above each matchup. Displaying results from 1000 simulations.", 120))+
                coord_cartesian(ylim = c(1100, 2400))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5))+
                ylab("Pregame Elo Rating")+
                xlab("Game")+
                scale_x_continuous(breaks = int_breaks)+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())+
                annotate("text",
                         x=plot_team_data %>% filter(.id == 1) %>% 
                                 pull(GAME),
                         y=2300,
                         label = plot_team_data %>% filter(.id == 1) %>%
                                 pull(PLOT_LABEL),
                         size = 2.5,
                         color = unique(plot_team_data$primary)
                         )+
                geom_hline(yintercept = c(fbs_average),
                                          linetype = 'dashed',
                                          color = 'grey60')+
                annotate("text",
                         x=0.25,
                         y=fbs_average + 25,
                         color = 'grey60',
                         label = 'FBS Average')
        
}
