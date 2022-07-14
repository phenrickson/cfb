plot_elo_conference_season <-
function(sim_team_outcomes,
                                      games,
                                      conference,
                                      season) {
        
        mode_func <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }
        
        team_outcomes_temp = sim_team_outcomes %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, CONFERENCE_CHAMPIONSHIP),
                          by = c("GAME_ID"))  %>%
                filter(SEASON == season) %>%
                filter(CONFERENCE %in% conference) %>%
                filter(CONFERENCE_CHAMPIONSHIP ==F) %>%
                filter(SEASON_TYPE == 'regular') %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                ungroup() %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                left_join(., games %>%
                                  select(GAME_ID, AWAY_TEAM, HOME_TEAM),
                          by = c("GAME_ID")) %>%
                mutate(OPPONENT_LABEL = case_when(OPPONENT == HOME_TEAM ~ paste("@", OPPONENT),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs", OPPONENT)))
        
        
        # mean wins by team
        team_win_totals = sim_team_outcomes %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, CONFERENCE_CHAMPIONSHIP),
                          by = c("GAME_ID"))  %>%
                filter(SEASON == season) %>%
                filter(CONFERENCE %in% conference) %>%
                filter(CONFERENCE_CHAMPIONSHIP ==F) %>%
                filter(SEASON_TYPE == 'regular') %>%
                mutate(WIN = case_when(SIM_MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM) %>%
                summarize(mean = round(mean(WINS),1),
                          mode = mode_func(WINS),
                          sd = sd(WINS),
                          .groups = 'drop') %>%
                arrange(desc(mean)) %>%
                mutate(TEAM_LABEL = factor(paste(TEAM, SEASON)))
        
        #
        plot_teams = team_outcomes_temp %>%
                bind_rows(.,
                          team_outcomes_temp %>%
                                  group_by(TEAM) %>%
                                  filter(GAME_DATE == max(GAME_DATE)) %>%
                                  mutate(GAME = max(GAME) +1, 
                                         OPPONENT_LABEL = 'Season End',
                                         PREGAME_ELO = POSTGAME_ELO)) %>%
                left_join(., team_win_totals,
                          by = c("TEAM", "SEASON")) %>%
                mutate(TEAM_LABEL = factor(TEAM_LABEL,
                                           levels = c(team_win_totals$TEAM_LABEL)))
        
        plot_teams %>%
                ggplot(., aes(x=GAME,
                              color = name,
                              group = .id,
                              y = PREGAME_ELO)) +
                geom_line(alpha = 0.08)+
                # geom_line(stat = 'smooth',
                #           method = 'loess',
                #           formula = 'y ~ x',
                #           span = 0.25,
                #           alpha = 0.5)+
                theme_phil()+
                facet_wrap(TEAM_LABEL~.,
                           ncol =4)+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                ggtitle(paste("Simulated Elo Ratings for", conference, season, "Season"),
                        subtitle = str_wrap("Each line shows one simulation for a team's season. Teams Displaying 1000 simulations.", 120))+
                scale_x_continuous(breaks = int_breaks)+
                coord_cartesian(ylim = c(1100, 2300))+
                theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 12))+
                ylab("Pregame Elo Rating")+
                xlab("Game")
        
}
