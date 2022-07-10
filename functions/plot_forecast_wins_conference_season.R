plot_forecast_wins_conference_season <-
function(actual_team_outcomes,
                                                  midseason_sim_team_outcomes,
                                                  week,
                                                  conference,
                                                season) {

        mode_func <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }
        
        # create span func
        span_func = function(week) {
                
                if (week < 2) {span = 1} else 
                        if (week < 3) {span = 0.9} else 
                                if (week < 8) {span = 0.7} else 
                                        if (week < 11) {span = 0.4} else
                                                if (week < 17) {span = 0.2}
                
                if (week < 2) {method = 'lm'} else 
                                if (week < 17) {method = 'loess'}
                
                return(list("span" = span,
                            "method" = method))
        }
        
        
        week_wins = actual_team_outcomes %>%
                filter(SEASON == season) %>%
                arrange(GAME_DATE) %>%
                filter(!is.na(CONFERENCE)) %>%
                mutate(win = case_when(POINTS > OPPONENT_POINTS ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(SEASON, TEAM, WEEK) %>%
                summarise(win = sum(win),
                          .groups = 'drop') %>%
                rename(SIM_WEEK = WEEK)
        
        midseason_win_forecast =  midseason_sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(!is.na(CONFERENCE)) %>%
                mutate(sim_win = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SIM_WEEK, SEASON, CONFERENCE, TEAM) %>%
                summarize(sim_remaining_wins = sum(sim_win),
                          .groups = 'drop') %>%
                left_join(., week_wins,
                          by= c("SEASON", "TEAM", "SIM_WEEK")) %>%
                mutate(win = replace_na(win, 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                arrange(SIM_WEEK) %>%
                mutate(wins_so_far = cumsum(win)) %>%
             #   filter(TEAM == 'Wisconsin') %>%
                mutate(sim_total_wins = sim_remaining_wins + wins_so_far) %>%
                select(-win) %>%
                ungroup() %>%
                group_by(SIM_WEEK, SEASON, CONFERENCE, TEAM) %>%
                summarize(mean_total_wins = mean(sim_total_wins),
                          .groups = 'drop')
                # summarize(quantile = c("perc_low", "perc_mid", "perc_upper"),
                #           wins = quantile(sim_total_wins, c(0.1, 0.5, 0.9)),
                #           .groups ='drop')
        
        midseason_win_forecast %>%
                filter(SIM_WEEK <= week) %>%
                filter(CONFERENCE == conference) %>%
              #  filter(quantile == 'perc_mid') %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                group_by(SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                mutate(TEAM_LABEL = case_when(GAME == max(GAME) ~ TEAM)) %>%
                ggplot(., aes(x=SIM_WEEK,
                              y = mean_total_wins,
                              label = TEAM_LABEL,
                              color = name,
                              fill = TEAM)) +
                geom_text_repel(
                        fontface = "bold",
                        size = 3,
                        direction = "y",
                        hjust = -1.2, 
                        segment.size = .7,
                        segment.alpha = .5,
                        segment.linetype = "dotted",
                        box.padding = .4,
                        segment.curvature = -0.1,
                        segment.ncp = 3,
                        segment.angle = 20
                ) +
                geom_line(stat = 'smooth',
                            formula = 'y ~ x',
                            method = span_func(week)$method,
                          #  alpha =0.75,
                          alpha = 0.7,
                            span = span_func(week)$span,
                            lwd = 1.1)+
                theme_phil()+
                scale_fill_teams(name = "TEAM")+
                scale_color_teams(name = "TEAM")+
                scale_x_continuous(limits = c(-1, 14),
                                   breaks = seq(0, 12, 3))+
                scale_y_continuous(limits = c(0, 12),
                                   breaks = seq(2, 12, 2))+
                guides(fill = "none",
                       color = "none",
                       alpha = "none",
                       linetype = 'none')+
                xlab("Week of Simulation")+
                ylab("Expected Season Win Total")+
            #    facet_wrap(paste(CONFERENCE, SEASON) ~.)+
                ggtitle(paste("Season Win Totals for", conference, season, "Season", "\n",
                              "Simulating as of Week", week),
                        subtitle = str_wrap(" Displaying average total wins for each team in 1000 simulations of the remaining games in the season. Simulations run at the beginning of the season and after every week.", 90))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 10,
                                                  hjust = 0.5))+
                geom_vline(xintercept = 12,
                           linetype = 'dashed',
                           color = 'grey30')
}
