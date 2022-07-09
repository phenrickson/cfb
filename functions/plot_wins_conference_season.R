plot_wins_conference_season <-
function(sim_team_outcomes,
                                       season,
                                       conference,
                                       week) {
        
        mode_func <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }
        
        
        season_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                filter(CONFERENCE %in% conference) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM, WINS) %>%
                count() %>%
                group_by(SEASON, TEAM) %>%
                mutate(perc = round(n / sum(n),2))
        
        team_win_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                filter(CONFERENCE %in% conference) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
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
                mutate(TEAM_LABEL = paste(paste(TEAM, SEASON),
                                          paste("Mean:", mean),
                                          paste("Mode:", mode),
                                          sep = "\n"))
        
        season_totals %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                left_join(., team_win_totals,
                          by = c("TEAM", "SEASON")) %>%
                mutate(perc = round(perc, 2)) %>%
                mutate(perc = case_when(perc ==0 ~ '<0.01',
                                        TRUE ~ as.character(perc))) %>%
                mutate(TEAM_LABEL = factor(TEAM_LABEL, levels = team_win_totals$TEAM_LABEL)) %>%
                ggplot(., aes(x=WINS,
                              y = n,
                              color = name,
                              label = perc,
                              fill = name))+
                geom_col()+
                geom_text(vjust = -0.5,
                          check_overlap=T,
                          size = 2)+
                theme_phil()+
                facet_wrap(TEAM_LABEL ~.,
                           ncol=4)+
                scale_fill_teams(name = "TEAM")+
                scale_color_teams(name = "TEAM")+
                guides(fill = 'none',
                       color = 'none')+
                ylab("Number of Simulations")+
                xlab("Season Win Total")+
                ggtitle(paste("Simulated Win Totals for", conference, season, "Season"),
                        subtitle = str_wrap(" Displaying total wins for each team in 1000 simulations of selected season.", 120))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 10,
                                                  hjust = 0.5))+
                coord_cartesian(ylim = c(0, max(season_totals$n + 25)))
        
}
