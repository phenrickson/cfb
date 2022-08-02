plot_elo_historical_team <-
function(elo_ratings,
                                    min_year = 1869,
                                    team,
         smooth = F,
                                    input_teamcolors = teamcolors) {
        
        min_date = as.Date(paste(min_year, "01", "01", sep="-"))
        min_year = year(min_date)
        max_year = max(year(elo_ratings$GAME_DATE))
        
        all_teams_data = elo_ratings %>%
                filter(SEASON > min_year) %>%
                filter(DIVISION == 'fbs') 
        
        all_teams_median = all_teams_data %>%
                summarize(median = median(POSTGAME_ELO)) %>%
                pull(median)
        
        p = all_teams_data %>%
                ggplot(., aes(x=GAME_DATE,
                              by = TEAM,
                              y= POSTGAME_ELO))+
                geom_point(alpha = 0.004)+
                geom_hline(yintercept = all_teams_median,
                           linetype = 'dashed',
                           color = 'grey60')+
                annotate("text",
                         x = min_date,
                         y=all_teams_median + 40,
                         size = 3,
                         color = 'grey60',
                         label = paste("FBS", "\n", "Median"))
        
        team_data = elo_ratings %>%
                filter(SEASON > min_year) %>%
                filter(TEAM %in% team) %>%
                left_join(., input_teamcolors,
                          by = c("TEAM")) %>%
                mutate(SELECTED_TEAM = TEAM) %>%
                group_by(TEAM) %>%
                mutate(TEAM_LABEL = case_when(GAME_DATE == max(GAME_DATE) ~ TEAM)) %>%
                ungroup()
        
        
        if(smooth == T ) {p = p + geom_line(data =team_data,
                                  aes(color = name),
                                  stat = 'smooth',
                                  formula = 'y ~ x',
                                  method = 'loess',
                                  alpha =0.85,
                                  span = 0.2,
                                  lwd = 1.04)}
        else {p = p}
        
        p + 
                geom_text_repel(data =team_data,
                                aes(label = TEAM_LABEL,
                                    color = name),
                        fontface = "bold",
                        size = 4,
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
                geom_point(data = team_data,
                           aes(color = name),
                           alpha  =0.4)+
                theme_phil()+
                ggtitle(paste("Historical Elo Rating(s) for", paste(team, collapse="-")),
                        subtitle = str_wrap(paste('Displaying Postagme Elo Ratings for all FBS teams from',
                                                  min_year, 'to present. Highlighted points/line indicate selected team.'), 120))+
                theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 12))+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                xlab("Game Date")+
                ylab("Postgame Elo Rating")+
                scale_x_date(breaks = as.Date(paste(seq(min_year, 2020, 10), "01", "01", sep="-")),
                             #date_breaks = c("10 year"),
                             date_labels = c("%Y"))+
                xlab("Season (Game Date)")
        
}
