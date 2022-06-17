plot_scoreline_func <-
function(input_plays_data, 
                               input_games_data,
                               input_game_ids) {
        
        plot_data = input_plays_data %>% 
                mutate(ID = as.numeric(ID),
                       GAME_ID = as.numeric(GAME_ID)) %>%
                filter(GAME_ID %in% input_game_ids) %>%
                rename(PLAY_ID = ID) %>%
                group_by(GAME_ID) %>%
                arrange(PLAY_ID) %>%
                mutate(GAME_PLAY_NUMBER = row_number()) %>%
                mutate(IS_HOME_OFFENSE = case_when(OFFENSE == HOME  ~ T,
                                                   OFFENSE == AWAY ~ F)) %>%
                mutate(HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == F ~ DEFENSE_SCORE),
                       AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == T ~ DEFENSE_SCORE)) %>%
                select(GAME_ID, DRIVE_ID, PLAY_ID, GAME_PLAY_NUMBER, HOME, AWAY, HOME_SCORE, AWAY_SCORE, PLAY_TEXT, SCORING) %>%
                gather("variable",
                       "value",
                       -GAME_ID, -DRIVE_ID, -PLAY_ID, -PLAY_TEXT, -SCORING,
                       -GAME_PLAY_NUMBER, -HOME, -AWAY) %>%
                left_join(., input_games_data %>%
                                  rename(GAME_ID = ID),
                          by = c("GAME_ID")) %>%
                mutate(GAME_DESCRIPTION = paste(SEASON,
                                                paste("H:", HOME),
                                                paste("A:", AWAY),
                                                paste("ID:", GAME_ID),
                                                sep="\n")) %>%
                mutate(GAME_ID = as.character(GAME_ID)) %>%
                mutate(PROBLEM_SCORE = case_when(value < lag(value, 1) & GAME_PLAY_NUMBER > 1 ~ T)) %>%
                mutate(GAME_ID = as.character(GAME_ID))
        
        
        p = plot_data %>%
                ggplot(., aes(x=GAME_PLAY_NUMBER,
                              by = GAME_ID,
                              y = value,
                              color = variable))+
                geom_line(alpha = 0.8,
                          lwd = 1.05,
                          position=position_jitter(w=0.02, h=0))+
                theme_phil()+
                scale_color_manual(values = c("black", "grey60"))+
                facet_wrap(GAME_DESCRIPTION ~.,
                           ncol = 5)
        
        plot_game = p +
                geom_vline(data = plot_data %>%
                                   filter(PROBLEM_SCORE ==T),
                           aes(xintercept = GAME_PLAY_NUMBER),
                           lwd = 0.8,
                           alpha = 0.8,
                           color = 'red')
        
        
        return(list("plot_data" = plot_data,
                    "plot_game" = plot_game))
        
}
