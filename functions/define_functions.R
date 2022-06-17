# define functions

# display scorelines
# input_plays_data = play by play level data
# input_games_data = game level data
# input_game_ids = vector of game IDs to display, can be one or multiple
plot_scoreline_func = function(input_plays_data, 
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

dump("plot_scoreline_func",
     file = here::here("functions", "plot_scoreline_func.R"))


# create function to clean up the clock JSON field manually
# x is a df with CLOCK and PERIOD features
make_time_features_func = function(x) {
        
        
        x %>%
                mutate(TIME = gsub('\\{|}|"', '', CLOCK)) %>%
                separate(TIME, into=c("MINUTES", "SECOND"), sep=",") %>%
                mutate(MINUTES = as.numeric(gsub("minutes:", "", MINUTES))) %>% 
                mutate(SECOND = as.numeric(gsub("seconds:", "", SECOND))) %>%
                mutate(MINUTES_IN_HALF = case_when(PERIOD == 1 ~ MINUTES+ 15,
                                                   PERIOD == 2 ~ MINUTES,
                                                   PERIOD == 3 ~ MINUTES + 15,
                                                   PERIOD == 4 ~ MINUTES)) %>%
                mutate(SECONDS_IN_HALF = MINUTES_IN_HALF*60 + SECOND)
        #    mutate(SECONDS_IN_HALF = )
        
}

dump("make_time_features_func",
     file = here::here("functions", "make_time_features_func.R"))

# clean plays func

### periods
# if PERIOD = 0 and there is less than (or eqaul to) 1 character in the PLAY TEXT field, we'll drop the play.
# otherwise, if PERIOD is equal to zero then fill it in with the previous play's PERIOD.
### downs
# if down isn't 1,2,3,4, set to -1 to indicate special teams
### score line

clean_plays_func = function(input_plays_data) {
        
        input_plays_data %>%
                mutate(ID = as.numeric(ID)) %>%
                group_by(GAME_ID) %>%
                arrange(ID) %>% 
                # clean up PERIOD by game
                mutate(STATUS_PERIOD = case_when(PERIOD == 0 & nchar(PLAY_TEXT) <=1 ~ 'Drop',
                                                PERIOD == 0 ~ 'Take Previous Value',
                                                TRUE ~ 'No Change')) %>%
                filter(STATUS_PERIOD != 'Drop') %>%
                mutate(PERIOD = case_when(STATUS_PERIOD == 'Take Previous Value' ~ lag(PERIOD, 1),
                                          TRUE ~ PERIOD)) %>%
                ungroup() %>% 
                # then clean up the down
                mutate(STATUS_DOWN = case_when(DOWN %in% c(1, 2, 3, 4) ~ 'Regular Down',
                                               TRUE ~ 'Change to Special Teams')) %>%
                mutate(DOWN = case_when(DOWN %in% c(1, 2, 3, 4) ~ DOWN,
                                        TRUE ~ -1)) %>%
                # flag yard lines that are outside correct
                mutate(STATUS_YARD_LINE = case_when(YARD_LINE < 0 | YARD_LINE > 100 ~ 'Invalid',
                                                    TRUE ~ 'Valid'))
                # # then fix score line
                # group_by(GAME_ID) %>%
                # mutate(IS_HOME_OFFENSE = case_when(OFFENSE == HOME  ~ T,
                #                                    OFFENSE == AWAY ~ F)) %>%
                # mutate(HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ OFFENSE_SCORE,
                #                               IS_HOME_OFFENSE == F ~ DEFENSE_SCORE),
                #        AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ OFFENSE_SCORE,
                #                               IS_HOME_OFFENSE == T ~ DEFENSE_SCORE)) %>%
                # mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                #        AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1),
                #        AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1)) %>%
                # # remove records where both scores have changed too dramatically
                # filter(!(HOME_SCORE_DIFF > 8 & AWAY_SCORE_DIFF > 8)) %>%
                # mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                #        AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1),
                #        AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1)) %>%
                # mutate(STATUS_SCORE = case_when(HOME_SCORE_DIFF > 8 ~ 'Home Score Increase Too High',
                #                                 AWAY_SCORE_DIFF > 8 ~ 'Away Score Increase Too High',
                #                                 (HOME_SCORE_DIFF > 0 | AWAY_SCORE_DIFF > 0) & SCORING ==F ~ 'Score Increase without Scoring Flag',
                #                                 (HOME_SCORE ==0 & AWAY_SCORE ==0) & lag(HOME_SCORE_MAX, 1) > 0 & lag(HOME_SCORE_MAX, 1) > 0 ~ 'Home and Away Scores Less than Previous Scores',
                #                                 HOME_SCORE < (lag(HOME_SCORE_MAX, 1)) ~ 'Home Score Less Than Previous Max Home Score',
                #                                 AWAY_SCORE < (lag(AWAY_SCORE_MAX, 1))  ~ 'Away Score Less Than Previous Max Away Score',
                #                                 TRUE ~ 'No Change')) %>%
                # mutate(HOME_SCORE = case_when(STATUS_SCORE == 'Home Score Increase Too High' ~ lag(HOME_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Home Score Less Than Previous Max Home Score' ~ lag(HOME_SCORE_MAX, 1),
                #                               TRUE ~ HOME_SCORE),
                #        AWAY_SCORE = case_when(STATUS_SCORE == 'Away Score Increase Too High' ~ lag(AWAY_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Away Score Less Than Previous Max Away Score' ~ lag(AWAY_SCORE_MAX, 1),
                #                               TRUE ~ AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                #        AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1),
                #        AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1)) %>%
                # mutate(STATUS_SCORE = case_when(HOME_SCORE_DIFF > 8 ~ 'Home Score Increase Too High',
                #                                 AWAY_SCORE_DIFF > 8 ~ 'Away Score Increase Too High',
                #                                 (HOME_SCORE_DIFF > 0 | AWAY_SCORE_DIFF > 0) & SCORING ==F ~ 'Score Increase without Scoring Flag',
                #                                 (HOME_SCORE ==0 & AWAY_SCORE ==0) & lag(HOME_SCORE_MAX, 1) > 0 & lag(HOME_SCORE_MAX, 1) > 0 ~ 'Home and Away Scores Less than Previous Scores',
                #                                 HOME_SCORE < (lag(HOME_SCORE_MAX, 1)) ~ 'Home Score Less Than Previous Max Home Score',
                #                                 AWAY_SCORE < (lag(AWAY_SCORE_MAX, 1))  ~ 'Away Score Less Than Previous Max Away Score',
                #                                 TRUE ~ 'No Change')) %>%
                # mutate(HOME_SCORE = case_when(STATUS_SCORE == 'Home Score Increase Too High' ~ lag(HOME_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Home Score Less Than Previous Max Home Score' ~ lag(HOME_SCORE_MAX, 1),
                #                               TRUE ~ HOME_SCORE),
                #        AWAY_SCORE = case_when(STATUS_SCORE == 'Away Score Increase Too High' ~ lag(AWAY_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Away Score Less Than Previous Max Away Score' ~ lag(AWAY_SCORE_MAX, 1),
                #                               TRUE ~ AWAY_SCORE)) %>%
                # mutate(STATUS_SCORE = case_when(HOME_SCORE_DIFF > 8 ~ 'Home Score Increase Too High',
                #                                 AWAY_SCORE_DIFF > 8 ~ 'Away Score Increase Too High',
                #                                 (HOME_SCORE_DIFF > 0 | AWAY_SCORE_DIFF > 0) & SCORING ==F ~ 'Score Increase without Scoring Flag',
                #                                 (HOME_SCORE ==0 & AWAY_SCORE ==0) & lag(HOME_SCORE_MAX, 1) > 0 & lag(HOME_SCORE_MAX, 1) > 0 ~ 'Home and Away Scores Less than Previous Scores',
                #                                 HOME_SCORE < (lag(HOME_SCORE_MAX, 1)) ~ 'Home Score Less Than Previous Max Home Score',
                #                                 AWAY_SCORE < (lag(AWAY_SCORE_MAX, 1))  ~ 'Away Score Less Than Previous Max Away Score',
                #                                 TRUE ~ 'No Change')) %>%
                # ungroup()
                
                
                # mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                #        AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1),
                #        AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1)) %>%
                #  mutate(STATUS_SCORE = case_when(HOME_SCORE_DIFF > 8 & AWAY_SCORE_DIFF > 8 ~ 'Home and Away Score Increase Too High',
                #                                 HOME_SCORE_DIFF > 8 ~ 'Home Score Increase Too High',
                #                                 AWAY_SCORE_DIFF > 8 ~ 'Away Score Increase Too High',
                #                                 (HOME_SCORE_DIFF > 0 | AWAY_SCORE_DIFF > 0) & SCORING ==F ~ 'Score Increase without Scoring Flag',
                #                                 (HOME_SCORE ==0 & AWAY_SCORE ==0) & lag(HOME_SCORE_MAX, 1) > 0 & lag(HOME_SCORE_MAX, 1) > 0 ~ 'Home and Away Scores Less than Previous Scores',
                #                                 HOME_SCORE < (lag(HOME_SCORE_MAX, 1) | lag(HOME_SCORE_MAX, 2) | lag(HOME_SCORE_MAX, 3) | lag(HOME_SCORE_MAX, 4)) ~ 'Home Score Less Than Previous Max Home Score',
                #                                 AWAY_SCORE < (lag(AWAY_SCORE_MAX, 1) | lag(AWAY_SCORE_MAX, 2) | lag(AWAY_SCORE_MAX, 3) | lag(AWAY_SCORE_MAX, 4))  ~ 'Away Score Less than Previous Max Away Score',
                #                                 TRUE ~ 'No Change')) %>%
                # mutate(HOME_SCORE = case_when(STATUS_SCORE == 'Home Score Increase Too High' ~ lag(HOME_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Home Score Less Than Previous Max Home Score' ~ lag(HOME_SCORE_MAX, 1),
                #                               TRUE ~ HOME_SCORE),
                #        AWAY_SCORE = case_when(STATUS_SCORE == 'Away Score Increase Too High' ~ lag(AWAY_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Away Score Less Than Previous Max Away Score' ~ lag(AWAY_SCORE_MAX, 1),
                #                               TRUE ~ AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                #        AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1),
                #        AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1)) %>%
                # mutate(STATUS_SCORE = case_when(HOME_SCORE_DIFF > 8 & AWAY_SCORE_DIFF > 8 ~ 'Home and Away Score Increase Too High',
                #                                 HOME_SCORE_DIFF > 8 ~ 'Home Score Increase Too High',
                #                                 AWAY_SCORE_DIFF > 8 ~ 'Away Score Increase Too High',
                #                                 (HOME_SCORE_DIFF > 0 | AWAY_SCORE_DIFF > 0) & SCORING ==F ~ 'Score Increase without Scoring Flag',
                #                                 (HOME_SCORE ==0 & AWAY_SCORE ==0) & lag(HOME_SCORE_MAX, 1) > 0 & lag(HOME_SCORE_MAX, 1) > 0 ~ 'Home and Away Scores Less than Previous Scores',
                #                                 HOME_SCORE < (lag(HOME_SCORE_MAX, 1) | lag(HOME_SCORE_MAX, 2) | lag(HOME_SCORE_MAX, 3) | lag(HOME_SCORE_MAX, 4)) ~ 'Home Score Less Than Previous Max Home Score',
                #                                 AWAY_SCORE < (lag(AWAY_SCORE_MAX, 1) | lag(AWAY_SCORE_MAX, 2) | lag(AWAY_SCORE_MAX, 3) | lag(AWAY_SCORE_MAX, 4))  ~ 'Away Score Less than Previous Max Away Score',
                #                                 TRUE ~ 'No Change')) %>%
                # mutate(HOME_SCORE = case_when(STATUS_SCORE == 'Home Score Increase Too High' ~ lag(HOME_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Home Score Less Than Previous Max Home Score' ~ lag(HOME_SCORE_MAX, 1),
                #                               TRUE ~ HOME_SCORE),
                #        AWAY_SCORE = case_when(STATUS_SCORE == 'Away Score Increase Too High' ~ lag(AWAY_SCORE_MAX, 1),
                #                               STATUS_SCORE == 'Away Score Less Than Previous Max Away Score' ~ lag(AWAY_SCORE_MAX, 1),
                #                               TRUE ~ AWAY_SCORE)) %>%
                # ungroup()
                                                
}

dump("clean_plays_func", 
     file = here::here("functions", "clean_plays_func.R"))

# plays_data_raw %>%
#         filter(GAME_ID == 322592567) %>%
#         clean_plays_func %>%
#         View()
#         


# function to define sequences

define_score_events_func = function(input_plays_data) {
        
        equences_data_raw =
                plays_data_raw %>%
                filter(PERIOD %in% c(1,2,3,4)) %>%
                mutate(HALF = case_when(PERIOD == 1 | PERIOD == 2 ~ 1,
                                        PERIOD == 3 | PERIOD == 4 ~ 2)) %>%
                mutate(ID = as.numeric(ID),
                       DRIVE_ID = as.numeric(DRIVE_ID),
                       GAME_ID = as.numeric(GAME_ID)) %>%
                rename(PLAY_ID = ID) %>%
                group_by(GAME_ID) %>%
                arrange(GAME_ID, DRIVE_ID, PLAY_ID) %>%
                mutate(GAME_PLAY_NUMBER = row_number()) %>%
                mutate(IS_HOME_OFFENSE = case_when(OFFENSE == HOME  ~ T,
                                                   OFFENSE == AWAY ~ F)) %>%
                mutate(HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == F ~ DEFENSE_SCORE),
                       AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == T ~ DEFENSE_SCORE)) %>%
                mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                       AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE_MAX, 1, 0),
                       AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE_MAX, 1, 0)) %>%
                mutate(HOME_SCORE_FIX = case_when((HOME_SCORE < lag(HOME_SCORE_MAX, 1, 0) | 
                                                           HOME_SCORE_DIFF > 8) ~ lag(HOME_SCORE_MAX, 1, 0),
                                                  TRUE ~ HOME_SCORE),
                       AWAY_SCORE_FIX = case_when((AWAY_SCORE < lag(AWAY_SCORE_MAX, 1, 0) | 
                                                           AWAY_SCORE_DIFF > 8) ~ lag(AWAY_SCORE_MAX, 1, 0),
                                                  TRUE ~ AWAY_SCORE)) %>%
                group_by(GAME_ID, DRIVE_ID, DRIVE_NUMBER) %>%
                group_by(GAME_ID, HALF) %>%
                mutate(FINAL_HALF_PLAY = n(),
                       HALF_PLAY_NUMBER = row_number()) %>% 
                mutate(IS_FINAL_HALF_PLAY = case_when(HALF_PLAY_NUMBER == FINAL_HALF_PLAY ~ T,
                                                      TRUE ~ F)) %>%
                filter(SCORING == T | IS_FINAL_HALF_PLAY == T) %>%
                select(GAME_ID, DRIVE_ID, PLAY_ID, HALF, PERIOD, HOME, AWAY, IS_HOME_OFFENSE, DRIVE_NUMBER, PLAY_NUMBER, SCORING, IS_FINAL_HALF_PLAY,
                       HOME_SCORE, HOME_SCORE_FIX,
                       AWAY_SCORE, AWAY_SCORE_FIX) %>%
                group_by(GAME_ID, DRIVE_ID) %>%
                filter(HOME_SCORE == max(HOME_SCORE)
                       & AWAY_SCORE == max(AWAY_SCORE)) %>% 
                filter(PLAY_NUMBER == max(PLAY_NUMBER)) %>%
                group_by(GAME_ID) %>%
                mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1, default=0),
                       AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1, default=0)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE_FIX - lag(HOME_SCORE_FIX, 1, default=0),
                #        AWAY_SCORE_DIFF = AWAY_SCORE_FIX - lag(AWAY_SCORE_FIX, 1, default=0)) %>%
                mutate(HOME_SCORE_DIFF = case_when(HOME_SCORE_DIFF > 7 ~ 7,
                                                   HOME_SCORE_DIFF < 0 ~ 0,
                                                   TRUE ~ HOME_SCORE_DIFF),
                       AWAY_SCORE_DIFF = case_when(AWAY_SCORE_DIFF > 7 ~ 7,
                                                   AWAY_SCORE_DIFF < 0 ~ 0,
                                                   TRUE ~ AWAY_SCORE_DIFF)) %>%
                mutate(NEXT_SCORE_DIFF = case_when(HOME_SCORE_DIFF != 0 ~ HOME_SCORE_DIFF,
                                                   HOME_SCORE_DIFF ==0 ~ -1*AWAY_SCORE_DIFF)) %>%
                mutate(NEXT_SCORE_DIFF = case_when(NEXT_SCORE_DIFF < 7 & NEXT_SCORE_DIFF > 3 ~ 7,
                                                   NEXT_SCORE_DIFF < 2 & NEXT_SCORE_DIFF > 0  ~7,
                                                   NEXT_SCORE_DIFF >-7 & NEXT_SCORE_DIFF < -3 ~ -7,
                                                   NEXT_SCORE_DIFF >-2 & NEXT_SCORE_DIFF <0 ~ -7,
                                                   TRUE ~ NEXT_SCORE_DIFF)) %>%
                ungroup() %>%
                group_by(GAME_ID) %>%
                mutate(SEQUENCE_NUMBER = row_number()) %>%
                ungroup()
        
}


