clean_plays_func <-
function(input_plays_data) {
        
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
