get_drive_score_events <-
function(input_drives) {
        
        input_drives %>%
                group_by(GAME_ID) %>%
                arrange(DRIVE_ID) %>%
                mutate(HALF = case_when(START_PERIOD == 1 | START_PERIOD == 2 ~ 'First Half',
                                        START_PERIOD == 3 | START_PERIOD == 4 ~ 'Second Half',
                                        TRUE ~ 'OT')) %>%
                mutate(HOME = case_when(IS_HOME_OFFENSE == T ~ OFFENSE,
                                        IS_HOME_OFFENSE ==F ~ DEFENSE),
                       AWAY = case_when(IS_HOME_OFFENSE == T  ~ DEFENSE,
                                        IS_HOME_OFFENSE ==F ~ OFFENSE)) %>%
                mutate(END_HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ END_OFFENSE_SCORE,
                                                  IS_HOME_OFFENSE == F ~ END_DEFENSE_SCORE),
                       END_AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ END_OFFENSE_SCORE,
                                                  IS_HOME_OFFENSE == T ~ END_DEFENSE_SCORE)) %>%
                group_by(GAME_ID, HALF) %>%
                mutate(LAST_DRIVE_HALF = case_when(DRIVE_NUMBER == max(DRIVE_NUMBER) ~ T,
                                                   TRUE ~ F)) %>%
                ungroup() %>%
                group_by(GAME_ID) %>%
                mutate(HOME_SCORE_DIFF = END_HOME_SCORE - lag(END_HOME_SCORE, 1, default=0),
                       AWAY_SCORE_DIFF = END_AWAY_SCORE - lag(END_AWAY_SCORE, 1, default=0)) %>%
                mutate(MULTIPLE_SCORE_CHANGES = HOME_SCORE_DIFF & AWAY_SCORE_DIFF > 0) %>%
                mutate(PROBLEM_AWAY_CHANGE = grepl("rush td|pass td|rushing td|passing td|fg good", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == T & AWAY_SCORE_DIFF > 0,
                       PROBLEM_HOME_CHANGE = grepl("rush td|pass td|rushing td|passing td|fg good", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == F & HOME_SCORE_DIFF > 0) %>%
                mutate(SCORE_EVENT = case_when((tolower(DRIVE_RESULT) == 'td' | grepl("rush td|pass td|rushing td|passing td", tolower(DRIVE_RESULT))) & IS_HOME_OFFENSE == T ~ 'HOME TD',
                                               (tolower(DRIVE_RESULT) == 'td' | grepl("rush td|pass td|rushing td|passing td", tolower(DRIVE_RESULT))) & IS_HOME_OFFENSE == F ~ 'AWAY TD',
                                               grepl("rush td|pass td|rushing td|passing td", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == T ~ 'HOME TD',
                                               grepl("rush td|pass td|rushing td|passing td", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == F ~ 'AWAY TD',
                                               grepl("kickoff return td|kickoff td|kick return td|punt return td", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE ==T ~ 'AWAY TD',
                                               grepl("kickoff return td|kickoff td|kick return td|punt return td", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE ==F ~ 'HOME TD',
                                               grepl("int return touch", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == T ~ 'AWAY_TD',
                                               grepl("int return touch", tolower(DRIVE_RESULT)) & IS_HOME_OFFENSE == F ~ 'HOME_TD',
                                               (tolower(DRIVE_RESULT) == 'fg' | grepl("made fg|fg good", tolower(DRIVE_RESULT))) & IS_HOME_OFFENSE == T ~ 'HOME FG',
                                               (tolower(DRIVE_RESULT) == 'fg' | grepl("made fg|fg good", tolower(DRIVE_RESULT))) & IS_HOME_OFFENSE == F ~ 'AWAY FG',
                                               HOME_SCORE_DIFF == 6 | HOME_SCORE_DIFF == 7 | HOME_SCORE_DIFF == 8 ~ 'HOME TD',
                                               HOME_SCORE_DIFF == 3 ~ 'HOME FG',
                                               HOME_SCORE_DIFF == 2 ~ 'HOME Safety',
                                               AWAY_SCORE_DIFF == 6 | AWAY_SCORE_DIFF == 7 | AWAY_SCORE_DIFF == 8 ~ 'AWAY TD',
                                               AWAY_SCORE_DIFF == 3 ~ 'AWAY FG',
                                               AWAY_SCORE_DIFF == 2 ~ 'AWAY Safety', 
                                               (HOME_SCORE_DIFF == 0 & AWAY_SCORE_DIFF == 0) & LAST_DRIVE_HALF == T ~ 'No_Score',
                                               (SCORING == T & tolower(DRIVE_RESULT)=='punt' & IS_HOME_OFFENSE == T) ~ 'AWAY_TD',
                                               (SCORING == T & tolower(DRIVE_RESULT)=='punt' & IS_HOME_OFFENSE == F) ~ 'HOME_TD')) %>%
                mutate(MISSING_SCORING_EVENT = SCORING == T & is.na(SCORE_EVENT)) %>%
                #  filter(MISSING_SCORING_EVENT == T) %>%
                mutate(NEXT_SCORE_EVENT = SCORE_EVENT) %>%
                # this fills in previous drives with no scoring event with the next observed scoring event in a half
                fill(NEXT_SCORE_EVENT, .direction = "up") %>%
                ungroup() 
}
