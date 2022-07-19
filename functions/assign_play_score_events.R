assign_play_score_events = function(input_plays) {
        
        input_plays %>%
                rename(NEXT_SCORE_EVENT_HOME = NEXT_SCORE_EVENT) %>%
                mutate(NEXT_SCORE_EVENT_OFFENSE = case_when(OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'HOME TD' ~ 'TD',
                                                            OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'AWAY TD' ~ 'Opp_TD',
                                                            OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'HOME FG' ~ 'FG',
                                                            OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'AWAY FG' ~ 'Opp_FG',
                                                            OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'HOME Safety' ~ 'Safety',
                                                            OFFENSE == HOME & NEXT_SCORE_EVENT_HOME == 'AWAY Safety' ~ 'Opp_Safety',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'HOME TD' ~ 'Opp_TD',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'AWAY TD' ~ 'TD',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'HOME FG' ~ 'Opp_FG',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'AWAY FG' ~ 'FG',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'HOME Safety' ~ 'Opp_Safety',
                                                            OFFENSE == AWAY & NEXT_SCORE_EVENT_HOME == 'AWAY Safety' ~ 'Safety',
                                                            NEXT_SCORE_EVENT_HOME == 'No_Score' ~ 'No_Score')) %>%
                mutate(NEXT_SCORE_EVENT_HOME_DIFF = case_when(NEXT_SCORE_EVENT_HOME == 'HOME TD' ~ 7,
                                                              NEXT_SCORE_EVENT_HOME == 'HOME FG' ~ 3,
                                                              NEXT_SCORE_EVENT_HOME == 'HOME Safety' ~ 2,
                                                              NEXT_SCORE_EVENT_HOME == 'No_Score' ~ 0,
                                                              NEXT_SCORE_EVENT_HOME == 'HOME Safety' ~ -2,
                                                              NEXT_SCORE_EVENT_HOME == 'HOME FG' ~ -3,
                                                              NEXT_SCORE_EVENT_HOME == 'HOME TD' ~ -7)) %>%
                mutate(NEXT_SCORE_EVENT_OFFENSE_DIFF = case_when(NEXT_SCORE_EVENT_OFFENSE == 'TD' ~ 7,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'FG' ~ 3,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'Safety' ~ 2,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'No_Score' ~ 0,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'Opp_Safety' ~ -2,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'Opp_FG' ~ -3,
                                                                 NEXT_SCORE_EVENT_OFFENSE == 'Opp_TD' ~ -7)) %>%
                mutate(NEXT_SCORE_EVENT_OFFENSE = factor(NEXT_SCORE_EVENT_OFFENSE,
                                                         levels = c("TD",
                                                                    "FG",
                                                                    "Safety",
                                                                    "No_Score",
                                                                    "Opp_Safety",
                                                                    "Opp_FG",
                                                                    "Opp_TD")))

        
}

# dump("assign_play_score_events",
#      file = here::here("functions", "assign_play_score_events.R"))
