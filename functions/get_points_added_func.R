get_points_added_func <-
function(input_predicted_plays) {
        
        input_predicted_plays %>%
                select(SEASON, GAME_ID, DRIVE_ID, PLAY_ID, HALF, PERIOD, OFFENSE, DEFENSE, HOME, AWAY, OFFENSE_SCORE, DEFENSE_SCORE,SECONDS_IN_HALF, DOWN, DISTANCE, YARDS_TO_GOAL, PLAY_TEXT, EP, PLAY_TYPE, NEXT_SCORE_EVENT_OFFENSE) %>%
                # compute home and away score
                mutate(HOME_SCORE = case_when(OFFENSE == HOME ~ OFFENSE_SCORE,
                                              DEFENSE == HOME ~ DEFENSE_SCORE),
                       AWAY_SCORE = case_when(DEFENSE == HOME ~ OFFENSE_SCORE,
                                              OFFENSE == HOME ~ DEFENSE_SCORE)) %>%
                # make features to denote scoring plays
                mutate(TOUCHDOWN = case_when(
                        (grepl("TOUCHDOWN", PLAY_TEXT) | PLAY_TYPE == 'TD' | grepl("td|touchdown", tolower(PLAY_TYPE))) ~ 1,
                        TRUE ~ 0)) %>%
                mutate(FG = case_when(
                        ((grepl("Field Goal", PLAY_TEXT) | PLAY_TYPE == 'FG' | grepl("fg|field goal", tolower(PLAY_TYPE))) & !grepl("missed|blocked", tolower(PLAY_TEXT))) ~ 1,
                        TRUE ~ 0)) %>%
                mutate(SAFETY = case_when(
                        grepl("safety", tolower(PLAY_TEXT)) ~ 1,
                        TRUE ~ 0)) %>%
                # make features to denote special teams
                mutate(SPECIAL_TEAMS = case_when(
                        grepl("punt|kick", tolower(PLAY_TEXT)) | FG == 1 ~ 1,
                              TRUE ~ 0)) %>%
                # compute the actual points after scoring plays
                mutate(Points_Post = case_when(TOUCHDOWN==1 & NEXT_SCORE_EVENT_OFFENSE == 'TD' ~ 7,
                                               TOUCHDOWN==1 & NEXT_SCORE_EVENT_OFFENSE == 'Opp_TD' ~ -7,
                                               FG==1 & NEXT_SCORE_EVENT_OFFENSE == 'FG' ~ 3,
                                               FG==1 & NEXT_SCORE_EVENT_OFFENSE == 'Opp_FG' ~ -3,
                                               SAFETY==1 & NEXT_SCORE_EVENT_OFFENSE == 'Safety' ~ 2,
                                               SAFETY==1 & NEXT_SCORE_EVENT_OFFENSE == 'Opp Safety' ~ -2)) %>%
                # rename expected points for play as EPA_Pre
                rename(EP_Pre = EP)  %>%
                # now, compute EPA_Pre, EPA_Post, EPA_Added for individual drives
                # all of these calculations are keeping plays in the order in which they appear from the API
                group_by(GAME_ID, OFFENSE, HALF, DRIVE_ID) %>%
                # compute EP_Post, the expected points of the situation in the next play. 
                # for scoring plays we'll be computing a slightly different quantity
                mutate(EP_Post = case_when(
                        (TOUCHDOWN == 0 & FG ==0 & SAFETY==0)  ~ dplyr::lead(EP_Pre,1))
                ) %>%
                mutate(EP_Added = EP_Post-EP_Pre) %>%
                # unfortunately, this doesn't catch what happens if possession changes before a scoring event
                # define a sequence as plays within a half where the score remains the same
                # add a sequence id
                bind_cols(.,
                          group_indices(., GAME_ID, HALF, HOME_SCORE, AWAY_SCORE) %>% # this creates a group index
                                  as_tibble() %>%
                                  rename(SEQUENCE = value)) %>%group_by(GAME_ID, HALF, SEQUENCE) %>%
                # now, if EP_Post is NA, it is typically because a drive ended without a scoring event
                # in the event that possession changed, flip the sign of the expected points for the next play (ie, what is the other teams EP now that they have the ball)
                # the difference in this case will be the result of the turnover/change in possession
                mutate(EP_Post = case_when(
                        (is.na(EP_Post) & (TOUCHDOWN == 0 & FG ==0 & SAFETY==0) & (OFFENSE!=dplyr::lead(OFFENSE,1))) ~ -1*(dplyr::lead(EP_Pre,1)),
                        (is.na(EP_Post) & (TOUCHDOWN == 0 & FG ==0 & SAFETY==0) & (OFFENSE==dplyr::lead(OFFENSE,1))) ~ 1*(dplyr::lead(EP_Pre,1)),
                        TRUE ~ EP_Post)) %>%
                mutate(EP_Added = case_when(
                        (is.na(EP_Added) & (TOUCHDOWN == 0 & FG ==0 & SAFETY==0)) ~ EP_Post-EP_Pre,
                        TRUE ~ EP_Added)) %>%
                # Points added by scoring plays
                mutate(Points_Added = Points_Post-EP_Pre) %>%
                ungroup()
        # %>%
        #         select(YARDS_TO_GOAL, PLAY_ID, DOWN, DISTANCE, PLAY_TEXT, EP_Pre, EP_Post, EP_Added)
        
}

