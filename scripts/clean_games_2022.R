### combine datasets to get games
games = games_raw %>%
        filter(SEASON_TYPE == 'regular') %>%
        filter(CONFERENCE_CHAMPIONSHIP == F) %>%
        filter(SEASON == params$input_season) %>%
        # get most recent
        group_by(GAME_ID) %>%
        arrange(desc(is.na(OUTCOME))) %>%
        #  arrange(desc(HOME_POINTS)) %>%
        #   arrange(desc(START_DATE)) %>%
        slice_tail(n =1) %>%
        ungroup() %>%
        mutate(HOME_DIVISION = case_when(is.na(HOME_DIVISION) & SEASON < 1900 ~ 'fbs',
                                         TRUE ~ HOME_DIVISION)) %>%
        mutate(AWAY_DIVISION = case_when(is.na(AWAY_DIVISION) & SEASON < 1900 ~ 'fbs',
                                         TRUE ~ AWAY_DIVISION)) %>%
        mutate(FBS = HOME_DIVISION == 'fbs' | AWAY_DIVISION == 'fbs') %>%
        filter((FBS == T) | (FBS==F & SEASON < 1981)) %>%
        arrange(GAME_DATE) %>%
        select(GAME_ID, 
               SEASON, 
               WEEK,
               SEASON_TYPE,
               START_DATE,
               GAME_DATE,
               NEUTRAL_SITE, 
               HOME_TEAM,
               HOME_TEAM_ABBR,
               AWAY_TEAM,
               AWAY_TEAM_ABBR,
               CONFERENCE_GAME,
               CONFERENCE_CHAMPIONSHIP,
               HOME_CONFERENCE,
               AWAY_CONFERENCE,
               HOME_DIVISION,
               AWAY_DIVISION,
               HOME_POINTS,
               AWAY_POINTS) %>%
        left_join(.,
                  recruiting %>%
                          select(SEASON,
                                 TEAM,
                                 RECRUITING_SCORE) %>%
                          rename(HOME_TEAM = TEAM,
                                 HOME_RECRUITING_SCORE = RECRUITING_SCORE),
                  by = c("SEASON",
                         "HOME_TEAM")) %>%
        left_join(.,
                  recruiting %>%
                          select(SEASON,
                                 TEAM,
                                 RECRUITING_SCORE) %>%
                          rename(AWAY_TEAM = TEAM,
                                 AWAY_RECRUITING_SCORE = RECRUITING_SCORE),
                  by = c("SEASON",
                         "AWAY_TEAM"))

# games that for some damn reason the API swapped the home and away team
problem_ids = games_raw %>%
        filter(SEASON == params$input_season) %>% 
        group_by(GAME_ID) %>%
        summarize(home_teams = n_distinct(HOME_TEAM)) %>% 
        arrange(desc(home_teams)) %>%
        filter(home_teams > 1) %>%
        pull(GAME_ID)

# fix issue
games = games %>%
        filter(SEASON == params$input_season) %>%
        filter(!(GAME_ID %in% problem_ids)) %>%
        bind_rows(.,
                  games %>%
                          filter(GAME_ID %in% problem_ids) %>%
                          select(-starts_with("AWAY_")) %>%
                          set_names(., gsub("HOME_", "AWAY_", names(.))) %>%
                          bind_cols(.,
                                    games %>%
                                            filter(GAME_ID %in% problem_ids) %>%
                                            select(starts_with("AWAY")) %>%
                                            set_names(., gsub("AWAY_", "HOME_", names(.))))) %>%
        # fix missing score on select games
        # mutate(HOME_POINTS = case_when(GAME_ID == 401415214 ~ 14,
        #                                TRUE ~ HOME_POINTS)) %>%
        # mutate(AWAY_POINTS = case_when(GAME_ID == 401415214 ~ 31,
        #                                TRUE ~ AWAY_POINTS)) %>%
        filter(GAME_ID != 401416593)