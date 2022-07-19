table_wins_team_season <-
function(sim_team_outcomes,
                                  games,
                                  team,
                                  season,
                                  input_teamcolors = teamcolors) {
        
        # get teams primary color
        team_color = input_teamcolors %>%
             #   filter(league == 'ncaa') %>%
                filter(TEAM == team) %>%
                mutate(TEAM = location) %>%
                pull(primary)
        
        # define a function based on that team's color
        team_col_func = 
                function(x) {
                        
                        breaks = seq(0, 1.5, 0.1)
                        colorRamp=colorRampPalette(c("white", team_color))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                }
        # # win col func
        # win_col_func = 
        #         function(x) {
        #                 
        #                 colorRamp=colorRampPalette(c("white", team_color))
        #                 col_palette <- colorRamp(length(seq(0, 1.3, 0.1)))
        #                 
        #                 if (x == 'W') {col_palette[9]} else {'white'}
        #         }
        
        # join with games data
        sim_team_outcomes = sim_team_outcomes %>%
                filter(SEASON_TYPE == 'regular') %>%
                left_join(., games %>%
                                  select(GAME_ID, CONFERENCE_CHAMPIONSHIP),
                          by = c("GAME_ID"))
        
        # create a col func
        sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(TEAM == team) %>%
                filter(CONFERENCE_CHAMPIONSHIP != T) %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                left_join(.,
                          games %>%
                                  select(GAME_ID, HOME_TEAM, AWAY_TEAM),
                          by = c("GAME_ID")) %>%
                mutate(OPPONENT = case_when(OPPONENT == HOME_TEAM ~ paste("@", OPPONENT),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs", OPPONENT))) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                mutate(win = SIM_MARGIN > 0) %>% 
                group_by(SEASON, GAME_DATE, WEEK, OPPONENT, TEAM) %>% 
                summarize(wins = sum(win), 
                          games = n(),
                          margin = round(median(SIM_MARGIN), 1),
                          .groups = 'drop') %>%
                mutate(perc = round(wins / games, 3)) %>%
                mutate(perc = case_when(perc == 1 ~ (games-1)/100,
                                        perc == 0 ~ (1-games)/100,
                                        TRUE ~ perc)) %>%
                mutate(expected_win = case_when(perc > .5 ~ 'W',
                                                TRUE ~ 'L')) %>%
                select(SEASON, WEEK, GAME_DATE, TEAM, OPPONENT, perc, margin) %>%
                rename(PROB = perc) %>%
                #       PRED = expected_win) %>%
                mutate(SEASON = factor(SEASON)) %>%
                mutate(WEEK = factor(WEEK)) %>%
                arrange(GAME_DATE) %>%
                mutate(GAME_DATE = factor(GAME_DATE)) %>%
                rename(Season = SEASON,
                       Week = WEEK,
                       Date = GAME_DATE,
                       Team = TEAM,
                       Opponent = OPPONENT,
                       `Prob. Win` = PROB,
                       `Pred. Margin` = margin) %>%
                #       `Prediction` = PRED) %>%
                flextable() %>%
                flextable::align(., j=c("Prob. Win", "Pred. Margin"),
                      align = "center",
                      part = "all") %>%
                bg(., j=c("Prob. Win"),
                   bg = team_col_func) %>%
                color(part = "all",
                      color = "grey20") 
        #%>%
                # bg(., j= "Prediction",
                #    bg = win_col_func)
                
}
