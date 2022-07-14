table_wins_conference_season <-
function (sim_team_outcomes,
                                         games,
                                         conference,
                                         season) {
        
        col_func = 
                function(x) {
                        
                        breaks = c(0, 5, 10, 15, 20, 40, 60)/100
                        colorRamp=colorRampPalette(c("white", "grey50"))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                        
                }
        
        # join with games data
        sim_team_outcomes = sim_team_outcomes %>%
                left_join(., games %>%
                          select(GAME_ID, CONFERENCE_CHAMPIONSHIP),
                  by = c("GAME_ID"))
        
        # total games per team
        max_games = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(CONFERENCE %in% conference) %>%
                filter(SEASON_TYPE == 'regular') %>%
                #    filter(CONFERENCE_GAME == T) %>%
                filter(CONFERENCE_CHAMPIONSHIP != T) %>%
                group_by(TEAM) %>%
                summarize(games = n_distinct(GAME_ID)) %>%
                summarize(max_games = max(games)) %>%
                pull(max_games)
        
        season_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(CONFERENCE %in% conference) %>%
                filter(SEASON_TYPE == 'regular') %>%
                #      filter(CONFERENCE_GAME == T) %>%
                filter(CONFERENCE_CHAMPIONSHIP != T) %>%
                mutate(WIN = case_when(SIM_MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          GAMES = n_distinct(GAME_ID),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM, WINS, GAMES) %>%
                count() %>%
                group_by(SEASON, TEAM) %>%
                mutate(perc = round(n / sum(n),2))
        
        # make sequence from zero to max games
        win_counts = seq(0, max_games, 1)
        teams = unique(season_totals$TEAM)
        
        max_wins = max(season_totals$WINS)
        min_wins = min(season_totals$WINS)
        
        # make empty grid
        table_long = expand.grid(SEASON = season,
                                 TEAM = teams,
                                 WINS = win_counts) %>%
                left_join(.,
                          season_totals %>%
                                  select(SEASON, TEAM, WINS, perc) %>%
                                  mutate(perc = perc),
                          by = c("SEASON", "TEAM", "WINS")) %>%
                mutate(perc = replace_na(perc, 0))
        
        table = table_long %>%
                select(SEASON, TEAM, WINS, perc) %>%
                pivot_wider(.,
                            id_cols = c("SEASON", "TEAM"),
                            values_from = c("perc"),
                            names_from = c("WINS")) %>%
                left_join(., season_totals %>%
                                  select(SEASON, TEAM, WINS, n, perc) %>%
                                  #       mutate(perc = perc *100) %>%
                                  group_by(SEASON, TEAM) %>%
                                  mutate(points = sum(WINS*n)) %>%
                                  distinct(SEASON, TEAM, points) %>%
                                  ungroup(),
                          by = c("SEASON", "TEAM")) %>%
                arrange(desc(points)) %>%
                mutate(SEASON  = factor(SEASON)) %>%
                select(-points) %>%
                select(SEASON, TEAM, one_of(paste(win_counts)))
        
        # if (max_wins < 12) {table$`12` = rep(0, nrow(table))} else {table$`12` = table$`12`}
        # if (max_wins < 11) {table$`11` = rep(0, nrow(table))} else {table$`11` = table$`11`}
        # if (min_wins > 0) {table$`0` = rep(0, nrow(table))} else {table$`0` = table$`0`}
        # if (min_wins > 1) {table$`1` = rep(0, nrow(table))} else {table$`1` = table$`1`}
        # if (min_wins > 2) {table$`2` = rep(0, nrow(table))} else {table$`2` = table$`2`}
        # if (min_wins > 3) {table$`3` = rep(0, nrow(table))} else {table$`3` = table$`3`}
        # if (min_wins > 4) {table$`4` = rep(0, nrow(table))} else {table$`4` = table$`4`}
        
        
        table %>%
                rename(Season = SEASON,
                       Team = TEAM) %>%
                flextable() %>%
                autofit() %>%
                bg(., j = paste(win_counts),
                   bg = col_func) %>%
                add_header_row(.,
                               values = c("","", paste("Probability of Win Totals for", conference)),
                               colwidths = c(1, 2, max(win_counts))) %>%
                flextable::align(j = c(paste(seq(0, max(win_counts), 1))),
                                 part = "all",
                                 align = "center") %>%
                # set_formatter( 
                #         `0`= function(x) sprintf( "%.0f%%", x ),
                #         `1`= function(x) sprintf( "%.0f%%", x ),
                #         `2`= function(x) sprintf( "%.0f%%", x ),
                #         `3`= function(x) sprintf( "%.0f%%", x ),
                #         `4`= function(x) sprintf( "%.0f%%", x ),
                #         `5`= function(x) sprintf( "%.0f%%", x ),
                #         `6`= function(x) sprintf( "%.0f%%", x ),
                #         `7`= function(x) sprintf( "%.0f%%", x ),
                #         `8`= function(x) sprintf( "%.0f%%", x ),
                #         `9`= function(x) sprintf( "%.0f%%", x ),
        #         `10`= function(x) sprintf( "%.0f%%", x ),
        #         `11`= function(x) sprintf( "%.0f%%", x ),
        #         `12`= function(x) sprintf( "%.0f%%", x )
        #   ) %>%
        color(part = "all",
              color = "grey20")
        
}
