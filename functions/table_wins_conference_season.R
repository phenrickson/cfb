table_wins_conference_season <-
function (sim_team_outcomes,
                                         season,
                                         week,
                                         conference) {
        
        col_func = 
                function(x) {
                        
                        breaks = c(0, 5, 10, 15, 20, 40)
                        colorRamp=colorRampPalette(c("white", "grey50"))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                        
                }
        
        season_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                filter(CONFERENCE %in% conference) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM, WINS) %>%
                count() %>%
                group_by(SEASON, TEAM) %>%
                mutate(perc = round(n / sum(n),2))
        
        max_wins = max(season_totals$WINS)
        min_wins = min(season_totals$WINS)
        
        table = season_totals %>%
                select(SEASON, TEAM, WINS, perc) %>%
                mutate(perc = perc *100) %>%
                pivot_wider(.,
                            id_cols = c("SEASON", "TEAM"),
                            values_from = c("perc"),
                            names_from = c("WINS")) %>%
                ungroup() %>%
                mutate_if(is.numeric,
                          replace_na, 0) %>%
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
                select(-points)
        
        if (max_wins < 12) {table$`12` = rep(0, nrow(table))} else {table$`12` = table$`12`}
        if (max_wins < 11) {table$`11` = rep(0, nrow(table))} else {table$`11` = table$`11`}
        if (min_wins > 0) {table$`0` = rep(0, nrow(table))} else {table$`0` = table$`0`}
        if (min_wins > 1) {table$`1` = rep(0, nrow(table))} else {table$`1` = table$`1`}
        if (min_wins > 2) {table$`2` = rep(0, nrow(table))} else {table$`2` = table$`2`}
        if (min_wins > 3) {table$`3` = rep(0, nrow(table))} else {table$`3` = table$`3`}
        if (min_wins > 4) {table$`4` = rep(0, nrow(table))} else {table$`4` = table$`4`}

        
        table %>%
                select(SEASON, TEAM, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`) %>%
                rename(Season = SEASON,
                       Team = TEAM) %>%
                flextable() %>%
                autofit() %>%
                bg(., j = c(paste(seq(0, 12, 1))),
                   bg = col_func) %>%
                add_header_row(.,
                               values = c("","", paste("Simulated Win Totals for", conference)),
                               colwidths = c(1, 2, 12)) %>%
                flextable::align(j = c(paste(seq(0, 12, 1))),
                                 part = "all",
                                 align = "center") %>%
                set_formatter( 
                        `0`= function(x) sprintf( "%.0f%%", x ),
                        `1`= function(x) sprintf( "%.0f%%", x ),
                        `2`= function(x) sprintf( "%.0f%%", x ),
                        `3`= function(x) sprintf( "%.0f%%", x ),
                        `4`= function(x) sprintf( "%.0f%%", x ),
                        `5`= function(x) sprintf( "%.0f%%", x ),
                        `6`= function(x) sprintf( "%.0f%%", x ),
                        `7`= function(x) sprintf( "%.0f%%", x ),
                        `8`= function(x) sprintf( "%.0f%%", x ),
                        `9`= function(x) sprintf( "%.0f%%", x ),
                        `10`= function(x) sprintf( "%.0f%%", x ),
                        `11`= function(x) sprintf( "%.0f%%", x ),
                        `12`= function(x) sprintf( "%.0f%%", x )
                ) %>%
                color(part = "all",
                      color = "grey20")
        
}
