table_conference_championship <-
function(
                conference_championship_probs,
                conference) {
        
        ## make color func
        bg_col_func = 
                function(x) {
                        
                        breaks = c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.75, 1)
                        colorRamp=colorRampPalette(c("white", "deepskyblue1"))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                        
                }
        
                
        conference_championship_probs %>%
                filter(CONFERENCE == conference) %>%
                mutate_at(c("CHAMPION", "RUNNER_UP"),
                          ~ replace_na(., 0)) %>%
                select(SEASON, CONFERENCE, CONFERENCE_DIVISION, TEAM, CHAMPION, RUNNER_UP) %>%
                arrange(CONFERENCE_DIVISION, desc(CHAMPION)) %>%
                mutate(SEASON = factor(SEASON)) %>%
                rename(Season = SEASON,
                       Conference = CONFERENCE,
                       Division = CONFERENCE_DIVISION,
                       Team = TEAM,
                       Champion = CHAMPION,
                       `Runner-Up` = RUNNER_UP) %>%
                flextable() %>%
                autofit() %>%
                bg(.,
                   j = c("Champion", "Runner-Up"),
                   bg = bg_col_func) %>%
                add_header_row(.,
                               values = paste("    ", params$input_season, "Conference Championship Probabilties"),
                               colwidths = c(6)) %>%
                flextable::align(part = "header", align = "center") %>%
                flextable::align(j = c("Season", "Conference","Division","Champion", "Runner-Up"),
                                 part = "body",
                                 align = "center") %>%
                flextable::align(j = c("Team"),
                                 part = "header",
                                 align = "left")
}
