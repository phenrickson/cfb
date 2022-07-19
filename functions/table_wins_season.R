table_wins_season <-
function(sim_team_outcomes,
                             games,
                             season) {
        
        elo_func = 
                function(x) {
                        
                        breaks = c(0,1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400)
                        colorRamp=colorRampPalette(c("white", "grey50"))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                        
                }
        
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
        
        # simulated end season elo
       season_elo =sim_team_outcomes %>%
               filter(SEASON == season) %>%
               filter(SEASON_TYPE == 'regular') %>%
               filter(CONFERENCE_CHAMPIONSHIP != T) %>%
               filter(DIVISION == 'fbs') %>%
               group_by(SEASON, TEAM, CONFERENCE) %>%
               mutate(max_week = max(WEEK)) %>%
               filter(WEEK == max_week) %>%
               summarize(ELO = mean(POSTGAME_ELO),
                     sd = sd(POSTGAME_ELO), .groups = 'drop') %>%
               arrange(desc(ELO))
        
      # total games per team
      max_games = sim_team_outcomes %>%
            filter(SEASON_TYPE == 'regular') %>%
               filter(SEASON == season) %>%
               filter(DIVISION == 'fbs') %>%
               filter(CONFERENCE_CHAMPIONSHIP != T) %>%
               group_by(TEAM) %>%
               summarize(games = n_distinct(GAME_ID)) %>%
               summarize(max_games = max(games)) %>%
               pull(max_games)
        
        # # now get total expected wins
        season_totals = sim_team_outcomes %>%
                filter(SEASON_TYPE == 'regular') %>%
                filter(DIVISION == 'fbs') %>%
                filter(SEASON == season) %>%
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

        # get teams
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
                left_join(., season_elo,
                          by = c("SEASON", "TEAM")) %>%
                #     arrange(desc(points)) %>%
                arrange(desc(ELO)) %>%
                mutate(SEASON  = factor(SEASON)) %>%
                select(-points) %>%
                select(SEASON, TEAM, ELO, one_of(paste(win_counts)))

        table %>%
                rename(Elo = ELO) %>%
                mutate(`End Elo`= round(Elo, 0)) %>%
                mutate(Rank = row_number()) %>%
                select(SEASON, TEAM, Rank, `End Elo`, one_of(paste(win_counts))) %>%
                rename(Season = SEASON,
                       Team = TEAM) %>%
                flextable() %>%
                autofit() %>%
                bg(., j = paste(win_counts),
                   bg = col_func) %>%
                add_header_row(.,
                               values = c("","", "Simulated", paste("Simulated Win Probabilities for", season, "Regular Season")),
                               colwidths = c(1, 1, 2, 1+max(win_counts))) %>%
                flextable::align(j = c("Rank", "End Elo", paste(seq(0, max(win_counts), 1))),
                                 part = "all",
                                 align = "center") %>%
                color(part = "all",
                      color = "grey20") %>%
                bg(.,
                   j = 'End Elo',
                   bg = elo_func)
}
