# define functions


### plot team historical elo
# input team outcomes table from elo ratings
# specify min date
# specify team
plot_elo_historical_team = function(elo_ratings,
                                    min_year = 1970,
                                    team) {
        
        min_date = as.Date(paste(min_year, "01", "01", sep="-"))
        min_year = year(min_date)
        
        all_teams_data = elo_ratings %>%
                filter(SEASON > min_year) %>%
                filter(!is.na(CONFERENCE))
        
        all_teams_median = all_teams_data %>%
                summarize(median = median(POSTGAME_ELO)) %>%
                pull(median)
        
        
        p = all_teams_data %>%
                ggplot(., aes(x=GAME_DATE,
                              by = TEAM,
                              y= POSTGAME_ELO))+
                geom_point(alpha = 0.01)+
                geom_hline(yintercept = all_teams_median,
                           linetype = 'dashed',
                           color = 'grey60')+
                annotate("text",
                         x = min_date,
                         y=all_teams_median + 40,
                         size = 3,
                         color = 'grey60',
                         label = paste("FBS", "\n", "Median"))
        
        team_data = elo_train$team_outcomes %>%
                filter(SEASON > min_year) %>%
                filter(TEAM == team) %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM"))
        
        p + geom_line(data =team_data,
                      aes(color = name),
                      stat = 'smooth',
                      formula = 'y ~ x',
                      method = 'loess',
                      alpha =0.75,
                      span = 0.11,
                      lwd = 1.1)+
                geom_point(data = team_data,
                           aes(color = name),
                           alpha  =0.4)+
                theme_phil()+
                ggtitle(paste("Historical Elo Rating for", team),
                        subtitle = str_wrap(paste('Displaying Postagme Elo Ratings for all FBS teams from',
                                                  min_year, 'to present. Highlighted points/line indicate selected team.'), 120))+
                theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 12))+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                xlab("Game Date")+
                ylab("Postgame Elo Rating")+
                scale_x_date(breaks = as.Date(paste(seq(min_year, 2020, 10), "01", "01", sep="-")),
                             #date_breaks = c("10 year"),
                             date_labels = c("%Y"))
        
}


### functions for plotting simulations from elo ratings
# spaghetti plot elo rating simulations by conference by season
plot_elo_conference_season = function(sim_team_outcomes,
                                      season,
                                      conference,
                                      week) {
        
        mode_func <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }
        
        team_outcomes_temp = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(CONFERENCE %in% conference) %>%
                filter(WEEK < week) %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                ungroup() %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                left_join(., games_data_raw %>%
                                  select(GAME_ID, AWAY_TEAM, HOME_TEAM),
                          by = c("GAME_ID")) %>%
                mutate(OPPONENT_LABEL = case_when(OPPONENT == HOME_TEAM ~ paste("@", OPPONENT),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs", OPPONENT)))
        
        
        # mean wins by team
        team_win_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                filter(CONFERENCE %in% conference) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM) %>%
                summarize(mean = round(mean(WINS),1),
                          mode = mode_func(WINS),
                          sd = sd(WINS),
                          .groups = 'drop') %>%
                arrange(desc(mean)) %>%
                mutate(TEAM_LABEL = factor(paste(TEAM, SEASON)))
        
        #
        plot_teams = team_outcomes_temp %>%
                bind_rows(.,
                          team_outcomes_temp %>%
                                  group_by(TEAM) %>%
                                  filter(GAME_DATE == max(GAME_DATE)) %>%
                                  mutate(GAME = max(GAME) +1, 
                                         OPPONENT_LABEL = 'Season End',
                                         PREGAME_ELO = POSTGAME_ELO)) %>%
                left_join(., team_win_totals,
                          by = c("TEAM", "SEASON")) %>%
                mutate(TEAM_LABEL = factor(TEAM_LABEL,
                                           levels = c(team_win_totals$TEAM_LABEL)))
        
        plot_teams %>%
                ggplot(., aes(x=GAME,
                              color = name,
                              group = .id,
                              y = PREGAME_ELO)) +
                geom_line(alpha = 0.08)+
                # geom_line(stat = 'smooth',
                #           method = 'loess',
                #           formula = 'y ~ x',
                #           span = 0.25,
                #           alpha = 0.5)+
                theme_phil()+
                facet_wrap(TEAM_LABEL~.,
                           ncol =4)+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                ggtitle(paste("Simulated Elo Ratings for", conference, season, "Season"),
                        subtitle = str_wrap("Each line shows one simulation for a team's season. Teams Displaying 1000 simulations.", 120))+
                scale_x_continuous(breaks = int_breaks)+
                coord_cartesian(ylim = c(1100, 2300))+
                theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 12))+
                ylab("Pregame Elo Rating")+
                xlab("Game")
        
}

dump("plot_elo_conference_season",
     file = here::here("functions/plot_elo_conference_season.R"))

### plot individual team spaghetti plot with matchups and win probabilities
# plot team elo season with matchups and win probability
plot_elo_team_season = function(sim_team_outcomes,
                                season, 
                                team,
                                week) {
        
        # elo rating
        team_temp = 
                sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(TEAM == team) %>%
                filter(WEEK < week) %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                left_join(.,
                          games_data_raw %>%
                                  select(GAME_ID, HOME_TEAM, AWAY_TEAM),
                          by = c("GAME_ID")) %>%
                mutate(OPPONENT_LABEL = case_when(OPPONENT == HOME_TEAM ~ paste("@", OPPONENT),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs", OPPONENT))) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM"))
        
        # team wins based on simulations
        team_wins =   team_temp %>% 
                mutate(win = MARGIN > 0) %>% 
                group_by(SEASON, GAME, TEAM) %>% 
                summarize(wins = sum(win), games = n(),
                          .groups = 'drop') %>%
                mutate(perc = wins / games) %>%
                mutate(expected_win = case_when(perc > .5 ~ 'W',
                                                TRUE ~ 'L'))
        
        # get team record
        team_record = team_wins %>%
                mutate(wins = case_when(expected_win == 'W' ~ 1,
                                        TRUE ~ 0)) %>%
                summarize(win = sum(wins),
                          games = n()) %>%
                mutate(loss = games - win) %>%
                mutate(record = paste(win, loss, sep="-")) %>%
                pull(record)
                
        
        # create data for plot
        plot_team_data = team_temp %>%
                bind_rows(.,
                          team_temp %>% 
                                  filter(GAME_DATE == max(GAME_DATE)) %>%
                                  mutate(GAME = max(GAME) +1, 
                                         OPPONENT_LABEL = paste("Season End", "Elo", sep="\n"),
                                    #     OPPONENT_LABEL = paste("Predicted", "\n", "Record:", "\n", team_record, "\n", "\n"),
                                         PREGAME_ELO = POSTGAME_ELO)) %>%
                left_join(., team_wins, 
                          by = c("SEASON", "GAME", "TEAM")) %>%
                mutate(perc = replace_na(as.character(perc), "")) %>%
                mutate(expected_win = replace_na(as.character(expected_win), "")) %>%
                mutate(PLOT_LABEL = case_when(OPPONENT_LABEL == team_record ~ OPPONENT_LABEL,
                                              TRUE ~ paste(expected_win,
                                              perc, 
                                              OPPONENT_LABEL,
                                              sep = '\n')))
        
        # now make plot
        plot_team_data %>%
                ggplot(., aes(x=GAME,
                              color = name,
                              group = .id,
                              y = PREGAME_ELO)) +
                geom_vline(xintercept = seq(1, max(team_temp$GAME+1)),
                           alpha = 0.8,
                           linetype = 'dashed',
                           color = 'grey90')+
                # geom_text(aes(label = OPPONENT_LABEL,
                #               x = GAME,
                #               y = 2250),
                #           size = 4)+
                geom_line(alpha = 0.25)+
                theme_phil()+
                scale_color_teams(name = "TEAM")+
                guides(color = 'none')+
                ggtitle(paste("Simulated Elo Ratings for" , team, season),
                        subtitle = str_wrap("Each line is one result from simulating a team's regular season. Win probabilities displayed above each matchup. Displaying results from 1000 simulations.", 120))+
                coord_cartesian(ylim = c(1100, 2300))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5))+
                ylab("Pregame Elo Rating")+
                xlab("Game")+
                scale_x_continuous(breaks = int_breaks)+
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank())+
                annotate("text",
                         x=plot_team_data %>% filter(.id == 1) %>% pull(GAME),
                         y=2250,
                         label = plot_team_data %>% filter(.id == 1) %>% pull(PLOT_LABEL),
                         size = 4,
                         color = unique(plot_team_data$primary)
                         )
        
}

dump("plot_elo_team_season",
     file = here::here("functions/plot_elo_team_season.R"))


### plot win totals for a conference in a season
plot_wins_conference_season = function(sim_team_outcomes,
                                       season,
                                       conference,
                                       week) {
        
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
        
        team_win_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                filter(CONFERENCE %in% conference) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, TEAM) %>%
                summarize(mean = round(mean(WINS),1),
                          mode = mode_func(WINS),
                          sd = sd(WINS),
                          .groups = 'drop') %>%
                arrange(desc(mean)) %>%
                mutate(TEAM_LABEL = paste(paste(TEAM, SEASON),
                                          paste("Mean:", mean),
                                          paste("Mode:", mode),
                                          sep = "\n"))
        
        season_totals %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                left_join(., team_win_totals,
                          by = c("TEAM", "SEASON")) %>%
                mutate(perc = round(perc, 2)) %>%
                mutate(perc = case_when(perc ==0 ~ '<0.01',
                                        TRUE ~ as.character(perc))) %>%
                mutate(TEAM_LABEL = factor(TEAM_LABEL, levels = team_win_totals$TEAM_LABEL)) %>%
                ggplot(., aes(x=WINS,
                              y = n,
                              color = name,
                              label = perc,
                              fill = name))+
                geom_col()+
                geom_text(vjust = -0.5,
                          check_overlap=T,
                          size = 2)+
                theme_phil()+
                facet_wrap(TEAM_LABEL ~.,
                           ncol=4)+
                scale_fill_teams(name = "TEAM")+
                scale_color_teams(name = "TEAM")+
                guides(fill = 'none',
                       color = 'none')+
                ylab("Number of Simulations")+
                xlab("Season Win Total")+
                ggtitle(paste("Simulated Win Totals for", conference, season, "Season"),
                        subtitle = str_wrap(" Displaying total wins for each team in 1000 simulations of selected season.", 120))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 10,
                                                  hjust = 0.5))+
                coord_cartesian(ylim = c(0, max(season_totals$n + 25)))
        
}

dump("plot_wins_conference_season",
     file = here::here("functions/plot_wins_conference_season.R"))


## make table for expected win totals by team and season
table_wins_conference_season = function (sim_team_outcomes,
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

dump("table_wins_conference_season",
     file = here::here("functions/table_wins_conference_season.R"))


## make table for a team's season
table_wins_team_season = function(sim_team_outcomes,
                                  season,
                                  team,
                                  week) {
        
        # get teams primary color
        team_color = teamcolors %>%
                filter(league == 'ncaa') %>%
                filter(location == team) %>%
                mutate(TEAM = location) %>%
                pull(primary)
        
        # define a function based on that team's color
        team_col_func = 
                function(x) {
                        
                        breaks = seq(0, 1.3, 0.1)
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
        
        # create a col func
        sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(TEAM == team) %>%
                filter(WEEK < week) %>%
                mutate(POSTGAME_ELO = round(POSTGAME_ELO,0)) %>%
                left_join(.,
                          games_data_raw %>%
                                  select(GAME_ID, HOME_TEAM, AWAY_TEAM),
                          by = c("GAME_ID")) %>%
                mutate(OPPONENT = case_when(OPPONENT == HOME_TEAM ~ paste("@", OPPONENT),
                                                  OPPONENT == AWAY_TEAM ~ paste("vs", OPPONENT))) %>%
                arrange(.id, SEASON, TEAM, GAME_DATE) %>%
                group_by(.id, SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                mutate(win = MARGIN > 0) %>% 
                group_by(SEASON, GAME_DATE, WEEK, OPPONENT, TEAM) %>% 
                summarize(wins = sum(win), 
                          games = n(),
                          margin = round(median(MARGIN), 0),
                          .groups = 'drop') %>%
                mutate(perc = round(wins / games, 3)) %>%
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
                       `Pr(Win)` = PROB,
                       Margin = margin) %>%
                #       `Prediction` = PRED) %>%
                flextable() %>%
                flextable::align(., j=c("Pr(Win)", "Margin"),
                      align = "center",
                      part = "all") %>%
                bg(., j=c("Pr(Win)"),
                   bg = team_col_func) %>%
                color(part = "all",
                      color = "grey20")
        #%>%
                # bg(., j= "Prediction",
                #    bg = win_col_func)
                
}

dump("table_wins_team_season",
     file = here::here("functions/table_wins_team_season.R"))


## make table for a team's season
table_wins_season = function(sim_team_outcomes,
                                  season,
                                  week) {
        
        
        elo_func = 
                function(x) {
                        
                        breaks = c(0, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400)
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
                        
                        breaks = c(0, 5, 10, 15, 20, 40, 60)
                        colorRamp=colorRampPalette(c("white", "grey50"))
                        col_palette <- colorRamp(length(breaks))
                        mycut <- cut(x, 
                                     breaks = breaks,
                                     include.lowest = TRUE, 
                                     right=T,
                                     label = FALSE)
                        col_palette[mycut]
                        
                }
        
        # end season elo
        season_elo = sim_team_outcomes %>% 
                filter(SEASON == season) %>% 
                filter(!is.na(CONFERENCE)) %>% 
                filter(WEEK < week) %>%
                group_by(SEASON, TEAM, CONFERENCE) %>% 
                mutate(max_week = max(WEEK)) %>%
                filter(WEEK == max_week) %>% 
                summarize(ELO = mean(POSTGAME_ELO), .groups = 'drop') %>%
                arrange(desc(ELO))
        
        season_totals = sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(WEEK < week) %>%
                mutate(WIN = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SEASON, CONFERENCE, TEAM) %>%
                summarize(WINS = sum(WIN),
                          .groups = 'drop') %>%
                group_by(SEASON, CONFERENCE, TEAM, WINS) %>%
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
                left_join(., season_elo,
                          by = c("SEASON", "TEAM")) %>%
           #     arrange(desc(points)) %>%
                arrange(desc(ELO)) %>%
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
                filter(!is.na(CONFERENCE)) %>%
                rename(Elo = ELO) %>%
                mutate(`End Elo`= round(Elo, 0)) %>%
                select(SEASON, TEAM, `End Elo`, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`) %>%
                rename(Season = SEASON,
                       Team = TEAM) %>%
                flextable() %>%
                autofit() %>%
                bg(., j = c(paste(seq(0, 12, 1))),
                   bg = col_func) %>%
                add_header_row(.,
                               values = c("","", paste("Simulated Win Totals for", season, "Season")),
                               colwidths = c(1, 2, 13)) %>%
                flextable::align(j = c("End Elo", paste(seq(0, 12, 1))),
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
                      color = "grey20") %>%
                bg(.,
                   j = 'End Elo',
                   bg = elo_func)
        
}

dump("table_wins_season",
     file = here::here("functions/table_wins_season.R"))


plot_forecast_wins_conference_season = function(actual_team_outcomes,
                                                  midseason_sim_team_outcomes,
                                                  week,
                                                  conference,
                                                season) {

        mode_func <- function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }
        
        # create span func
        span_func = function(week) {
                
                if (week < 2) {span = 1} else 
                        if (week < 3) {span = 0.9} else 
                                if (week < 8) {span = 0.7} else 
                                        if (week < 11) {span = 0.4} else
                                                if (week < 17) {span = 0.2}
                
                if (week < 2) {method = 'lm'} else 
                                if (week < 17) {method = 'loess'}
                
                return(list("span" = span,
                            "method" = method))
        }
        
        
        week_wins = actual_team_outcomes %>%
                filter(SEASON == season) %>%
                arrange(GAME_DATE) %>%
                filter(!is.na(CONFERENCE)) %>%
                mutate(win = case_when(POINTS > OPP_POINTS ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(SEASON, TEAM, WEEK) %>%
                summarise(win = sum(win),
                          .groups = 'drop') %>%
                rename(SIM_WEEK = WEEK)
        
        midseason_win_forecast =  midseason_sim_team_outcomes %>%
                filter(SEASON == season) %>%
                filter(!is.na(CONFERENCE)) %>%
                mutate(sim_win = case_when(MARGIN > 0 ~ 1,
                                       TRUE ~ 0)) %>%
                group_by(.id, SIM_WEEK, SEASON, CONFERENCE, TEAM) %>%
                summarize(sim_remaining_wins = sum(sim_win),
                          .groups = 'drop') %>%
                left_join(., week_wins,
                          by= c("SEASON", "TEAM", "SIM_WEEK")) %>%
                mutate(win = replace_na(win, 0)) %>%
                group_by(.id, SEASON, TEAM) %>%
                arrange(SIM_WEEK) %>%
                mutate(wins_so_far = cumsum(win)) %>%
             #   filter(TEAM == 'Wisconsin') %>%
                mutate(sim_total_wins = sim_remaining_wins + wins_so_far) %>%
                select(-win) %>%
                ungroup() %>%
                group_by(SIM_WEEK, SEASON, CONFERENCE, TEAM) %>%
                summarize(mean_total_wins = mean(sim_total_wins),
                          .groups = 'drop')
                # summarize(quantile = c("perc_low", "perc_mid", "perc_upper"),
                #           wins = quantile(sim_total_wins, c(0.1, 0.5, 0.9)),
                #           .groups ='drop')
        
        midseason_win_forecast %>%
                filter(SIM_WEEK <= week) %>%
                filter(CONFERENCE == conference) %>%
              #  filter(quantile == 'perc_mid') %>%
                left_join(., teamcolors %>%
                                  filter(league == 'ncaa') %>%
                                  mutate(TEAM = location),
                          by = c("TEAM")) %>%
                group_by(SEASON, TEAM) %>%
                mutate(GAME = row_number()) %>%
                mutate(TEAM_LABEL = case_when(GAME == max(GAME) ~ TEAM)) %>%
                ggplot(., aes(x=SIM_WEEK,
                              y = mean_total_wins,
                              label = TEAM_LABEL,
                              color = name,
                              fill = TEAM)) +
                geom_text_repel(
                        fontface = "bold",
                        size = 3,
                        direction = "y",
                        hjust = -1.2, 
                        segment.size = .7,
                        segment.alpha = .5,
                        segment.linetype = "dotted",
                        box.padding = .4,
                        segment.curvature = -0.1,
                        segment.ncp = 3,
                        segment.angle = 20
                ) +
                geom_line(stat = 'smooth',
                            formula = 'y ~ x',
                            method = span_func(week)$method,
                          #  alpha =0.75,
                          alpha = 0.7,
                            span = span_func(week)$span,
                            lwd = 1.1)+
                theme_phil()+
                scale_fill_teams(name = "TEAM")+
                scale_color_teams(name = "TEAM")+
                scale_x_continuous(limits = c(-1, 14),
                                   breaks = seq(0, 12, 3))+
                scale_y_continuous(limits = c(0, 12),
                                   breaks = seq(2, 12, 2))+
                guides(fill = "none",
                       color = "none",
                       alpha = "none",
                       linetype = 'none')+
                xlab("Week of Simulation")+
                ylab("Expected Season Win Total")+
            #    facet_wrap(paste(CONFERENCE, SEASON) ~.)+
                ggtitle(paste("Season Win Totals for", conference, season, "Season", "\n",
                              "Simulating as of Week", week),
                        subtitle = str_wrap(" Displaying average total wins for each team in 1000 simulations of the remaining games in the season. Simulations run at the beginning of the season and after every week.", 90))+
                theme(plot.title = element_text(hjust = 0.5,
                                                size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 10,
                                                  hjust = 0.5))+
                geom_vline(xintercept = 12,
                           linetype = 'dashed',
                           color = 'grey30')
}

dump("plot_forecast_wins_conference_season",
     file = here::here("functions/plot_forecast_wins_conference_season.R"))
        
### functions for elo ratings
# get expected score for two teams based on elo
# input elo ratings and scaling factor V
get_expected_score = function(team_rating, opponent_rating, V=400) {
        
        return(1 / (1 + 10^((opponent_rating - team_rating) / V)))
        
}

dump("get_expected_score",
     file = here::here("functions", "get_expected_score.R"))

# calculate post game elo ratings
# based on: 
# pre game elo ratings
# margin of victory
# update factor k in calculating team rating
# update scaling factor V in difference between team ratings
get_new_elos = function(home_rating, 
                        away_rating,
                        home_margin,
                        home_field_advantage,
                        k,
                        v) {
        
        # get observed home score
        # if the home team wins, then home score = 1
        # if the home team loses, then home score = 0
        # in a tie, home score = 0.5
        if (home_margin > 0) {home_score = 1} else
                if (home_margin < 0) {home_score = 0} else
                {home_score = 0.5}
        
        # get observed away score, 1-home
        away_score = 1-home_score
        
        ## determine whether there is home field advantage
        
        ## get home and away expected scores
        # get expected home score based on the pre game rating and home field advantage
        home_expected_score = get_expected_score(home_rating + home_field_advantage,
                                                 away_rating,
                                                 V=v)
        
        
        # get expected away score based on pre game rating
        away_expected_score = get_expected_score(away_rating,
                                                 home_rating + home_field_advantage,
                                                 V=v)
        
        ## define margin of victory multiplier based on winner
        if (home_margin > 0) {
                mov_multi = log(abs(home_margin)+1) * (2.2 / (((home_rating + home_field_advantage - away_rating)*0.001) + 2.2))
        } else if (home_margin <0) {
                mov_multi = log(abs(home_margin)+1) * (2.2 / (((away_rating - home_rating + home_field_advantage)*0.001) + 2.2))
        } else {
                mov_multi = 2.2*log(2)
        }
        
        ## update ratings
        # update home rating
        home_new_rating = home_rating + 
                (mov_multi *
                         k * 
                         (home_score - home_expected_score))
        
        # update away rating
        away_new_rating = away_rating + 
                (mov_multi *
                         k * 
                         (away_score - away_expected_score))
        
        return(c(home_new_rating,
                 away_new_rating,
                 home_expected_score,
                 away_expected_score))
}

dump("get_new_elos",
     file = here::here("functions", "get_new_elos.R"))

# function to loop over games and calculate elo ratings given selected parameters
calc_elo_ratings = function(games,
                            teams = list(),
                            team_seasons = list(),
                            home_field_advantage,
                            reversion,
                            k,
                            v,
                            verbose=T) {
        
        # define an empty tibble to store the game outcomes
        game_outcomes = tibble()
        
        # loop over games
        for(i in 1:nrow(games)) {
                
                ### get the individual game 
                game = games[i,]
                
                ### get pre game elo ratings
                # look for team in teams list
                # if not defined, set to 1500 for FBS teams and 1200 for non FBS
                
                # get home elo rating
                if (game$HOME_TEAM %in% names(teams))
                {home_rating = teams[[game$HOME_TEAM]]} else 
                        if (!is.na(game$HOME_CONFERENCE)) {home_rating = 1500} else {home_rating = 1200}
                
                # get away elo rating
                if (game$AWAY_TEAM %in% names(teams))
                {away_rating = teams[[game$AWAY_TEAM]]} else 
                        if (!is.na(game$AWAY_CONFERENCE)) {away_rating = 1500} else {away_rating = 1200}
                
                # check whether its a neutral site game to apply home field advantage adjustment
                if (game$NEUTRAL_SITE==T) {home_field_advantage = 0} else 
                        if (game$NEUTRAL_SITE==F) {home_field_advantage = home_field_advantage}
                
                # check whether the team has already played in a season
                # check whether the season of the game is the same season 
                # as the current team season value
                # if not, apply a mean reversion
                
                # set reversion amount
                # home team
                if (length(team_seasons[[game$HOME_TEAM]]) ==0) {team_seasons[[game$HOME_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$HOME_TEAM]]) {home_rating = home_rating} else 
                                if (
                                        (team_seasons[[game$HOME_TEAM]] < game$SEASON) & !is.na(game$HOME_CONFERENCE)
                                )
                                {home_rating = ((reversion*1500)+ (1-reversion)*home_rating)} else
                                        if (
                                                (team_seasons[[game$HOME_TEAM]] < game$SEASON) & is.na(game$HOME_CONFERENCE)
                                        )
                                        {home_rating = ((reversion*1200)+(1-reversion)*home_rating)}
                
                # away team
                if (length(team_seasons[[game$AWAY_TEAM]]) ==0) {team_seasons[[game$AWAY_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$AWAY_TEAM]]) {away_rating = away_rating} else 
                                if (
                                        (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & !is.na(game$HOME_CONFERENCE)
                                )
                                {away_rating = ((reversion*1500)+ (1-reversion)*away_rating)} else
                                        if (
                                                (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & is.na(game$HOME_CONFERENCE)
                                        )
                                        {away_rating = ((reversion*1200)+(1-reversion)*away_rating)}
                
                ## get the score margin based on the ome team
                home_margin = game$HOME_POINTS - game$AWAY_POINTS
                
                # get updated elo for both teams
                new_elos = get_new_elos(home_rating,
                                        away_rating,
                                        home_margin,
                                        home_field_advantage,
                                        k,
                                        v)
                
                # add pre game elo ratings to the selected game
                # do not include the adjustment for home advantage in the pre game
                game$HOME_PREGAME_ELO = home_rating
                game$AWAY_PREGAME_ELO = away_rating
                
                # add pre game prob
                game$HOME_PROB = new_elos[3]
                game$AWAY_PROB = new_elos[4]
                
                # add post game elo ratings to the selected game
                game$HOME_POSTGAME_ELO = new_elos[1]
                game$AWAY_POSTGAME_ELO = new_elos[2]
                
                # update the list storing the current elo rating for each team
                teams[[game$HOME_TEAM]] = new_elos[1]
                teams[[game$AWAY_TEAM]] = new_elos[2]
                
                # upaate the list storing the current team season
                team_seasons[[game$HOME_TEAM]] = game$SEASON
                team_seasons[[game$AWAY_TEAM]] = game$SEASON
                
                # store 
                game_outcomes = bind_rows(game_outcomes,
                                          game)
                
                # log output
                if (verbose == T) {cat("\r", i, "of", nrow(games), "games completed");  flush.console()}
                
        }
        
        # create a table at the team level that is easy to examine the results
        team_outcomes = game_outcomes %>% 
                select(GAME_ID, 
                       SEASON,
                       WEEK,
                       GAME_DATE,
                       starts_with("HOME_"), 
                       AWAY_POINTS) %>%
                rename(OPP_POINTS = AWAY_POINTS) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          game_outcomes %>% 
                                  select(GAME_ID, 
                                         SEASON,
                                         WEEK,
                                         GAME_DATE,
                                         starts_with("AWAY_"),
                                         HOME_POINTS) %>%
                                  rename(OPP_POINTS = HOME_POINTS) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                mutate(home_field_advantage = home_field_advantage,
                       reversion = reversion,
                       k = k,
                       v = v)
        
        return(list(
                "game_outcomes" = game_outcomes,
                "team_outcomes" = team_outcomes,
                "team_seasons" = team_seasons,
                "teams" = teams)
        )
        
}

dump("calc_elo_ratings",
     file = here::here("functions", "calc_elo_ratings.R"))

# tweak base function
simulateX <- function(object, nsim = 1, seed = NULL, X, ...) {
        object$fitted.values <- predict(object, X)
        pull(simulate(object = object, nsim = nsim, seed = seed, ...))
}
dump("simulateX",
     file = here::here("functions", "simulateX.R"))

# define a function for simulating margin of game based on elo ratings
# input a model and the two ratings
# return home score diff and outcome
sim_game_margin = function(home_rating,
                           away_rating,
                           points_model) {

        # using base lm with custom simulate function
        sim <- simulateX(points_model, nsim = 1,
                               X = data.frame(HOME_ELO_DIFF = (home_rating - away_rating)))
        
        return(round(sim, 0))
}

dump("sim_game_margin",
     file = here::here("functions", "sim_game_margin.R"))

# function for hot simulations with elo ratings
sim_elo_ratings = function(games,
                            teams,
                            team_seasons,
                            home_field_advantage,
                            reversion,
                            k,
                            v,
                           points_model,
                           ties = F,
                           nsims = 1000,
                           verbose=T) {
        
        # create empty tibble to store game outcomes
        # sim_game_outcomes = tibble()
        # sim_team_outcomes = tibble()
        
        game_outcomes = tibble()
        
        # # loop over nsims
        # for (s in 1:nsims) {
        #         
        #         # define an empty tibble to store the game outcomes
        #         game_outcomes = tibble()
        #         teams = teams
        #         team_seasons = team_seasons
                
        # loop over games
        for(i in 1:nrow(games)) {
                
                ### get the individual game 
                game = games[i,]
                
                ### get pre game elo ratings
                # look for team in teams list
                # if not defined, set to 1500 for FBS teams and 1200 for non FBS
                
                # get home elo rating
                if (game$HOME_TEAM %in% names(teams))
                {home_rating = teams[[game$HOME_TEAM]]} else 
                        if (!is.na(game$HOME_CONFERENCE)) {home_rating = 1500} else {home_rating = 1200}
                
                # get away elo rating
                if (game$AWAY_TEAM %in% names(teams))
                {away_rating = teams[[game$AWAY_TEAM]]} else 
                        if (!is.na(game$AWAY_CONFERENCE)) {away_rating = 1500} else {away_rating = 1200}
                
                # check whether its a neutral site game to apply home field advantage adjustmnet
                if (game$NEUTRAL_SITE==T) {home_field_advantage = 0} else 
                        if (game$NEUTRAL_SITE==F) {home_field_advantage = home_field_advantage}
                
                # check whether the team has already played in a season
                # check whether the season of the game is the same season 
                # as the current team season value
                # if not, apply a mean reversion
                
                # set reversion amount
                # home team
                if (length(team_seasons[[game$HOME_TEAM]]) ==0) {team_seasons[[game$HOME_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$HOME_TEAM]]) {home_rating = home_rating} else 
                                if (
                                        (team_seasons[[game$HOME_TEAM]] < game$SEASON) & !is.na(game$HOME_CONFERENCE)
                                )
                                {home_rating = ((reversion*1500)+ (1-reversion)*home_rating)} else
                                        if (
                                                (team_seasons[[game$HOME_TEAM]] < game$SEASON) & is.na(game$HOME_CONFERENCE)
                                        )
                                        {home_rating = ((reversion*1200)+(1-reversion)*home_rating)}
                
                # away team
                if (length(team_seasons[[game$AWAY_TEAM]]) ==0) {team_seasons[[game$AWAY_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$AWAY_TEAM]]) {away_rating = away_rating} else 
                                if (
                                        (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & !is.na(game$HOME_CONFERENCE)
                                )
                                {away_rating = ((reversion*1500)+ (1-reversion)*away_rating)} else
                                        if (
                                                (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & is.na(game$HOME_CONFERENCE)
                                        )
                                        {away_rating = ((reversion*1200)+(1-reversion)*away_rating)}
                
                ## simulate the margin via points model
                home_margin = sim_game_margin(home_rating, 
                                              away_rating,
                                              points_model)
                
                # adjust for ties
                # if ties =F, then a margin of 0 will give home team a 1 point win
                if (ties == F & home_margin == 0) {home_margin = 1} else {home_margin = home_margin}
                
                # get updated elo for both teams
                new_elos = get_new_elos(home_rating,
                                        away_rating,
                                        home_margin,
                                        home_field_advantage,
                                        k,
                                        v)
                
                # add pre game elo ratings to the selected game
                game$HOME_PREGAME_ELO = home_rating
                game$AWAY_PREGAME_ELO = away_rating
                
                # add pre game prob
                game$HOME_PROB = new_elos[3]
                game$AWAY_PROB = new_elos[4]
                
                # add the margin
                game$HOME_MARGIN = home_margin
                
                # add post game elo ratings to the selected game
                game$HOME_POSTGAME_ELO = new_elos[1]
                game$AWAY_POSTGAME_ELO = new_elos[2]
                
                # update the list storing the current elo rating for each team
                teams[[game$HOME_TEAM]] = new_elos[1]
                teams[[game$AWAY_TEAM]] = new_elos[2]
                
                # upaate the list storing the current team season
                team_seasons[[game$HOME_TEAM]] = game$SEASON
                team_seasons[[game$AWAY_TEAM]] = game$SEASON
                
                # store 
                game_outcomes = bind_rows(game_outcomes,
                                          game)
                
                if (verbose == T) {cat("\r", i, "of", nrow(games), "games completed");  flush.console()}
                
        }
        
        # create a table at the team level that is easy to examine the results
        team_outcomes = game_outcomes %>% 
                mutate(HOME_OPPONENT = AWAY_TEAM) %>%
                select(GAME_ID, 
                       SEASON,
                       WEEK,
                       GAME_DATE,
                       starts_with("HOME_")) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          game_outcomes %>% 
                                  mutate(AWAY_OPPONENT = HOME_TEAM) %>%
                                  mutate(AWAY_MARGIN = -HOME_MARGIN) %>%
                                  select(GAME_ID, 
                                         SEASON,
                                         WEEK,
                                         GAME_DATE,
                                         starts_with("AWAY_")) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                mutate(home_field_advantage = home_field_advantage,
                       reversion = reversion,
                       k = k,
                       v = v)
        
        return(list(
                "game_outcomes" = game_outcomes,
                "team_outcomes" = team_outcomes,
                "team_seasons" = team_seasons,
                "teams" = teams)
        )
        
}

dump("sim_elo_ratings",
     file = here::here("functions", "sim_elo_ratings.R"))


# function for calculating expected points from .pred columns from the model
# takes a df with these fiels as input and calculates the expected points
expected_points_func = function(x) {
        
        x %>%
                mutate(EP = 0 * `.pred_No_Score` + 
                               7 * .pred_TD + 3 * .pred_FG + 
                               2 * .pred_Safety + 
                               -2*`.pred_Opp_Safety` + 
                               -3*`.pred_Opp_FG` + 
                               -7*`.pred_Opp_TD`)
        
}
dump("expected_points_func",
     file = here::here("functions", "expected_points_func.R"))

# scoring plays function
# takes as input a df with plays that have been scored then computes the difference in points between plays
get_points_added_func = function(input_predicted_plays) {
        
        input_predicted_plays %>%
                select(method, SEASON, GAME_ID, DRIVE_ID, PLAY_ID, HALF, PERIOD, OFFENSE, DEFENSE, HOME, AWAY, OFFENSE_SCORE, DEFENSE_SCORE,SECONDS_IN_HALF, DOWN, DISTANCE, YARDS_TO_GOAL, PLAY_TEXT, EP, PLAY_TYPE, NEXT_SCORE_EVENT_OFFENSE) %>%
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
                        (grepl("Field Goal", PLAY_TEXT) | PLAY_TYPE == 'FG' | grepl("fg|field goal", tolower(PLAY_TYPE)) | !grepl("missed", tolower(PLAY_TEXT))) ~ 1,
                        TRUE ~ 0)) %>%
                mutate(SAFETY = case_when(
                        grepl("safety", tolower(PLAY_TEXT)) ~ 1,
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
                group_by(method, GAME_ID, OFFENSE, HALF, DRIVE_ID) %>%
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
                          group_indices(., method, GAME_ID, HALF, HOME_SCORE, AWAY_SCORE) %>% # this creates a group index
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
        
}

dump("get_points_added_func",
     file = here::here("functions", "get_points_added_func.R"))

# display scorelines
# input_plays_data = play by play level data
# input_games_data = game level data
# input_game_ids = vector of game IDs to display, can be one or multiple
plot_scoreline_func = function(input_plays_data, 
                               input_games_data,
                               input_game_ids) {
        
        plot_data = input_plays_data %>% 
                mutate(ID = as.numeric(ID),
                       GAME_ID = as.numeric(GAME_ID)) %>%
                filter(GAME_ID %in% input_game_ids) %>%
                rename(PLAY_ID = ID) %>%
                group_by(GAME_ID) %>%
                arrange(PLAY_ID) %>%
                mutate(GAME_PLAY_NUMBER = row_number()) %>%
                mutate(IS_HOME_OFFENSE = case_when(OFFENSE == HOME  ~ T,
                                                   OFFENSE == AWAY ~ F)) %>%
                mutate(HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == F ~ DEFENSE_SCORE),
                       AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == T ~ DEFENSE_SCORE)) %>%
                select(GAME_ID, DRIVE_ID, PLAY_ID, GAME_PLAY_NUMBER, HOME, AWAY, HOME_SCORE, AWAY_SCORE, PLAY_TEXT, SCORING) %>%
                gather("variable",
                       "value",
                       -GAME_ID, -DRIVE_ID, -PLAY_ID, -PLAY_TEXT, -SCORING,
                       -GAME_PLAY_NUMBER, -HOME, -AWAY) %>%
                left_join(., input_games_data %>%
                                  rename(GAME_ID = ID),
                          by = c("GAME_ID")) %>%
                mutate(GAME_DESCRIPTION = paste(SEASON,
                                                paste("H:", HOME),
                                                paste("A:", AWAY),
                                                paste("ID:", GAME_ID),
                                                sep="\n")) %>%
                mutate(GAME_ID = as.character(GAME_ID)) %>%
                mutate(PROBLEM_SCORE = case_when(value < lag(value, 1) & GAME_PLAY_NUMBER > 1 ~ T)) %>%
                mutate(GAME_ID = as.character(GAME_ID))
        
        
        p = plot_data %>%
                ggplot(., aes(x=GAME_PLAY_NUMBER,
                              by = GAME_ID,
                              y = value,
                              color = variable))+
                geom_line(alpha = 0.8,
                          lwd = 1.05,
                          position=position_jitter(w=0.02, h=0))+
                theme_phil()+
                scale_color_manual(values = c("black", "grey60"))+
                facet_wrap(GAME_DESCRIPTION ~.,
                           ncol = 5)
        
        plot_game = p +
                geom_vline(data = plot_data %>%
                                   filter(PROBLEM_SCORE ==T),
                           aes(xintercept = GAME_PLAY_NUMBER),
                           lwd = 0.8,
                           alpha = 0.8,
                           color = 'red')
        
        
        return(list("plot_data" = plot_data,
                    "plot_game" = plot_game))
        
}

dump("plot_scoreline_func",
     file = here::here("functions", "plot_scoreline_func.R"))


# create function to clean up the clock JSON field manually
# x is a df with CLOCK and PERIOD features
make_time_features_func = function(x) {
        
        
        x %>%
                mutate(TIME = gsub('\\{|}|"', '', CLOCK)) %>%
                separate(TIME, into=c("MINUTES", "SECOND"), sep=",") %>%
                mutate(MINUTES = as.numeric(gsub("minutes:", "", MINUTES))) %>% 
                mutate(SECOND = as.numeric(gsub("seconds:", "", SECOND))) %>%
                mutate(MINUTES_IN_HALF = case_when(PERIOD == 1 ~ MINUTES+ 15,
                                                   PERIOD == 2 ~ MINUTES,
                                                   PERIOD == 3 ~ MINUTES + 15,
                                                   PERIOD == 4 ~ MINUTES)) %>%
                mutate(SECONDS_IN_HALF = MINUTES_IN_HALF*60 + SECOND)
        #    mutate(SECONDS_IN_HALF = )
        
}

dump("make_time_features_func",
     file = here::here("functions", "make_time_features_func.R"))

# clean plays func

### periods
# if PERIOD = 0 and there is less than (or eqaul to) 1 character in the PLAY TEXT field, we'll drop the play.
# otherwise, if PERIOD is equal to zero then fill it in with the previous play's PERIOD.
### downs
# if down isn't 1,2,3,4, set to -1 to indicate special teams
### score line

clean_plays_func = function(input_plays_data) {
        
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

dump("clean_plays_func", 
     file = here::here("functions", "clean_plays_func.R"))

# plays_data_raw %>%
#         filter(GAME_ID == 322592567) %>%
#         clean_plays_func %>%
#         View()
#         


# function to define sequences

define_score_events_func = function(input_plays_data) {
        
        equences_data_raw =
                plays_data_raw %>%
                filter(PERIOD %in% c(1,2,3,4)) %>%
                mutate(HALF = case_when(PERIOD == 1 | PERIOD == 2 ~ 1,
                                        PERIOD == 3 | PERIOD == 4 ~ 2)) %>%
                mutate(ID = as.numeric(ID),
                       DRIVE_ID = as.numeric(DRIVE_ID),
                       GAME_ID = as.numeric(GAME_ID)) %>%
                rename(PLAY_ID = ID) %>%
                group_by(GAME_ID) %>%
                arrange(GAME_ID, DRIVE_ID, PLAY_ID) %>%
                mutate(GAME_PLAY_NUMBER = row_number()) %>%
                mutate(IS_HOME_OFFENSE = case_when(OFFENSE == HOME  ~ T,
                                                   OFFENSE == AWAY ~ F)) %>%
                mutate(HOME_SCORE = case_when(IS_HOME_OFFENSE == T ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == F ~ DEFENSE_SCORE),
                       AWAY_SCORE = case_when(IS_HOME_OFFENSE == F ~ OFFENSE_SCORE,
                                              IS_HOME_OFFENSE == T ~ DEFENSE_SCORE)) %>%
                mutate(HOME_SCORE_MAX = cummax(HOME_SCORE),
                       AWAY_SCORE_MAX = cummax(AWAY_SCORE)) %>%
                mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE_MAX, 1, 0),
                       AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE_MAX, 1, 0)) %>%
                mutate(HOME_SCORE_FIX = case_when((HOME_SCORE < lag(HOME_SCORE_MAX, 1, 0) | 
                                                           HOME_SCORE_DIFF > 8) ~ lag(HOME_SCORE_MAX, 1, 0),
                                                  TRUE ~ HOME_SCORE),
                       AWAY_SCORE_FIX = case_when((AWAY_SCORE < lag(AWAY_SCORE_MAX, 1, 0) | 
                                                           AWAY_SCORE_DIFF > 8) ~ lag(AWAY_SCORE_MAX, 1, 0),
                                                  TRUE ~ AWAY_SCORE)) %>%
                group_by(GAME_ID, DRIVE_ID, DRIVE_NUMBER) %>%
                group_by(GAME_ID, HALF) %>%
                mutate(FINAL_HALF_PLAY = n(),
                       HALF_PLAY_NUMBER = row_number()) %>% 
                mutate(IS_FINAL_HALF_PLAY = case_when(HALF_PLAY_NUMBER == FINAL_HALF_PLAY ~ T,
                                                      TRUE ~ F)) %>%
                filter(SCORING == T | IS_FINAL_HALF_PLAY == T) %>%
                select(GAME_ID, DRIVE_ID, PLAY_ID, HALF, PERIOD, HOME, AWAY, IS_HOME_OFFENSE, DRIVE_NUMBER, PLAY_NUMBER, SCORING, IS_FINAL_HALF_PLAY,
                       HOME_SCORE, HOME_SCORE_FIX,
                       AWAY_SCORE, AWAY_SCORE_FIX) %>%
                group_by(GAME_ID, DRIVE_ID) %>%
                filter(HOME_SCORE == max(HOME_SCORE)
                       & AWAY_SCORE == max(AWAY_SCORE)) %>% 
                filter(PLAY_NUMBER == max(PLAY_NUMBER)) %>%
                group_by(GAME_ID) %>%
                mutate(HOME_SCORE_DIFF = HOME_SCORE - lag(HOME_SCORE, 1, default=0),
                       AWAY_SCORE_DIFF = AWAY_SCORE - lag(AWAY_SCORE, 1, default=0)) %>%
                # mutate(HOME_SCORE_DIFF = HOME_SCORE_FIX - lag(HOME_SCORE_FIX, 1, default=0),
                #        AWAY_SCORE_DIFF = AWAY_SCORE_FIX - lag(AWAY_SCORE_FIX, 1, default=0)) %>%
                mutate(HOME_SCORE_DIFF = case_when(HOME_SCORE_DIFF > 7 ~ 7,
                                                   HOME_SCORE_DIFF < 0 ~ 0,
                                                   TRUE ~ HOME_SCORE_DIFF),
                       AWAY_SCORE_DIFF = case_when(AWAY_SCORE_DIFF > 7 ~ 7,
                                                   AWAY_SCORE_DIFF < 0 ~ 0,
                                                   TRUE ~ AWAY_SCORE_DIFF)) %>%
                mutate(NEXT_SCORE_DIFF = case_when(HOME_SCORE_DIFF != 0 ~ HOME_SCORE_DIFF,
                                                   HOME_SCORE_DIFF ==0 ~ -1*AWAY_SCORE_DIFF)) %>%
                mutate(NEXT_SCORE_DIFF = case_when(NEXT_SCORE_DIFF < 7 & NEXT_SCORE_DIFF > 3 ~ 7,
                                                   NEXT_SCORE_DIFF < 2 & NEXT_SCORE_DIFF > 0  ~7,
                                                   NEXT_SCORE_DIFF >-7 & NEXT_SCORE_DIFF < -3 ~ -7,
                                                   NEXT_SCORE_DIFF >-2 & NEXT_SCORE_DIFF <0 ~ -7,
                                                   TRUE ~ NEXT_SCORE_DIFF)) %>%
                ungroup() %>%
                group_by(GAME_ID) %>%
                mutate(SEQUENCE_NUMBER = row_number()) %>%
                ungroup()
        
}


