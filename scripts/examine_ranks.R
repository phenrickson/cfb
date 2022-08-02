library(teamcolors)
library(cfbplotR)

seasons = seq(2010, 2021)

expand.grid(SEASON = seasons,
            TEAM = elo_games$team_outcomes %>%
                    filter(SEASON %in% seasons)%>%
                    filter(DIVISION == 'fbs') %>%
                    distinct(TEAM) %>%
                    pull(TEAM),
            WEEK_NUM = seq(1, 15, 1)) %>%
        arrange(WEEK_NUM) %>%
        left_join(.,
                  elo_games$team_outcomes %>%
                          group_by(SEASON, TEAM) %>%
                          arrange(GAME_DATE) %>%
                          mutate(WEEK_NUM = row_number()) %>%
                          filter(SEASON %in% seasons) %>%
                          select(SEASON, WEEK_NUM, TEAM, PREGAME_ELO, POSTGAME_ELO),
                  ) %>% 
        group_by(SEASON, TEAM) %>%
     #   fill(POSTGAME_ELO) %>%
        fill(POSTGAME_ELO, .direction = 'down') %>%
        mutate(PREGAME_ELO = case_when(is.na(PREGAME_ELO) ~ POSTGAME_ELO,
                                       TRUE ~ PREGAME_ELO)) %>%
        group_by(SEASON, WEEK_NUM) %>%
        arrange(WEEK_NUM, desc(PREGAME_ELO)) %>%
        mutate(RANK = row_number()) %>%
        ungroup() %>%
        select(SEASON, TEAM, WEEK_NUM, PREGAME_ELO, RANK) %>%
        left_join(.,
                  teamcolors) %>%
        ggplot(., aes(x=WEEK_NUM,  
                      group = TEAM,
                      color = name,
                      fill =  name,
                      y=RANK))+
      #  geom_point()+
        geom_line()+
        scale_fill_teams(name = 'TEAM')+
        scale_color_teams(name = 'TEAM')+
        scale_y_reverse()+
        guides(color = "none",
               fill = "none")+
        theme_phil()+
        facet_wrap(SEASON ~.)
        


# get ranks
seasons = 2016
expand.grid(SEASON = seasons,
            TEAM = elo_games$team_outcomes %>%
                    filter(SEASON %in% seasons)%>%
                    filter(DIVISION == 'fbs') %>%
                    distinct(TEAM) %>%
                    pull(TEAM),
            WEEK_NUM = seq(1, 15, 1)) %>%
        arrange(WEEK_NUM) %>%
        left_join(.,
                  elo_games$team_outcomes %>%
                          group_by(SEASON, TEAM) %>%
                          arrange(GAME_DATE) %>%
                          mutate(WEEK_NUM = row_number()) %>%
                          filter(SEASON %in% seasons) %>%
                          select(SEASON, WEEK_NUM, TEAM, PREGAME_ELO, POSTGAME_ELO),
        ) %>% 
        group_by(SEASON, TEAM) %>%
        #   fill(POSTGAME_ELO) %>%
        fill(POSTGAME_ELO, .direction = 'down') %>%
        mutate(PREGAME_ELO = case_when(is.na(PREGAME_ELO) ~ POSTGAME_ELO,
                                       TRUE ~ PREGAME_ELO)) %>%
        group_by(SEASON, WEEK_NUM) %>%
        arrange(WEEK_NUM, desc(PREGAME_ELO)) %>%
        mutate(RANK = row_number()) %>%
        ungroup() %>%
        select(SEASON, TEAM, WEEK_NUM, PREGAME_ELO, RANK) %>%
        filter(RANK <= 25) %>%
        mutate(WEEK_NUM = paste("Week", WEEK_NUM)) %>%
        pivot_wider(id_cols = c("SEASON", "RANK"),
                    names_from = c("WEEK_NUM"),
                    values_from = c("TEAM")) %>%
        head(25) %>%
        flextable() %>%
        autofit()

elo_games$team_outcomes %>%
        group_by(SEASON, TEAM) %>%
        arrange(GAME_DATE) %>%
        mutate(WEEK_NUM = row_number()) %>%
        group_by(SEASON, SEASON_TYPE, WEEK) %>%
        arrange(desc(PREGAME_ELO)) %>%
        mutate(RANK = row_number()) %>%
        filter(SEASON == 2015) %>%
        arrange(WEEK_NUM, desc(PREGAME_ELO)) %>%
        ungroup() %>%
        select(SEASON, WEEK_NUM, TEAM, RANK) %>%
        filter(RANK <=25) %>%
        pivot_wider(id_cols = c("SEASON", "RANK"),
                    names_from = c("WEEK_NUM"),
                    values_from = c("TEAM"))
        unnest()
        
