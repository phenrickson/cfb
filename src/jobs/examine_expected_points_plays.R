load("~/Documents/projects/cfb/data/season_team_rankings.Rdata")

predicted_plays %>%
        filter(DRIVE_ID == 32315033306) %>%
        select(PLAY_ID, everything())

predicted_plays %>% 
        filter(SEASON == 2012) %>%
        filter(HOME== 'Alabama' & AWAY == 'Texas A&M') %>%
        filter(PLAY_ID == 323150333051) %>%
        select(starts_with(".pred")) %>%
     #   gather() %>%
        mutate_if(is.numeric, round, 3) %>%
        gather


scored_plays %>% 
        filter(SEASON == 2012) %>%
        filter(HOME== 'Alabama' & AWAY == 'Texas A&M') %>%
        select(PLAY_ID, everything()) %>% 
        filter(DRIVE_ID == 32315033306) %>% 
        select(PLAY_ID, PLAY_TEXT, DOWN, DISTANCE, YARDS_TO_GOAL, EP_Pre, EP_Post, EP_Added)


load("~/Documents/projects/cfb/data/season_team_rankings.Rdata")


library(cfbplotR)
library(gt)

source(here::here("functions", "theme_phil.R"))

team_efficiency = season_team_rankings %>%
        set_names(., str_to_title(names(.))) %>%
        filter(Type == 'adjusted')


team_efficiency %>%
        filter(Season == 2021) %>%
        ggplot(., aes(x=Offense,
                      y=Defense))+
      #  geom_point() +
        coord_cartesian(xlim = c(-0.35, 0.35),
                        ylim = c(-0.35, 0.35))+
        geom_vline(xintercept = 0,
                   linetype = 'dotted',
                   alpha = 0.5)+
        geom_hline(yintercept = 0,
                   linetype = 'dotted',
                   alpha = 0.5)+
        theme_phil()+
        geom_cfb_logos(aes(team = Team), width = 0.05)+
        xlab("Offense Expected Points per Play (opponent adjusted)")+
        ylab("Defense Expected Points per Play (opponent adjusted)")+
        ggtitle("Team Efficiency from the 2021 CFB Season",
                subtitle = str_wrap(paste("Evaluating teams via average expected points added per play."), 90))
        


team_efficiency %>%
        select(Season, Team, Type, Offense, Defense, Margin) %>%
        rename(Overall = Margin) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, everything()) %>%
        head(15) %>%
        mutate(Type = 'Opponent-Adjusted') %>%
        select(-Type) %>%
        gt() %>%
        data_color(
                columns = c("Offense", "Defense"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-.75,0, .75)))) %>%
        data_color(
                columns = c("Overall"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-50,0, 50))))%>%
        tab_header(
                title = "Best CFB Teams by Overall Efficiency, 2007-2021",
                subtitle = paste("Based on opponent adjusted expected points model. Garbage time excluded."))  %>%
        tab_options(
                table.font.size = 10,
                table.width = pct(90)) %>%
        gt_fmt_cfb_logo(columns = c("Team"), height=20) %>%
        cols_align(align = c("center"),
                   columns = c("Rank",
                   "Season",
              #     "Type",
                               "Team",
                               "Offense",
                               "Defense",
                               "Overall"))

team_efficiency %>%
        filter(Season == 2021) %>%
        arrange(desc(Overall)) %>%
        mutate(Rank = row_number()) %>%
        select(Season, Team, Type, Offense, Defense, Margin) %>%
        rename(Overall = Margin) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, everything()) %>%
        head(15) %>%
        mutate(Type = 'Opponent-Adjusted') %>%
        select(-Type) %>%
        gt() %>%
        data_color(
                columns = c("Offense", "Defense"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-.75,0, .75)))) %>%
        data_color(
                columns = c("Overall"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-50,0, 50))))%>%
        tab_header(
                title = "Best CFB Teams by Overall Efficiency in 2021",
                subtitle = paste("Based on opponent adjusted expected points model. Garbage time excluded."))  %>%
        tab_options(
                table.font.size = 10,
                table.width = pct(90)) %>%
        gt_fmt_cfb_logo(columns = c("Team"), height=20) %>%
        cols_align(align = c("center"),
                   columns = c("Rank",
                               "Season",
                               #     "Type",
                               "Team",
                               "Offense",
                               "Defense",
                               "Overall"))

team_efficiency %>%
        select(Season, Team, Type, Offense, Defense, Margin) %>%
        rename(Overall = Margin) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, everything()) %>%
        filter(Season == 2021) %>%
        arrange(desc(Overall)) %>%
        head(5) %>%
        mutate(Type = 'Opponent-Adjusted') %>%
        select(-Type) %>%
        arrange(desc(Defense)) %>%
        ggplot(., aes(x=Offense,
                      label = paste(Season, Team),
                      y=Defense))+
        geom_point()+
        geom_cfb_logos(aes(team = Team))+
        theme_phil()
        
        gt() %>%
        data_color(
                columns = c("Offense", "Defense"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-.75,0, .75)))) %>%
        data_color(
                columns = c("Overall"),
                colors = scales::col_numeric(
                        palette = c("red", "white", "deepskyblue1"),
                        domain = range(c(-50,0, 50))))%>%
        tab_header(
                title = "Best CFB Teams by Defensive Efficiency, 2007-2021",
                subtitle = paste("Based on opponent adjusted expected points model. Garbage time excluded."))  %>%
        tab_options(
                table.font.size = 10,
                table.width = pct(90)) %>%
        gt_fmt_cfb_logo(columns = c("Team"), height=20) %>%
        cols_align(align = c("center"),
                   columns = c("Rank",
                               "Season",
                               #     "Type",
                               "Team",
                               "Offense",
                               "Defense",
                               "Overall"))



        
        

                  