all_simulations$sim_nc_games %>%
        filter(TEAM == 'Florida State') %>%
        filter(SIM_OUTCOME == 'win') %>%
        select(.id, SEASON, WEEK, TEAM, OPPONENT, SIM_MARGIN, SIM_OUTCOME)

all_simulations$sim_playoff_games %>% 
        arrange(.id, PLAYOFF_GAME) %>% 
        group_by(.id, PLAYOFF_GAME) %>%
        mutate(row = row_number()) %>% 
        mutate(playoff_rank = case_when(PLAYOFF_GAME == '1v4' & row == 1 ~ 1,
                                        PLAYOFF_GAME == '1v4' & row == 2 ~ 4,
                                        PLAYOFF_GAME == '2v3' & row == 1 ~ 2,
                                        PLAYOFF_GAME == '2v3' & row == 2 ~ 3)) %>%
        group_by(playoff_rank, TEAM) %>%
        count(sort=T) %>%
        ggplot(., aes(x=))
        group_by(playoff_rank) %>%
        
        arrange(playoff_rank, desc(n))
        




ggplotly(
        all_simulations$sim_playoff_games %>%
                ungroup() %>%
                group_by(TEAM, OPPONENT, SIM_OUTCOME) %>%
                count(sort=T) %>%
                ungroup() %>%
                spread(SIM_OUTCOME, n) %>%
                mutate_at(c("loss", "win"), replace_na, 0) %>%
                mutate(n = win + loss) %>%
                mutate(perc = win / n) %>%
                mutate(TEAM = abbreviate(TEAM, minlength=15),
                       OPPONENT = abbreviate(OPPONENT, minlength=15)) %>%
                #  mutate(n = factor(n)) %>%
                # mutate(SIM_OUTCOME = case_when(SIM_OUTCOME == 'win' ~ 1,
                #                                TRUE ~ 0)) %>%
                ggplot(., aes(y=reorder(TEAM,n),
                              text = paste(" Team: ", TEAM,
                                           "\n",
                                           "Opponent:", OPPONENT,
                                           "\n",
                                           "Simulations:", n,
                                           "\n",
                                           "Wins:", win,
                                           "\n",
                                           "Losses:", loss),
                              fill = perc,
                              #       alpha = n,
                              x=reorder(OPPONENT,desc(n))))+
                geom_tile(color = 'grey20')+
                theme_phil()+
                theme(legend.title = element_text(),
                      legend.position = 'right')+
                scale_fill_gradient2(low = 'red',
                                     mid = 'white',
                                     midpoint = 0.5,
                                     high = 'deepskyblue1')+
                #  scale_fill_gradient2_tableau()+
                # scale_fill_gradient(low = 'grey100', high = 'red', 
                #                     limit = c(0, 25),
                #                     oob = scales::squish)+
                theme(panel.grid.major = element_blank())+
                scale_alpha_continuous(breaks = c(1, 5, 10, 25, 50, 100),
                                       oob = scales::squish,
                                       range = c(0.25, 1))+
                scale_x_discrete(position = 'top')+
                theme(axis.text.x = element_text(angle = 90,
                                                 hjust = 0,
                                                 size = 8),
                      axis.text.y = element_text(size = 8))+
                guides(fill = guide_colorbar(barheight = 15,
                                             barwidth = 1,
                                             title = 'Pr(Team Win)',
                                             title.position = 'top'))+
                guides(alpha = 'none')+
                xlab("Opponent")+
                ylab("Team")+
                ggtitle(paste("National Championship Matchups as of Week", sim_week),
                        subtitle = "")+
                theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
                      strip.text.x = element_text(size = 12))
        
        , tooltip = 'text')






ggplotly(
all_simulations$sim_nc_games %>%
        ungroup() %>%
        group_by(TEAM, OPPONENT, SIM_OUTCOME) %>%
        count(sort=T) %>%
        ungroup() %>%
        spread(SIM_OUTCOME, n) %>%
        mutate_at(c("loss", "win"), replace_na, 0) %>%
        mutate(n = win + loss) %>%
        mutate(perc = win / n) %>%
        mutate(TEAM = abbreviate(TEAM, minlength=15),
               OPPONENT = abbreviate(OPPONENT, minlength=15)) %>%
        #  mutate(n = factor(n)) %>%
        # mutate(SIM_OUTCOME = case_when(SIM_OUTCOME == 'win' ~ 1,
        #                                TRUE ~ 0)) %>%
        ggplot(., aes(y=reorder(TEAM,n),
                      text = paste(" Team: ", TEAM,
                                   "\n",
                                   "Opponent:", OPPONENT,
                                   "\n",
                                   "Simulations:", n,
                                   "\n",
                                   "Wins:", win,
                                   "\n",
                                   "Losses:", loss),
                      fill = perc,
               #       alpha = n,
                      x=reorder(OPPONENT,desc(n))))+
        geom_tile(color = 'grey20')+
        theme_phil()+
        theme(legend.title = element_text(),
              legend.position = 'right')+
        scale_fill_gradient2(low = 'red',
                             mid = 'white',
                             midpoint = 0.5,
                             high = 'deepskyblue1')+
        #  scale_fill_gradient2_tableau()+
        # scale_fill_gradient(low = 'grey100', high = 'red', 
        #                     limit = c(0, 25),
        #                     oob = scales::squish)+
        theme(panel.grid.major = element_blank())+
        scale_alpha_continuous(breaks = c(1, 5, 10, 25, 50, 100),
                               oob = scales::squish,
                               range = c(0.25, 1))+
        scale_x_discrete(position = 'top')+
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 0,
                                         size = 8),
              axis.text.y = element_text(size = 8))+
        guides(fill = guide_colorbar(barheight = 15,
                                     barwidth = 1,
                                     title = 'Pr(Team Win)',
                                     title.position = 'top'))+
        guides(alpha = 'none')+
        xlab("Opponent")+
        ylab("Team")+
        ggtitle(paste("National Championship Matchups as of Week", sim_week),
                subtitle = "")+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle =  element_text(hjust = 0.5),
              strip.text.x = element_text(size = 12))

, tooltip = 'text')

sim_week = 4
nsims = 1000

library(plotly)

#ggplotly(
all_simulations$sim_nc_games %>%
        ungroup() %>%
        group_by(TEAM, OPPONENT) %>%
        count(sort=T) %>%
        ungroup() %>%
        filter(n > 1) %>%
        mutate(TEAM = abbreviate(TEAM, minlength=15),
               OPPONENT = abbreviate(OPPONENT, minlength=15)) %>%
        #  mutate(n = factor(n)) %>%
        # mutate(SIM_OUTCOME = case_when(SIM_OUTCOME == 'win' ~ 1,
        #                                TRUE ~ 0)) %>%
        ggplot(., aes(y=reorder(TEAM, n),
                      text = paste(" Team: ", TEAM,
                                   "\n",
                                   "Opponent:", OPPONENT,
                                   "\n",
                                   "Probability:", n/nsims),
                      fill = n/nsims,
                      label = round(n/nsims, digits=2),
                      x=reorder(OPPONENT, desc(n))))+
        geom_tile(colour = 'black')+
     #   geom_text(size = 2)+
        theme_phil()+
        scale_x_discrete(position = 'top')+
        theme(legend.title = element_text(size = 10),
              legend.position = 'right')+
        scale_fill_gradientn(colours = c("grey100", "red"),
                               breaks = seq(0, 0.25, by = 0.05),
                               limits = c(0, 0.25))+        #                      limit = c(0, 0.1),
        #                      oob = scales::squish,
        #                      high = 'red')+
        theme(panel.grid.major = element_blank())+
        theme(axis.text.x = element_text(angle = 90,
                                         hjust=0,
                                         size = 8),
              axis.text.y = element_text(size = 8))+
        guides(alpha = 'none')+
        ylab("Team")+
        xlab("Opponent")+
        ggtitle(paste("National Championship Matchups as of Week", sim_week))+
            #    subtitle = str_wrap("Displaying number of times two teams played each other in the national championship in 1000 simulations of the remaining season.",75))+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle =  element_text(hjust = 0.5, size = 10))+
        guides(fill = guide_colorbar(barheight = 15,
                                                   barwidth = 1,
                                                   title = 'Pr(Matchup)',
                                                   title.position = 'top'))
#, tooltip = 'text')


