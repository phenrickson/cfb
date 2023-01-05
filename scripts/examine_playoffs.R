# examine issues with simulations
source(here::here("scripts/load_packages.R"))
source(here::here("functions/theme_phil.R"))

load(here::here("data", "team_colors.Rdata"))
custom_color_teams <- scale_colour_manual(name = "TEAM", values = team_colors)
custom_fill_teams <- scale_fill_manual(name = "TEAM", values = team_colors)


params = list()
params$input_season = 2022
params$sim_week = 9
params$sims = params$nsims = 1000

# get files from season folder
files = list.files(here::here("simulations",
                              "season",
                              params$input_season))

# get objects from each week
files_weeks =  files[grepl("sims_week", files)]

# load in simulations from previous weeks
week_list = foreach(i = 1:length(files_weeks)) %do% {
        
        load(here::here("simulations",
                        "season",
                        params$input_season,
                        files_weeks[i]))
        
        foo = all_simulations
        
        rm(all_simulations)
        
        foo
        
}

rm(foo)

# pull predictions table
team_predictions = do.call(rbind, lapply(week_list, function (x) x$team_predictions)) %>%
        filter(SIM_FROM_WEEK <= params$sim_week)

# pull game predictions
game_predictions = do.call(rbind, lapply(week_list, function (x) x$game_predictions)) %>%
        filter(SIM_FROM_WEEK <= params$sim_week)

# pull remaining games
sim_remaining_games = do.call(rbind, lapply(week_list, function (x) x$sim_remaining_games)) %>%
        filter(SIM_FROM_WEEK == params$sim_week)

# conference championships
sim_conference_championship_games = do.call(rbind, lapply(week_list, function (x) x$sim_conference_championship_games)) %>%
        filter(SIM_FROM_WEEK <= params$sim_week)

# playoff games
sim_playoff_games = do.call(rbind, lapply(week_list, function (x) x$sim_playoff_games)) %>%
        filter(SIM_FROM_WEEK <= params$sim_week)

# playoff games
sim_nc_games= do.call(rbind, lapply(week_list, function (x) x$sim_nc_games)) %>%
        filter(SIM_FROM_WEEK <= params$sim_week)


# # in how many simulations do alabama and tennessee both have one loss where neither wins the SEC?
# sim_remaining_games %>%
#         filter(SIM_FROM_WEEK == params$sim_week) %>%
#         filter(TEAM == 'Tennessee') %>%
#         select(.id, SIM_FROM_WEEK, GAME_ID, SEASON, GAME_DATE, TEAM, OPPONENT, SIM_OUTCOME, SIM_MARGIN) %>%
#         group_by(.id, SIM_FROM_WEEK, SEASON, TEAM, SIM_OUTCOME) %>%
#         count() %>% 
#         spread(SIM_OUTCOME, n) %>%
#         mutate_at(c("loss", "win"),
#                     replace_na, 0) %>%
#         ggplot(aes(x=win))+
#         geom_bar()

library(tidytext)

sim_playoff_games %>%
        filter(SIM_FROM_WEEK == params$sim_week) %>%
        group_by(.id) %>%
        mutate(rank = row_number()) %>%
        group_by(SEASON, rank, TEAM) %>%
        count() %>%
        mutate(Playoff_Seed = rank) %>%
        mutate(label_text = case_when(n > .01 ~ round(n/params$sims, 2))) %>%
        ggplot(., aes(x=n,
                      label = label_text,
                      y=reorder_within(TEAM, n, Playoff_Seed)))+
        geom_col()+
        geom_text(aes(x = n + 50),
                  size = 2.5)+
        facet_wrap(paste("Playoff Seed:", Playoff_Seed) ~.,
                   scales = "free_y")+
        scale_y_reordered()+
        theme_phil()+
        ylab("")+
        xlab("Number of Simulations")+
        theme(panel.grid.major = element_line(size = 0.25))+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
                      plot.subtitle =  element_text(hjust = 0.5),
              strip.text.x = element_text(size = 12))+
        ggtitle(paste("Playoff Seed Probabilities as of Week", params$sim_week))+
        coord_cartesian()
        


sim_playoff_games %>%
        filter(SIM_FROM_WEEK == params$sim_week) %>%
        group_by(.id, CONFERENCE) %>%
        count() %>%
        arrange(desc(n)) %>%
        group_by(CONFERENCE, n) %>%
        count() %>%
        set_names(., c("CONFERENCE", "playoff_teams", "simulations")) 

foo = sim_remaining_games %>% 
        filter(SIM_FROM_WEEK == params$sim_week) %>%
        group_by(.id, SEASON, SIM_FROM_WEEK, TEAM) %>% 
        slice_max(WEEK, n=1) %>% 
        group_by(.id) %>% 
        arrange(desc(POSTGAME_ELO)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <=10)

foo %>%
        group_by(rank, TEAM) %>%
        count() %>%
        ggplot(., aes(x=n,
                      y=reorder_within(TEAM, n, rank)))+
        geom_col()+
        facet_wrap(rank ~.,
                   ncol = 2,
                   scales = "free_y")+
        scale_y_reordered()
        
# plot expected wins over course of the season
team_predictions %>%
        filter(CONFERENCE %in% c('SEC')) %>%
        group_by(TEAM) %>%
        mutate(team_label = case_when(SIM_FROM_WEEK == max(SIM_FROM_WEEK) ~ TEAM)) %>%
        ungroup() %>%
        ggplot(., aes(x=SIM_FROM_WEEK,
                      label = team_label,
                      color = TEAM,
                      y=mean_wins))+
        geom_line(
                stat = 'smooth',
                method = 'loess',
                lwd =1,
                span = 0.5,
                #stat = 'smooth',
                # method = span_func(max_week)$method,
                #   se = F,
                #     formula = 'y ~ x',
                lwd = 1,
                #    span = span_func(max_week)$span,
                alpha = 0.85)+
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
                segment.angle = 20)+
        guides(color = "none")+
        coord_cartesian(xlim = c(-3, 18),
                        ylim = c(-1, 12))+
        theme_phil()+
        xlab("Season Week")+
        custom_color_teams+
        guides(color = 'none')+
        facet_wrap(CONFERENCE + CONFERENCE_DIVISION ~.)+
        ggtitle(paste(params$input_season, "Predicted Win Totals as of Week", params$sim_week),
                subtitle = str_wrap(paste("Displaying each team's average predicted win total in simulations of remaining games in the regular season."), 120))+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle =  element_text(hjust = 0.5),
              strip.text.x = element_text(size = 12))+
        ylab("Predicted Win Totals")


# plot bowl prob
# plot expected wins over course of the season
team_predictions %>%
        filter(CONFERENCE %in% c('Big Ten')) %>%
        group_by(TEAM) %>%
        mutate(team_label = case_when(SIM_FROM_WEEK == max(SIM_FROM_WEEK) ~ TEAM)) %>%
        ungroup() %>%
        ggplot(., aes(x=SIM_FROM_WEEK,
                      label = team_label,
                      color = TEAM,
                      y=made_bowl))+
        geom_line(
                stat = 'smooth',
                method = 'loess',
                lwd =1,
                span = 0.5,
                #stat = 'smooth',
                # method = span_func(max_week)$method,
                #   se = F,
                #     formula = 'y ~ x',
                lwd = 1,
                #    span = span_func(max_week)$span,
                alpha = 0.85)+
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
                segment.angle = 20)+
        guides(color = "none")+
        coord_cartesian(xlim = c(-3, 18),
                        ylim = c(-0.05, 1.05))+
        theme_phil()+
        xlab("Season Week")+
        custom_color_teams+
        guides(color = 'none')+
        facet_wrap(CONFERENCE + CONFERENCE_DIVISION ~.)+
        ggtitle(paste(params$input_season, "Bowl Probabilities as of Week", params$sim_week),
                subtitle = str_wrap(paste("Displaying each team's bowl probability based on simulations of remaining games in the regular season."), 120))+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle =  element_text(hjust = 0.5),
              strip.text.x = element_text(size = 12))+
        ylab("Pr(Bowl)")


team_predictions %>%
        #   filter(CONFERENCE %in% c('SEC')) %>%
        filter(SIM_FROM_WEEK == 0 | SIM_FROM_WEEK == params$sim_week) %>%
        select(SEASON, SIM_FROM_WEEK, TEAM, CONFERENCE, CONFERENCE_DIVISION, mean_wins) %>%
        mutate(SIM_FROM_WEEK = case_when(SIM_FROM_WEEK == min(SIM_FROM_WEEK) ~ 'begin',
                                         SIM_FROM_WEEK == max(SIM_FROM_WEEK) ~ 'current')) %>%
        spread(SIM_FROM_WEEK, mean_wins) %>%
     #   mutate(label_TEAM = case_when(abs(current - begin) >= 1 ~ TEAM)) %>%
        ggplot(., aes(x=begin,
                      label = TEAM,
                      color = TEAM,
                      y=current))+
        geom_abline(slope = 1,
                    intercept = 0,
                    alpha = 0.5,
                    col = 'grey60')+
                 #  linetype = 'dotted')+
        # geom_abline(slope = 1,
        #             intercept = 2,
        #             alpha = 0.5,
        #             col = 'deepskyblue1')+
        # geom_abline(slope = 1,
        #             intercept = -2,
        #             alpha = 0.5,
        #             col = 'red')+
        #           #  linetype = 'dotted')+
        geom_text(check_overlap=T,
                  fontface = 'bold',
                  size = 2,
                  vjust = -1)+
        # geom_text_repel(fontface = "bold",
        #               #  max.overlaps = 10,
        #                    size = 3)+
        geom_point()+
        theme_phil()+
        custom_color_teams+
        guides(color = 'none')+
        facet_wrap(CONFERENCE ~.)+
        coord_cartesian(xlim = c(-1, 12),
                        ylim = c(-1, 12))+
        xlab("Preseason Predicted Win Total")+
        ylab("Current Predicted Win Total")+
        ggtitle(paste(params$input_season, "Current vs Preseason Predicted Win Totals", params$sim_week),
                subtitle = str_wrap(paste("Displaying each team's average predicted win total in simulations of remaining games, current vs preseason."), 120))+
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              plot.subtitle =  element_text(hjust = 0.5),
              strip.text.x = element_text(size = 12))


        # annotate("label",
        #          x = 10,
        #          y = 0,
        #          size = 3,
        #         label =  "Worse than \n expected")+
        # annotate("label",
        #          x = 1,
        #          y = 11,
        #          size = 3,
        #          label =  "Better than \n expected")
        
        
        
        

load(here::here("data", "team_colors.Rdata"))
custom_color_teams <- scale_colour_manual(name = "TEAM", values = team_colors)
custom_fill_teams <- scale_fill_manual(name = "TEAM", values = team_colors)




        
