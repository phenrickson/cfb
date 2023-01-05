# ```{r forecast}
# 
# library(timetk)
# 
# lag_vars = c("OFFENSE", "DEFENSE", "OVERALL")
# 
# horizons = c(1, 2, 3)
# lookback = c(1, 2)
# 
# rankings_lags = adjusted_team_rankings %>%
#         arrange(SEASON) %>%
#         group_by(TEAM) %>%
#         mutate(n = n()) %>%
#         ungroup() %>%
#         filter(n > 4) %>%
#         select(-n) %>%
#         left_join(., team_conference_season,
#                   by = c("SEASON", "TEAM")) %>%
#         mutate(CONFERENCE = case_when(CONFERENCE == 'Pac-10' | CONFERENCE == 'Pac-12' ~ 'Pac-10/Pac-12',
#                                       TRUE ~ CONFERENCE)) %>%
#         #   filter(CONFERENCE %in% c('SEC', 'Big Ten', 'Big 12', 'Pac-10/Pac-12', 'ACC')) %>%
#         group_by(TEAM) %>%
#         tk_augment_lags(.,
#                         .value = one_of(lag_vars),
#                         .lags = lookback) %>%
#         tk_augment_leads(.,
#                          .value = one_of(lag_vars),
#                          .lags = -horizons,
#         ) %>%
#         ungroup()
# 
# foo = rankings_lags %>%
#         select(TEAM, SEASON, OFFENSE, DEFENSE, OVERALL, 
#                contains("_lag"), contains("_lead")) %>%
#         gather("outcome", "value",
#                -TEAM, -SEASON, -OFFENSE, -DEFENSE, -OVERALL,
#                -contains("lag")) %>%
#         mutate(horizon = gsub("OVERALL_lead","",  gsub("OFFENSE_lead", "", gsub("DEFENSE_lead", "", outcome)))) %>%
#         select(horizon, everything()) %>%
#         na.omit() %>%
#         nest(-horizon, -outcome) %>%
#         filter(grepl("OVERALL", outcome)) %>%
#         mutate(train = map(data,
#                            ~ .x %>% 
#                                    select(value, contains("OVERALL")))) %>%
#         mutate(lm = map(train,
#                         ~ lm(value ~.,
#                              data = .x))) %>%
#         mutate(glanced = map(lm, glance)) %>%
#         mutate(tidied = map(lm, tidy, se='robust')) %>%
#         mutate(augmented = map2(lm, data,
#                                 ~ augment(.x, interval = 'prediction', new_data = .y) %>%
#                                         bind_cols(., .y %>%
#                                                           select(SEASON, TEAM))))
# 
# foo %>%
#         select(horizon, tidied) %>%
#         unnest() %>%
#         mutate_if(is.numeric, round, 3) 
# 
# foo %>%
#         select(horizon, glanced) %>%
#         unnest() %>%
#         mutate_if(is.numeric, round, 3) 
# 
# 
# foo %>%
#         select(horizon, augmented) %>%
#         unnest() %>%
#         ggplot(., aes(x=.fitted,
#                       label = paste(TEAM, SEASON),
#                       y= value))+
#         facet_wrap(horizon ~.)+
#         geom_text(check_overlap = T,
#                   size = 3,
#                   vjust = -1)+
#         geom_point()+
#         geom_abline(slope=1,
#                     intercept=0)+
#         stat_cor(p.accuracy = 0.01)
# 
# foo %>%
#         select(horizon, augmented)
# 
# foo %>%
#         mutate(preds = map(lm, 
#                            ~ predict(.x,
#                                      interval = 'prediction',
#                                      newdata = rankings_lags %>%
#                                              filter(TEAM == 'Texas A&M') %>%
#                                              na.omit()) %>%
#                                    as.data.frame() %>%
#                                    set_names(., c("fit", "lwr", "upr")) %>%
#                                    bind_cols(., rankings_lags %>%
#                                                      filter(TEAM == 'Texas A&M') %>%
#                                                      na.omit()))) %>%
#         select(horizon, preds) %>%
#         unnest(preds) %>%
#         mutate(horizon = as.numeric(horizon)) %>%
#         ggplot(., aes(x=horizon,
#                       y=fit,
#                       ymin = lwr,
#                       ymax = upr))+
#         geom_ribbon(alpha = 0.2)+
#         geom_line()+
#         facet_wrap(SEASON ~.)
# 
# ```
# 
