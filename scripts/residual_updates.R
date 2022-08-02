# script that looked at doing a residual update
## Adjusting After Each Week

I can take the coefficients from the model trained on previous seasons and predict how teams performed relative to their expectations in week one.

```{r look at coefficients for each from the model}

week_one_adjusted = ppa_data %>%
        filter(SEASON == 2015) %>%
        mutate(YEAR = 2016) %>%
        nest(-YEAR) %>%
        rename(SEASON = YEAR) %>%
        mutate(workflow = map(data,
                              ~ ppa_wf %>%
                                      fit(.x))) %>%
        mutate(fit = map(workflow, 
                         ~ extract_fit_parsnip(.x))) %>%
        mutate(coefs = map(fit,
                           ~ tidy(.x))) %>%
        mutate(coefs_vector = map(coefs,
                                  ~ .x %>%
                                          as.data.frame() %>%
                                          select(term, estimate) %>%
                                          column_to_rownames("term"))) %>%
        mutate(intercept = map(coefs,
                               ~ .x %>% filter(term == '(Intercept)') %>% pull())) %>%
        mutate(adjusted = map2(coefs, intercept,
                               ~ .x %>% 
                                       filter(term != '(Intercept)' & term != 'HOME_FIELD_ADVANTAGE') %>%
                                       mutate(intercept = .y) %>%
                                       mutate(adjusted = estimate + intercept) %>%
                                       mutate(type = case_when(grepl("OFFENSE", term) ~ 'OFFENSE',
                                                               grepl("DEFENSE", term) ~ 'DEFENSE')) %>%
                                       mutate(TEAM = gsub("DEFENSE_ID_", "", gsub("OFFENSE_ID_", "", term))) %>%
                                       select(TEAM, type, adjusted) %>%
                                       spread(type, adjusted) %>%
                                       mutate(DEFENSE = -DEFENSE) %>%
                                       mutate(OVERALL = OFFENSE + DEFENSE) %>%
                                       mutate(TYPE = 'adjusted'))) %>%
        mutate(preds = map(workflow,
                           ~ .x %>% augment(ppa_data %>%
                                                    filter(SEASON == 2016 & WEEK ==1))))

# get coefficients
week_one_adjusted %>%
        select(adjusted) %>%
        unnest() %>% 
        rename(TEAM_ABBR = TEAM) %>%
        left_join(., team_mapping,
                  by = c("TEAM_ABBR")) %>%
        filter(TEAM %in% c("Notre Dame", "Texas")) %>%
        select(TEAM, TYPE, OFFENSE, DEFENSE, OVERALL) %>%
        mutate_if(is.numeric, round, 3) 

```

For instance, going into the week one 2016 game between Notre Dame and Texas, if we predict the net points per play for each team based on last season, we can see that after taking into account home field advantage and each other's offense/defense, Notre Dame's offense was expected to outperform Texas to the tune of an 11-12 point win. 

```{r show week one predicted efficiency notre dame texas}

# get pred plays
preds = week_one_adjusted %>%
        mutate(pred_matchup = map(workflow,
                                  ~ augment(.,
                                            bind_rows(ppa_data[0,],
                                                      ppa_data %>%
                                                              filter(SEASON == 2016) %>%
                                                              filter(WEEK == 1) %>%
                                                              filter(OFFENSE_ID %in% c("Notre Dame", "Texas")) %>%
                                                              select(SEASON, GAME_ID, WEEK,
                                                                     HOME, AWAY,
                                                                     OFFENSE_ID, 
                                                                     DEFENSE_ID,HOME_FIELD_ADVANTAGE) %>%
                                                              distinct())))) %>%
        select(pred_matchup) %>%
        unnest() %>%
        select(GAME_ID, SEASON, WEEK, HOME, AWAY, OFFENSE_ID, DEFENSE_ID, .pred) %>%
        select(GAME_ID, SEASON, WEEK, HOME, AWAY, OFFENSE_ID, DEFENSE_ID, .pred) %>%
        rename(OFFENSE = OFFENSE_ID,
               DEFENSE = DEFENSE_ID)

preds %>%
        select(-HOME, -AWAY) %>%
        mutate_if(is.numeric, round, 3)

preds %>%
        mutate(.pred = case_when(HOME == DEFENSE ~ -.pred,
                                 HOME == OFFENSE ~ .pred)) %>%
        group_by(GAME_ID, SEASON, WEEK, HOME, AWAY) %>%
        summarize(diff = sum(.pred),
                  .groups = 'drop') %>%
        mutate(margin = diff * 65) %>%
        mutate_if(is.numeric, round, 2)

```

```{r get the prediction}

matchup_predictor = function(input_workflow_obj,
                             input_game_id,
                             coefs_vec,
                             ppa_data) {
        
        # team matchup data
        matchup_info = bind_rows(ppa_data[0,],
                                 ppa_data %>%
                                         filter(GAME_ID %in% input_game_id) %>%
                                         select(SEASON, GAME_ID, WEEK,
                                                HOME, AWAY,
                                                OFFENSE_ID, 
                                                DEFENSE_ID,HOME_FIELD_ADVANTAGE) %>%
                                         distinct())
        
        # prep teams in a matrix
        matchup_obj = input_workflow_obj %>%
                mutate(matchup = map(workflow,
                                     ~ .x %>% 
                                             extract_recipe() %>%
                                             bake(matchup_info) %>%
                                             select(HOME_FIELD_ADVANTAGE, starts_with("OFFENSE_ID"), starts_with("DEFENSE_ID"))))
        
        # convert to matrix
        x= matchup_obj %>%
                select(matchup) %>%
                unnest() %>%
                select(HOME_FIELD_ADVANTAGE, starts_with("OFFENSE_ID"), starts_with("DEFENSE_ID")) %>%
                bind_cols('(Intercept)' = 1,
                          .) %>%
                as.matrix()
        
        # preds
        preds = tibble(".pred" = x %*% coefs_vec %>%
                               as.vector())
        
        # coefs
        matchup_preds = cbind.data.frame(matchup_info,
                                         preds) %>%
                select(SEASON, GAME_ID, WEEK, HOME, AWAY, OFFENSE_ID, DEFENSE_ID, HOME_FIELD_ADVANTAGE, .pred)
        
        # pred outcom
        outcome_pred = matchup_preds %>%
                mutate(.pred = case_when(HOME == DEFENSE_ID ~ -.pred,
                                         HOME == OFFENSE_ID ~ .pred)) %>%
                group_by(GAME_ID, SEASON, WEEK, HOME, AWAY) %>%
                summarize(PRED_PPA_DIFF = sum(.pred),
                          .groups = 'drop') %>%
                mutate(PRED_MARGIN = PRED_PPA_DIFF * 65)
        
        return(list("matchup_pred" = matchup_preds,
                    "outcome_pred" = outcome_pred))
        
}


matchup_predictor(input_workflow_obj = week_one_adjusted,
                  input_game_id = 400868946,
                  coefs_vec = week_one_adjusted %>% 
                          pluck("coefs_vector", 1) %>%
                          pull(estimate),
                  ppa_data
)

```

If we see how well each team actually did in terms of predicted points per play, in the case of this game, Notre Dame's offense didn't fare as well as expected against Texas' defense, but it was Texas' offense that really outperformed expectations against Notre Dame's defense. As a result, Notre Dame was expected to win by about 12, but ultimately lost by 3 (in OT), though in terms of expected points Texas barely edged them out through 4 quarters.

```{r compare preds to actual}

week_one_preds = bind_rows(week_one_adjusted %>%
                select(preds) %>%
                unnest(preds) %>%
                mutate(.resid = PPA - .pred) %>% 
                select(SEASON, WEEK, GAME_ID, GAME_DATE, OFFENSE_ID, DEFENSE_ID, PPA, .pred, .resid) %>%
                group_by(SEASON, WEEK, OFFENSE_ID) %>%
                summarize(raw = mean(PPA, na.rm=T),
                          resid = mean(.resid, na.rm=T),
                          .groups = 'drop') %>%
                  rename(TEAM = OFFENSE_ID) %>%
                  mutate(TYPE = 'OFFENSE'),
          week_one_adjusted %>%
                select(preds) %>%
                unnest(preds) %>%
                mutate(.resid = PPA - .pred) %>% 
                select(SEASON, WEEK, GAME_ID, GAME_DATE, OFFENSE_ID, DEFENSE_ID, PPA, .pred, .resid) %>%
                group_by(SEASON, WEEK, DEFENSE_ID) %>%
                summarize(raw = -mean(PPA, na.rm=T),
                          resid = -mean(.resid, na.rm=T),
                          .groups = 'drop') %>%
                  rename(TEAM = DEFENSE_ID) %>%
                  mutate(TYPE = 'DEFENSE'))

## 
week_one_preds %>%
        mutate_if(is.numeric, round, 3)  %>%
        filter(TEAM %in% c("Texas", "Notre Dame"))  %>%
        select(-resid) %>%
        filter(TYPE == 'OFFENSE') %>%
        spread(TYPE, TEAM) %>%
        select(SEASON, WEEK, OFFENSE, raw) %>%
        left_join(.,
                  week_one_adjusted %>%
                          mutate(pred_matchup = map(workflow,
                                  ~ augment(.,
                                            bind_rows(ppa_data[0,],
                                                      ppa_data %>%
                                                              filter(SEASON == 2016) %>%
                                                              filter(WEEK == 1) %>%
                                                              filter(OFFENSE_ID %in% c("Texas", "Notre Dame")) %>%
                                                              select(SEASON, GAME_ID, WEEK,
                                                                     HOME, AWAY,
                                                                     OFFENSE_ID, 
                                                                     DEFENSE_ID,HOME_FIELD_ADVANTAGE) %>%
                                                              distinct())))) %>%
                          select(pred_matchup) %>%
                          unnest() %>%
                          select(GAME_ID, SEASON, WEEK, OFFENSE_ID, DEFENSE_ID, .pred) %>%
                          rename(OFFENSE = OFFENSE_ID,
                                 DEFENSE = DEFENSE_ID) %>%
                          mutate_if(is.numeric, round, 3),
                  by = c("SEASON", "WEEK", "OFFENSE")) %>%
        rename(actual = raw) %>%
        select(GAME_ID, SEASON, WEEK, OFFENSE, DEFENSE, .pred, actual) %>%
        mutate(resid = actual - .pred)

# sum the residual
week_one_preds %>%
        mutate_if(is.numeric, round, 3)  %>%
        filter(TEAM %in% c("Texas", "Notre Dame"))  %>%
        select(-resid) %>%
        filter(TYPE == 'OFFENSE') %>%
        spread(TYPE, TEAM) %>%
        select(SEASON, WEEK, OFFENSE, raw) %>%
        left_join(.,
                  week_one_adjusted %>%
                          mutate(pred_matchup = map(workflow,
                                  ~ augment(.,
                                            bind_rows(ppa_data[0,],
                                                      ppa_data %>%
                                                              filter(SEASON == 2016) %>%
                                                              filter(WEEK == 1) %>%
                                                              filter(OFFENSE_ID %in% c("Texas", "Notre Dame")) %>%
                                                              select(SEASON, GAME_ID, WEEK,
                                                                     HOME, AWAY,
                                                                     OFFENSE_ID, 
                                                                     DEFENSE_ID,HOME_FIELD_ADVANTAGE) %>%
                                                              distinct())))) %>%
                          select(pred_matchup) %>%
                          unnest() %>%
                          select(GAME_ID, SEASON, HOME, AWAY, WEEK, OFFENSE_ID, DEFENSE_ID, .pred) %>%
                          rename(OFFENSE = OFFENSE_ID,
                                 DEFENSE = DEFENSE_ID) %>%
                          mutate_if(is.numeric, round, 3),
                  by = c("SEASON", "WEEK", "OFFENSE")) %>%
        select(GAME_ID, SEASON, WEEK, HOME, AWAY, OFFENSE, DEFENSE, .pred, raw) %>%
        mutate(resid = raw - .pred) %>%
        mutate(raw = case_when(HOME == DEFENSE ~ -raw,
                                 HOME == OFFENSE ~ raw)) %>%
        rename(actual = raw) %>%
        group_by(GAME_ID, SEASON, WEEK, HOME, AWAY) %>%
        summarize(diff = sum(actual),
                  .groups = 'drop') %>%
        mutate(margin = diff * 65) %>%
        mutate_if(is.numeric, round, 2)

```

What should Texas and Notre Dame then look like for week 2? One way we could update their ratings week over week is by evaluating how well they performed compared to expected - the residual between their predicted performance and their actual performance. Based on the matchup, if a team performed exactly as we expected on offense and defense their residual would be zero and next week's performance would be the same as last week. In the case of Notre Dame and Texas, the residual indicates how each team performed relative to expectations on each side of the ball.

```{r updated rankings after week one}

week_one_resids = week_one_preds %>%
        select(-resid) %>%
        left_join(.,
                  week_one_adjusted %>%
                          mutate(pred_matchup = map(workflow,
                                                    ~ augment(.,
                                                              bind_rows(ppa_data[0,],
                                                                        ppa_data %>%
                                                                                filter(SEASON == 2016) %>%
                                                                                filter(WEEK == 1) %>%
                                                                                select(SEASON, GAME_ID, WEEK,
                                                                                       HOME, AWAY,
                                                                                       OFFENSE_ID, 
                                                                                       DEFENSE_ID,HOME_FIELD_ADVANTAGE) %>%
                                                                                distinct())))) %>%
                          select(pred_matchup) %>%
                          unnest() %>%
                          select(GAME_ID, SEASON, WEEK, OFFENSE_ID, DEFENSE_ID, .pred) %>%
                          rename(OFFENSE = OFFENSE_ID,
                                 DEFENSE = DEFENSE_ID) %>%
                          gather("TYPE", "TEAM", 
                                 -GAME_ID, -SEASON, -WEEK, -.pred) %>%
                          mutate(.pred = case_when(TYPE == 'OFFENSE' ~ .pred,
                                                   TYPE == 'DEFENSE' ~ -.pred)),
                  by = c("SEASON", "WEEK", "TYPE", "TEAM")) %>%
        select(GAME_ID, SEASON, WEEK, TEAM, TYPE, .pred, raw) %>%
        mutate(.resid = raw - .pred) %>%
        mutate_if(is.numeric, round, 2) %>%
        group_by(TEAM) %>%
        filter(GAME_ID == max(GAME_ID)) %>%
        left_join(., team_mapping %>%
                          select(TEAM, TEAM_ABBR),
                  by = c("TEAM")) %>%
        mutate(term = paste(TYPE, "ID", TEAM_ABBR, sep="_")) %>%
        select(SEASON, WEEK, TEAM, term, .pred, raw, .resid) %>%
        ungroup()


week_one_resids %>%
        filter(TEAM %in% c("Texas", "Notre Dame"))

```

Note: the prediction for each team's offense/defense isn't their coefficient, it's their expected points per play based on the two team's coefficients and the home field advantage. So, to upate each team's rating, I need to get their coefficient going into week one. Then, for week two, I combine the residual from from week 1 with their original coefficient to get their expectation going into week two.

```{r get updated coefs}

# get the updated coef
delta = 0.75
season = 2016
week = 1

# coefs table
coefs_table = tibble(SEASON = 2016,
                     WEEK = 1) %>%
        bind_cols(.,
                  week_one_adjusted %>%
                             select(coefs) %>%
                             unnest() %>%
                          select(term, estimate))

coefs_update= coefs_table %>%
        filter(WEEK == week ) %>%
        select(SEASON, term, estimate) %>%
        left_join(.,  week_one_resids %>%
                          select(SEASON,  term, .resid),
                  by = c("SEASON", "term")) %>%
        mutate(update = case_when(is.na(.resid) ~ estimate,
                                  TRUE ~ (estimate * delta) + (.resid*(1-delta)))) %>%
        mutate(WEEK = week +1) %>%
        select(SEASON, WEEK, term, estimate, .resid, update)

coefs_table = bind_rows(coefs_table,
                        coefs_update %>%
                                select(SEASON, WEEK, term, update) %>%
                                rename(estimate =update))

coefs_table %>%
        filter(term %in% c("OFFENSE_ID_Texas",
                           "DEFENSE_ID_Texas",
                           "OFFENSE_ID_Notre.Dame",
                           "DEFENSE_ID_Notre.Dame")) %>%
        mutate_if(is.numeric, round, 3) %>% 
        spread(WEEK, estimate)

```

Assigning a 50% weight to their original estimate and a 50% weight to the estimate + the residual, we get an updated coefficient, which we can then use to predict them moving forward. Based on this result, if we had the two teams play again at Texas based on what we learned from Week 1, we'd have Notre Dame expected to win by 4. If we assign a full weight to the residual, we have Texas favored to win by 2.

```{r now predict last week}

matchup_predictor(input_workflow_obj = week_one_adjusted,
                  input_game_id = 400868946,
                  coefs_vec = coefs_table %>%
                          filter(WEEK ==1) %>%
                          pull(estimate),
                  ppa_data
) %$% outcome_pred

matchup_predictor(input_workflow_obj = week_one_adjusted,
                  input_game_id = 400868946,
                  coefs_vec = coefs_table %>%
                          filter(WEEK ==2) %>%
                          pull(estimate),
                  ppa_data
) %$% outcome_pred

```

We can then predict every matchup for week two given the updated ratings and see how we do.

```{r predictd week 2 outcomes}

week_two_preds = matchup_predictor(week_one_adjusted,
                                   input_game_id = ppa_data %>%
                                           filter(SEASON == 2016 & WEEK ==2) %>%
                                           distinct(GAME_ID) %>%
                                           pull(GAME_ID),
                                   coefs_vec = coefs_table %>%
                                           filter(WEEK == 2) %>%
                                           pull(estimate),
                                   ppa_data) %$% outcome_pred %>%
        select(-PRED_PPA_DIFF) %>%
        mutate_if(is.numeric, round, 1) %>%
        left_join(.,
                  games_raw %>%
                          mutate(HOME_MARGIN = HOME_POINTS - AWAY_POINTS) %>%
                          select(GAME_ID, HOME_MARGIN),
                  by = c("GAME_ID"))


week_two_preds %>%
        yardstick::rmse(truth = HOME_MARGIN,
                        estimate = PRED_MARGIN)
select(-GAME_ID) %>%
        mutate(SEASON = factor(SEASON)) %>%
        flextable() %>%
        autofit() %>%
        bg(., j = c("PRED_MARGIN",
                    "HOME_MARGIN"),
           bg = col_power_func)

week_two_preds %>%
        mutate(HOME_PRED = factor(case_when(PRED_MARGIN > 0 ~ 'yes',
                                            TRUE ~ 'no'),
                                  levels = c("no", "yes"))) %>%
        mutate(HOME_WIN = factor(case_when(HOME_MARGIN > 0 ~ 'yes',
                                           TRUE ~ 'no'),
                                 levels = c("no", "yes"))) %>%
        yardstick::accuracy(truth = HOME_WIN,
                            estimate = HOME_PRED)

```

```{r compute rolling residual}

ppa_data %>%
        filter(SEASON == 2015) %>%
        mutate(YEAR = 2016) %>%
        nest(-YEAR) %>%
        rename(SEASON = YEAR) %>%
        mutate(workflow = map(data,
                              ~ ppa_wf %>%
                                      fit(.x))) %>%
        mutate(fit = map(workflow, 
                         ~ extract_fit_parsnip(.x))) %>%
        mutate(coefs = map(fit,
                           ~ tidy(.x))) %>%
        mutate(coefs_vector = map(coefs,
                                  ~ .x %>%
                                          as.data.frame() %>%
                                          select(term, estimate) %>%
                                          column_to_rownames("term"))) %>%
        mutate(intercept = map(coefs,
                               ~ .x %>% filter(term == '(Intercept)') %>% pull())) %>%
        mutate(adjusted = map2(coefs, intercept,
                               ~ .x %>% 
                                       filter(term != '(Intercept)' & term != 'HOME_FIELD_ADVANTAGE') %>%
                                       mutate(intercept = .y) %>%
                                       mutate(adjusted = estimate + intercept) %>%
                                       mutate(type = case_when(grepl("OFFENSE", term) ~ 'OFFENSE',
                                                               grepl("DEFENSE", term) ~ 'DEFENSE')) %>%
                                       mutate(TEAM = gsub("DEFENSE_ID_", "", gsub("OFFENSE_ID_", "", term))) %>%
                                       select(TEAM, type, adjusted) %>%
                                       spread(type, adjusted) %>%
                                       mutate(DEFENSE = -DEFENSE) %>%
                                       mutate(OVERALL = OFFENSE + DEFENSE) %>%
                                       mutate(TYPE = 'adjusted')))

```

```{r compute residual week over week}

# get the updated coef
delta = 0.5
season = 2016
weeks = seq(1, 16, 1)

# coefs table
workflow = ppa_data %>%
        filter(SEASON == 2015) %>%
        mutate(YEAR = 2016) %>%
        nest(-YEAR) %>%
        rename(SEASON = YEAR) %>%
        mutate(workflow = map(data,
                              ~ ppa_wf %>%
                                      fit(.x))) %>%
        mutate(fit = map(workflow, 
                         ~ extract_fit_parsnip(.x))) %>%
        mutate(coefs = map(fit,
                           ~ tidy(.x)))

coefs_table = workflow %>%
        select(SEASON, coefs) %>%
        unnest() %>%
        mutate(WEEK = 0) %>%
        select(SEASON, WEEK, term, estimate)

# now update for each week
foreach(i = 1:length(weeks),
        .combine = bind_rows) %do% {
                
                
                # compute resids
                
                coefs_update= coefs_table %>%
                        filter(WEEK == weeks[i]) %>%
                        select(SEASON, term, estimate) %>%
                        left_join(.,  week_one_resids %>%
                                          select(SEASON,  term, .resid),
                                  by = c("SEASON", "term")) %>%
                        mutate(update = case_when(is.na(.resid) ~ estimate,
                                                  TRUE ~ (estimate * delta) + (.resid*(1-delta)))) %>%
                        mutate(WEEK = week +1) %>%
                        select(SEASON, WEEK, term, estimate, .resid, update)
                
                coefs_table = bind_rows(coefs_table,
                                        coefs_update %>%
                                                select(SEASON, WEEK, term, update) %>%
                                                rename(estimate =update))
                
                coefs_table %>%
                        filter(term %in% c("OFFENSE_ID_Texas",
                                           "DEFENSE_ID_Texas",
                                           "OFFENSE_ID_Notre.Dame",
                                           "DEFENSE_ID_Notre.Dame")) %>%
                        mutate_if(is.numeric, round, 3) %>% 
                        spread(WEEK, estimate)
                
                
        }




```