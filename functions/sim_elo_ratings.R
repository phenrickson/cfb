sim_elo_ratings <-
function(games,
                           teams = list(),
                           team_seasons = list(),
                           home_field_advantage,
                           reversion,
                           k,
                           v,
                           ties =F,
                           points_model = points_model,
                           verbose=F) {
        
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
                        if (game$HOME_DIVISION == 'fbs') {home_rating = 1500} else {home_rating = 1200}
                
                # get away elo rating
                if (game$AWAY_TEAM %in% names(teams))
                {away_rating = teams[[game$AWAY_TEAM]]} else 
                        if (game$AWAY_DIVISION == 'fbs') {away_rating = 1500} else {away_rating = 1200}
                
                # check whether its a neutral site game to apply home field advantage adjustment
                if (game$NEUTRAL_SITE==T) {add_home_field_advantage = 0} else 
                        if (game$NEUTRAL_SITE==F) {add_home_field_advantage = home_field_advantage}
                
                # check whether the team has already played in a season
                # check whether the season of the game is the same season 
                # as the current team season value
                # if not, apply a mean reversion
                
                # set reversion amount
                # home team
                if (length(team_seasons[[game$HOME_TEAM]]) ==0) {team_seasons[[game$HOME_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$HOME_TEAM]]) {home_rating = home_rating} else 
                                if (
                                        (team_seasons[[game$HOME_TEAM]] < game$SEASON) & game$HOME_DIVISION == 'fbs'
                                )
                                {home_rating = ((reversion*1500)+ (1-reversion)*home_rating)} else
                                        if (
                                                (team_seasons[[game$HOME_TEAM]] < game$SEASON) & game$HOME_DIVISION != 'fbs'
                                        )
                                        {home_rating = ((reversion*1200)+(1-reversion)*home_rating)}
                
                # away team
                if (length(team_seasons[[game$AWAY_TEAM]]) ==0) {team_seasons[[game$AWAY_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$AWAY_TEAM]]) {away_rating = away_rating} else 
                                if (
                                        (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & game$AWAY_DIVISION == 'fbs'
                                )
                                {away_rating = ((reversion*1500)+ (1-reversion)*away_rating)} else
                                        if (
                                                (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & game$AWAY_DIVISION != 'fbs'
                                        )
                                        {away_rating = ((reversion*1200)+(1-reversion)*away_rating)}
                
                ## simulate the margin via specified points model
                home_margin = sim_game_margin(home_rating + add_home_field_advantage, 
                                              away_rating,
                                              points_model)
                
                # adjust for ties
                # if ties =F, then a margin of 0 will give home team a 1 point win
                if (ties == F & home_margin == 0) {home_margin = 1} else {home_margin = home_margin}
                
                ## get the score margin based on the home team
                #  home_margin = game$HOME_POINTS - game$AWAY_POINTS
                
                # define outcome
                if (home_margin > 0) {home_outcome = 'win'} else
                        if (home_margin < 0) {home_outcome = 'loss'} else
                                if (home_margin == 0) {home_outcome = 'tie'}
                if (home_margin > 0) {away_outcome = 'loss'} else
                        if (home_margin < 0) {away_outcome = 'win'} else
                                if (home_margin == 0) {away_outcome = 'tie'}
                
                # get updated elo for both teams
                new_elos = get_new_elos(home_rating,
                                        away_rating,
                                        home_margin,
                                        add_home_field_advantage,
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
                
                # get the score and game outcome
                game$HOME_SIM_MARGIN = home_margin
                game$HOME_SIM_OUTCOME = home_outcome
                game$AWAY_SIM_MARGIN = -home_margin
                game$AWAY_SIM_OUTCOME = away_outcome
                
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
                       SEASON_TYPE,
                       WEEK,
                       GAME_DATE,
                       HOME_TEAM,
                       HOME_CONFERENCE,
                       HOME_DIVISION,
                       HOME_SIM_MARGIN,
                       HOME_SIM_OUTCOME,
                       HOME_PREGAME_ELO,
                       HOME_POSTGAME_ELO,
                       AWAY_TEAM,
                       AWAY_POINTS) %>%
                rename(OPPONENT = AWAY_TEAM,
                       OPPONENT_POINTS = AWAY_POINTS) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          game_outcomes %>% 
                                  select(GAME_ID, 
                                         SEASON,
                                         SEASON_TYPE,
                                         WEEK,
                                         GAME_DATE,
                                         AWAY_TEAM,
                                         AWAY_CONFERENCE,
                                         AWAY_DIVISION,
                                         AWAY_SIM_MARGIN,
                                         AWAY_SIM_OUTCOME,
                                         AWAY_PREGAME_ELO,
                                         AWAY_POSTGAME_ELO,
                                         HOME_TEAM,
                                         HOME_POINTS) %>%
                                  rename(OPPONENT = HOME_TEAM,
                                         OPPONENT_POINTS = HOME_POINTS) %>%
                                  set_names(., gsub("AWAY_", "", names(.)))) %>%
                arrange(GAME_DATE) %>%
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
