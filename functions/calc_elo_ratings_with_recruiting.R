calc_elo_ratings_with_recruiting <-
function(games,
                                            recruiting,
                                            teams = list(),
                                            team_seasons = list(),
                                            home_field_advantage,
                                            reversion,
                                            recruiting_adjustment =0,
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
                if (game$NEUTRAL_SITE==T) {add_home_field_advantage = 0} else 
                        if (game$NEUTRAL_SITE==F) {add_home_field_advantage = home_field_advantage}
                
                # check whether the team has already played in a season
                # check whether the season of the game is the same season as last game for the team
                # if not and in FBS, apply a mean reversion + recruiting adjustment
                # if not and in FCS, apply a mean reversion
                
                if(length(team_seasons[[game$HOME_TEAM]])==0) {home_last_season = 0} else 
                {home_last_season = team_seasons[[game$HOME_TEAM]]}
                
                if(length(team_seasons[[game$AWAY_TEAM]])==0) {away_last_season = 0} else 
                {away_last_season = team_seasons[[game$AWAY_TEAM]]}
                
                home_current_season = game$SEASON
                away_current_season = game$SEASON
                
                # now apply reversion to home team
                # home team
                # if the team is not present in previous seasons, then assign them as this season
                # if the season is the same as the current season then the home rating is the same
                # if the saeson is less than the current season and the team isnt missing conference (an FBS team), then reversion by 1500
                # if the season is less than the current season and the team is missing conference, then reversion by 1200
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
                
                # do the same for the away team
                if (length(team_seasons[[game$AWAY_TEAM]]) ==0) {team_seasons[[game$AWAY_TEAM]] = game$SEASON} else
                        if (game$SEASON == team_seasons[[game$AWAY_TEAM]]) {away_rating = away_rating} else 
                                if (
                                        (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & !is.na(game$AWAY_CONFERENCE)
                                )
                                {away_rating = ((reversion*1500)+ (1-reversion)*away_rating)} else
                                        if (
                                                (team_seasons[[game$AWAY_TEAM]] < game$SEASON) & is.na(game$AWAY_CONFERENCE)
                                        )
                                        {away_rating = ((reversion*1200)+(1-reversion)*away_rating)}
                
                
                ## now apply recruiting adjustment at beginning of the season
                # pull home recruiting if current season is one less than previous season (ie, next year)
                if ((home_current_season - home_last_season) ==1) {home_recruiting = recruiting %>% 
                        filter(SEASON == game$SEASON) %>%
                        filter(TEAM == game$HOME_TEAM) %>%
                        pull(RECRUITING_SCORE)} else {away_recruiting = vector()}
                
                # pull away
                if ((away_current_season - away_last_season) ==1) {away_recruiting = recruiting %>% 
                        filter(SEASON == game$SEASON) %>%
                        filter(TEAM == game$AWAY_TEAM) %>%
                        pull(RECRUITING_SCORE)} else {away_recruiting = vector()}
                
                # if it is first game of the new season but we don't have a recruiting score, then home rating isn't adjusted
                # otherwise, if it is first game of the new season and we have a recruiting score, then adjust home rating
                if ((home_current_season - home_last_season) !=1) {home_rating = home_rating} else
                        if ((home_current_season - home_last_season) ==1 & length(home_recruiting)==1) {home_rating = home_rating + 
                                (home_recruiting*recruiting_adjustment)} else {home_rating = home_rating}
                
                # apply same to away team
                if ((away_current_season - away_last_season) !=1 | length(away_recruiting)==0) {away_rating = away_rating} else
                        if ((away_current_season - away_last_season) ==1 & length(home_recruiting)==1) {away_rating = away_rating + 
                                (away_recruiting*recruiting_adjustment)} else {away_rating = away_rating}
                
                ## get the score margin based on the home team
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
                       AWAY_TEAM,
                       AWAY_POINTS) %>%
                rename(OPPONENT = AWAY_TEAM,
                       OPPONENT_POINTS = AWAY_POINTS) %>%
                set_names(., gsub("HOME_", "", names(.))) %>%
                bind_rows(.,
                          game_outcomes %>% 
                                  select(GAME_ID, 
                                         SEASON,
                                         WEEK,
                                         GAME_DATE,
                                         starts_with("AWAY_"),
                                         HOME_TEAM,
                                         HOME_POINTS) %>%
                                  rename(OPPONENT = HOME_TEAM,
                                         OPPONENT_POINTS = HOME_POINTS) %>%
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
