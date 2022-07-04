get_new_elos <-
function(home_rating, 
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
