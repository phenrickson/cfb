sim_game_margin <-
function(home_rating,
                           away_rating,
                           points_model) {

        # using base lm with custom simulate function
        sim <- simulateX(points_model, nsim = 1,
                               X = data.frame(HOME_ELO_DIFF = (home_rating - away_rating)))
        
        return(round(sim, 0))
}
