get_expected_score <-
function(team_rating, opponent_rating, V=400) {
        
        return(1 / (1 + 10^((opponent_rating - team_rating) / V)))
        
}
