expected_points_func <-
function(x) {
        
        x %>%
                mutate(EP = 0 * `.pred_No_Score` + 
                               7 * .pred_TD + 3 * .pred_FG + 
                               2 * .pred_Safety + 
                               -2*`.pred_Opp_Safety` + 
                               -3*`.pred_Opp_FG` + 
                               -7*`.pred_Opp_TD`)
        
}
