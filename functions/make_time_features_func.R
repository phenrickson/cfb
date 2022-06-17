make_time_features_func <-
function(x) {
        
        
        x %>%
                mutate(TIME = gsub('\\{|}|"', '', CLOCK)) %>%
                separate(TIME, into=c("MINUTES", "SECOND"), sep=",") %>%
                mutate(MINUTES = as.numeric(gsub("minutes:", "", MINUTES))) %>% 
                mutate(SECOND = as.numeric(gsub("seconds:", "", SECOND))) %>%
                mutate(MINUTES_IN_HALF = case_when(PERIOD == 1 ~ MINUTES+ 15,
                                                   PERIOD == 2 ~ MINUTES,
                                                   PERIOD == 3 ~ MINUTES + 15,
                                                   PERIOD == 4 ~ MINUTES)) %>%
                mutate(SECONDS_IN_HALF = MINUTES_IN_HALF*60 + SECOND)
        #    mutate(SECONDS_IN_HALF = )
        
}
