# recruiting
ranking_teams_raw = DBI::dbGetQuery(myconn,
                                    paste('SELECT * FROM CFB_DEMO.CFD_RAW.RANKINGS')) %>%
        as_tibble() %>%
        arrange(SEASON, WEEK)

# flatten out the initial json
ranking_teams_flattened = lapply(ranking_teams_raw$POLLS,
                                 function(x) fromJSON(x, flatten=T))

# name with the season and week
names(ranking_teams_flattened) = paste(ranking_teams_raw$SEASON,
                                       ranking_teams_raw$WEEK,
                                       sep = ";")

# create a function to assign the name of the poll to the rankings
tidy_rankings = function(x) {
        
        polls = x[1]$poll
        names(x[2]$ranks) = polls
        
        ranks = rbindlist(x[2]$ranks,
                          idcol = T,
                          fill = T) %>%
                rename(poll = .id)
        
        return(ranks)
        
        # df = x %$% ranks %>%
        #         arrange(rank)
        # 
        # df$poll = x
        
}

# map the function over every element of the list to create a df, then bind that all together
rankings_tidied = rbindlist(purrr::map(ranking_teams_flattened, tidy_rankings),
                            fill = T,
                            idcol=T) %>%
        separate(.id, into = c("SEASON", "WEEK"), sep=";")
