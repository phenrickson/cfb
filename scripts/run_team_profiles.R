# load necessary pieces
source(here::here("scripts/load_packages.R"))

# connect
library(DBI)
library(odbc)
library(RODBC)
library(keyring)

### connect to DBS
# connect to snowflake
myconn <- DBI::dbConnect(odbc::odbc(),
                         "SnowflakeDSII",
                         Database = "CFB_DEMO",
                         warehouse = "DEMO_WH",
                         uid="phil.henrickson",
                         pwd=keyring::key_get("AE_Snowflake"))

# function
run_game_report = function(input_team,
                           input_season)
{
        # run through
        foreach(i=1:length(input_team)) %do% {
                
                rmarkdown::render(here::here("preseason_team_profiles.Rmd"),
                                  params = list(team = input_team[i],
                                                input_season = input_season),
                                  output_file =  input_team[i],
                                  output_dir = here::here("team_profiles", "preseason", input_season))
        }
        
}

# load team names
team_conference_season = DBI::dbGetQuery(myconn,
                                         paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_TEAM_CONFERENCE_SEASON')) %>%
        as_tibble()

# teams
teams = team_conference_season %>%
        filter(DIVISION == 'fbs' & SEASON == 2022) %>%
        select(TEAM) %>%
        pull(TEAM)
        
# coooperate 
run_game_report(input_team = teams,
                input_season = 2022)
