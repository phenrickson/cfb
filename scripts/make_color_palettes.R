# make custom color and fill scales for ggplot

# packages
source(here::here("scripts/load_packages.R"))

library(jsonlite)
library(forcats)
library(DBI)
library(odbc)
library(RODBC)
library(keyring)

# connect to snowflake
myconn <- DBI::dbConnect(odbc::odbc(),
                         "SnowflakeDSII",
                         Database = "CFB_DEMO",
                         warehouse = "DEMO_WH",
                         uid="phil.henrickson",
                         pwd=keyring::key_get("AE_Snowflake"))
# query teams
teams_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.TEAMS')) %>%
        as_tibble() %>%
        rename(TEAM = SCHOOL,
               TEAM_ID = ID,
               TEAM_MASCOT = MASCOT,
               TEAM_ABBREVIATION = ABBREVIATION)

# make mapping for color
team_mapping = teams_raw %>%
        mutate(TEAM_ABBR = gsub(" ", "\\.", gsub("[[:punct:]]", "\\.", TEAM))) %>%
        select(TEAM, TEAM_ABBR, TEAM_ABBREVIATION, COLOR, ALT_COLOR) %>%
        mutate(COLOR = case_when(TEAM == 'Toledo' ~ '#003E7E',
                                 TRUE ~ COLOR))

# get colors
team_colors <- team_mapping %>%
        filter(!is.na(COLOR)) %>%
        pull(COLOR)

# set names
names(team_colors) = team_mapping %>%
        filter(!is.na(COLOR)) %>%
        pull(TEAM)

# save team colors
save(team_colors,
     file = here::here("data", "team_colors.Rdata"))

# now save color
custom_color_teams <- scale_colour_manual(name = "TEAM", values = team_colors)
custom_fill_teams <- scale_fill_manual(name = "TEAM", values = team_colors)
