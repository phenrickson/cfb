# train expected points model
# currently set to train through 2018

# databases
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

source(here::here("scripts/load_packages.R"))
library(jsonlite)
library(forcats)
conflict_prefer("lag", "dplyr")

source(here::here("functions/theme_phil.R"))
source(here::here("functions/clean_plays_func.R"))
source(here::here("functions/make_time_features_func.R"))
source(here::here("functions/expected_points_func.R"))
source(here::here("functions/get_drive_score_events.R"))
source(here::here("functions/assign_play_score_events.R"))
rm(a)

### pull from snowflake
# get plays
plays_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.PLAY_BY_PLAY')) %>%
        as_tibble() %>%
        mutate(TIME = gsub('\\{|}|"', '', CLOCK)) %>%
        separate(TIME, into=c("MINUTES", "SECONDS"), sep=",") %>%
        mutate(MINUTES = as.numeric(gsub("minutes:", "", MINUTES))) %>% 
        mutate(SECONDS = as.numeric(gsub("seconds:", "", SECONDS))) %>%
        mutate(MINUTES_IN_HALF = case_when(PERIOD == 1 ~ MINUTES+ 15,
                                           PERIOD == 2 ~ MINUTES,
                                           PERIOD == 3 ~ MINUTES + 15,
                                           PERIOD == 4 ~ MINUTES)) %>%
        mutate(SECONDS_IN_HALF = MINUTES_IN_HALF*60 + SECONDS) %>%
        # flag if seconds in half is invalid
        mutate(FLAG_SECONDS_IN_HALF = case_when(SECONDS_IN_HALF > 1800 ~ 1,
                                                TRUE ~ 0)) %>%
        # convert IDs to numeric
        mutate(ID = as.numeric(ID),
               DRIVE_ID = as.numeric(DRIVE_ID)) %>%
        rename(PLAY_ID = ID) %>%
        group_by(GAME_ID) %>%
        # sort each game by drive
        arrange(PLAY_ID) %>% 
        ungroup() %>%
        # clean up PERIOD by game
        mutate(STATUS_PERIOD = case_when(PERIOD == 0 & nchar(PLAY_TEXT) <=1 ~ 'Drop',
                                         PERIOD == 0 ~ 'Take Previous Value',
                                         TRUE ~ 'Valid')) %>%
        filter(STATUS_PERIOD != 'Drop') %>%
        mutate(PERIOD = case_when(STATUS_PERIOD == 'Take Previous Value' ~ lag(PERIOD, 1),
                                  TRUE ~ PERIOD)) %>%
        ungroup() %>% 
        # make half feature
        mutate(HALF = case_when(PERIOD == 1 ~ 'First Half',
                                PERIOD == 2 ~ 'First Half',
                                PERIOD == 3 ~ 'Second Half',
                                PERIOD == 4 ~ 'Second Half',
                                TRUE ~ 'OT')) %>%
        # then clean up the down
        mutate(FLAG_DOWN = case_when(DOWN %in% c(1, 2, 3, 4) ~ 0,
                                     TRUE ~ 1)) %>%
        mutate(DOWN = case_when(DOWN %in% c(1, 2, 3, 4) ~ DOWN,
                                TRUE ~ -1)) %>%
        # then flag the distance
        mutate(FLAG_DISTANCE = case_when((DISTANCE < 0 | DISTANCE >=99) ~ 1,
                                         TRUE ~ 0)) %>%
        # flag yard lines that are outside correct
        mutate(FLAG_YARD_LINE = case_when(YARD_LINE < 0 | YARD_LINE > 100 ~ 1,
                                          TRUE ~ 0))

# get games
games_raw = DBI::dbGetQuery(myconn,
                            paste('SELECT * FROM CFB_DEMO.CFD_RAW.ANALYSIS_GAMES')) %>%
        as_tibble() %>%
        mutate(GAME_DATE = as.Date(START_DATE)) %>%
        arrange(START_DATE)

# get drives
suppressMessages({
drives_raw = DBI::dbGetQuery(myconn,
                             paste('SELECT * FROM CFB_DEMO.CFD_RAW.DRIVES')) %>%
        as_tibble() %>%
        mutate(ID = as.numeric(ID),
               GAME_ID = as.numeric(GAME_ID)) %>%
        rename(DRIVE_ID = ID) %>%
        # get drive time features
        # start of drive
        mutate(START_TIME = gsub('\\{|}|"', '', START_TIME)) %>%
        separate(START_TIME, into=c("START_MINUTES", "START_SECONDS"), sep=",") %>%
        mutate(START_MINUTES = as.numeric(gsub("minutes:", "", START_MINUTES))) %>% 
        mutate(START_SECONDS = as.numeric(gsub("seconds:", "", START_SECONDS))) %>%
        # end of drive
        mutate(END_TIME = gsub('\\{|}|"', '', END_TIME)) %>%
        separate(END_TIME, into=c("END_MINUTES", "END_SECONDS"), sep=",") %>%
        mutate(END_MINUTES = as.numeric(gsub("minutes:", "", END_MINUTES))) %>% 
        mutate(END_SECONDS = as.numeric(gsub("seconds:", "", END_SECONDS))) %>%
        # duration of drive
        mutate(ELAPSED = gsub('\\{|}|"', '', ELAPSED)) %>%
        separate(ELAPSED, into=c("ELAPSED_MINUTES", "ELAPSED_SECONDS"), sep=",") %>%
        mutate(ELAPSED_MINUTES = as.numeric(gsub("minutes:", "", ELAPSED_MINUTES))) %>% 
        mutate(ELAPSED_SECONDS = as.numeric(gsub("seconds:", "", ELAPSED_SECONDS))) 
})

# now get scoring events for the drives data
drives_score_events = drives_raw %>%
        # run through score events function
        get_drive_score_events()

# now join with plays
plays_score_events = plays_raw %>%
        left_join(.,
                  drives_score_events %>%
                          select(GAME_ID,
                                 DRIVE_ID,
                                 HOME, 
                                 AWAY,
                                 LAST_DRIVE_HALF,
                                 END_HOME_SCORE,
                                 END_AWAY_SCORE,
                                 SCORE_EVENT,
                                 NEXT_SCORE_EVENT),
                  by = c("GAME_ID",
                         "DRIVE_ID",
                         "HOME", 
                         "AWAY")) %>%
        # now join with games to get seasons
        left_join(.,
                  games_raw %>%
                          select(GAME_ID,
                                 SEASON,
                                 SEASON_TYPE,
                                 HOME_TEAM,
                                 AWAY_TEAM,
                                 HOME_DIVISION,
                                 AWAY_DIVISION),
                  by = c("GAME_ID")) %>%
        # now assign offense defense score events using function
        assign_play_score_events()

# prepare data for modeling
# full plays
plays_full = plays_score_events %>%
        filter(PLAY_TYPE != 'Kickoff') %>%
        arrange(SEASON, GAME_ID) %>%
        ungroup() %>%
        mutate(OFFENSE_DIVISION = case_when(HOME_TEAM == OFFENSE ~ HOME_DIVISION,
                                            HOME_TEAM == DEFENSE ~ AWAY_DIVISION),
               DEFENSE_DIVISION = case_when(AWAY_TEAM == DEFENSE ~ AWAY_DIVISION,
                                            AWAY_TEAM == OFFENSE ~ HOME_DIVISION)) %>%
        select(GAME_ID,
               DRIVE_ID,
               PLAY_ID,
               SEASON,
               SEASON_TYPE,
               HOME,
               AWAY,
               OFFENSE,
               DEFENSE,
               OFFENSE_CONFERENCE,
               DEFENSE_CONFERENCE,
               OFFENSE_DIVISION,
               DEFENSE_DIVISION,
               OFFENSE_SCORE,
               DEFENSE_SCORE,
               # OFFENSE_PLAY_NUMBER,
               # DEFENSE_PLAY_NUMBER,
               SCORING,
               PLAY_TEXT,
               PLAY_TYPE,
               NEXT_SCORE_EVENT_HOME,
               NEXT_SCORE_EVENT_HOME_DIFF,
               NEXT_SCORE_EVENT_OFFENSE,
               NEXT_SCORE_EVENT_OFFENSE_DIFF,
               YARD_LINE,
               HALF,
               PERIOD,
               MINUTES_IN_HALF,
               SECONDS_IN_HALF,
               DOWN,
               DISTANCE,
               YARD_LINE,
               YARDS_TO_GOAL) %>%
        # kickoff and time outs
        mutate(KICKOFF = case_when(grepl("kickoff", tolower(PLAY_TEXT)) | grepl("kickoff", tolower(PLAY_TYPE))==T ~ 1,
                                        TRUE ~ 0)) %>%
        mutate(TIMEOUT = case_when(grepl("timeout", tolower(PLAY_TEXT)) ~ 1,
                                        TRUE ~ 0)) %>%
        mutate(OFFENSE_ID = factor(case_when(OFFENSE_DIVISION == 'fbs' ~ OFFENSE,
                                             TRUE ~ 'fcs')),
               DEFENSE_ID = factor(case_when(DEFENSE_DIVISION == 'fbs' ~ DEFENSE,
                                             TRUE ~ 'fcs'))) %>%
        # apply filters
        filter(TIMEOUT != 1) %>%
        filter(KICKOFF != 1) %>%
        filter(DOWN %in% c(1, 2, 3, 4)) %>%
        filter(PERIOD %in% c(1,2,3,4)) %>%
        filter(SECONDS_IN_HALF <= 1800) %>%
        filter(DISTANCE >=0 & DISTANCE <=100) %>%
        filter(YARD_LINE <= 100 & YARD_LINE >=0) %>%
        filter(!is.na(SECONDS_IN_HALF)) %>%
        # create down and goal indicator
        mutate(DOWN_TO_GOAL = case_when(DISTANCE == YARDS_TO_GOAL ~ 1,
                                             TRUE ~ 0)) %>%
        # set to factor
        mutate(NEXT_SCORE_EVENT_OFFENSE = factor(NEXT_SCORE_EVENT_OFFENSE,
                                                 levels = c("No_Score",
                                                            "TD",
                                                            "FG",
                                                            "Safety",
                                                            "Opp_Safety",
                                                            "Opp_FG",
                                                            "Opp_TD"))) %>%
        arrange(SEASON, GAME_ID, PLAY_ID)

# plays filter to postseason
plays_postseason = plays_full %>%
        select(
                SEASON_TYPE,
                SEASON, 
                NEXT_SCORE_EVENT_OFFENSE, 
                PERIOD,
                SECONDS_IN_HALF,
                DOWN,
                DISTANCE, 
                YARDS_TO_GOAL,
                DOWN_TO_GOAL
                ) %>%
        filter(SEASON_TYPE == 'postseason')

# regular season
# training set
plays_train = plays_full %>%
        select(
                SEASON_TYPE,
                SEASON, 
                NEXT_SCORE_EVENT_OFFENSE, 
                PERIOD,
                SECONDS_IN_HALF,
                DOWN,
                DISTANCE, 
                YARDS_TO_GOAL,
                DOWN_TO_GOAL
        ) %>%
        filter(SEASON_TYPE == 'regular') %>%
        filter(SEASON >= 2007 & SEASON <2018)

# valid
plays_valid = plays_full %>%
        select(
                SEASON_TYPE,
                SEASON, 
                NEXT_SCORE_EVENT_OFFENSE, 
                PERIOD,
                SECONDS_IN_HALF,
                DOWN,
                DISTANCE, 
                YARDS_TO_GOAL,
                DOWN_TO_GOAL
        ) %>%
        filter(SEASON_TYPE == 'regular') %>%
        filter(SEASON > 2018)

# make an initial split based on previously defined splits
valid_split = make_splits(list(analysis = seq(nrow(plays_train)),
                               assessment = nrow(plays_train) + seq(nrow(plays_valid))),
                          bind_rows(plays_train,
                                    plays_valid))

# trim for modeling
baseline_recipe = recipe(NEXT_SCORE_EVENT_OFFENSE ~.,
                         data = plays_train) %>%
        update_role(all_predictors(),
                    new_role = "ID") %>%
        # create id out of period
        step_mutate(PERIOD_ID = PERIOD,
                    role = "ID") %>%
        # features we're inheriting
        update_role(
                c("PERIOD", 
                  "SECONDS_IN_HALF",
                  "DOWN",
                  "DOWN_TO_GOAL",
                  "DISTANCE",
                  "YARDS_TO_GOAL"),
                new_role = "predictor") %>%
        # filters for issues
        step_filter(!is.na(NEXT_SCORE_EVENT_OFFENSE)) %>%
        # feature engineering
        step_mutate(DOWN = factor(DOWN)) %>%
        step_mutate(PERIOD = factor(PERIOD)) %>%
        step_log(DISTANCE, offset =1) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_novel(all_nominal_predictors(),
                   new_level = "new") %>%
        step_interact(terms = ~ DISTANCE:(starts_with("DOWN_"))) %>%
        step_interact(terms = ~ YARDS_TO_GOAL:(starts_with("DOWN_"))) %>%
        step_interact(terms = ~ YARDS_TO_GOAL*SECONDS_IN_HALF) %>%
        check_missing(all_predictors()) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_numeric_predictors())

# from glmnet
multinom_mod = multinom_reg(
        mode = "classification",
        engine = "glmnet",
        penalty = 0,
        mixture = NULL
)

# create baseline wf
baseline_wf = workflow() %>%
        add_recipe(baseline_recipe) %>%
        add_model(multinom_mod)

# workflow settings
# metrics
class_metrics<-metric_set(yardstick::roc_auc,
                          yardstick::mn_log_loss)

# control for resamples
keep_pred <- control_resamples(save_pred = TRUE, 
                               save_workflow = TRUE,
                               allow_par=T)

# At this point I can remove the recipes, as they're now embedded in the workflows.
rm(baseline_recipe)

# register parallel
library(doParallel)
numcores = parallel::detectCores()
registerDoParallel(numcores -2)

# fit to train
# fit the model to the whole training set
last_fit_train = baseline_wf %>%
        last_fit(split = valid_split)

# pluck workflow
fit_train = last_fit_train %>%
        pluck(".workflow", 1)

# get predicted plays from the training set
predicted_plays_train = 
        augment(fit_train, 
                new_data = plays_train) %>%
        mutate(type = 'train') %>%
        # add back in original features
        bind_cols(., plays_full %>%
                          filter(SEASON_TYPE == 'regular') %>%
                          filter(SEASON >= 2007 & SEASON <2018) %>%
                          # remove features already present in recipe
                          select(-one_of(names(fit_train$pre$mold$blueprint$ptypes$predictors)),
                                 -one_of(names(fit_train$pre$mold$blueprint$ptypes$outcomes)),
                                 -one_of(names(fit_train$pre$mold$blueprint$extra_role_ptypes$ID))))

# get predicted plays from the validation set
predicted_plays_valid = 
        last_fit_train %>%
        pluck(".predictions", 1) %>%
        mutate(type = 'valid') %>%
        bind_cols(., plays_full %>%
                          filter(SEASON_TYPE == 'regular') %>%
                          filter(SEASON >2018) %>%
                          # remove features already present in recipe
                          select(
                                 -one_of(names(fit_train$pre$mold$blueprint$ptypes$outcomes)),
                                 )) %>%
        select(-.config, -.row)


# predict postseasons
# this is all postseasons in training + postseasons after validation end
predicted_plays_postseason = augment(fit_train,
                                           new_data = plays_postseason) %>%
        mutate(type = 'valid') %>%
        bind_cols(., plays_full %>%
                          filter(SEASON_TYPE == 'postseason') %>%
                          # remove features already present in recipe
                          select(-one_of(names(fit_train$pre$mold$blueprint$ptypes$predictors)),
                                 -one_of(names(fit_train$pre$mold$blueprint$ptypes$outcomes)),
                                 -one_of(names(fit_train$pre$mold$blueprint$extra_role_ptypes$ID))))

# combine all and save
predicted_plays = bind_rows(predicted_plays_train,
                            predicted_plays_valid,
                            predicted_plays_postseason)

# save these predicted plays
save(predicted_plays,
     file = here::here("workflows",
                "expected_points",
                "data",
                "predicted_plays.Rdata"))

# final fit of workflow
expected_points_wf = baseline_wf %>%
        fit(bind_rows(plays_train,
                      plays_valid))

# save/deploy model
library(vetiver)

# save as vetiver object
expected_points_model <- vetiver_model(expected_points_wf, 
                                       "expected_points")

# set model board to GCS
library(pins)

# GCS board
library(bigrquery)
library(googleCloudStorageR)
library(googleAuthR)

Sys.setenv('GCS_AUTH_FILE'="/Users/Phil/Documents/gcp-analytics-326219-ffcd8f94e1b7.json",
           'GAR_CLIENT_WEB_JSON' ="/Users/Phil/Documents/oauth.json")

#R part
library(googleCloudStorageR)
library(googleAuthR)

bq_auth()

#set the scope
gar_set_client(scopes = c("https://www.googleapis.com/auth/devstorage.read_write",
                          "https://www.googleapis.com/auth/cloud-platform"))    



# authenticate
Sys.setenv('GCS_AUTH_FILE' = '/Users/Phil/Documents/gcp-analytics-326219-c76fe0dc89d8.json')
Sys.setenv('GCLOUD_STORAGE_BUCKET' = 'phil_model_storage')
location_of_json = Sys.getenv("GCS_AUTH_FILE")
gcs_auth(location_of_json)

service_token <- gar_auth_service(json_file=location_of_json)

PROJECT_ID <- "gcp-analytics-326219"
BUCKET_NAME <- "phil_model_storage"

gcs_global_bucket(BUCKET_NAME)

# bq_auth(email = "phil.henrickson@aebs.com")

# register bucket
board_register_gcloud(token = location_of_json,
                      versioned = T)

# pin to board
pin(expected_points_model,
    board = "gcloud")

gcs_load(file = "bgg_user_owned_Phil_workflow.Rds",
         bucket = 'phil_model_storage')


# # check on this model for sanity
# # define min and max for feature
# setting_1 = seq(1, 100, 5)
# setting_2 = c(1, 2, 3, 4)
# 
# # loop over both
# pred_settings_12 = foreach(i = 1:length(setting_1),
#                            .combine = bind_rows) %:% 
#         foreach(j = 1:length(setting_2)) %do% {
#                 
#                 set.seed(1999)
#                 final_fit %>%
#                         predict(new_data = bind_rows(plays_train,
#                                                      plays_valid) %>% 
#                                         sample_n(10000) %>%
#                                         mutate(YARDS_TO_GOAL = setting_1[i],
#                                                DOWN = setting_2[j]),
#                                 type = 'prob') %>%
#                         apply(., 2, mean) %>%
#                         as.data.frame() %>%
#                         rownames_to_column("outcome") %>% 
#                         set_names(., c("outcome", ".pred")) %>%
#                         mutate(YARDS_TO_GOAL = setting_1[i],
#                                DOWN = setting_2[j])
#         }
# 
# 
# pred_settings_12 %>%
#         mutate(outcome = paste(gsub(".pred_", "Pr(", outcome), ")", sep="")) %>%
#         mutate(outcome = factor(outcome,
#                                 levels = paste("Pr(", c("TD",
#                                                         "FG",
#                                                         "Safety",
#                                                         "No_Score",
#                                                         "Opp_Safety",
#                                                         "Opp_FG",
#                                                         "Opp_TD"),
#                                                ")",
#                                                sep =""))) %>%
#         mutate(DOWN = factor(DOWN)) %>%
#         ggplot(., 
#                aes(x=YARDS_TO_GOAL,
#                    y = .pred,
#                    color = DOWN,
#                    group = DOWN))+
#         facet_wrap(outcome ~.,
#                    ncol = 4)+
#         geom_line(lwd=1.1)+
#         scale_color_viridis_d(option = 'D',
#                               begin = 0.2)+        
#         theme_phil()+
#         theme(legend.title = element_text())+
#         guides(color = guide_legend(title = 'Down',
#                                     title.position = 'top'))+
#         xlab("Yards to Opponent End Zone")+
#         ylab("Pr(Next Scoring Event)")
# 
# 

