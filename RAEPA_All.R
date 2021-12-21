library(glmnet)
library(tidyverse)
library(tidyr)
library(broom)
library(Matrix)
library(dplyr)
library(purrr)


## PRIORS

Qbs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Qbs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season == 2000) %>%
  mutate(across(5:28, as.numeric, digits = 4)) %>%
  filter(Dropbacks > 30) %>%
  mutate(DB_Perc = round(Dropbacks / team_db, digits = 2),
         Weighted_RAEPA = round(Pass_RAEPA_NW * DB_Perc, digits = 2)) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  dplyr::select(Name, Season, Team, Weighted_RAEPA, Pass_Weight = Pass_RAEPA_NW, CPOE, Dropbacks) 

Qbs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Qbs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  mutate(across(5:38, as.numeric, digits = 4)) %>%
  mutate(Season = Season + 1) %>%
  filter(Season > 2000) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  dplyr::select(Name, Season, Team, Weighted_RAEPA, Pass_Weight = Pass_RAEPA_NW, CPOE, Dropbacks) 


Qbs_Priors_PORP <- rbind(Qbs_Priors_PORP, Qbs_Priors_Chain)

Qbs_Priors_Weight <- Qbs_Priors_PORP %>%
  group_by(Team, Season) %>%
  summarise(Qbs_Weight = 1 - mean(Weighted_RAEPA)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(Qbs_Weight = scale(Qbs_Weight)) %>%
  ungroup()


qb_test <- Qbs_Priors_PORP %>%
  group_by(Name) %>%
  mutate(lRaepa = lag(Pass_Weight, order_by = Season),
         lCPOE = lag(CPOE, order_by = Season)) %>%
  ungroup() %>%
  filter(!is.na(lRaepa), !is.na(lCPOE), Dropbacks > 200, Season > 2008)
summary(lm(Pass_Weight ~ lRaepa + lCPOE, data = qb_test))

Qbs_Priors_PORP <- Qbs_Priors_PORP %>%
  mutate(Pass_Weight = 0.03838 + 0.21640*Pass_Weight + 0.00661*CPOE) %>%
  dplyr::select(-Team, -Weighted_RAEPA, -CPOE, -Dropbacks)

Rbs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Rbs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season == 2000) %>%
  mutate(across(5:28, as.numeric, digits = 4)) %>%
  filter(Rush_Attempts > 30) %>%
  mutate(R_Perc = round(Rush_Attempts / Team_Rush_Snaps, digits = 2),
         EPA_Attempt = round(Total_Rush_EPA / Rush_Attempts, digits = 2)) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  dplyr::select(Name, Season, Rush_Weight = Rush_RAEPA_NW) %>%
  mutate(Rush_Weight = Rush_Weight * 2/3)


Rbs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Rbs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  mutate(across(5:37, as.numeric, digits = 4)) %>%
  mutate(Sesason = Season + 1) %>%
  filter(Season > 2000) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  dplyr::select(Name, Season, Rush_Weight = Rush_RAEPA_NW) %>%
  mutate(Rush_Weight = Rush_Weight * 2/3)
  

Rbs_Priors_PORP <- rbind(Rbs_Priors_PORP, Rbs_Priors_Chain)

Wrs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Wrs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season == 2000) %>%
  mutate(across(4:26, as.numeric, digits = 4)) %>%
  mutate(T_Perc = round(Targets / Team_Rec_Snaps, digits = 2),
         EPA_T = round(Total_Receiving_EPA / Targets, digits = 2),
         Weighted_RAEPA = round(Receiving_RAEPA_NW * T_Perc, digits = 2)) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  dplyr::select(Name, Season, Team, Weighted_RAEPA, Receiving_Weight = Receiving_RAEPA_NW) %>%
  mutate(Receiving_Weight = Receiving_Weight * 2/3)

Wrs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Wrs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  mutate(across(4:36, as.numeric, digits = 4)) %>%
  mutate(Season = Season + 1) %>%
  filter(Season > 2000) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  mutate(EPA_T = round(Total_Receiving_EPA / Targets, digits = 2)) %>%
  dplyr::select(Name, Season, Team, Weighted_RAEPA, Receiving_Weight = Receiving_RAEPA_NW) %>%
  mutate(Receiving_Weight = Receiving_Weight * 2/3)
  

Wrs_Priors_PORP <- rbind(Wrs_Priors_PORP, Wrs_Priors_Chain)

Wrs_Priors_Weight <- Wrs_Priors_PORP %>%
  group_by(Team, Season) %>%
  summarise(Wrs_Weight = 1 - mean(Weighted_RAEPA)) %>%
  ungroup() %>%
  group_by(Season) %>%
  mutate(Wrs_Weight = scale(Wrs_Weight)) %>%
  ungroup()

Wrs_Priors_PORP <- Wrs_Priors_PORP %>%
  dplyr::select(-Team, -Weighted_RAEPA)

## START RIDGE ADJUSTMENTS & ADD DUMMY VARIABLES

year <- 2021

ridge_variables <- pbp_ridge %>%
  filter(season == {year} & qb_dropback == 1) %>%
  filter(!is.na(penalty) | penalty_type %in% c("Defensive Offside", "Intentional Grounding", 
                                               "Encroachment", "Neutral Zone Infraction",
                                               "Delay of Game", "Inelgible Downfield Pass", 
                                               "Unneccessary Roughness",
                                               "Offensive Too Many Men on Field")) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0)) %>%
  mutate(epa = case_when(week == 11 ~ round(epa * ((1 - 0.025) ^ (11 - week)), digits = 4),
                         week == 12 ~ round(epa * ((1 - 0.025) ^ (12 - week)), digits = 4),
                         week == 13 ~ round(epa * ((1 - 0.025) ^ (13 - week)), digits = 4),
                         week == 14 ~ round(epa * ((1 - 0.025) ^ (14 - week)), digits = 4),
                         week == 15 ~ round(epa * ((1 - 0.025) ^ (15 - week)), digits = 4),
                         week == 16 ~ round(epa * ((1 - 0.025) ^ (16 - week)), digits = 4),
                         week == 17 ~ round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4),
                         TRUE ~ epa)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_Weight, by = c("posteam" = "Team", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Pass_Weight - Receiving_Weight) * ((1 - 0.025) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.025) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.025) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17 & vegas_wp > 0.85, Total_Weight * 0.25, Total_Weight)) %>%
  mutate(Total_Weight = if_else(wp >= 0.015 & wp <= 0.985, Total_Weight, Total_Weight * 0.12)) %>%
  mutate(epa = if_else(!is.na(Pass_Weight), round(epa - Pass_Weight, digits = 4), epa))

ridge_variables <- ridge_variables %>%
  make_passer_mutations()

## CREATE MODEL DATA FRAME SELECTING ONLY RIDGE VARIABLES
## LASSO SELECTED - YARDLINE_100, SHOTGUN, TOTAL_LINE, DRIVE_PLAY_COUNT,
## YARDS_TIMES_DOWN, YARDLINE_SQUARED, VEGAS_WP_SQUARED, LOG_VEGAS_WP,
## SIDE_OF_FIELD_INDICATOR, YARDS_TO_GO_SQUARE, IN_FG_RANGE

model_dataframe <- ridge_variables %>%
  select_at(vars(contains(c("Passer_", "Rusher_", "Receiver_", "Pass_Defense", "yardline", "shotgun",
                            "total_line", "drive_play_count", "yards_times_down",
                            "yards_to_go_sqaure", "vegas_wp", "field", "series", "epa")))) %>%
  dplyr::select(-passer_player_id, -passer_player_name, -passer_jersey_number, -passer_id,
                -rusher_player_id, -rusher_player_name, -rusher_jersey_number, -rusher_id, -Rusher_gsis,
         -receiver_player_id, -receiver_player_name, -receiver_jersey_number, -receiver_id, -Receiver_gsis,
         -vegas_wpa, -vegas_wp, -side_of_field, -total_home_epa, -total_away_epa, -total_home_rush_epa, 
         -total_away_rush_epa, -total_home_pass_epa, -total_away_pass_epa, -air_epa, -yac_epa, -comp_air_epa,
         -comp_yac_epa, -total_home_comp_air_epa, -total_away_comp_air_epa, -total_home_comp_yac_epa,
         -total_away_comp_yac_epa, -total_home_raw_air_epa, -total_away_raw_air_epa, -total_home_raw_yac_epa,
         -total_away_raw_yac_epa, -qb_epa, -xyac_epa, -pass_defense_1_player_id, -pass_defense_1_player_name,
         -pass_defense_2_player_id, -pass_defense_2_player_name, -series_success, -series_result,
         -series, -drive_play_count, -side_of_field_indicator) %>%
  mutate(across(c("yardline_100", "yardline_squared", "total_line",
                  "yards_times_down","vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4))) %>%
  filter(!is.na(epa))


epa_table <- model_dataframe %>%
  dplyr::select(epa)

## ADJUST VARIABLES TO WORK IN GLMNET
epa_vector <- as.vector(epa_table)
epa_vector[is.na(epa_vector)] <- 0
model_vector <- as.vector(model_dataframe) %>%
  select(-epa)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
epa_final <- as(epa_final, "sparseMatrix")
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- as(model_final, "sparseMatrix")

## REPEAT SAME PROCESS WITH PRIORS
weights_table <- ridge_variables %>%
  filter(!is.na(epa)) %>%
  dplyr::select(Wrs_Weight, week)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights = (Wrs_Weight) * ((1 - 0.025) ^ (17 - week))) %>%
  mutate(weights = weights + -min(weights) + 0.01) %>%
  dplyr::select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))
weights_final <- as(weights_final, "sparseMatrix")

## WORKING SEQUENCE LAMBDAS
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)
grid <- 10^seq(2,-2,length=100)

set.seed(01061997)


fit <- glmnet(model_final, epa_final, alpha = 0, 
              lambda = lambdas_to_try, parallel = TRUE,
              standardize = FALSE) ## ADD WEIGHTS AFTER CREATING
summary(fit)


ridge_cv <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10, lambda = grid,
                       weights = weights_final, parallel = TRUE, standardize = FALSE)
best_lambda_season <- ridge_cv$lambda.min
best_lambda_season

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge_season <- glmnet(model_final, epa_final, alpha = 0, lambda = best_lambda_season,
                            weights = weights_final, parallel = TRUE, standardize = TRUE)
coef(best_ridge_season)
coef_ridge <- tidy(best_ridge_season)

## NO WEIGHTS VERSION
ridge_cv_no_weights <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10,
                                 lambda = grid, parallel = TRUE, standardize = TRUE)
best_lambda_season_no_weights <- ridge_cv_no_weights$lambda.min
best_lambda_season_no_weights

best_fit_no_weights <- ridge_cv_no_weights$glmnet.fit
head(best_fit_no_weights)

best_ridge_season_no_weights <- glmnet(model_final, epa_final, alpha = 0, 
                                       lambda = best_lambda_season_no_weights,
                                       parallel = TRUE)
coef(best_ridge_season_no_weights)
coef_ridge_no_weights <- tidy(best_ridge_season_no_weights)


## CHANGE COEFS WITH EACH YEAR - REPLACE PASSER/RUSHER/RECEIVER/PASS/RUN - JOIN EACH PLAYER INFO LIST


## PASSING
passer_coef <- coef_ridge %>%
  filter(grepl('Passer_', term))
passer_names <- passer_coef$term %>%
  str_remove_all("Passer_") %>%
  str_replace_all("_"," ")

pass_d_coef <- coef_ridge %>%
  filter(grepl('Pass_Defense_', term))
pass_d_names <- pass_d_coef$term %>%
  str_remove_all("Pass_Defense") %>%
  str_replace_all("_"," ")

pass_d_coef <- cbind(pass_d_coef, pass_d_names)


passer_coef <- cbind(passer_coef, passer_names) %>%
  mutate(passer_names = case_when(passer_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  passer_names == "AJ Feeley" ~ "A.J. Feeley",
                                  passer_names == "JP Losman" ~ "J.P. Losman",
                                  passer_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  passer_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  passer_names == "TJ Yates" ~ "T.J. Yates",
                                  passer_names == "CJ Beathard" ~ "C.J. Beathard",
                                  TRUE ~ passer_names)) %>%
  dplyr::select(Name = passer_names, Pass_RAEPA = estimate)

passer_coef_no_weights <- coef_ridge_no_weights %>%
  filter(grepl('Passer_', term))
passer_names <- passer_coef_no_weights$term %>%
  str_remove_all("Passer_") %>%
  str_replace_all("_"," ")

passer_coef_no_weights <- cbind(passer_coef_no_weights, passer_names) %>%
  mutate(passer_names = case_when(passer_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  passer_names == "AJ Feeley" ~ "A.J. Feeley",
                                  passer_names == "JP Losman" ~ "J.P. Losman",
                                  passer_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  passer_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  passer_names == "TJ Yates" ~ "T.J. Yates",
                                  passer_names == "CJ Beathard" ~ "C.J. Beathard",
                                  TRUE ~ passer_names)) %>%
  dplyr::select(Name = passer_names, Pass_RAEPA_no_weights = estimate)

Qbs_Weights_Add_Back <- Qbs_Priors_PORP %>%
  filter(Season == {year})

passer_coef <- left_join(passer_coef, passer_coef_no_weights, by = "Name") %>%
  left_join(Qbs_Weights_Add_Back, by = "Name") %>%
  mutate(Pass_RAEPA = if_else(!is.na(Pass_Weight), Pass_RAEPA + Pass_Weight, Pass_RAEPA),
         Pass_RAEPA_no_weights = if_else(!is.na(Pass_Weight), Pass_RAEPA_no_weights + Pass_Weight, Pass_RAEPA_no_weights)) %>%
  dplyr::select(Name, Pass_RAEPA, Pass_RAEPA_no_weights)



## FILTER BY POSITION FOR PLAYERS TO INCLUDE & REMOVE UNQUALIFIED
## RECEIVING  

ridge_variables <- pbp_ridge %>%
  filter(season == {year} & pass == 1) %>%
  filter(!is.na(penalty) | penalty_type %in% c("Illegal Contact", "Offensive Pass Interference",
                                               "Defensive Holding", "Defensive Pass Interference",
                                               "Defensive Offside", "Unneccessary Roughness",
                                               "Horse Collar Tackle", 
                                               "Lowering the Head to Initiate Contact")) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0)) %>%
  mutate(epa = case_when(week == 11 ~ round(epa * ((1 - 0.025) ^ (11 - week)), digits = 4),
                         week == 12 ~ round(epa * ((1 - 0.025) ^ (12 - week)), digits = 4),
                         week == 13 ~ round(epa * ((1 - 0.025) ^ (13 - week)), digits = 4),
                         week == 14 ~ round(epa * ((1 - 0.025) ^ (14 - week)), digits = 4),
                         week == 15 ~ round(epa * ((1 - 0.025) ^ (15 - week)), digits = 4),
                         week == 16 ~ round(epa * ((1 - 0.025) ^ (16 - week)), digits = 4),
                         week == 17 ~ round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4),
                         TRUE ~ epa)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>%
  left_join(Qbs_Priors_Weight, by = c("posteam" = "Team", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Receiving_Weight - Pass_Weight) * ((1 - 0.025) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.025) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.025) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17 & vegas_wp > 0.85, Total_Weight * 0.25, Total_Weight)) %>%
  mutate(Total_Weight = if_else(wp >= 0.015 & wp <= 0.985, Total_Weight, Total_Weight * 0.12)) %>%
  mutate(epa = if_else(!is.na(Receiving_Weight), round(epa - Receiving_Weight, digits = 4), epa))

ridge_variables <- ridge_variables %>%
  make_receiver_mutations()
ridge_variables <- ridge_variables %>%
  make_receiver_mutations_2()
  

## CREATE MODEL DATA FRAME SELECTING ONLY RIDGE VARIABLES
## LASSO SELECTED - YARDLINE_100, SHOTGUN, QTR, HOME, TOTAL_LINE, DRIVE_PLAY_COUNT, POSTEAM_TIMEOUTS,
## DEFTEAM_TIMEOUTS_REMAINING, YARDS_TIMES_DOWN, YARDLINE_SQUARED, VEGAS_WP_SQUARED, LOG_VEGAS_WP,
## SIDE_OF_FIELD_INDICATOR, YARDS_TO_GO_SQUARE

model_dataframe <- ridge_variables %>%
  select_at(vars(contains(c("Passer_", "Rusher_", "Receiver_", "Pass_Defense", "yardline", "shotgun",
                            "total_line", "drive_play_count", "yards_times_down",
                            "yards_to_go_sqaure", "vegas_wp", "field", "series", "epa")))) %>%
  dplyr::select(-passer_player_id, -passer_player_name, -passer_jersey_number, -passer_id,
                -rusher_player_id, -rusher_player_name, -rusher_jersey_number, -rusher_id, -Rusher_gsis,
                -receiver_player_id, -receiver_player_name, -receiver_jersey_number, -receiver_id, -Receiver_gsis,
                -vegas_wpa, -vegas_wp, -side_of_field, -total_home_epa, -total_away_epa, -total_home_rush_epa, 
                -total_away_rush_epa, -total_home_pass_epa, -total_away_pass_epa, -air_epa, -yac_epa, -comp_air_epa,
                -comp_yac_epa, -total_home_comp_air_epa, -total_away_comp_air_epa, -total_home_comp_yac_epa,
                -total_away_comp_yac_epa, -total_home_raw_air_epa, -total_away_raw_air_epa, -total_home_raw_yac_epa,
                -total_away_raw_yac_epa, -qb_epa, -xyac_epa, -pass_defense_1_player_id, -pass_defense_1_player_name,
                -pass_defense_2_player_id, -pass_defense_2_player_name, -series_success, -series_result,
                -series, -drive_play_count, -side_of_field_indicator) %>%
  mutate(across(c("yardline_100", "yardline_squared", "total_line",
                  "yards_times_down","vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4))) %>%
  filter(!is.na(epa))


epa_table <- model_dataframe %>%
  dplyr::select(epa)


## ADJUST VARIABLES TO WORK IN GLMNET
epa_vector <- as.vector(epa_table)
epa_vector[is.na(epa_vector)] <- 0
model_vector <- as.vector(model_dataframe) %>%
  dplyr::select(-epa)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
epa_final <- as(epa_final, "sparseMatrix")
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- as(model_final, "sparseMatrix")
## REPEAT SAME PROCESS WITH PRIORS
weights_table <- ridge_variables %>%
  filter(!is.na(epa)) %>%
  dplyr::select(Qbs_Weight, week)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights = (Qbs_Weight) * ((1 - 0.025) ^ (17 - week))) %>%
  mutate(weights = weights + -min(weights) + 0.01) %>%
  dplyr::select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))
weights_final <- as(weights_final, "sparseMatrix")


## WORKING SEQUENCE
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)


set.seed(01061997)


fit <- glmnet(model_final, epa_final, alpha = 0, 
              lambda = grid, parallel = TRUE) ## ADD WEIGHTS AFTER CREATING
summary(fit)

ridge_cv <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10, weights = weights_final,
                      lambda = grid, parallel = TRUE, standardize = FALSE) ## ADD WEIGHTS AFTER CREATING
best_lambda_season <- ridge_cv$lambda.min
best_lambda_season

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge_season <- glmnet(model_final, epa_final, alpha = 0, lambda = best_lambda_season,
                            weights = weights_final, parallel = TRUE)
coef(best_ridge_season)
coef_ridge_receiving <- tidy(best_ridge_season)


## NO WEIGHTS VERSION
ridge_cv_no_weights <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10,
                                 lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season_no_weights <- ridge_cv_no_weights$lambda.min
best_lambda_season_no_weights

best_fit_no_weights <- ridge_cv_no_weights$glmnet.fit
head(best_fit_no_weights)

best_ridge_season_no_weights <- glmnet(model_final, epa_final, alpha = 0, 
                                       lambda = best_lambda_season_no_weights, 
                                       parallel = TRUE)
coef(best_ridge_season_no_weights)
coef_ridge_receiving_no_weights <- tidy(best_ridge_season_no_weights)


## Receiver 
receiver_coef <- coef_ridge_receiving %>%
  filter(grepl('Receiver_', term))
receiver_names <- receiver_coef$term %>%
  str_remove_all("Receiver_") %>%
  str_replace_all("_"," ")

receiver_coef <- cbind(receiver_coef, receiver_names) %>%
  mutate(receiver_names = case_when(receiver_names == "Abdul Karim al Jabbar" ~ "Abdul-Karim al-Jabbar",
                                    receiver_names == "Michael Pittman 1" ~ "Michael Pittman_1",
                                    receiver_names == "Chris Warren 1" ~ "Chris Warren_1",
                                    receiver_names == "Justin Watson 1" ~ "Justin Watson_1",
                                    receiver_names == "DeMond Parker" ~ "De'Mond Parker",
                                    receiver_names == "JJ Johnson" ~ "J.J. Johnson",
                                    receiver_names == "Irv Smith 1" ~ "Irv Smith_1",
                                    receiver_names == "Rod Smith 1" ~ "Rod Smith_1",
                                    receiver_names == "Kevin RWilliams" ~ "Kevin R. Williams",
                                    receiver_names == "EG Green" ~ "E.G. Green",
                                    receiver_names == "OJ McDuffie" ~ "O.J. McDuffie",
                                    receiver_names == "JJ Stokes" ~ "J.J. Stokes",
                                    receiver_names == "Karl Williams 1" ~ "Karl Williams_1",
                                    receiver_names == "OJ Santiago" ~ "O.J. Santiago",
                                    receiver_names == "DWayne Bates" ~ "D'Wayne Bates",
                                    receiver_names == "Az Zahir Hakim" ~ "Az-Zahir Hakim",
                                    receiver_names == "JR Redmond" ~ "J.R. Redmond",
                                    receiver_names == "KiJana Carter" ~ "Ki-Jana Carter",
                                    receiver_names == "TJ Houshmandzadeh" ~ "T.J. Houshmandzadeh",
                                    receiver_names == "Ricky Williams 1" ~ "Ricky Williams_1",
                                    receiver_names == "TJ Duckett" ~ "T.J. Duckett",
                                    receiver_names == "Adrian Peterson 1" ~ "Adrian Peterson_1",
                                    receiver_names == "Donte Stallworth" ~ "Donte' Stallworth",
                                    receiver_names == "LJ Smith" ~ "L.J. Smith",
                                    receiver_names == "Chris Brown 2" ~ "Chris Brown_2",
                                    receiver_names == "JJ Arrington" ~ "J.J. Arrington",
                                    receiver_names == "JP Foschi" ~ "J.P. Foschi",
                                    receiver_names == "Alex Smith 1" ~ "Alex Smith_1",
                                    receiver_names == "DJ Hackett" ~ "D.J. Hackett",
                                    receiver_names == "Matt Jones 1" ~ "Matt Jones_1",
                                    receiver_names == "Mike Williams 1" ~ "Mike Williams_1",
                                    receiver_names == "Mike Williams 2" ~ "Mike Williams_2",
                                    receiver_names == "Chris Henry 1" ~ "Chris Henry_1",
                                    receiver_names == "Maurice Jones Drew" ~ "Maurice Jones-Drew",
                                    receiver_names == "Steve Smith 1" ~ "Steve Smith_1",
                                    receiver_names == "Ryan Grant 1" ~ "Ryan Grant_1",
                                    receiver_names == "BJ Askew" ~ "B.J. Askew",
                                    receiver_names == "Mike Sims Walker" ~ "Mike Sims-Walker",
                                    receiver_names == "LeRon McClain" ~ "Le'Ron McClain",
                                    receiver_names == "Kevin Smith 1" ~ "Kevin Smith_1",
                                    receiver_names == "Zach Miller 1" ~ "Zach Miller_1",
                                    receiver_names == "Mike Thomas 1" ~ "Mike Thomas_1",
                                    receiver_names == "CJ Spiller" ~ "C.J. Spiller",
                                    receiver_names == "LaRod Stephens Howling" ~ "LaRod Stephens-Howling",
                                    receiver_names == "Darrius Heyward Bey" ~ "Darrius Heyward-Bey",
                                    receiver_names == "BenJarvus Green Ellis" ~ "BenJarvus Green-Ellis",
                                    receiver_names == "AJ Green" ~ "A.J. Green",
                                    receiver_names == "DJ Ware" ~ "D.J. Ware",
                                    receiver_names == "TY Hilton" ~ "T.Y. Hilton",
                                    receiver_names == "LeVeon Bell" ~ "Le'Veon Bell",
                                    receiver_names == "CJ Fiedorowicz" ~ "C.J. Fiedorowicz",
                                    receiver_names == "Austin Seferian Jenkins" ~ "Austin Seferian-Jenkins",
                                    receiver_names == "CharlesD Johnson" ~ "Charles D. Johnson",
                                    receiver_names == "CJ Anderson" ~ "C.J. Anderson",
                                    receiver_names == "DeAnthony Thomas" ~ "De'Anthony Thomas",
                                    receiver_names == "Dorial Green Beckham" ~ "Dorial Green-Beckham",
                                    receiver_names == "JJ Nelson" ~ "J.J. Nelson",
                                    receiver_names == "CJ Uzomah" ~ "C.J. Uzomah",
                                    receiver_names == "Nick OLeary" ~ "Nick O'Leary",
                                    receiver_names == "TJ Jones" ~ "T.J. Jones",
                                    receiver_names == "TJ Yeldon" ~ "T.J. Yeldon",
                                    receiver_names == "DJ Foster" ~ "D.J. Foster",
                                    receiver_names == "CJ Prosise" ~ "C.J. Prosise",
                                    receiver_names == "James OShaughnessy" ~ "James O'Shaughnessy",
                                    receiver_names == "JuJu Smith Schuster" ~ "JuJu Smith-Schuster",
                                    receiver_names == "Ricky Seals Jones" ~ "Ricky Seals-Jones",
                                    receiver_names == "OJ Howard" ~ "O.J. Howard",
                                    receiver_names == "JD McKissic" ~ "J.D. McKissic",
                                    receiver_names == "RayRay McCloud" ~ "Ray-Ray McCloud",
                                    receiver_names == "Mo Alie Cox" ~ "Mo Alie-Cox",
                                    receiver_names == "TreQuan Smith" ~ "Tre'Quan Smith",
                                    receiver_names == "Marquez Valdes Scantling" ~ "Marquez Valdes-Scantling",
                                    receiver_names == "DJ Moore" ~ "D.J. Moore",
                                    receiver_names == "Equanimeous St Brown" ~ "Equanimeous St. Brown",
                                    receiver_names == "NKeal Harry" ~ "N'Keal Harry",
                                    receiver_names == "AJ Brown" ~ "A.J. Brown",
                                    receiver_names == "TJ Hockenson" ~ "T.J. Hockenson",
                                    receiver_names == "CJ Ham" ~ "C.J. Ham",
                                    receiver_names == "DJ Chark" ~ "D.J. Chark",
                                    receiver_names == "KJ Hamler" ~ "K.J. Hamler",
                                    receiver_names == "Clyde Edwards Helaire" ~ "Clyde Edwards-Helaire",
                                    receiver_names == "JK Dobbins" ~ "J.K. Dobbins",
                                    receiver_names == "DAndre Swift" ~ "D'Andre Swift",
                                    receiver_names == "AJ Derby" ~ "A.J. Derby",
                                    receiver_names == "LaMical Perine" ~ "La'Mical Perine",
                                    receiver_names == "Donovan Peoples Jones" ~ "Donovan Peoples-Jones",
                                    receiver_names == "CJ Board" ~ "C.J. Board",
                                    receiver_names == "JJ Arcega Whiteside" ~ "J.J. Arcega-Whiteside",
                                    receiver_names == "AmonRa St Brown" ~ "Amon-Ra St. Brown",
                                    receiver_names == "JaMarr Chase" ~ "Ja'Marr Chase",
                                    receiver_names == "DWayne Eskridge" ~ "D'Wayne Eskridge",
                                    receiver_names == "TySon Williams" ~ "Ty'Son Williams",
                                    receiver_names == "Nick Westbrook Ikhine" ~ "Nick Westbrook-Ikhine",
                                    receiver_names == "KJ Osborn" ~ "K.J. Osborn",
                                    TRUE ~ receiver_names)) %>%
  dplyr::select(Name = receiver_names, Receiving_RAEPA = estimate)


receiver_coef_no_weights <- coef_ridge_receiving_no_weights %>%
  filter(grepl('Receiver_', term))
receiver_names <- receiver_coef_no_weights$term %>%
  str_remove_all("Receiver_") %>%
  str_replace_all("_"," ")

receiving_d_coef <- coef_ridge_receiving %>%
  filter(grepl('Pass_Defense_', term))
receiving_d_names <- receiving_d_coef$term %>%
  str_remove_all("Pass_Defense") %>%
  str_replace_all("_"," ")

receiving_d_coef <- cbind(receiving_d_coef, receiving_d_names)

receiver_coef_no_weights <- cbind(receiver_coef_no_weights, receiver_names) %>%
  mutate(receiver_names = case_when(receiver_names == "Abdul Karim al Jabbar" ~ "Abdul-Karim al-Jabbar",
                                    receiver_names == "Michael Pittman 1" ~ "Michael Pittman_1",
                                    receiver_names == "Chris Warren 1" ~ "Chris Warren_1",
                                    receiver_names == "Justin Watson 1" ~ "Justin Watson_1",
                                    receiver_names == "DeMond Parker" ~ "De'Mond Parker",
                                    receiver_names == "JJ Johnson" ~ "J.J. Johnson",
                                    receiver_names == "Irv Smith 1" ~ "Irv Smith_1",
                                    receiver_names == "Rod Smith 1" ~ "Rod Smith_1",
                                    receiver_names == "Kevin RWilliams" ~ "Kevin R. Williams",
                                    receiver_names == "EG Green" ~ "E.G. Green",
                                    receiver_names == "OJ McDuffie" ~ "O.J. McDuffie",
                                    receiver_names == "JJ Stokes" ~ "J.J. Stokes",
                                    receiver_names == "Karl Williams 1" ~ "Karl Williams_1",
                                    receiver_names == "OJ Santiago" ~ "O.J. Santiago",
                                    receiver_names == "DWayne Bates" ~ "D'Wayne Bates",
                                    receiver_names == "Az Zahir Hakim" ~ "Az-Zahir Hakim",
                                    receiver_names == "JR Redmond" ~ "J.R. Redmond",
                                    receiver_names == "KiJana Carter" ~ "Ki-Jana Carter",
                                    receiver_names == "TJ Houshmandzadeh" ~ "T.J. Houshmandzadeh",
                                    receiver_names == "Ricky Williams 1" ~ "Ricky Williams_1",
                                    receiver_names == "TJ Duckett" ~ "T.J. Duckett",
                                    receiver_names == "Adrian Peterson 1" ~ "Adrian Peterson_1",
                                    receiver_names == "Donte Stallworth" ~ "Donte' Stallworth",
                                    receiver_names == "LJ Smith" ~ "L.J. Smith",
                                    receiver_names == "Chris Brown 2" ~ "Chris Brown_2",
                                    receiver_names == "JJ Arrington" ~ "J.J. Arrington",
                                    receiver_names == "JP Foschi" ~ "J.P. Foschi",
                                    receiver_names == "Alex Smith 1" ~ "Alex Smith_1",
                                    receiver_names == "DJ Hackett" ~ "D.J. Hackett",
                                    receiver_names == "Matt Jones 1" ~ "Matt Jones_1",
                                    receiver_names == "Mike Williams 1" ~ "Mike Williams_1",
                                    receiver_names == "Mike Williams 2" ~ "Mike Williams_2",
                                    receiver_names == "Chris Henry 1" ~ "Chris Henry_1",
                                    receiver_names == "Maurice Jones Drew" ~ "Maurice Jones-Drew",
                                    receiver_names == "Steve Smith 1" ~ "Steve Smith_1",
                                    receiver_names == "Ryan Grant 1" ~ "Ryan Grant_1",
                                    receiver_names == "BJ Askew" ~ "B.J. Askew",
                                    receiver_names == "Mike Sims Walker" ~ "Mike Sims-Walker",
                                    receiver_names == "LeRon McClain" ~ "Le'Ron McClain",
                                    receiver_names == "Kevin Smith 1" ~ "Kevin Smith_1",
                                    receiver_names == "Zach Miller 1" ~ "Zach Miller_1",
                                    receiver_names == "Mike Thomas 1" ~ "Mike Thomas_1",
                                    receiver_names == "CJ Spiller" ~ "C.J. Spiller",
                                    receiver_names == "LaRod Stephens Howling" ~ "LaRod Stephens-Howling",
                                    receiver_names == "Darrius Heyward Bey" ~ "Darrius Heyward-Bey",
                                    receiver_names == "BenJarvus Green Ellis" ~ "BenJarvus Green-Ellis",
                                    receiver_names == "AJ Green" ~ "A.J. Green",
                                    receiver_names == "DJ Ware" ~ "D.J. Ware",
                                    receiver_names == "TY Hilton" ~ "T.Y. Hilton",
                                    receiver_names == "LeVeon Bell" ~ "Le'Veon Bell",
                                    receiver_names == "CJ Fiedorowicz" ~ "C.J. Fiedorowicz",
                                    receiver_names == "Austin Seferian Jenkins" ~ "Austin Seferian-Jenkins",
                                    receiver_names == "CharlesD Johnson" ~ "Charles D. Johnson",
                                    receiver_names == "CJ Anderson" ~ "C.J. Anderson",
                                    receiver_names == "DeAnthony Thomas" ~ "De'Anthony Thomas",
                                    receiver_names == "Dorial Green Beckham" ~ "Dorial Green-Beckham",
                                    receiver_names == "JJ Nelson" ~ "J.J. Nelson",
                                    receiver_names == "CJ Uzomah" ~ "C.J. Uzomah",
                                    receiver_names == "Nick OLeary" ~ "Nick O'Leary",
                                    receiver_names == "TJ Jones" ~ "T.J. Jones",
                                    receiver_names == "TJ Yeldon" ~ "T.J. Yeldon",
                                    receiver_names == "DJ Foster" ~ "D.J. Foster",
                                    receiver_names == "CJ Prosise" ~ "C.J. Prosise",
                                    receiver_names == "James OShaughnessy" ~ "James O'Shaughnessy",
                                    receiver_names == "JuJu Smith Schuster" ~ "JuJu Smith-Schuster",
                                    receiver_names == "Ricky Seals Jones" ~ "Ricky Seals-Jones",
                                    receiver_names == "OJ Howard" ~ "O.J. Howard",
                                    receiver_names == "JD McKissic" ~ "J.D. McKissic",
                                    receiver_names == "RayRay McCloud" ~ "Ray-Ray McCloud",
                                    receiver_names == "Mo Alie Cox" ~ "Mo Alie-Cox",
                                    receiver_names == "TreQuan Smith" ~ "Tre'Quan Smith",
                                    receiver_names == "Marquez Valdes Scantling" ~ "Marquez Valdes-Scantling",
                                    receiver_names == "DJ Moore" ~ "D.J. Moore",
                                    receiver_names == "Equanimeous St Brown" ~ "Equanimeous St. Brown",
                                    receiver_names == "NKeal Harry" ~ "N'Keal Harry",
                                    receiver_names == "AJ Brown" ~ "A.J. Brown",
                                    receiver_names == "TJ Hockenson" ~ "T.J. Hockenson",
                                    receiver_names == "CJ Ham" ~ "C.J. Ham",
                                    receiver_names == "DJ Chark" ~ "D.J. Chark",
                                    receiver_names == "KJ Hamler" ~ "K.J. Hamler",
                                    receiver_names == "Clyde Edwards Helaire" ~ "Clyde Edwards-Helaire",
                                    receiver_names == "JK Dobbins" ~ "J.K. Dobbins",
                                    receiver_names == "DAndre Swift" ~ "D'Andre Swift",
                                    receiver_names == "AJ Derby" ~ "A.J. Derby",
                                    receiver_names == "LaMical Perine" ~ "La'Mical Perine",
                                    receiver_names == "Donovan Peoples Jones" ~ "Donovan Peoples-Jones",
                                    receiver_names == "CJ Board" ~ "C.J. Board",
                                    receiver_names == "JJ Arcega Whiteside" ~ "J.J. Arcega-Whiteside",
                                    receiver_names == "AmonRa St Brown" ~ "Amon-Ra St. Brown",
                                    receiver_names == "JaMarr Chase" ~ "Ja'Marr Chase",
                                    receiver_names == "DWayne Eskridge" ~ "D'Wayne Eskridge",
                                    receiver_names == "TySon Williams" ~ "Ty'Son Williams",
                                    receiver_names == "Nick Westbrook Ikhine" ~ "Nick Westbrook-Ikhine",
                                    receiver_names == "KJ Osborn" ~ "K.J. Osborn",
                                    TRUE ~ receiver_names)) %>%
  dplyr::select(Name = receiver_names, Receiving_RAEPA_no_weights = estimate)


Wrs_Weights_Add_Back <- Wrs_Priors_PORP %>%
  filter(Season == {year})
## JOINED COEFFICIENTS
receiver_coef <- left_join(receiver_coef, receiver_coef_no_weights, by = "Name") %>%
  left_join(Wrs_Weights_Add_Back, by = "Name") %>%
  mutate(Receiving_RAEPA = if_else(!is.na(Receiving_Weight), Receiving_RAEPA + Receiving_Weight, Receiving_RAEPA),
         Receiving_RAEPA_no_weights = if_else(!is.na(Receiving_Weight), 
                                             Receiving_RAEPA_no_weights + Receiving_Weight, Receiving_RAEPA_no_weights)) %>%
  dplyr::select(-Season, -Receiving_Weight)


## FILTER BY POSITION FOR PLAYERS TO INCLUDE & REMOVE UNQUALIFIED
## UNSURE WHY 1 GAME IS MISSING IN 1999 (STL & BAL)
## RUSHING


ridge_variables <- pbp_ridge %>%
  filter(season == {year} & rush == 1) %>%
  filter(!is.na(penalty) | penalty_type %in% c("Unneccessary Roughness", "Face Mask",
                                               "Horse Collar Tackle",
                                               "Lowering the Head to Initiate Contact")) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0)) %>%
  mutate(epa = case_when(week == 11 ~ round(epa * ((1 - 0.025) ^ (11 - week)), digits = 4),
                         week == 12 ~ round(epa * ((1 - 0.025) ^ (12 - week)), digits = 4),
                         week == 13 ~ round(epa * ((1 - 0.025) ^ (13 - week)), digits = 4),
                         week == 14 ~ round(epa * ((1 - 0.025) ^ (14 - week)), digits = 4),
                         week == 15 ~ round(epa * ((1 - 0.025) ^ (15 - week)), digits = 4),
                         week == 16 ~ round(epa * ((1 - 0.025) ^ (16 - week)), digits = 4),
                         week == 17 ~ round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4),
                         TRUE ~ epa)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Total_Weight = case_when(!is.na(Rush_Weight) ~ (Rush_Weight) * ((1 - 0.025) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17 & vegas_wp > 0.85, Total_Weight * 0.25, Total_Weight)) %>%
  mutate(Total_Weight = if_else(wp >= 0.015 & wp <= 0.985, Total_Weight, Total_Weight * 0.12)) %>%
  mutate(epa = if_else(!is.na(Rush_Weight), round(epa - Rush_Weight, digits = 4), epa))

ridge_variables <- ridge_variables %>%
  make_rusher_mutations()

## CREATE MODEL DATA FRAME SELECTING ONLY RIDGE VARIABLES

model_dataframe <- ridge_variables %>%
  select_at(vars(contains(c("Passer_", "Rusher_", "Receiver_", "Run_Defense", "yardline", "shotgun",
                            "total_line", "drive_play_count", "yards_times_down",
                            "yards_to_go_sqaure", "vegas_wp", "field", "series", "epa")))) %>%
  dplyr::select(-passer_player_id, -passer_player_name, -passer_jersey_number, -passer_id,
                -rusher_player_id, -rusher_player_name, -rusher_jersey_number, -rusher_id, -Rusher_gsis,
                -receiver_player_id, -receiver_player_name, -receiver_jersey_number, -receiver_id, -Receiver_gsis,
                -vegas_wpa, -vegas_wp, -side_of_field, -total_home_epa, -total_away_epa, -total_home_rush_epa, 
                -total_away_rush_epa, -total_home_pass_epa, -total_away_pass_epa, -air_epa, -yac_epa, -comp_air_epa,
                -comp_yac_epa, -total_home_comp_air_epa, -total_away_comp_air_epa, -total_home_comp_yac_epa,
                -total_away_comp_yac_epa, -total_home_raw_air_epa, -total_away_raw_air_epa, -total_home_raw_yac_epa,
                -total_away_raw_yac_epa, -qb_epa, -xyac_epa, -series_success, -series_result,
                -series, -drive_play_count, -side_of_field_indicator) %>%
  mutate(across(c("yardline_100", "yardline_squared", "total_line",
                  "yards_times_down","vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4))) %>%
  filter(!is.na(epa))


epa_table <- model_dataframe %>%
  dplyr::select(epa) 


## ADJUST VARIABLES TO WORK IN GLMNET
epa_vector <- as.vector(epa_table)
epa_vector[is.na(epa_vector)] <- 0
model_vector <- as.vector(model_dataframe) %>%
  select(-epa)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
epa_final <- as(epa_final, "sparseMatrix")
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- as(model_final, "sparseMatrix")
## REPEAT SAME PROCESS WITH PRIORS
weights_table <- ridge_variables %>%
  filter(!is.na(epa)) %>%
  dplyr::select(Rush_Weight, week)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights =  Rush_Weight * ((1 - 0.025) ^ (17 - week))) %>%
  mutate(weights = weights + -min(weights) + 0.01) %>%
  dplyr::select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))
weights_final <- as(weights_final, "sparseMatrix")


## WORKING SEQUENCE
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)
grid <- 10^seq(2,-2,length=100)

set.seed(01061997)


fit <- glmnet(model_final, epa_final, alpha = 0, 
              lambda = lambdas_to_try, parallel = TRUE,
              standardize = FALSE) ## ADD WEIGHTS AFTER CREATING
summary(fit)

ridge_cv <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10, weights = weights_final,
                      lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season <- ridge_cv$lambda.min
best_lambda_season

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge_season <- glmnet(model_final, epa_final, alpha = 0, lambda = best_lambda_season,
                            weights = weights_final, parallel = TRUE, standardize = FALSE)
coef(best_ridge_season)
coef_ridge_rushing <- tidy(best_ridge_season)

## NO WEIGHTS VERSION
ridge_cv_no_weights <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10,
                                 lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season_no_weights <- ridge_cv_no_weights$lambda.min
best_lambda_season_no_weights

best_fit_no_weights <- ridge_cv_no_weights$glmnet.fit
head(best_fit_no_weights)

best_ridge_season_no_weights <- glmnet(model_final, epa_final, alpha = 0, 
                                       lambda = best_lambda_season_no_weights,
                                       parallel = TRUE, standardize = FALSE)
coef(best_ridge_season_no_weights)
coef_ridge_rushing_no_weights <- tidy(best_ridge_season_no_weights)


## Rusher 
rusher_coef <- coef_ridge_rushing %>%
  filter(grepl('Rusher_', term))
rusher_names <- rusher_coef$term %>%
  str_remove_all("Rusher_") %>%
  str_replace_all("_"," ")

rusher_coef <- cbind(rusher_coef, rusher_names) %>%
  mutate(rusher_names = case_when(rusher_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  rusher_names == "AJ Feeley" ~ "A.J. Feeley",
                                  rusher_names == "JP Losman" ~ "J.P. Losman",
                                  rusher_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  rusher_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  rusher_names == "TJ Yates" ~ "T.J. Yates",
                                  rusher_names == "CJ Beathard" ~ "C.J. Beathard",
                                  rusher_names == "Abdul Karim al Jabbar" ~ "Abdul-Karim al-Jabbar",
                                  rusher_names == "Michael Pittman 1" ~ "Michael Pittman_1",
                                  rusher_names == "Chris Warren 1" ~ "Chris Warren_1",
                                  rusher_names == "Justin Watson 1" ~ "Justin Watson_1",
                                  rusher_names == "DeMond Parker" ~ "De'Mond Parker",
                                  rusher_names == "JJ Johnson" ~ "J.J. Johnson",
                                  rusher_names == "Chris Fuamatu Maafala" ~ "Chris Fuamatu-Ma'afala",
                                  rusher_names == "JR Redmond" ~ "J.R. Redmond",
                                  rusher_names == "KiJana Carter" ~ "Ki-Jana Carter",
                                  rusher_names == "Ricky Williams 1" ~ "Ricky Williams_1",
                                  rusher_names == "Adrian Peterson 1" ~ "Adrian Peterson_1",
                                  rusher_names == "TJ Duckett" ~ "T.J. Duckett",
                                  rusher_names == "Karl Williams 1" ~ "Karl Williams_1",
                                  rusher_names == "Chris Brown 2" ~ "Chris Brown_2",
                                  rusher_names == "JJ Arrington" ~ "J.J. Arrington",
                                  rusher_names == "Maurice Jones Drew" ~ "Maurice Jones-Drew",
                                  rusher_names == "Ryan Grant 1" ~ "Ryan Grant_1",
                                  rusher_names == "LeRon McClain" ~ "Le'Ron McClain",
                                  rusher_names == "Kevin Smith 1" ~ "Kevin Smith_1",
                                  rusher_names == "DJ Ware" ~ "D.J. Ware",
                                  rusher_names == "BenJarvus Green Ellis" ~ "BenJarvus Green-Ellis",
                                  rusher_names == "LaRod Stephens Howling" ~ "LaRod Stephens-Howling",
                                  rusher_names == "CJ Spiller" ~ "C.J. Spiller",
                                  rusher_names == "LeVeon Bell" ~ "Le'Veon Bell",
                                  rusher_names == "CJ Anderson" ~ "C.J. Anderson",
                                  rusher_names == "KaDeem Carey" ~ "Ka'Deem Carey",
                                  rusher_names == "TJ Yeldon" ~ "T.J. Yeldon",
                                  rusher_names == "TJ Jones" ~ "T.J. Jones",
                                  rusher_names == "CJ Prosise" ~ "C.J. Prosise",
                                  rusher_names == "JD McKissic" ~ "J.D. McKissic",
                                  rusher_names == "Cameron Artis Payne" ~ "Cameron Artis-Payne",
                                  rusher_names == "DOnta Foreman" ~ "D'Onta Foreman",
                                  rusher_names == "DErnest Johnson" ~ "D'Ernest Johnson",
                                  rusher_names == "Clyde Edwards Helaire" ~ "Clyde Edwards-Helaire",
                                  rusher_names == "JK Dobbins" ~ "J.K. Dobbins",
                                  rusher_names == "DAndre Swift" ~ "D'Andre Swift",
                                  rusher_names == "JJ Taylor" ~ "J.J. Taylor",
                                  rusher_names == "AJ Dillon" ~ "A.J. Dillon",
                                  rusher_names == "KeShawn Vaughn" ~ "Ke'Shawn Vaughn",
                                  rusher_names == "Jonathan Williams 1" ~ "Jonathan Williams_1",
                                  rusher_names == "DaRel Scott" ~ "Da'Rel Scott",
                                  rusher_names == "LaMical Perine" ~ "La'Mical Perine",
                                  rusher_names == "TySon Williams" ~ "Ty'Son Williams",
                                  TRUE ~ rusher_names)) %>%
  dplyr::select(Name = rusher_names, Rush_RAEPA = estimate)


rusher_coef_no_weights <- coef_ridge_rushing_no_weights %>%
  filter(grepl('Rusher_', term))
rusher_names <- rusher_coef_no_weights$term %>%
  str_remove_all("Rusher_") %>%
  str_replace_all("_"," ")

rusher_coef_no_weights <- cbind(rusher_coef_no_weights, rusher_names) %>%
  mutate(rusher_names = case_when(rusher_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  rusher_names == "AJ Feeley" ~ "A.J. Feeley",
                                  rusher_names == "JP Losman" ~ "J.P. Losman",
                                  rusher_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  rusher_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  rusher_names == "TJ Yates" ~ "T.J. Yates",
                                  rusher_names == "CJ Beathard" ~ "C.J. Beathard",
                                  rusher_names == "Abdul Karim al Jabbar" ~ "Abdul-Karim al-Jabbar",
                                  rusher_names == "Michael Pittman 1" ~ "Michael Pittman_1",
                                  rusher_names == "Chris Warren 1" ~ "Chris Warren_1",
                                  rusher_names == "Justin Watson 1" ~ "Justin Watson_1",
                                  rusher_names == "DeMond Parker" ~ "De'Mond Parker",
                                  rusher_names == "JJ Johnson" ~ "J.J. Johnson",
                                  rusher_names == "Chris Fuamatu Maafala" ~ "Chris Fuamatu-Ma'afala",
                                  rusher_names == "JR Redmond" ~ "J.R. Redmond",
                                  rusher_names == "KiJana Carter" ~ "Ki-Jana Carter",
                                  rusher_names == "Ricky Williams 1" ~ "Ricky Williams_1",
                                  rusher_names == "Adrian Peterson 1" ~ "Adrian Peterson_1",
                                  rusher_names == "TJ Duckett" ~ "T.J. Duckett",
                                  rusher_names == "Karl Williams 1" ~ "Karl Williams_1",
                                  rusher_names == "Chris Brown 2" ~ "Chris Brown_2",
                                  rusher_names == "JJ Arrington" ~ "J.J. Arrington",
                                  rusher_names == "Maurice Jones Drew" ~ "Maurice Jones-Drew",
                                  rusher_names == "Ryan Grant 1" ~ "Ryan Grant_1",
                                  rusher_names == "LeRon McClain" ~ "Le'Ron McClain",
                                  rusher_names == "Kevin Smith 1" ~ "Kevin Smith_1",
                                  rusher_names == "DJ Ware" ~ "D.J. Ware",
                                  rusher_names == "BenJarvus Green Ellis" ~ "BenJarvus Green-Ellis",
                                  rusher_names == "LaRod Stephens Howling" ~ "LaRod Stephens-Howling",
                                  rusher_names == "CJ Spiller" ~ "C.J. Spiller",
                                  rusher_names == "LeVeon Bell" ~ "Le'Veon Bell",
                                  rusher_names == "CJ Anderson" ~ "C.J. Anderson",
                                  rusher_names == "KaDeem Carey" ~ "Ka'Deem Carey",
                                  rusher_names == "TJ Yeldon" ~ "T.J. Yeldon",
                                  rusher_names == "TJ Jones" ~ "T.J. Jones",
                                  rusher_names == "CJ Prosise" ~ "C.J. Prosise",
                                  rusher_names == "JD McKissic" ~ "J.D. McKissic",
                                  rusher_names == "Cameron Artis Payne" ~ "Cameron Artis-Payne",
                                  rusher_names == "DOnta Foreman" ~ "D'Onta Foreman",
                                  rusher_names == "DErnest Johnson" ~ "D'Ernest Johnson",
                                  rusher_names == "Clyde Edwards Helaire" ~ "Clyde Edwards-Helaire",
                                  rusher_names == "JK Dobbins" ~ "J.K. Dobbins",
                                  rusher_names == "DAndre Swift" ~ "D'Andre Swift",
                                  rusher_names == "JJ Taylor" ~ "J.J. Taylor",
                                  rusher_names == "AJ Dillon" ~ "A.J. Dillon",
                                  rusher_names == "KeShawn Vaughn" ~ "Ke'Shawn Vaughn",
                                  rusher_names == "Jonathan Williams 1" ~ "Jonathan Williams_1",
                                  rusher_names == "DaRel Scott" ~ "Da'Rel Scott",
                                  rusher_names == "LaMical Perine" ~ "La'Mical Perine",
                                  rusher_names == "TySon Williams" ~ "Ty'Son Williams",
                                  TRUE ~ rusher_names)) %>%
  dplyr::select(Name = rusher_names, Rush_RAEPA_no_weights = estimate)

Rbs_Weights_Add_Back <- Rbs_Priors_PORP %>%
  filter(Season == {year})

rusher_coef <- left_join(rusher_coef, rusher_coef_no_weights, by = "Name") %>%
  left_join(Rbs_Weights_Add_Back, by = "Name") %>%
  mutate(Rush_RAEPA = if_else(!is.na(Rush_Weight), Rush_RAEPA + Rush_Weight, Rush_RAEPA),
         Rush_RAEPA_no_weights = if_else(!is.na(Rush_Weight), Rush_RAEPA_no_weights + Rush_Weight, Rush_RAEPA_no_weights)) %>%
  dplyr::select(-Season, -Rush_Weight)
RB_Coefs <- left_join(rusher_coef, receiver_coef, by = "Name") 



## PASSING DEFENSE

ridge_variables <- pbp_ridge %>%
  filter(season == {year} & qb_dropback == 1) %>%
  filter(!is.na(penalty) | penalty_type %in% c("Defensive Offside", "Intentional Grounding", 
                                               "Encroachment", "Neutral Zone Infraction",
                                               "Delay of Game", "Inelgible Downfield Pass", 
                                               "Unneccessary Roughness",
                                               "Offensive Too Many Men on Field")) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0)) %>%
  mutate(epa = round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Pass_Weight - Receiving_Weight) * ((1 - 0.025) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.025) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.025) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17 & vegas_wp > 0.85, Total_Weight * 0.25, Total_Weight)) %>%
  mutate(Total_Weight = if_else(wp >= 0.015 & wp <= 0.985, Total_Weight, Total_Weight * 0.12))

ridge_variables <- ridge_variables %>%
  make_passer_mutations()


model_dataframe <- ridge_variables %>%
  select_at(vars(contains(c("Passer_", "Rusher_", "Receiver_", "Pass_Defense", "yardline", "shotgun",
                            "total_line", "drive_play_count", "yards_times_down",
                            "yards_to_go_sqaure", "vegas_wp", "field", "series", "epa")))) %>%
  dplyr::select(-passer_player_id, -passer_player_name, -passer_jersey_number, -passer_id,
                -rusher_player_id, -rusher_player_name, -rusher_jersey_number, -rusher_id, -Rusher_gsis,
                -receiver_player_id, -receiver_player_name, -receiver_jersey_number, -receiver_id, -Receiver_gsis,
                -vegas_wpa, -vegas_wp, -side_of_field, -total_home_epa, -total_away_epa, -total_home_rush_epa, 
                -total_away_rush_epa, -total_home_pass_epa, -total_away_pass_epa, -air_epa, -yac_epa, -comp_air_epa,
                -comp_yac_epa, -total_home_comp_air_epa, -total_away_comp_air_epa, -total_home_comp_yac_epa,
                -total_away_comp_yac_epa, -total_home_raw_air_epa, -total_away_raw_air_epa, -total_home_raw_yac_epa,
                -total_away_raw_yac_epa, -qb_epa, -xyac_epa, -pass_defense_1_player_id, -pass_defense_1_player_name,
                -pass_defense_2_player_id, -pass_defense_2_player_name, -series_success, -series_result,
                -series, -drive_play_count, -side_of_field_indicator) %>%
  mutate(across(c("yardline_100", "yardline_squared", "total_line",
                  "yards_times_down","vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4))) %>%
  filter(!is.na(epa))


epa_table <- model_dataframe %>%
  dplyr::select(epa)

## ADJUST VARIABLES TO WORK IN GLMNET
epa_vector <- as.vector(epa_table)
epa_vector[is.na(epa_vector)] <- 0
model_vector <- as.vector(model_dataframe) %>%
  select(-epa)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
epa_final <- as(epa_final, "sparseMatrix")
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- as(model_final, "sparseMatrix")

## REPEAT SAME PROCESS WITH PRIORS
weights_table <- ridge_variables %>%
  filter(!is.na(epa)) %>%
  dplyr::select(Pass_Weight, Receiving_Weight, week)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights = (Pass_Weight - Receiving_Weight) * ((1 - 0.025) ^ (17 - week))) %>%
  mutate(weights = weights + -min(weights) + 0.01) %>%
  dplyr::select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))
weights_final <- as(weights_final, "sparseMatrix")

## WORKING SEQUENCE LAMBDAS
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)
grid <- 10^seq(2,-2,length=100)

set.seed(01061997)


fit <- glmnet(model_final, epa_final, alpha = 0, 
              lambda = lambdas_to_try, parallel = TRUE,
              standardize = FALSE) ## ADD WEIGHTS AFTER CREATING
summary(fit)


ridge_cv <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10, lambda = grid,
                      weights = weights_final, parallel = TRUE, standardize = FALSE)
best_lambda_season <- ridge_cv$lambda.min
best_lambda_season

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge_season <- glmnet(model_final, epa_final, alpha = 0, lambda = best_lambda_season,
                            weights = weights_final, parallel = TRUE)
coef(best_ridge_season)
coef_ridge_pass_defense <- tidy(best_ridge_season)

## NO WEIGHTS VERSION
ridge_cv_no_weights <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10,
                                 lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season_no_weights <- ridge_cv_no_weights$lambda.min
best_lambda_season_no_weights

best_fit_no_weights <- ridge_cv_no_weights$glmnet.fit
head(best_fit_no_weights)

best_ridge_season_no_weights <- glmnet(model_final, epa_final, alpha = 0, 
                                       lambda = best_lambda_season_no_weights,
                                       parallel = TRUE)
coef(best_ridge_season_no_weights)
coef_ridge_pass_defense_no_weights <- tidy(best_ridge_season_no_weights)

pass_d_coef <- coef_ridge_pass_defense %>%
  filter(grepl('Pass_Defense_', term))
pass_d_names <- pass_d_coef$term %>%
  str_remove_all("Pass_Defense") %>%
  str_replace_all("_"," ")

pass_d_coef <- cbind(pass_d_coef, pass_d_names) %>%
  dplyr::select(Team = pass_d_names, Pass_D_RAEPA = estimate)


pass_d_coef_no_weights <- coef_ridge_pass_defense_no_weights %>%
  filter(grepl('Pass_Defense_', term))
pass_d_names <- pass_d_coef_no_weights$term %>%
  str_remove_all("Pass_Defense") %>%
  str_replace_all("_"," ")

pass_d_coef_no_weights <- cbind(pass_d_coef_no_weights, pass_d_names) %>%
  dplyr::select(Team = pass_d_names, Pass_D_RAEPA_no_weights = estimate)

pass_d_coef <- left_join(pass_d_coef, pass_d_coef_no_weights, by = "Team")

## DEFENSE VALUE
Mean_D_Snaps <- ridge_variables %>%
  filter(season == {year}) %>%
  group_by(defteam) %>%
  summarise(Plays = n()) %>%
  mutate(PPG = Plays / 16)

mean_pd <- mean(pass_d_coef$Pass_D_RAEPA)

Pass_D_Value <- cbind(Mean_D_Snaps, pass_d_coef) %>%
  mutate(Pass_D_Value = (Pass_D_RAEPA - mean_pd) * PPG)


## RUSH DEFENSE

ridge_variables <- pbp_ridge %>%
  filter(season == {year} & rush == 1) %>%
  filter(!is.na(penalty) | penalty_type %in% c("Unneccessary Roughness", "Face Mask",
                                               "Horse Collar Tackle",
                                               "Lowering the Head to Initiate Contact")) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0)) %>%
  mutate(epa = round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Total_Weight = case_when(!is.na(Rush_Weight) ~ (Rush_Weight) * ((1 - 0.025) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17 & vegas_wp > 0.85, Total_Weight * 0.25, Total_Weight)) %>%
  mutate(Total_Weight = if_else(wp >= 0.015 & wp <= 0.985, Total_Weight, Total_Weight * 0.12))

ridge_variables <- ridge_variables %>%
  make_rusher_mutations()

## CREATE MODEL DATA FRAME SELECTING ONLY RIDGE VARIABLES

model_dataframe <- ridge_variables %>%
  select_at(vars(contains(c("Passer_", "Rusher_", "Receiver_", "Run_Defense", "yardline", "shotgun",
                            "total_line", "drive_play_count", "yards_times_down",
                            "yards_to_go_sqaure", "vegas_wp", "field", "series", "epa")))) %>%
  dplyr::select(-passer_player_id, -passer_player_name, -passer_jersey_number, -passer_id,
                -rusher_player_id, -rusher_player_name, -rusher_jersey_number, -rusher_id, -Rusher_gsis,
                -receiver_player_id, -receiver_player_name, -receiver_jersey_number, -receiver_id, -Receiver_gsis,
                -vegas_wpa, -vegas_wp, -side_of_field, -total_home_epa, -total_away_epa, -total_home_rush_epa, 
                -total_away_rush_epa, -total_home_pass_epa, -total_away_pass_epa, -air_epa, -yac_epa, -comp_air_epa,
                -comp_yac_epa, -total_home_comp_air_epa, -total_away_comp_air_epa, -total_home_comp_yac_epa,
                -total_away_comp_yac_epa, -total_home_raw_air_epa, -total_away_raw_air_epa, -total_home_raw_yac_epa,
                -total_away_raw_yac_epa, -qb_epa, -xyac_epa, -series_success, -series_result,
                -series, -drive_play_count, -side_of_field_indicator) %>%
  mutate(across(c("yardline_100", "yardline_squared", "total_line",
                  "yards_times_down","vegas_wp_squared", "log_vegas_wp"), 
                function(x) round((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE), 4))) %>%
  filter(!is.na(epa))


epa_table <- model_dataframe %>%
  dplyr::select(epa) 


## ADJUST VARIABLES TO WORK IN GLMNET
epa_vector <- as.vector(epa_table)
epa_vector[is.na(epa_vector)] <- 0
model_vector <- as.vector(model_dataframe) %>%
  select(-epa)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
epa_final <- as(epa_final, "sparseMatrix")
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- as(model_final, "sparseMatrix")
## REPEAT SAME PROCESS WITH PRIORS
weights_table <- ridge_variables %>%
  filter(!is.na(epa)) %>%
  dplyr::select(Rush_Weight, week)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights =  Rush_Weight * ((1 - 0.025) ^ (17 - week))) %>%
  mutate(weights = weights + -min(weights) + 0.01) %>%
  dplyr::select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))
weights_final <- as(weights_final, "sparseMatrix")


## WORKING SEQUENCE
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lambda_seq <- 10^seq(2, -2, by = -.1)
grid <- 10^seq(2,-2,length=100)

set.seed(01061997)

fit <- glmnet(model_final, epa_final, alpha = 0, 
              lambda = lambdas_to_try, parallel = TRUE,
              standardize = FALSE) ## ADD WEIGHTS AFTER CREATING
summary(fit)

ridge_cv <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10, weights = weights_final,
                      lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season <- ridge_cv$lambda.min
best_lambda_season

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

best_ridge_season <- glmnet(model_final, epa_final, alpha = 0, lambda = best_lambda_season,
                            weights = weights_final, parallel = TRUE, standardize = FALSE)
coef(best_ridge_season)
coef_ridge_rushing_defense <- tidy(best_ridge_season)

## NO WEIGHTS VERSION
ridge_cv_no_weights <- cv.glmnet(model_final, epa_final, alpha = 0, nfolds = 10,
                                 lambda = grid, parallel = TRUE, standardize = FALSE)
best_lambda_season_no_weights <- ridge_cv_no_weights$lambda.min
best_lambda_season_no_weights

best_fit_no_weights <- ridge_cv_no_weights$glmnet.fit
head(best_fit_no_weights)

best_ridge_season_no_weights <- glmnet(model_final, epa_final, alpha = 0, 
                                       lambda = best_lambda_season_no_weights,
                                       parallel = TRUE, standardize = FALSE)
coef(best_ridge_season_no_weights)
coef_ridge_rushing_defense_no_weights <- tidy(best_ridge_season_no_weights)

## RUN DEFENSE COEFFICIENTS
run_d_coef <- coef_ridge_rushing_defense %>%
  filter(grepl('Run_Defense_', term))
run_d_names <- run_d_coef$term %>%
  str_remove_all("Run_Defense") %>%
  str_replace_all("_"," ")

run_d_coef <- cbind(run_d_coef, run_d_names) %>%
  dplyr::select(Team = run_d_names, Rush_D_RAEPA = estimate)

run_d_coef_no_weights <- coef_ridge_rushing_defense_no_weights %>%
  filter(grepl('Run_Defense_', term))
run_d_names <- run_d_coef_no_weights$term %>%
  str_remove_all("Run_Defense") %>%
  str_replace_all("_"," ")

run_d_coef_no_weights <- cbind(run_d_coef_no_weights, run_d_names) %>%
  dplyr::select(Team = run_d_names, Rush_D_RAEPA_no_weights = estimate)


run_d_coef <- left_join(run_d_coef, run_d_coef_no_weights, by = "Team")

## DEFENSE VALUE
Mean_D_Snaps <- ridge_variables %>%
  filter(season == {year}) %>%
  group_by(defteam) %>%
  summarise(Plays = n()) %>%
  mutate(PPG = Plays / 16)

mean_rd <- mean(run_d_coef$Rush_D_RAEPA)

Run_D_Value <- cbind(Mean_D_Snaps, run_d_coef) %>%
  mutate(Run_D_Value = (Rush_D_RAEPA - mean_rd) * PPG)


## JOINED COEFFICIENTS
Team_D_Coefs <- left_join(pass_d_coef, run_d_coef, by = "Team") 

Reg_EPA <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(pass == 1) %>%
  group_by(defteam) %>%
  summarise(Pass_EPA = mean(epa, na.rm = TRUE)) %>%
  ungroup()
Reg_EPA_R <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(rush == 1) %>%
  group_by(defteam) %>%
  summarise(Rush_EPA = mean(epa, na.rm = TRUE)) %>%
  ungroup()
Reg_EPA <- left_join(Reg_EPA, Reg_EPA_R, by = "defteam")


Team_D_Coefs <- cbind(Team_D_Coefs, Reg_EPA) %>%
  dplyr::select(-defteam)

## DEFENSE VALUE 

Mean_P_Snaps <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(pass == 1) %>%
  group_by(defteam) %>%
  summarise(Plays_P = n()) %>%
  mutate(PPG_P = Plays_P / 16)
Mean_R_Snaps <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(rush == 1) %>%
  group_by(defteam) %>%
  summarise(Plays_R = n()) %>%
  mutate(PPG_R = Plays_R / 16)
Mean_D_Snaps <- left_join(Mean_P_Snaps, Mean_R_Snaps, by = "defteam")

mean_pd <- mean(Team_D_Coefs$Pass_D_RAEPA)
mean_rd <- mean(Team_D_Coefs$Rush_D_RAEPA)
mean_ppg <- mean(Mean_D_Snaps$PPG_P)
mean_ppgr <- mean(Mean_D_Snaps$PPG_R)

Team_D_Value <- cbind(Mean_D_Snaps, Team_D_Coefs) %>%
  mutate(Season = {year}) %>%
  mutate(Pass_D_Value_W = Pass_D_RAEPA * mean_ppg) %>%
  mutate(Run_D_Value_W = Rush_D_RAEPA * mean_ppgr) %>%
  mutate(Pass_D_Value_W = round(Pass_D_Value_W - mean(Pass_D_Value_W), digits = 2),
         Run_D_Value_W = round(Run_D_Value_W - mean(Run_D_Value_W), digits = 2)) %>%
  mutate(Pass_D_Value = Pass_D_RAEPA_no_weights * mean_ppg) %>%
  mutate(Run_D_Value = Rush_D_RAEPA_no_weights * mean_ppgr) %>%
  mutate(Pass_D_Value = round(Pass_D_Value - mean(Pass_D_Value), digits = 2),
         Run_D_Value = round(Run_D_Value - mean(Run_D_Value), digits = 2)) %>%
  dplyr::select(Team, Pass_D_RAEPA_no_weights, Rush_D_RAEPA_no_weights,
                Pass_EPA, Rush_EPA, Pass_D_Value, Run_D_Value, Pass_D_Value_W, Run_D_Value_W) %>%
  mutate(Total_D_Value = Pass_D_Value + Run_D_Value,
         Total_D_Value_W = Pass_D_Value_W + Run_D_Value_W)

Team_Coefs <- left_join(pass_d_coef, run_d_coef, by = "Team")

## TEAM SNAPS
Rushing_Snaps <- pbp_ridge %>%
  filter(season == {year},
         rush == 1,
         !is.na(penalty) | penalty_type %in% c("Unneccessary Roughness", "Face Mask",
                                               "Horse Collar Tackle",
                                               "Lowering the Head to Initiate Contact")) %>%
  group_by(posteam) %>%
  summarise(Team_Rush_Snaps = n())
Receiving_Snaps <- pbp_ridge %>%
  filter(season == {year},
         pass == 1,
         !is.na(penalty) | penalty_type %in% c("Illegal Contact", "Offensive Pass Interference",
                                               "Defensive Holding", "Defensive Pass Interference",
                                               "Defensive Offside", "Unneccessary Roughness",
                                               "Horse Collar Tackle", 
                                               "Lowering the Head to Initiate Contact")) %>%
  group_by(posteam) %>%
  summarise(Team_Rec_Snaps = n())


## RUSHING STATS
Rushing_stats <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(rush == 1) %>%
  group_by(Rusher, Rusher_gsis, posteam) %>%
  summarise(Games = length(unique(game_id)),
            Rush_Attempts = sum(rush_attempt, na.rm = TRUE),
            Rush_Per_Game = Rush_Attempts / Games,
            Rush_Yards = sum(rushing_yards, na.rm = TRUE),
            Rush_Touchdowns = sum(rush_touchdown, na.rm = TRUE),
            YPA = round(Rush_Yards / Rush_Attempts, digits = 2),
            Fumbles = sum(fumble, na.rm = TRUE),
            Rush_EPA = sum(mean(epa * rush_attempt, na.rm = TRUE), digits = 2),
            Total_Rush_EPA = round(sum(epa, na.rm = TRUE), digits = 2),
            Success = round(mean(success, na.rm = TRUE), digits = 2))

## RECEIVING STATS - ADD ADOT AFTER 2005
Receiving_stats_rb <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(pass == 1) %>%
  group_by(Receiver, Receiver_gsis, posteam) %>%
  filter(!is.na(Receiver)) %>%
  summarise(Games = length(unique(game_id)),
            Targets = sum(pass_attempt, na.rm = TRUE),
            T_Per_Game = Targets / Games,
            Receptions = sum(complete_pass, na.rm = TRUE),
            Rec_Yards = sum(yards_gained, na.rm = TRUE),
            ADOT = round(mean(air_yards, na.rm = TRUE), digits = 2),
            Rec_Touchdowns = sum(pass_touchdown, na.rm = TRUE),
            YPT = round(Rec_Yards / Targets, digits = 2),
            YPC = round(Rec_Yards / Receptions, digits = 2),
            YAC = sum(yards_after_catch, na.rm = TRUE),
            YAC_Catch = mean(yards_after_catch, na.rm = TRUE),
            EPA_T = round(mean(epa, na.rm = TRUE), digits = 2),
            Total_Receiving_EPA = round(sum(epa, na.rm = TRUE), digits = 2))


Rbs_stats <- left_join(Rushing_stats, Receiving_stats_rb, 
                       by = c("Rusher" = "Receiver", "Rusher_gsis" = "Receiver_gsis", "posteam")) %>%
  mutate(
    W_EPA = round((EPA_T * Targets) + (Rush_EPA * Rush_Attempts), digits = 2),
    Squid = if_else(is.na(Targets), Rush_Attempts, Targets + Rush_Attempts)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  filter(Rush_Attempts > 5) %>%
  dplyr::select(-Games.y) %>%
  rename(Games = Games.x)


Qb_position <- passer_coef %>%
  dplyr::select(Name) %>%
  mutate(Position = "QB")

Rbs_stats <- left_join(Rbs_stats, Qb_position, by = c("Rusher" = "Name")) %>%
  mutate(Position = if_else(is.na(Position), "RB", Position)) %>%
  left_join(Rushing_Snaps, by = "posteam") %>%
  left_join(Receiving_Snaps, by = "posteam")

mean_r_rae <- mean(RB_Coefs$Rush_RAEPA)
## RB DATASET WITH SEASON VALUES  
Rbs <- left_join(Rbs_stats, RB_Coefs, by = c("Rusher" = "Name")) %>%
  filter(!is.na(Rush_RAEPA)) %>%
  mutate(Season = {year}) %>%
  dplyr::select(Name = Rusher, ID = Rusher_gsis, Team = posteam, Position, Season, Games, Rush_Attempts,
         Rush_Per_Game, Rush_Yards, Rush_Touchdowns, YPA, Fumbles, Rush_EPA, Total_Rush_EPA, Targets,
         Receptions, Rec_Yards, YPC, YAC, YAC_Catch, Rec_Touchdowns, EPA_T, Total_Receiving_EPA, Team_Rush_Snaps,
         Team_Rec_Snaps, Rush_RAEPA, Rush_RAEPA_NW = Rush_RAEPA_no_weights, Receiving_RAEPA,
         Receiving_RAEPA_NW = Receiving_RAEPA_no_weights)

mean_rg <- mean(Rbs$Rush_Per_Game)
Rbs <- Rbs %>%
  mutate(R_Perc = round(Rush_Attempts / (Team_Rush_Snaps * (Games/16)), digits = 4)) %>%
  mutate(R_Perc = case_when(Name == "Lamar Jackson" ~ 0.85,
                            Name == "Mike Vick" ~ 0.70,
                            Name == "Kyler Murray" ~ 0.40,
                            TRUE ~ R_Perc)) %>%
  mutate(R_Perc = if_else(R_Perc > 1, 1, R_Perc)) %>%
  mutate(Total_R_Perc = round(Rush_Attempts / Team_Rush_Snaps, digits = 4)) %>%
  mutate(Total_R_Perc = case_when(Name == "Lamar Jackson" ~ 0.75,
                                  Name == "Mike Vick" ~ 0.50,
                                  Name == "Kyler Murray" ~ 0.40,
                            TRUE ~ Total_R_Perc)) %>%
  mutate(R_RTG = round(Rush_RAEPA * Rush_Per_Game, digits = 2)) %>%
  mutate(Adj_R_RTG = round((Rush_RAEPA * mean_rg) + ((Rush_Per_Game - 8) * .025) 
                           + ((Total_R_Perc*100 - 25)* .015), digits = 2)) %>%
  mutate(Adj_R_RTG = round((Adj_R_RTG / (mean_rg / 8 / .25)) * (Rush_Per_Game / mean_rg), digits = 2))

mean_r_rtg <- mean(Rbs$R_RTG)
mean_adj_r_rtg <- mean(Rbs$Adj_R_RTG)

Rbs <- Rbs %>%
  mutate(R_RTG = round(R_RTG - mean_r_rtg, digits = 2),
         Adj_R_RTG = round(Adj_R_RTG - mean_adj_r_rtg, digits = 2))

## FINISH WRS GROUPING AND STATS
## COLLECT SEASON LONG STATS AND JOIN WITH RIDGE COEFS

## RECEIVING STATS - ADD ADOT AFTER 2005
Receiving_stats <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(pass == 1) %>%
  group_by(Receiver, Receiver_gsis, posteam) %>%
  summarise(Games = length(unique(game_id)),
            Targets = sum(pass_attempt, na.rm = TRUE),
            T_Per_Game = Targets / Games,
            Receptions = sum(complete_pass, na.rm = TRUE),
            Rec_Yards = sum(yards_gained, na.rm = TRUE),
            YAC = sum(yards_after_catch, na.rm = TRUE),
            YAC_Catch = mean(yards_after_catch, na.rm = TRUE),
            ADOT = round(mean(air_yards, na.rm = TRUE), digits = 2),
            Rec_Touchdowns = sum(pass_touchdown, na.rm = TRUE),
            YPC = round(Rec_Yards / Receptions, digits = 2),
            YPT = round(Rec_Yards / Targets, digits = 2),
            EPA_T = round(mean(epa, na.rm = TRUE), digits = 2),
            Total_Receiving_EPA = round(sum(epa, na.rm = TRUE), digits = 2)) %>%
  filter(Receiver != "LeRon McCoy") %>%
  filter(Targets > 5) %>%
  mutate_all(~replace(., is.na(.), 0))


Receiving_stats <- left_join(Receiving_stats, Receiving_Snaps, by = "posteam")

mean_re_rae <- mean(receiver_coef$Receiving_RAEPA_no_weights)
## WR DATASET WITH SEASON VALUES  
Wrs <- left_join(Receiving_stats, receiver_coef, by = c("Receiver" = "Name")) %>%
  mutate(Season = {year}) %>%
  left_join(skill_season_snaps, by = c("Receiver" = "full_name", "Receiver_gsis" = "gsis_id",
                                       "Season" = "season")) %>%
  dplyr::select(Name = Receiver, ID = Receiver_gsis, Position = position, 
                Team = posteam, Season, Games, Targets, T_Per_Game, Receptions, Rec_Yards, YAC, 
                YAC_Catch, Rec_Touchdowns, ADOT, YPT, YPC, Total_Receiving_EPA, Team_Rec_Snaps, 
                Offense_Snaps = cum_o_snaps, Receiving_RAEPA,
                Receiving_RAEPA_NW = Receiving_RAEPA_no_weights)

wr_rushing <- Rbs %>%
  dplyr::select(Name, ID, Team, Season, Rush_Attempts, Rush_Yards, Rush_Touchdowns,
         Rush_EPA, Total_Rush_EPA, Team_Rush_Snaps, Rush_RAEPA)

Wrs <- left_join(Wrs, wr_rushing, by = c("Name", "ID", "Team", "Season")) %>%
  mutate_all(~replace(., is.na(.), 0))

mean_tg <- mean(Wrs$T_Per_Game)
Wrs <- Wrs %>%
  mutate(T_Perc = round(Targets / (Team_Rec_Snaps * (Games/16)), digits = 4)) %>%
  mutate(Total_T_Perc = round(Targets / Team_Rec_Snaps, digits = 4)) %>%
  mutate(Weighted_RAEPA = round(Receiving_RAEPA * Total_T_Perc, digits = 4),
         Unweighted_RAEPA = round(Receiving_RAEPA_NW * Total_T_Perc, digits = 4)) %>%
  mutate(Re_RTG = round(Receiving_RAEPA_NW * mean_tg * Total_T_Perc, digits = 2)) %>%
  mutate(Adj_Re_RTG = round((Receiving_RAEPA_NW * mean_tg) + ((ADOT - 7.5)*.05) +
                               ((Total_T_Perc*100 - 11.65)*.075), digits = 2)) %>%
  mutate(Adj_Re_RTG = round((Adj_Re_RTG / (mean_tg / 7.5 / .1165)) * (T_Per_Game / mean_tg), digits = 2))

mean_rec <- mean(Wrs$Re_RTG)
mean_rec_2 <- mean(Wrs$Adj_Re_RTG)

Wrs <- Wrs %>%
  mutate(Re_RTG = round(Re_RTG - mean_rec, digits = 2)) %>%
  mutate(Adj_Re_RTG = round(Adj_Re_RTG - mean_rec_2, digits = 2))


## FINISH QBS GROUPING AND STATS

## QB RUSH 
rusher_coef <- coef_ridge_rushing %>%
  filter(grepl('Rusher_', term))
rusher_names <- rusher_coef$term %>%
  str_remove_all("Rusher_") %>%
  str_replace_all("_"," ")

rusher_coef <- cbind(rusher_coef, rusher_names) %>%
  mutate(rusher_names = case_when(rusher_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  rusher_names == "AJ Feeley" ~ "A.J. Feeley",
                                  rusher_names == "JP Losman" ~ "J.P. Losman",
                                  rusher_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  rusher_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  rusher_names == "TJ Yates" ~ "T.J. Yates",
                                  rusher_names == "CJ Beathard" ~ "C.J. Beathard",
                                  TRUE ~ rusher_names)) %>%
  dplyr::select(Name = rusher_names, Rush_RAEPA = estimate)



rusher_coef_no_weights <- coef_ridge_rushing_no_weights %>%
  filter(grepl('Rusher_', term))
rusher_names <- rusher_coef_no_weights$term %>%
  str_remove_all("Rusher_") %>%
  str_replace_all("_"," ")

rusher_coef_no_weights <- cbind(rusher_coef_no_weights, rusher_names) %>%
  mutate(rusher_names = case_when(rusher_names == "Neil ODonnell" ~ "Neil O'Donnell",
                                  rusher_names == "AJ Feeley" ~ "A.J. Feeley",
                                  rusher_names == "JP Losman" ~ "J.P. Losman",
                                  rusher_names == "JT OSullivan" ~ "J.T. O'Sullivan",
                                  rusher_names == "Brian St Pierre" ~ "Brian St. Pierre",
                                  rusher_names == "TJ Yates" ~ "T.J. Yates",
                                  rusher_names == "CJ Beathard" ~ "C.J. Beathard",
                                  TRUE ~ rusher_names)) %>%
  dplyr::select(Name = rusher_names, Rush_RAEPA_no_weights = estimate)

rusher_coef <- left_join(rusher_coef, rusher_coef_no_weights, by = "Name")


QB_Coefs <- left_join(passer_coef, rusher_coef, by = "Name")


## COLLECT SEASON LONG STATS AND JOIN WITH RIDGE COEFS
Qbs_Rushing <- pbp_ridge %>%
  filter(season == {year}) %>%
  filter(rush == 1) %>%
  group_by(Rusher, Rusher_gsis, posteam) %>%
  filter(!is.na(Rusher)) %>%
  summarise(Rush_Attempts = sum(rush_attempt, na.rm = TRUE),
            Rush_Yards = sum(rushing_yards, na.rm = TRUE),
            Rush_Touchdowns = sum(rush_touchdown, na.rm = TRUE),
            Rush_EPA = sum(mean(epa * rush_attempt, na.rm = TRUE), digits = 2),
            Total_Rush_EPA = round(sum(epa, na.rm = TRUE), digits = 2))

Qbs_Passing <- pbp_ridge %>%  #### FOR SEASONS STARTING 2005 ADD CPOE, ADOT, COMP_AIR_EPA
  filter(season == {year}) %>%
  filter(pass == 1) %>%
  group_by(Passer, passer_id, posteam, Position) %>%
  summarise(
    Games = length(unique(game_id)),
    Dropbacks = sum(qb_dropback, na.rm = TRUE),
    DB_Per_Game = Dropbacks / Games,
    Completions = sum(complete_pass, na.rm = TRUE),
    Attempts = sum(pass_attempt, na.rm = TRUE),
    Yards = sum(yards_gained, na.rm = TRUE),
    Touchdowns = sum(pass_touchdown),
    Ints = sum(interception),
    Fumbles = sum(fumble, na.rm = TRUE),
    Sacks = sum(sack, na.rm = TRUE),
    Int_Rate = round((sum(interception, na.rm = TRUE) / Attempts), digits = 2),
    Total_EPA = round(sum(epa, na.rm = TRUE), digits = 2),
    EPA_Pass = round(mean(epa, na.rm = TRUE), digits = 2),
    Sacks_EPA = round(sum(epa * sack, na.rm = TRUE), digits = 2),
    CPOE = round(mean(cpoe, na.rm = TRUE), digits = 2),
    Air_Yards = round(mean(air_yards, na.rm = TRUE), 2)) %>%
  mutate(
    Comp_Percent = round(Completions / Attempts, digits = 2),
    EPA_Per_Game = round(Total_EPA / Games, digits = 2)) %>%
  filter(Attempts > 15) %>%
  ungroup()

Qbs_Stats <- left_join(Qbs_Passing, Qbs_Rushing, by = c("Passer" = "Rusher", "posteam")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(Passer) %>%
  mutate(Plays = Attempts + Rush_Attempts) %>%
  ungroup()

team_qb_snaps <- pbp_ridge %>%
  filter(season == {year},
         pass == 1,
         !is.na(penalty) | penalty_type %in% c("Defensive Offside", "Intentional Grounding", 
                                                      "Encroachment", "Neutral Zone Infraction",
                                                      "Delay of Game", "Inelgible Downfield Pass", 
                                                      "Unneccessary Roughness",
                                                      "Offensive Too Many Men on Field")) %>%
  group_by(posteam) %>%
  summarise(team_db = sum(qb_dropback, na.rm = TRUE),
            team_comp = sum(complete_pass, na.rm = TRUE),
            team_yards = sum(yards_gained, na.rm = TRUE))

mean_db <- mean(Qbs_Stats$DB_Per_Game)
## QB DATASET WITH SEASON VALUES
Qbs <- left_join(Qbs_Stats, QB_Coefs, by = c("Passer" = "Name")) %>%
  mutate(Season = {year}) %>%
  left_join(team_qb_snaps, by = "posteam") %>%
  left_join(skill_season_snaps, by = c("Passer" = "full_name", "passer_id" = "gsis_id",
                                   "Season" = "season", "Position" = "position")) %>%
  mutate(DB_Perc = round(Dropbacks / (team_db*(Games/16)), digits = 4)) %>%
  mutate(DB_Perc = if_else(DB_Perc > 1, 1, DB_Perc)) %>% 
  mutate(Total_DB_Perc = round(Dropbacks/team_db, digits = 4)) %>%
  mutate(P_RTG = round(Pass_RAEPA * mean_db * Total_DB_Perc, digits = 2)) %>%
  mutate(Adj_P_RTG = round((Pass_RAEPA * mean_db) 
                           + ((DB_Perc*100 - 72)*.05) + ((Air_Yards - 8.5) * .15) + ((CPOE - 0.1)*.10) +
                             (((Yards/Dropbacks) - 6) * .10), digits = 2)) %>%
  mutate(P_RTG = round(P_RTG - mean(P_RTG), digits = 2),
         Adj_P_RTG = round((Adj_P_RTG / (mean_db / (8.5 / 6 / 0.72 / 0.1))), digits = 2)) %>%
  mutate(Adj_P_RTG = round(Adj_P_RTG - mean(Adj_P_RTG), digits = 2),
         Weighted_RAEPA = round(Pass_RAEPA * Total_DB_Perc, digits = 4),
         Unweighted_RAEPA = round(Pass_RAEPA_no_weights * Total_DB_Perc, digits = 4)) %>%
  dplyr::select(Name = Passer, ID = passer_id, Position, Team = posteam, 
                Season, Games, Dropbacks, DB_Per_Game, Attempts, Completions, Yards, Touchdowns, 
                Ints, Fumbles, Sacks, EPA_Pass, Total_EPA, CPOE, Air_Yards, Rush_Attempts, Rush_Yards,
                Rush_Touchdowns, Rush_EPA, Total_Rush_EPA, team_db, Pass_RAEPA, 
                Pass_RAEPA_NW = Pass_RAEPA_no_weights, Rush_RAEPA, Weighted_RAEPA, Unweighted_RAEPA,
                Rush_RAEPA_NW = Rush_RAEPA_no_weights, P_RTG, DB_Perc, Adj_P_RTG)


## JOIN TOGETHER ALL THE PORPAGATU SCRIPTS AND CALCULATE BY POSITION
## QBS

Qbs_Rushing_Porp <- Rbs %>%
  dplyr::select(Name, Team, ID, Team, Season, R_RTG, Adj_R_RTG)

Qbs <- left_join(Qbs, Qbs_Rushing_Porp, by = c("Name", "Team", "ID", "Season")) %>%
  mutate(Total_RTG = if_else(!is.na(R_RTG), P_RTG + R_RTG, P_RTG),
         Total_Adj_RTG = if_else(!is.na(Adj_R_RTG), Adj_P_RTG + Adj_R_RTG, Adj_P_RTG))

## RBS
Rbs_Receiving_Porp <- Wrs %>%
  dplyr::select(Name, Team, ID, Team, Season, Re_RTG, Adj_Re_RTG)

Rbs <- left_join(Rbs, Rbs_Receiving_Porp, by = c("Name", "Team", "ID", "Season")) %>%
  mutate(Re_RTG = round(Re_RTG * ((Targets/Games) / mean_tg), digits = 2),
         Adj_Re_RTG = round(Adj_Re_RTG * ((Targets/Games) / mean_tg), digits = 2)) %>%
  mutate(Total_PORP = if_else(!is.na(Re_RTG), R_RTG + Re_RTG, R_RTG),
         Total_PORP_1 = if_else(!is.na(Adj_Re_RTG), Adj_R_RTG + Adj_Re_RTG, Adj_R_RTG))

## WRS
Wrs_Rushing_Porp <- Rbs %>%
  dplyr::select(Name, Team, ID, Team, Season, R_RTG, Adj_R_RTG)

Wrs <- left_join(Wrs, Wrs_Rushing_Porp, by = c("Name", "Team", "ID", "Season")) %>%
  mutate(Total_PORP = if_else(!is.na(R_RTG), Re_RTG + R_RTG, Re_RTG),
         Total_PORP_1 = if_else(!is.na(Adj_R_RTG), Adj_Re_RTG + Adj_R_RTG, Adj_Re_RTG))

Qbs_graph <- Qbs %>%
  filter(Dropbacks > 150)
qb_graph <- ggplot(Qbs_graph, aes(x=Unweighted_RAEPA,y=Total_Adj_RTG)) +
  geom_hline(yintercept = mean(Qbs_graph$Total_Adj_RTG), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(Qbs_graph$Unweighted_RAEPA), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(Qbs_graph$Name == "Patrick Mahomes", "red", "black"), cex=Qbs_graph$Dropbacks/60, alpha=1/4) +
  geom_text_repel(aes(label=Name),
                  force=1, point.padding=0.04,
                  segment.size=0.1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  labs(x = "Unweighted Pass RAEPA",
       y="Adjusted Rating",
       title = "Quarterback Ratings (2020)",
       subtitle = "Unweighted Coefficient and Volume Adjusted QB Rating > 150 Dropbacks") +
  theme_bw() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))




## WRITE CSV's
write_csv(Qbs, "/Users/dylanmervis/Desktop/Priors/Qbs/Qbs_2021_RAPM_Season.csv")
write_csv(Wrs, "/Users/dylanmervis/Desktop/Priors/Wrs/Wrs_2021_RAPM_Season.csv")
write_csv(Rbs, "/Users/dylanmervis/Desktop/Priors/Rbs/Rbs_2021_RAPM_Season.csv")
write_csv(Team_D_Value, "/Users/dylanmervis/Desktop/Priors/Team_Defense_2021_RAPM_Season.csv")

