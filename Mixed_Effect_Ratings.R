library(dplyr)
library(na.tools)
library(lme4) 
library(broom.mixed) 
library(ggplot2)
library(tidyr) 
library(plm) 

## PRIORS

Qbs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Qbs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(is.na(QB_Prior)) %>%
  select(-QB_Prior, -season) %>%
  filter(Season == 2000) %>%
  mutate(across(4:27, as.numeric, digits = 4)) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  mutate(DB_Perc = round(Dropbacks/quantile(Dropbacks, 0.80), digits = 2)) %>%
  mutate(DB_Perc = if_else(DB_Perc > 1.00, 1, DB_Perc)) %>%
  mutate(DB_Perc = if_else(DB_Perc <= .15, .15, DB_Perc)) %>%
  mutate(Pass_Weight_1 = round((Pass_RAEPA_NW - mean(Pass_RAEPA_NW)) * DB_Per_Game * DB_Perc, digits = 4)) %>%
  select(Name, Games, Season, Pass_Weight)

Qbs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Qbs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season > 2000) %>%
  mutate(across(5:38, as.numeric, digits = 4)) %>%
  distinct(Name, Season, .keep_all = TRUE) %>%
  select(Name, Season, Games, Pass_Weight = Unweighted_RAEPA)


Qbs_Priors_PORP <- rbind(Qbs_Priors_PORP, Qbs_Priors_Chain)

Rbs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Rbs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(is.na(RB_Prior)) %>%
  select(-RB_Prior, -season) %>%
  filter(Season == 2000) %>%
  mutate(across(4:23, as.numeric, digits = 4)) %>%
  mutate(Rush_Perc = round(Rush_Attempts/quantile(Rush_Attempts, 0.90), digits = 2)) %>%
  mutate(Rush_Perc = if_else(Rush_Perc > 1.00, 1, Rush_Perc)) %>%
  mutate(Rush_Perc = if_else(Rush_Perc < .15, .15, Rush_Perc)) %>%
  mutate(Rush_Weight_1 = round((Rush_RAEPA_NW - mean(Rush_RAEPA_NW)) * Rush_Per_Game * Rush_Perc, digits = 4)) %>%
  select(Name, Season, Rush_Weight) %>%
  distinct(Name, Season, .keep_all = TRUE)

Rbs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Rbs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season > 2000) %>%
  mutate(across(5:34, as.numeric, digits = 4)) %>%
  mutate(Rush_Weight = round(Rush_RAEPA_NW * R_Perc, digits = 4)) %>%
  select(Name, Season, Rush_Weight) %>%
  distinct(Name, Season, .keep_all = TRUE)

Rbs_Priors_PORP <- rbind(Rbs_Priors_PORP, Rbs_Priors_Chain)

Wrs_Priors_PORP <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Wrs",
             pattern = "PORP.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(is.na(WR_Prior)) %>%
  select(-WR_Prior) %>%
  filter(Season == 2000) %>%
  mutate(across(3:22, as.numeric, digits = 4)) %>%
  mutate(Target_Perc = round(Targets/quantile(Targets, 0.90), digits = 2)) %>%
  mutate(Target_Perc = if_else(Target_Perc > 1.00, 1, Target_Perc)) %>%
  mutate(Target_Perc = if_else(Target_Perc < .15, .15, Target_Perc)) %>%
  mutate(Receiving_Weight_1 = round((Receiving_RAEPA_NW - mean(Receiving_RAEPA_NW)) * T_Per_Game * Target_Perc, digits = 4)) %>%
  select(Name, Season, Receiving_Weight) %>%
  distinct(Name, Season, .keep_all = TRUE)

Wrs_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors/Wrs",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season > 2000) %>%
  mutate(across(4:35, as.numeric, digits = 4)) %>%
  select(Name, Season, Receiving_Weight = Unweighted_RAEPA) %>%
  distinct(Name, Season, .keep_all = TRUE)

Wrs_Priors_PORP <- rbind(Wrs_Priors_PORP, Wrs_Priors_Chain)


D_Priors_Chain <-
  list.files(path = "/Users/dylanmervis/Desktop/Priors",
             pattern = "Season.csv", 
             full.names = TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(Season > 2000) %>%
  mutate(across(2:7, as.numeric, digits = 4)) %>%
  distinct(Team, Season, .keep_all = TRUE) %>%
  select(Team, Season, D_Weight = Pass_D_RAEPA)

year <- 2020:2020

mixed_pbp <- pbp_ridge %>%
  filter(!is.na(penalty) | penalty_type %in% c("Defensive Offside", "Intentional Grounding", 
                                               "Encroachment", "Neutral Zone Infraction",
                                               "Delay of Game", "Inelgible Downfield Pass", 
                                               "Unneccessary Roughness",
                                               "Offensive Too Many Men on Field")) %>%
  filter(pass == 1) %>%
  filter(season %in% {year}) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0),
         in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Starter = if_else(Games > 8, 1, 0)) %>%
  mutate(epa = round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  mutate(Pass_Weight = if_else(wp >= 0.015 & wp <= 0.985, Pass_Weight, Pass_Weight * 0.12),
         Rush_Weight = if_else(wp >= 0.015 & wp <= 0.985, Rush_Weight, Rush_Weight * 0.12),
         Receiving_Weight = if_else(wp >= 0.015 & wp <= 0.985, Receiving_Weight, Receiving_Weight * 0.12)) %>%
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Pass_Weight - Receiving_Weight) * ((1 - 0.05) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.05) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.05) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17, Total_Weight * 0.25, Total_Weight)) 

mixed_pbp <- mixed_pbp %>%
  make_pass_d_mutations()

mixed_model <- mixed_pbp %>%
  select(Passer, yardline_100, yardline_squared, yards_to_go_square, yards_times_down, vegas_wp_squared, log_vegas_wp,
         drive_play_count, total_line, shotgun, qtr,
         Starter, Pass_Defense_ARI, Pass_Defense_ATL, Pass_Defense_BAL, Pass_Defense_BUF,
         Pass_Defense_CAR, Pass_Defense_CHI, Pass_Defense_CIN, Pass_Defense_CLE, Pass_Defense_DAL, Pass_Defense_DEN,
         Pass_Defense_DET, Pass_Defense_GB, Pass_Defense_HOU, Pass_Defense_IND, Pass_Defense_JAX, Pass_Defense_KC,
         Pass_Defense_LA, Pass_Defense_LAC, Pass_Defense_LV, Pass_Defense_MIA, Pass_Defense_MIN, Pass_Defense_NE,
         Pass_Defense_NO, Pass_Defense_NYG, Pass_Defense_NYJ, Pass_Defense_PHI, Pass_Defense_PIT, Pass_Defense_SEA,
         Pass_Defense_SF, Pass_Defense_TB, Pass_Defense_TEN, Pass_Defense_WAS) %>%
  mutate(across(2:6, function(x) round((x - mean(x)) / sd(x), 4)))


epa_mixed <- mixed_pbp %>%
  select(epa)

epa_vector <- as.vector(epa_mixed)
epa_vector[is.na(epa_vector)] <- 0

passer_vector <- mixed_model %>%
  select(Passer)

model_vector <- as.vector(mixed_model) %>%
  select(-Passer)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- cbind(model_final, passer_vector) %>%
  cbind(epa_final)

weights_table <- mixed_pbp %>%
  select(Total_Weight)

weights_vector <- as.vector(weights_table)
weights_vector[is.na(weights_table)] <- 0
weights_vector <- weights_vector %>%
  mutate(weights = Total_Weight) %>%
  mutate(weights = (weights -min(weights)) /
           (max(weights) - min(weights))) %>%
  select(weights)

weights_final = as.matrix(as.data.frame(lapply(weights_vector, as.numeric)))

mixed <- lmer(epa ~
                yards_to_go_square +
                yards_times_down + 
                yardline_squared +
                vegas_wp_squared +
                log_vegas_wp +
                shotgun +
                Starter +
                Pass_Defense_ARI + Pass_Defense_ATL + Pass_Defense_BAL + Pass_Defense_BUF +
              Pass_Defense_CAR + Pass_Defense_CHI + Pass_Defense_CIN + Pass_Defense_CLE + Pass_Defense_DAL + Pass_Defense_DEN +
              Pass_Defense_DET + Pass_Defense_GB + Pass_Defense_HOU + Pass_Defense_IND + Pass_Defense_JAX + Pass_Defense_KC +
              Pass_Defense_LA + Pass_Defense_LAC + Pass_Defense_LV + Pass_Defense_MIA + Pass_Defense_MIN + Pass_Defense_NE +
              Pass_Defense_NO + Pass_Defense_NYG + Pass_Defense_NYJ + Pass_Defense_PHI + Pass_Defense_PIT + Pass_Defense_SEA +
              Pass_Defense_SF + Pass_Defense_TB + Pass_Defense_TEN + Pass_Defense_WAS +
                (1|Passer),
            data = model_final)


resampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}


Qbs_Passing <- pbp_ridge %>%  #### FOR SEASONS STARTING 2005 ADD CPOE, ADOT, COMP_AIR_EPA
  filter(season %in% {year}) %>%
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
  filter(Attempts > 29) %>%
  ungroup()

Qbs_Rushing <- pbp_ridge %>%
  filter(season %in% {year}) %>%
  filter(rush == 1) %>%
  group_by(Rusher, Rusher_gsis, posteam) %>%
  filter(!is.na(Rusher)) %>%
  summarise(Rush_Attempts = sum(rush_attempt, na.rm = TRUE),
            Rush_Yards = sum(rushing_yards, na.rm = TRUE),
            Rush_Touchdowns = sum(rush_touchdown, na.rm = TRUE),
            Rush_EPA = sum(mean(epa * rush_attempt, na.rm = TRUE), digits = 2),
            Total_Rush_EPA = round(sum(epa, na.rm = TRUE), digits = 2))

Qbs_Stats <- left_join(Qbs_Passing, Qbs_Rushing, by = c("Passer" = "Rusher", "posteam")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(Passer) %>%
  mutate(Plays = Attempts  + Rush_Attempts) %>%
  ungroup()

team_qb_snaps <- pbp_ridge %>%
  filter(season %in% {year},
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

Qbs_Weights_Add_Back <- Qbs_Priors_PORP %>%
  filter(Season %in% {year})

Qbs_Stats <- left_join(Qbs_Stats, team_qb_snaps, by = "posteam") %>%
  mutate(DB_Perc = round(Dropbacks / team_db, digits = 2))

tt <- broom.mixed::tidy(mixed, effects = "ran_vals") %>% 
  select(Name = level, Estimate = estimate, Std_Err = std.error) %>%
  mutate(T_Stat = Estimate / Std_Err) %>%
  select(Name, Estimate, Std_Err, T_Stat)


Qbs <- left_join(Qbs_Stats, tt, by = c( "Passer" = "Name")) %>%
  mutate(Rating = round((Estimate - mean(Estimate)) * DB_Perc * DB_Per_Game, digits = 2))



## REPEAT FOR RECEIVING

mixed_pbp_receiving <- pbp_ridge %>%
  filter(!is.na(penalty) | penalty_type %in% c("Illegal Contact", "Offensive Pass Interference",
                                               "Defensive Holding", "Defensive Pass Interference",
                                               "Defensive Offside", "Unneccessary Roughness",
                                               "Horse Collar Tackle", 
                                               "Lowering the Head to Initiate Contact")) %>%
  filter(pass == 1) %>%
  filter(season %in% {year}) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0),
         in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Starter = if_else(Games > 8, 1, 0)) %>%
  mutate(epa = round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  mutate(Pass_Weight = if_else(wp >= 0.015 & wp <= 0.985, Pass_Weight, Pass_Weight * 0.12),
         Rush_Weight = if_else(wp >= 0.015 & wp <= 0.985, Rush_Weight, Rush_Weight * 0.12),
         Receiving_Weight = if_else(wp >= 0.015 & wp <= 0.985, Receiving_Weight, Receiving_Weight * 0.12)) %>%
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Pass_Weight - Receiving_Weight) * ((1 - 0.05) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.05) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.05) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17, Total_Weight * 0.25, Total_Weight)) 

mixed_pbp_receiving <- mixed_pbp_receiving %>%
  make_pass_d_mutations()

mixed_model_receiving <- mixed_pbp_receiving %>%
  select(Receiver, yardline_100, yardline_squared, yards_to_go_square, yards_times_down, vegas_wp_squared, log_vegas_wp,
         shotgun,
         Pass_Defense_ARI, Pass_Defense_ATL, Pass_Defense_BAL, Pass_Defense_BUF,
         Pass_Defense_CAR, Pass_Defense_CHI, Pass_Defense_CIN, Pass_Defense_CLE, Pass_Defense_DAL, Pass_Defense_DEN,
         Pass_Defense_DET, Pass_Defense_GB, Pass_Defense_HOU, Pass_Defense_IND, Pass_Defense_JAX, Pass_Defense_KC,
         Pass_Defense_LA, Pass_Defense_LAC, Pass_Defense_LV, Pass_Defense_MIA, Pass_Defense_MIN, Pass_Defense_NE,
         Pass_Defense_NO, Pass_Defense_NYG, Pass_Defense_NYJ, Pass_Defense_PHI, Pass_Defense_PIT, Pass_Defense_SEA,
         Pass_Defense_SF, Pass_Defense_TB, Pass_Defense_TEN, Pass_Defense_WAS) %>%
  mutate(across(2:6, function(x) round((x - mean(x)) / sd(x), 4)))


epa_mixed <- mixed_pbp_receiving %>%
  select(epa)

epa_vector <- as.vector(epa_mixed)
epa_vector[is.na(epa_vector)] <- 0

receiver_vector <- mixed_model_receiving %>%
  select(Receiver)

model_vector <- as.vector(mixed_model_receiving) %>%
  select(-Receiver)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- cbind(model_final, receiver_vector) %>%
  cbind(epa_final)

mixed_receiving <- lmer(epa ~
                yards_to_go_square +
                yards_times_down + 
                yardline_squared +
                vegas_wp_squared +
                log_vegas_wp +
                shotgun +
                Pass_Defense_ARI + Pass_Defense_ATL + Pass_Defense_BAL + Pass_Defense_BUF +
                Pass_Defense_CAR + Pass_Defense_CHI + Pass_Defense_CIN + Pass_Defense_CLE + Pass_Defense_DAL + Pass_Defense_DEN +
                Pass_Defense_DET + Pass_Defense_GB + Pass_Defense_HOU + Pass_Defense_IND + Pass_Defense_JAX + Pass_Defense_KC +
                Pass_Defense_LA + Pass_Defense_LAC + Pass_Defense_LV + Pass_Defense_MIA + Pass_Defense_MIN + Pass_Defense_NE +
                Pass_Defense_NO + Pass_Defense_NYG + Pass_Defense_NYJ + Pass_Defense_PHI + Pass_Defense_PIT + Pass_Defense_SEA +
                Pass_Defense_SF + Pass_Defense_TB + Pass_Defense_TEN + Pass_Defense_WAS +
                (1|Receiver),
              data = model_final)


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
  filter(Targets > 14) %>%
  mutate_all(~replace(., is.na(.), 0))


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


Receiving_stats <- left_join(Receiving_stats, Receiving_Snaps, by = "posteam") %>%
  mutate(T_Perc = round(Targets / Team_Rec_Snaps, digits = 2))


tt_receiving <- broom.mixed::tidy(mixed_receiving, effects = "ran_vals") %>% 
  select(Name = level, Estimate = estimate, Std_Err = std.error) %>%
  mutate(T_Stat = Estimate / Std_Err) %>%
  select(Name, Estimate, Std_Err, T_Stat)


Wrs <- left_join(Receiving_stats, tt_receiving, by = c( "Receiver" = "Name")) %>%
  mutate(Rating = round(Estimate * T_Perc * T_Per_Game, digits = 2))


## RUSHING

mixed_pbp_rushing <- pbp_ridge %>%
  filter(!is.na(penalty) | penalty_type %in% c("Unneccessary Roughness", "Face Mask",
                                               "Horse Collar Tackle",
                                               "Lowering the Head to Initiate Contact")) %>%
  filter(rush == 1) %>%
  filter(season %in% {year}) %>%
  mutate(yards_to_go_square = ydstogo * ydstogo,
         yards_times_down = ydstogo * down,
         yardline_squared = yardline_100^2,
         vegas_wp_squared = vegas_wp * vegas_wp,
         log_vegas_wp = log(vegas_wp),
         side_of_field_indicator = if_else(posteam == side_of_field, 1, 0),
         in_red_zone = if_else(yardline_100 <= 20, 1, 0),
         in_fg_range = if_else(yardline_100 <= 35, 1, 0)) %>%
  left_join(Qbs_Priors_PORP, by = c("Passer" = "Name", "season" = "Season")) %>%
  left_join(Rbs_Priors_PORP, by = c("Rusher" = "Name", "season" = "Season")) %>%
  left_join(Wrs_Priors_PORP, by = c("Receiver" = "Name", "season" = "Season")) %>% ## PRIORS JOINED
  mutate(Starter = if_else(Games > 8, 1, 0)) %>%
  mutate(epa = round(epa * ((1 - 0.025) ^ (17 - week)), digits = 4)) %>%
  mutate(epa = if_else(week == 17 & vegas_wp > 0.85, epa * 0.25, epa)) %>%
  mutate(epa = if_else(wp >= 0.015 & wp <= 0.985, epa, epa * 0.12)) %>%
  mutate(Pass_Weight = if_else(wp >= 0.015 & wp <= 0.985, Pass_Weight, Pass_Weight * 0.12),
         Rush_Weight = if_else(wp >= 0.015 & wp <= 0.985, Rush_Weight, Rush_Weight * 0.12),
         Receiving_Weight = if_else(wp >= 0.015 & wp <= 0.985, Receiving_Weight, Receiving_Weight * 0.12)) %>%
  mutate(Total_Weight = case_when(!is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    (Pass_Weight - Receiving_Weight) * ((1 - 0.05) ^ (17 - week)),
                                  !is.na(Pass_Weight) & is.na(Receiving_Weight) ~ 
                                    Pass_Weight * ((1 - 0.05) ^ (17 - week)),
                                  is.na(Pass_Weight) & !is.na(Receiving_Weight) ~ 
                                    Receiving_Weight * ((1 - 0.05) ^ (17 - week)),
                                  TRUE ~ 0)) %>%
  mutate(Total_Weight = if_else(week == 17, Total_Weight * 0.25, Total_Weight)) 

mixed_pbp_rushing <- mixed_pbp_rushing %>%
  make_rush_d_mutations()

mixed_model_rushing <- mixed_pbp_rushing %>%
  select(Rusher, yardline_100, yardline_squared, yards_to_go_square, yards_times_down, vegas_wp_squared, log_vegas_wp,
         drive_play_count, total_line, shotgun, qtr,
         Rush_Defense_ARI, Rush_Defense_ATL, Rush_Defense_BAL, Rush_Defense_BUF,
         Rush_Defense_CAR, Rush_Defense_CHI, Rush_Defense_CIN, Rush_Defense_CLE, Rush_Defense_DAL, Rush_Defense_DEN,
         Rush_Defense_DET, Rush_Defense_GB, Rush_Defense_HOU, Rush_Defense_IND, Rush_Defense_JAX, Rush_Defense_KC,
         Rush_Defense_LA, Rush_Defense_LAC, Rush_Defense_LV, Rush_Defense_MIA, Rush_Defense_MIN, Rush_Defense_NE,
         Rush_Defense_NO, Rush_Defense_NYG, Rush_Defense_NYJ, Rush_Defense_PHI, Rush_Defense_PIT, Rush_Defense_SEA,
         Rush_Defense_SF, Rush_Defense_TB, Rush_Defense_TEN, Rush_Defense_WAS) %>%
  mutate(across(2:6, function(x) round((x - mean(x)) / sd(x), 4)))

epa_mixed <- mixed_pbp_rushing %>%
  select(epa)

epa_vector <- as.vector(epa_mixed)
epa_vector[is.na(epa_vector)] <- 0

rusher_vector <- mixed_model_rushing %>%
  select(Rusher)

model_vector <- as.vector(mixed_model_rushing) %>%
  select(-Rusher)
model_vector[is.na(model_vector)] <- 0

epa_final = as.matrix(as.data.frame(lapply(epa_vector, as.numeric)))
model_final = as.matrix(as.data.frame(lapply(model_vector, as.numeric)))
model_final <- cbind(model_final, rusher_vector) %>%
  cbind(epa_final)

mixed_rushing <- lmer(epa ~
                          yards_to_go_square +
                          yards_times_down + 
                          yardline_squared +
                          vegas_wp_squared +
                          log_vegas_wp +
                          shotgun +
                          Rush_Defense_ARI + Rush_Defense_ATL + Rush_Defense_BAL + Rush_Defense_BUF +
                          Rush_Defense_CAR + Rush_Defense_CHI + Rush_Defense_CIN + Rush_Defense_CLE + Rush_Defense_DAL + Rush_Defense_DEN +
                          Rush_Defense_DET + Rush_Defense_GB + Rush_Defense_HOU + Rush_Defense_IND + Rush_Defense_JAX + Rush_Defense_KC +
                          Rush_Defense_LA + Rush_Defense_LAC + Rush_Defense_LV + Rush_Defense_MIA + Rush_Defense_MIN + Rush_Defense_NE +
                          Rush_Defense_NO + Rush_Defense_NYG + Rush_Defense_NYJ + Rush_Defense_PHI + Rush_Defense_PIT + Rush_Defense_SEA +
                          Rush_Defense_SF + Rush_Defense_TB + Rush_Defense_TEN + Rush_Defense_WAS +
                          (1|Rusher),
                        data = model_final)


