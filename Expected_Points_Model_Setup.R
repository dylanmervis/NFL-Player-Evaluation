## Load Packages
library(tidyverse)
library(jsonlite)
library(DBI)
library(RSQLite)
library(lme4)
library(tidyverse)
library(vip)
library(tidymodels)
library(workflows)
library(dials)
library(tune)
library(DT)
library(ggrepel)
library(future)
library(magrittr)
library(future)
library(dplyr)
library(DBI)
library(RSQLite)
library(nflfastR)
library(readr)
library(xml2)
library(XML)
library(rvest)
library(purrr)
library(na.tools)
library(stringr)
library(ggplot2)
library(ggimage)
library(lubridate)
library(nflreadr)

## Load in full data set
pbp_fastr <- nflreadr::load_pbp(c(1999:2021)) %>%
  clean_pbp()

pbp_db <- DBI::dbConnect(RSQLite::SQLite(), "/Users/dylanmervis/Google Drive/pbp_db")
pbp_db
DBI::dbWriteTable(pbp_db, "nflfastr_pbp", pbp_fastr, overwrite = TRUE)

pbp_fastr <- tbl(dbConnect(SQLite(), "/Users/dylanmervis/Google Drive/pbp_db"),
                 "nflfastr_pbp")

pbp_all <- as.data.frame(pbp_fastr)

## READ IN SEPARATE ROSTER DATA & CREATE NAME ABBREVIATION
library(lubridate)
read_roster_csv <- function(year){
  read_csv(url(glue::glue('https://raw.githubusercontent.com/mrcaseb/nflfastR-roster/master/data/seasons/roster_{year}.csv')))
}

all_roster <- 1999:2021 %>% 
  purrr::map_dfr(read_roster_csv)


all_roster <- all_roster %>% 
  mutate(Abbr = paste(substr(full_name, start = 1, stop = 1), last_name, sep = ".")) %>%
  mutate(Month = 1,
         Day = 1) %>%
  mutate(Age_Date = make_date(season, Day, Month)) %>%
  mutate(Age = as.period(interval(start = birth_date, end = Age_Date))$year) %>%
  dplyr::select(Season = season, Full_Name = full_name, Abbr, Position = position, Team = team, 
                Depth_Chart = depth_chart_position, Jersey = jersey_number, Birth_Date = birth_date,
                Age, College = college, Years_Exp = years_exp, gsis_id, pff_id, sportradar_id, headshot_url) %>%
  mutate(Team = case_when(Team == "STL" ~ "LA",
                          Team == "OAK" ~ "LV",
                          Team == "SD" ~ "LAC",
                          TRUE ~ Team)) %>%
  write_rds("/Users/dylanmervis/Google Drive/1999-2021_roster_raw.rds")


## Helper source
source("/Users/dylanmervis/Desktop/2019 NFL/R Scripts/Additional_Functions.R")


## Find next score in the half
find_game_next_score_half <- function(pbp_all) {
  
  # Which rows are the scoring plays:
  score_plays <- which(pbp_all$sp == 1 & pbp_all$play_type != "no_play")
  
  # Define a helper function that takes in the current play index,
  # a vector of the scoring play indices, play-by-play data,
  # and returns the score type and drive number for the next score:
  find_next_score <- function(play_i, score_plays_i,pbp_df) {
    
    # Find the next score index for the current play
    # based on being the first next score index:
    next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]
    
    # If next_score_i is NA (no more scores after current play)
    # or if the next score is in another half,
    # then return No_Score and the current drive number
    if (is.na(next_score_i) |
        (pbp_df$qtr[play_i] %in% c(1, 2) & pbp_df$qtr[next_score_i] %in% c(3, 4, 5)) |
        (pbp_df$qtr[play_i] %in% c(3, 4) & pbp_df$qtr[next_score_i] == 5)) {
      
      score_type <- "No_Score"
      
      # Make it the current play index
      score_drive <- pbp_df$drive[play_i]
      
      # Else return the observed next score type and drive number:
    } else {
      
      # Store the score_drive number
      score_drive <- pbp_df$drive[next_score_i]
      
      # Then check the play types to decide what to return
      # based on several types of cases for the next score:
      
      # 1: Return TD
      if (pbp_df$touchdown[next_score_i] == 1 & (pbp_df$td_team[next_score_i] != pbp_df$posteam[next_score_i])) {
        
        # For return touchdowns the current posteam would not have
        # possession at the time of return, so it's flipped:
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Touchdown"
          
        } else {
          
          score_type <- "Touchdown"
          
        }
      } else if (identical(pbp_df$field_goal_result[next_score_i], "made")) {
        
        # 2: Field Goal
        # Current posteam made FG
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Field_Goal"
          
          # Opponent made FG
        } else {
          
          score_type <- "Opp_Field_Goal"
          
        }
        
        # 3: Touchdown (returns already counted for)
      } else if (pbp_df$touchdown[next_score_i] == 1) {
        
        # Current posteam TD
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Touchdown"
          
          # Opponent TD
        } else {
          
          score_type <- "Opp_Touchdown"
          
        }
        # 4: Safety (similar to returns)
      } else if (pbp_df$safety[next_score_i] == 1) {
        
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Safety"
          
        } else {
          
          score_type <- "Safety"
          
        }
        # 5: Extra Points
      } else if (identical(pbp_df$extra_point_result[next_score_i], "good")) {
        
        # Current posteam Extra Point
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Extra_Point"
          
          # Opponent Extra Point
        } else {
          
          score_type <- "Opp_Extra_Point"
          
        }
        # 6: Two Point Conversions
      } else if (identical(pbp_df$two_point_conv_result[next_score_i], "success")) {
        
        # Current posteam Two Point Conversion
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Two_Point_Conversion"
          
          # Opponent Two Point Conversion
        } else {
          
          score_type <- "Opp_Two_Point_Conversion"
          
        }
        
        # 7: Defensive Two Point (like returns)
      } else if (identical(pbp_df$defensive_two_point_conv[next_score_i], 1)) {
        
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Defensive_Two_Point"
          
        } else {
          
          score_type <- "Defensive_Two_Point"
          
        }
        
        # 8: Errors of some sort so return NA (but shouldn't take place)
      } else {
        
        score_type <- NA
        
      }
    }
    
    return(data.frame(Next_Score_Half = score_type,
                      Drive_Score_Half = score_drive))
  }
  
  # Using lapply and then bind_rows is much faster than
  # using map_dfr() here:
  lapply(c(1:nrow(pbp_all)), find_next_score,
         score_plays_i = score_plays, pbp_df = pbp_all) %>%
    bind_rows() %>%
    return
}

################################################################################
# DATA PREP
################################################################################


pbp_ep_model <- pbp_all %>%
  mutate(
    Winner = if_else(home_score > away_score, home_team,
                     if_else(home_score < away_score, away_team, "TIE"))
  )

#get next score half using the provided function
pbp_next_score_half <- map_dfr(unique(pbp_all$game_id),
                               function(x) {
                                 pbp_all %>%
                                   filter(game_id == x) %>%
                                   find_game_next_score_half()
                               })

#bind to original df
pbp_ep_model <- bind_cols(pbp_ep_model, pbp_next_score_half)

#for estimating the models, apply some filters
pbp_ep_model <- pbp_ep_model %>%
  filter(Next_Score_Half %in% c("Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                                "Field_Goal", "No_Score", "Safety", "Touchdown") &
           play_type %in% c("field_goal", "no_play", "pass", "punt", "run",
                            "qb_spike") & is.na(two_point_conv_result) & is.na(extra_point_result) &
           !is.na(down) & !is.na(game_seconds_remaining)) %>%
  #to keep file size manageable
  select(
    game_id,
    play_id,
    Next_Score_Half,
    Drive_Score_Half,
    play_type,
    game_seconds_remaining,
    half_seconds_remaining,
    yardline_100,
    roof,
    posteam,
    defteam,
    home_team,
    ydstogo,
    season,
    qtr,
    down,
    week,
    drive,
    ep,
    score_differential,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    desc,
    receiver_player_name,
    pass_location,
    air_yards,
    yards_after_catch,
    complete_pass, incomplete_pass, interception,
    qb_hit,
    extra_point_result,
    field_goal_result,
    sp,
    Winner,
    spread_line,
    total_line, shotgun, qtr, ydstogo,
    side_of_field, vegas_wp,
  )

#for doing calibation etc
saveRDS(pbp_ep_model, '/Users/dylanmervis/Desktop/2019 NFL/ep_model_data.rds')


## PREP FOR XGBOOST

# save some space
rm(pbp_all) 
rm(pbp_next_score_half)

# some helper files are in these
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

set.seed(01061997)

# from local
pbp_ep_model <- readRDS("/Users/dylanmervis/Desktop/2019 NFL/ep_model_data.rds")

# sportradar data - do not join yet
sportradar_data <- purrr::map_df(2017 : 2020, function(x) {
  readRDS(
    
    # local
    glue::glue("/Users/dylanmervis/Google Drive/pbp_{x}.rds")
    
  )
}) 


## model data nflfastR
model_data <- pbp_ep_model %>%
  # in 'R/helper_add_nflscrapr_mutations.R'
  make_model_mutations() %>%
  filter(season > 2013) %>%
  filter(!is.na(drive)) %>%
  mutate(
    label = case_when(
      Next_Score_Half == "Touchdown" ~ 0,
      Next_Score_Half == "Opp_Touchdown" ~ 1,
      Next_Score_Half == "Field_Goal" ~ 2,
      Next_Score_Half == "Opp_Field_Goal" ~ 3,
      Next_Score_Half == "Safety" ~ 4,
      Next_Score_Half == "Opp_Safety" ~ 5,
      Next_Score_Half == "No_Score" ~ 6
    ),
    label = as.factor(label),
    # use nflscrapR weights
    Drive_Score_Dist = Drive_Score_Half - drive,
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    ScoreDiff_W = (max(abs(score_differential), na.rm = TRUE) - abs(score_differential)) /
      (max(abs(score_differential), na.rm = TRUE) - min(abs(score_differential), na.rm = TRUE)),
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W, na.rm = TRUE)) /
      (max(Total_W, na.rm = TRUE) - min(Total_W, na.rm = TRUE)),
    yards_to_go_square = ydstogo * ydstogo,
    yards_times_down = ydstogo * down,
    log_ydstogo = log(ydstogo),
    yardline_squared = yardline_100^2,
    vegas_wp_squared = vegas_wp * vegas_wp,
    log_vegas_wp = log(vegas_wp),
    side_of_field_indicator = if_else(posteam == side_of_field, 1, 0),
    in_red_zone = if_else(yardline_100 <= 20, 1, 0),
    in_fg_range = if_else(yardline_100 <= 35, 1, 0),
  ) %>%
  filter(
    !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
    !is.na(yardline_100), !is.na(total_line), !is.na(drive_play_count), !is.na(yards_to_go_square), !is.na(yards_times_down),
    !is.na(yardline_squared), !is.na(vegas_wp_squared), !is.na(log_vegas_wp)
  ) %>%
  dplyr::select(
    label,
    play_id,
    game_id,
    season,
    half_seconds_remaining,
    yardline_100,
    home,
    retractable,
    dome,
    outdoors,
    ydstogo,
    era3, era4,
    down1, down1, down3, down4,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    Total_W_Scaled, shotgun, qtr,
    total_line, yards_times_down, yardline_squared,
    vegas_wp_squared, log_vegas_wp
  )

## Join both data sets
pbp_post_16 <- model_data %>%
  filter(season > 2016) %>%
  left_join(sportradar_data, by = c("play_id", "game_id")) %>%
  select(-play_id, -sr_desc, -sequence, -incompletion_type, -on_target_throw,
         -blitz, -hurry, -dropped, -catchable, -pocket_location,
         -players_rushed, -running_lane, -pass_route) %>%
  filter(!is.na(play_action))


rm(pbp_ep_model)

## TIDY MODELS

cores <- parallel::detectCores(logical = FALSE)
cores
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

dat_split <- initial_split(model_data, prop = 0.75)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

ep_folds <- vfold_cv(dat_train, strata = game_id)

ep_recipe <- recipe(label ~ half_seconds_remaining + yardline_100 + home + retractable +
                      dome + outdoors + ydstogo + era0 + era1 + era2 + era3 + era4 +
                      down1 + down2 + down3 + down4 + posteam_timeouts_remaining+ 
                      defteam_timeouts_remaining + Total_W_Scaled + shotgun + qtr +
                      total_line + drive_play_count + yards_times_down + yardline_squared +
                      vegas_wp_squared + log_vegas_wp + in_fg_range,
                    data = dat_train) %>%
  update_role(Total_W_Scaled, new_role = "case_weight")

ep_model <-
  boost_tree(
    mtry = tune(),
    trees = 2000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = 40
  ) %>%
  set_engine("xgboost", objective = "multi:softprob", num_class = 7,
             print_every_n = 50, eval_metric = "mlogloss") %>%
  set_mode("classification")

ep_workflow <- workflow() %>%
  add_recipe(ep_recipe) %>%
  add_model(ep_model)

xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), dat_train),
  min_n(),
  tree_depth(),
  learn_rate(range = c(-1.5, -0.5), trans = log10_trans()),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 40
)

library(finetune)
xgb_res <- tune_race_anova(
  ep_workflow,
  resamples = ep_folds,
  grid = xgb_grid
)


## XGBOOST
library(future)
library(parallel)
library(parallelly)
library(doParallel)

cores <- parallel::detectCores(logical = FALSE)
cores
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

dat_split <- initial_split(model_data, strata = season, prop = 0.75)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)


ep_folds <- splitTools::create_folds(
  y = dat_train$game_id,
  k = 5,
  type = "grouped",
  invert = TRUE
)

train_labels <- dat_train %>%
  dplyr::select(label)
train_labels <- train_labels %>%
  mutate(
    label = as.numeric(as.factor(label)),
    label = label - 1
  )
weight_labels <- dat_train %>%
  dplyr::select(Total_W_Scaled)

dat_train <- dat_train %>%
  dplyr::select(-label,-Total_W_Scaled, -season, -game_id, -play_id)


## Create grid search
grid <- dials::grid_latin_hypercube(
  dials::finalize(dials::mtry(), dat_train),
  dials::min_n(),
  dials::tree_depth(),
  dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = 50
) %>%
  dplyr::mutate(
    mtry = mtry / length(dat_train)
  ) %>%
  dplyr::rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )

get_row <- function(row) {
  params <-
    list(
      booster = "gbtree",
      objective = "multi:softprob",
      num_class = length(unique(train_labels$label)),
      eval_metric = c("mlogloss"),
      eta = row$eta,
      gamma = row$gamma,
      subsample = row$subsample,
      colsample_bytree = row$colsample_bytree,
      max_depth = row$max_depth,
      min_child_weight = row$min_child_weight
    )
  
  ep_cv_model <- xgboost::xgb.cv(
    data = as.matrix(dat_train),
    label = train_labels$label,
    weight = weight_labels$Total_W_Scaled,
    params = params,
    nrounds = 4000,
    nfold = 5,
    metrics = list("mlogloss"),
    early_stopping_rounds = 50,
    print_every_n = 50
  )
  
  output <- params
  output$iter <- ep_cv_model$best_iteration
  output$mlogloss <- ep_cv_model$evaluation_log[output$iter]$test_mlogloss_mean
  
  row_result <- bind_rows(output)
  
  return(row_result)
}

results <- purrr::map_df(1:nrow(grid), function(x) {
  get_row(grid %>% dplyr::slice(x))
})

results %>%
  dplyr::select(mlogloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  tidyr::pivot_longer(
    eta:min_child_weight,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mlogloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "mlogloss") +
  theme_minimal()

results %>%
  dplyr::arrange(mlogloss) %>%
  dplyr::select(eta, subsample, colsample_bytree, max_depth, mlogloss, min_child_weight, iter)

best_model <- results %>%
  dplyr::arrange(mlogloss) %>%
  dplyr::slice(1)

## Collect best model parameters
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = length(unique(train_labels$label)),
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight
  )

nrounds <- best_model$iter

model_data <- model_data %>%
  mutate(
    label = as.numeric(label),
    label = label - 1)

seasons <- unique(model_data$season)

## Full Train & Testing
cv_results <- map_dfr(seasons, function(x) {
  test_data <- model_data %>%
    filter(season == x) %>%
    dplyr::select(-season, -game_id, -play_id)
  train_data <- model_data %>%
    filter(season != x) %>%
    dplyr::select(-season, -game_id, -play_id)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% dplyr::select(-label, -Total_W_Scaled)),
                                     label = train_data$label, weight = train_data$Total_W_Scaled
  )
  ep_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds,
                               verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(ep_model, as.matrix(test_data %>% dplyr::select(-label, -Total_W_Scaled))),
           ncol = 7, byrow = TRUE)
  )
  colnames(preds) <- c(
    "Touchdown", "Opp_Touchdown", "Field_Goal", "Opp_Field_Goal",
    "Safety", "Opp_Safety", "No_Score"
  )
  
  
  cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
  return(cv_data)
})


## Feature Importance
ep_model <- xgboost::xgboost(
  params = params,
  data = as.matrix(dat_train),
  label = train_labels$label,
  nrounds = nrounds,
  verbose = 2
)
library(Ckmeans.1d.dp)

importance <- xgboost::xgb.importance(
  feature_names = colnames(ep_model),
  model = ep_model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)


## Predictions
preds <- matrix(stats::predict(
  ep_model,
  # get rid of the things not needed for prediction here
  as.matrix(dat_test %>% select(-label, -game_id, -season, -Total_W_Scaled, -play_id)),
  weights = dat_test$Total_W_Scaled), 
            ncol = 7, byrow = TRUE
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(Touchdown = V1,
                Opp_Touchdown = V2,
                Field_Goal = V3,
                Opp_Field_Goal = V4,
                Safety = V5,
                Opp_Safety = V6,
                No_Score = V7) %>%
  dplyr::bind_cols(dat_test)

# get the BINS for the calibration plot
plot <- cv_results %>%
  dplyr::select(Touchdown, Opp_Touchdown, Field_Goal, Opp_Field_Goal, Safety, Opp_Safety, No_Score, label) %>%
  pivot_longer(-label, names_to = "type", values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  mutate(outcome = case_when(
    label == 0 ~ "Touchdown",
    label == 1 ~ "Opp_Touchdown",
    label == 2 ~ "Field_Goal",
    label == 3 ~ "Opp_Field_Goal",
    label == 4 ~ "Safety",
    label == 5 ~ "Opp_Safety",
    label == 6 ~ "No_Score"
  )) %>%
  group_by(type, bin_pred_prob) %>%
  mutate(correct = if_else(outcome == type, 1, 0)) %>%
  summarize(
    n_plays = n(),
    n_outcome = sum(correct),
    bin_actual_prob = n_outcome / n_plays
  )

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected"),
  next_score_type = factor("No Score (0)")
)
plot %>%
  # about .75M plays in total
  # filter(n_plays >= 50) %>%
  ungroup() %>%
  mutate(
    type = fct_relevel(
      type,
      "Opp_Safety", "Opp_Field_Goal",
      "Opp_Touchdown", "No_Score", "Safety",
      "Field_Goal", "Touchdown"
    ),
    type = fct_recode(type,
                      "-Field Goal (-3)" = "Opp_Field_Goal",
                      "-Safety (-2)" = "Opp_Safety",
                      "-Touchdown (-7)" = "Opp_Touchdown",
                      "Field Goal (3)" = "Field_Goal",
                      "No Score (0)" = "No_Score",
                      "Touchdown (7)" = "Touchdown",
                      "Safety (2)" = "Safety"
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    x = "Estimated next score probability",
    y = "Observed next score probability"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = c(1, .05), legend.justification = c(1, 0)
  ) +
  facet_wrap(~type, ncol = 4)


# calibration error
cv_cal_error <- plot %>%
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(type) %>%
  summarize(
    weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
    n_scoring_event = sum(n_outcome, na.rm = TRUE)
  )

round(with(cv_cal_error, weighted.mean(weight_cal_error, n_scoring_event)), 4)


