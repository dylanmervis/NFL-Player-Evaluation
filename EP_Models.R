library(nflfastR)
library(splitTools)
library(dials)
library(xgboost)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(future)

# some helper files are in these
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

set.seed(01061997)
grid_size <- 40

# from local
pbp_ep_model <- readRDS("/Users/dylanmervis/Desktop/2019 NFL/ep_model_data.rds")


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
    Total_W_Scaled
  )

# idk why this is all necessary for xgb but it is
model_data <- model_data %>%
  mutate(
    label = as.numeric(label),
    label = label - 1)


seasons <- unique(model_data$season)

## Params Full
# nrounds <- 309
# params <-
  # list(
    # booster = "gbtree",
    # objective = "multi:softprob",
    # eval_metric = c("mlogloss"),
    # num_class = 7,
    # eta = 0.064,
    # gamma = 0.0000000065,
    # subsample = 0.34,
    # colsample_bytree = 0.62,
    # max_depth = 6,
    # min_child_weight = 24
 #  )

nrounds <- 525
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 7,
    eta = 0.025,
    gamma = 1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1
  )

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
    matrix(predict(ep_model, as.matrix(test_data %>% dplyr::select(-label, -Total_W_Scaled))), ncol = 7, byrow = TRUE)
  )
  colnames(preds) <- c(
    "Touchdown", "Opp_Touchdown", "Field_Goal", "Opp_Field_Goal",
    "Safety", "Opp_Safety", "No_Score"
  )
  
  
  cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
  return(cv_data)
})

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

