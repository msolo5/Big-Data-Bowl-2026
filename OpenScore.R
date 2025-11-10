library(tidyverse)
library(ggimage)
library(scales)
library(nflfastR)
library(pacman)
library(rsample)
library(pROC)
library(xgboost)
library(Matrix)
library(dplyr)
library(purrr)

# Load individual input csv's
input_2023_w01 <- read_csv("train/input_2023_w01.csv")
input_2023_w02 <- read_csv("train/input_2023_w02.csv")
input_2023_w03 <- read_csv("train/input_2023_w03.csv")
input_2023_w04 <- read_csv("train/input_2023_w04.csv")
input_2023_w05 <- read_csv("train/input_2023_w05.csv")
input_2023_w06 <- read_csv("train/input_2023_w06.csv")
input_2023_w07 <- read_csv("train/input_2023_w07.csv")
input_2023_w08 <- read_csv("train/input_2023_w08.csv")
input_2023_w09 <- read_csv("train/input_2023_w09.csv")
input_2023_w10 <- read_csv("train/input_2023_w10.csv")
input_2023_w11 <- read_csv("train/input_2023_w11.csv")
input_2023_w12 <- read_csv("train/input_2023_w12.csv")
input_2023_w13 <- read_csv("train/input_2023_w13.csv")
input_2023_w10 <- read_csv("train/input_2023_w10.csv")
input_2023_w11 <- read_csv("train/input_2023_w11.csv")
input_2023_w12 <- read_csv("train/input_2023_w12.csv")
input_2023_w13 <- read_csv("train/input_2023_w13.csv")
input_2023_w14 <- read_csv("train/input_2023_w14.csv")
input_2023_w15 <- read_csv("train/input_2023_w15.csv")
input_2023_w16 <- read_csv("train/input_2023_w16.csv")
input_2023_w17 <- read_csv("train/input_2023_w17.csv")
input_2023_w18 <- read_csv("train/input_2023_w18.csv")

inputs_all <- bind_rows(
  input_2023_w01, input_2023_w02, input_2023_w03, input_2023_w04, input_2023_w05,
  input_2023_w06, input_2023_w07, input_2023_w08, input_2023_w09, input_2023_w10,
  input_2023_w11, input_2023_w12, input_2023_w13, input_2023_w14, input_2023_w15,
  input_2023_w16, input_2023_w17, input_2023_w18
)

# Load individual output csv's
output_2023_w01 <- read_csv("train/output_2023_w01.csv")
output_2023_w02 <- read_csv("train/output_2023_w02.csv")
output_2023_w03 <- read_csv("train/output_2023_w03.csv")
output_2023_w04 <- read_csv("train/output_2023_w04.csv")
output_2023_w05 <- read_csv("train/output_2023_w05.csv")
output_2023_w06 <- read_csv("train/output_2023_w06.csv")
output_2023_w07 <- read_csv("train/output_2023_w07.csv")
output_2023_w08 <- read_csv("train/output_2023_w08.csv")
output_2023_w09 <- read_csv("train/output_2023_w09.csv")
output_2023_w10 <- read_csv("train/output_2023_w10.csv")
output_2023_w11 <- read_csv("train/output_2023_w11.csv")
output_2023_w12 <- read_csv("train/output_2023_w12.csv")
output_2023_w13 <- read_csv("train/output_2023_w13.csv")
output_2023_w14 <- read_csv("train/output_2023_w14.csv")
output_2023_w15 <- read_csv("train/output_2023_w15.csv")
output_2023_w16 <- read_csv("train/output_2023_w16.csv")
output_2023_w17 <- read_csv("train/output_2023_w17.csv")
output_2023_w18 <- read_csv("train/output_2023_w18.csv")

outputs_all <- bind_rows(
  output_2023_w01, output_2023_w02, output_2023_w03, output_2023_w04, output_2023_w05,
  output_2023_w06, output_2023_w07, output_2023_w08, output_2023_w09, output_2023_w10,
  output_2023_w11, output_2023_w12, output_2023_w13, output_2023_w14, output_2023_w15,
  output_2023_w16, output_2023_w17, output_2023_w18
)


# Load supplementary data
supp_data <- read_csv("C:/Users/tanne/OneDrive/Documents/Developer/R/BigDataBowl2026/114239_nfl_competition_files_published_analytics_final/supplementary_data.csv")

#Calculate open pct
receiver_open_score <- inputs_all |>
  filter(player_side == "Offense", player_role == "Targeted Receiver") |>
  inner_join(
    inputs_all |> 
      filter(player_side == "Defense") |>
      select(game_id, play_id, frame_id, def_id = nfl_id, def_x = x, def_y = y),
    by = c("game_id", "play_id", "frame_id")
  ) |>
  mutate(distance = sqrt((x - def_x)^2 + (y - def_y)^2)) |>
  group_by(game_id, play_id, frame_id, nfl_id) |>
  summarise(min_distance = min(distance, na.rm = TRUE), .groups = "drop_last") |>
  mutate(is_open = min_distance > 2) |>
  summarise(open_pct = mean(is_open, na.rm = TRUE), .groups = "drop") 


# --- Pre-throw: last frame before pass thrown ---
pre_throw <- inputs_all |>
  group_by(game_id, play_id, nfl_id) |>
  filter(frame_id == max(frame_id)) |>  # last frame before pass
  rename(before_pass_x = x, before_pass_y = y)

# --- Post-throw: final frame after ball lands ---
post_throw <- outputs_all |>
  group_by(game_id, play_id, nfl_id) |>
  filter(frame_id == max(frame_id)) |>
  rename(after_pass_x = x, after_pass_y = y)

targeted_receivers <- pre_throw |>
  filter(player_role == "Targeted Receiver")

defenders <- pre_throw |>
  filter(player_side == "Defense") |>
  select(game_id, play_id, frame_id, defender_id = nfl_id, def_player_position = player_position,
         def_before_pass_x = before_pass_x, def_before_pass_y = before_pass_y, 
         def_s = s, def_a = a, def_dir = dir, def_o = o, def_name = player_name, ball_land_x, ball_land_y)

defender_distances <- defenders |>
  inner_join(
    targeted_receivers |>
      select(game_id, play_id, frame_id, off_player_position = player_position,
             rec_id = nfl_id, rec_before_pass_x = before_pass_x, rec_before_pass_y = before_pass_y, 
             rec_s = s, rec_a = a, rec_dir = dir, rec_o = o, rec_name = player_name),
    by = c("game_id", "play_id", "frame_id")
  ) |>
  mutate(distance_to_receiver = sqrt((def_before_pass_x - rec_before_pass_x)^2 + (def_before_pass_y - rec_before_pass_y)^2))

closest_defender_each_frame <- defender_distances |>
  group_by(game_id, play_id, frame_id) |>
  slice_min(distance_to_receiver, n = 1) |>
  ungroup()

# --- Merge defender post-throw positions ---
closest_defender_post_throw <- closest_defender_each_frame |>
  left_join(
    post_throw |>
      select(game_id, play_id, nfl_id, after_pass_x, after_pass_y),
    by = c("game_id", "play_id", "defender_id" = "nfl_id")
  ) |>
  rename(
    def_after_pass_x = after_pass_x,
    def_after_pass_y = after_pass_y
  ) |>
  mutate(ball_land_x = first(ball_land_x),
         ball_land_y = first(ball_land_y),
         .by = c(game_id, play_id))

# --- Merge receiver post-throw positions ---
closest_defender_post_throw <- closest_defender_post_throw |>
  left_join(
    post_throw |>
      select(game_id, play_id, nfl_id, after_pass_x, after_pass_y),
    by = c("game_id", "play_id", "rec_id" = "nfl_id")
  ) |>
  rename(
    rec_after_pass_x = after_pass_x,
    rec_after_pass_y = after_pass_y
  )

closest_defender_post_throw <- closest_defender_post_throw |>
  mutate(
    distance_def_to_ball = sqrt((def_after_pass_x - ball_land_x)^2 + (def_after_pass_y - ball_land_y)^2),
    distance_rec_to_ball = sqrt((rec_after_pass_x - ball_land_x)^2 + (rec_after_pass_y - ball_land_y)^2),
    defender_within_2yds = if_else(distance_def_to_ball <= 2, 1, 0)
  ) |> 
  filter(!is.na(def_after_pass_x)) |> 
  filter(distance_rec_to_ball <= 5)

model_data <- closest_defender_post_throw |>
  left_join(supp_data, by = c("game_id", "play_id")) |> 
  filter(def_player_position %in% c("SS", "FS", "S", "CB"))

model_data <- model_data |> 
  left_join(receiver_open_score, by = c("game_id", "play_id"))

# Select features (pre-throw only) and the target
df <- model_data |> 
  # keep only columns we will use (add more pre-throw features if useful)
  select(game_id, play_id, defender_id, def_name,
         # pre-throw defender features
         def_before_pass_x, def_before_pass_y, def_s, def_a, def_dir, def_o,
         # receiver pre-throw features (separation / relative info)
         rec_before_pass_x, rec_before_pass_y, rec_s, rec_a, rec_dir, rec_o,
         distance_to_receiver,
         # play / context features (from supp_data columns already in model_data)
         down, yards_to_go, pass_length, dropback_distance, team_coverage_type,
         defenders_in_the_box, pass_location_type,
         # target
         defender_within_2yds) |> 
  # remove rows with missing target
  filter(!is.na(defender_within_2yds)) |> 
  # convert target to numeric (0/1)
  mutate(defender_within_2yds = as.integer(defender_within_2yds))

set.seed(2025)
plays <- df |> distinct(game_id, play_id)
train_plays <- initial_split(plays, prop = 0.75)
train_ids <- training(train_plays)
test_ids  <- testing(train_plays)

train_df <- df |> inner_join(train_ids, by = c("game_id", "play_id"))
test_df  <- df |> inner_join(test_ids,  by = c("game_id", "play_id"))

# convert categorical to factors
train_glm <- train_df |> 
  mutate(across(c(team_coverage_type, pass_location_type), ~ as.factor(.x)))
test_glm  <- test_df  %>%
  mutate(across(c(team_coverage_type, pass_location_type), ~ as.factor(.x)))

# simple formula (add/remove predictors as desired)
glm_formula <- as.formula(
  "defender_within_2yds ~ def_s + def_a + distance_to_receiver + rec_s + rec_a + down + yards_to_go + defenders_in_the_box + team_coverage_type + pass_length + dropback_distance"
)

glm_model <- glm(glm_formula, data = train_glm, family = binomial())

# predictions
test_glm$pred_prob_glm <- predict(glm_model, newdata = test_glm, type = "response")

# AUC
auc_glm <- roc(test_glm$defender_within_2yds, test_glm$pred_prob_glm)$auc
print(paste0("GLM AUC: ", round(auc_glm, 4)))

summary(glm_model)

exp(coef(glm_model))

df$pred_prob_glm <- predict(glm_model, newdata = df, type = "response")

pobe_summary <- df |>
  group_by(def_name, defender_id) |>
  summarise(
    actual_play_on_ball_rate = mean(defender_within_2yds, na.rm = TRUE),
    expected_play_on_ball_rate = mean(pred_prob_glm, na.rm = TRUE),
    pobe = actual_play_on_ball_rate - expected_play_on_ball_rate,
    n_plays = n()
  ) |>
  arrange(desc(pobe))