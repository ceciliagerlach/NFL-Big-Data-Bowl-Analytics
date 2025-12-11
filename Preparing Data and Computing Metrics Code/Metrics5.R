#read in data sets
setwd("~/Downloads/NFL Big Data Bowl/Input Data ")
library(tidyverse)
inputs <- read.csv("input_2023_w05.csv")
supplementary <- read.csv("supplementary_data.csv")

#filter data set to clean up variables
inputs1 <- inputs %>%
  filter(player_role %in% c("Targeted Receiver", "Defensive Coverage")) %>%
  rename("frame_id_before" = "frame_id")

#filter to only include last pre-throw frame
inputs2 <- inputs1 %>%
  group_by(game_id, play_id, nfl_id) %>%
  arrange(frame_id_before) %>%
  slice_tail(n=1) %>%
  ungroup()

#only include week 5 supplementary data and zone coverage
supplementary1 <- supplementary %>%
  filter(week == 5) %>%
  filter(team_coverage_man_zone == "ZONE_COVERAGE")

#merge input and supplementary data
combined <- merge(inputs2, supplementary1, by=c("game_id", "play_id"))

#group plays/games and filter for plays with at least one targeted receiver
ds1 <- combined %>%
  group_by(game_id, play_id) %>%
  filter(any(player_role == "Targeted Receiver"))

#function to calculation time to ball
calc_time_to_ball <- function(x, y, ball_land_x, ball_land_y, s, a, dir) {
  dir_rad = dir * (pi/180) #convert degrees to radians
  distance_to_ball = sqrt( (ball_land_x - x)^2 + (ball_land_y - y)^2 ) #compute distance to ball
  s_eff = ( (ball_land_x - x)*s*sin(dir_rad) + (ball_land_y - y)*s*cos(dir_rad) ) / distance_to_ball #effective speed towards ball
  a_eff = ( (ball_land_x - x)*a*sin(dir_rad) + (ball_land_y - y)*a*cos(dir_rad) ) / distance_to_ball #effective acceleration towards ball
  #adjust to assume player will turn towards ball if moving away
  s_eff_adj <- ifelse(s_eff <= 0, 0.3 * abs(s), s_eff)
  discr <- s_eff_adj^2 + 2*a_eff*distance_to_ball #discriminant calculation
  #check moving towards ball & avoid negative discriminant
  t <- ifelse(discr < 0 | abs(a_eff) < 1e-6,
              distance_to_ball / pmax(s_eff_adj, 0.1),
              (-s_eff + sqrt(pmax(discr, 0))) / a_eff)
  t <- ifelse(t < 0 | is.infinite(t) | is.nan(t), NA_real_, t)
  return(t)
}

ds2 <- ds1 %>%
  #add variable with time to ball for every player
  mutate(time_to_ball = calc_time_to_ball(x, y, ball_land_x, ball_land_y, s, a, dir),
         time_receiver = if_else(player_role == "Targeted Receiver", time_to_ball, NA_real_),
         time_defender = if_else(player_role == "Defensive Coverage", time_to_ball, NA_real_)) %>%
  #add variable to calculate t_diff
  mutate(max_time_receiver = if(all(is.na(time_receiver))) NA_real_ else max(time_receiver, na.rm=TRUE),
         t_diff = ifelse(time_defender >= 0, abs(max_time_receiver - time_defender), NA_real_)) %>%
  #add boolean variable if defender collides with receiver
  mutate(is_collide = (player_role == "Defensive Coverage" & t_diff <= 0.2) |
           (player_role == "Targeted Receiver" & any(player_role == "Defensive Coverage" & t_diff <= 0.2, na.rm = TRUE))) %>%
  #add variable to sum players arriving to ball within time constraint
  mutate(num_collision = sum(is_collide, na.rm=TRUE))

#function to calculate kinetic energy 
calc_rel_kinetic_energy <- function(x, y, ball_land_x, ball_land_y, s, dir, weight, t) {
  dir_rad = dir * (pi/180) #convert degrees to radians
  distance_to_ball = sqrt( (ball_land_x - x)^2 + (ball_land_y - y)^2 ) #compute distance to ball
  s_eff = ( (ball_land_x - x)*s*sin(dir_rad) + (ball_land_y - y)*s*cos(dir_rad) ) / distance_to_ball #effective speed towards ball
  ke = 0.5 * weight * s_eff^2 #kinetic energy
  weighted_ke = ifelse(is.na(t), ke, ke * exp(-3.47 * t)) #weight energy by time difference
  return(weighted_ke)
}

#function to calculate risk score
calc_coll_risk_score <- function(is_collide, ke, num_collision) {
  ke_coll = sum(ke[is_collide], na.rm=TRUE) #sum kinetic energy for colliding players
  K = max(3, num_collision)
  K = min(K, sum(!is.na(ke)))
  ke_top = sum(sort(ke, decreasing = TRUE)[seq_len(K)], na.rm=TRUE) #top K kinetic energies
  M = min(1, 0.5 + 0.5*(num_collision/K)) #cluster multiplier
  if(ke_top > 0) {
    risk_pct <- (ke_coll / ke_top) * M * 100
  } else {risk_pct <- 0} #avoid division by 0
  return(risk_pct)
}

#add variable to calculate collision risk
ds3 <- ds2 %>%
  mutate(ke = calc_rel_kinetic_energy(x, y, ball_land_x, ball_land_y, s, dir, player_weight, t_diff)) %>%
  mutate(collision_risk_score = calc_coll_risk_score(is_collide, ke, num_collision))

#save smaller data set for analysis
ds4 <- ds3 %>%
  filter(collision_risk_score > 0 & player_role == "Targeted Receiver") %>%
  distinct(game_id, play_id, .keep_all = TRUE) %>%
  ungroup() %>%
  select(week, game_id, play_id, player_name, possession_team, defensive_team, pass_result, pass_length, route_of_targeted_receiver, game_clock, down, num_collision, collision_risk_score, ball_land_x, ball_land_y)

write_csv(ds4, "~/Downloads/NFL Big Data Bowl/result_w05.csv")
