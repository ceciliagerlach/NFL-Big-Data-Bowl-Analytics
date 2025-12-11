#Read in data sets
setwd("~/Downloads/NFL Big Data Bowl/Result Data")
library(tidyverse)
library(nnet)
library(dplyr)
library(ggplot2)
w1 <- read.csv("result_w01.csv")
w2 <- read.csv("result_w02.csv")
w3 <- read.csv("result_w03.csv")
w4 <- read.csv("result_w04.csv")
w5 <- read.csv("result_w05.csv")
w6 <- read.csv("result_w06.csv")
w7 <- read.csv("result_w07.csv")
w8 <- read.csv("result_w08.csv")
w9 <- read.csv("result_w09.csv")
w10 <- read.csv("result_w10.csv")
w11 <- read.csv("result_w11.csv")
w12 <- read.csv("result_w12.csv")
w13 <- read.csv("result_w13.csv")
w14 <- read.csv("result_w14.csv")
w15 <- read.csv("result_w15.csv")
w16 <- read.csv("result_w16.csv")
w17 <- read.csv("result_w17.csv")
w18 <- read.csv("result_w18.csv")

#Combine all result data sets
combined <- bind_rows(w1, w2, w3, w4, w5, w6, w7, w8, w9,
                      w10, w11, w12, w13, w14, w15, w16, w17, w18)

#Create risk categories
combined1 <- combined %>%
  mutate(risk_category = case_when(
    collision_risk_score > 0 & collision_risk_score <= 79.76602 ~ "Low",
    collision_risk_score > 79.76602 & collision_risk_score <= 82.88703 ~ "Moderate",
    collision_risk_score > 82.88703 & collision_risk_score <= 83.32385 ~ "High",
    collision_risk_score > 83.32385 & collision_risk_score <= 101 ~ "Severe"))

combined2 <- combined1 %>%
  mutate(risk_category = factor(risk_category, 
                           levels = c("Low","Moderate","High","Severe"), 
                           ordered = TRUE),
    risk_num = as.numeric(risk_category))

risk_colors <- c(
  "Low" = "skyblue",
  "Moderate" = "plum",
  "High" = "pink",
  "Severe" = "red"
)

range(combined2$ball_land_x, na.rm = TRUE)

#Plot of collision points
ggplot() +
  annotate("rect", xmin = -0.82, xmax = 120.1, ymin = -3.44, ymax = 56.42,
           fill = "darkgreen", alpha = 0.3) + #field background
  geom_vline(xintercept = seq(10,110,5), color = "white", linewidth = 0.4) + #yard lines
  geom_vline(xintercept = seq(10,110,10), color = "white", linewidth = 1) + #major yard lines
  geom_segment(data = tibble(x = rep(seq(10,110,1), each = 2),
                             y = rep(c(20, 33), times = 101)),
    aes(x = x, xend = x, y = y, yend = y + 0.5),
    color = "white", size = 0.4) + #hash marks
  geom_point(data = combined2,
             aes(x = ball_land_x, y = ball_land_y, color = risk_category),
             alpha = 0.7, size = 2) + #collision points
  scale_color_manual(values = risk_colors, name = "Collision Risk") +
  scale_x_continuous( #yardline labels
    limits = c(-0.82,120.1),
    breaks = c(0, 10,20,30,40,50,60,70,80,90,100,110, 120),
    labels = c("","G","10","20","30","40","50","40","30","20","10","G","")) +
  coord_fixed() +
  labs(title = "Collision Risk Score by Ball Landing Location",
       x = "Field Position (yards)", y = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )