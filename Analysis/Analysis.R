#Read in data sets
setwd("~/Downloads/NFL Big Data Bowl/Result Data")
library(tidyverse)
library(nnet)
library(dplyr)
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

#Save data set for injury analysis
write_csv(combined, "~/Downloads/NFL Big Data Bowl/Injury Data/results.csv")

#Create risk categories
combined1 <- combined %>%
  mutate(risk_category = case_when(
    collision_risk_score > 0 & collision_risk_score <= 79.76602 ~ "Low",
    collision_risk_score > 79.76602 & collision_risk_score <= 82.88703 ~ "Moderate",
    collision_risk_score > 82.88703 & collision_risk_score <= 83.32385 ~ "High",
    collision_risk_score > 83.32385 & collision_risk_score <= 101 ~ "Severe"))

#Summarize high-severe risk plays for each team and total in the season
combined1 %>%
  group_by(possession_team) %>%
  filter(risk_category == "High" | risk_category == "Severe") %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 32)

combined1 %>%
  filter(risk_category == "High" | risk_category == "Severe") %>%
  summarize(n = n())

#Box plot of proportion of high-severe plays for each team in the season
combined1 %>%
  group_by(possession_team) %>%
  summarise(high_risk_rate = mean(risk_category %in% c("High", "Severe"))) %>%
  ggplot(aes(x = reorder(possession_team, high_risk_rate), y = high_risk_rate)) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(title = "Proportion of High–Severe Collision Risk Plays by Team",
       x = "Team", y = "High–Severe Risk Rate")

#Summarize risk plays for each receiver in the season
combined1 %>%
  filter(risk_category == "High" | risk_category == "Severe") %>%
  group_by(player_name) %>%
  summarize(n = n()) %>%
  filter(n >= 2) %>%
  arrange(desc(n))

#Summarize risk plays by week
combined1 %>%
  group_by(week) %>%
  summarize(n = n()) %>% 
  arrange(desc(n))

#Turn pass results and routes into numerical values
#0 = completed, 1 = negative outcome (incomplete, interception)
#0 = go/post/corner (deep), 1 = slant/hitch/flat (short), 2 = in/out/cross (inter), 3 = angle/wheel/screen(rare)
combined2 <- combined1 %>%
  mutate(pass_result = recode(pass_result,
                              "C" = 0,
                              "I" = 1,
                              "IN" = 1),
         route_of_targeted_receiver = recode(route_of_targeted_receiver,
                                             "GO" = 0,
                                             "POST" = 0,
                                             "CORNER" = 0,
                                             "SLANT" = 1,
                                             "HITCH" = 1,
                                             "FLAT" = 1,
                                             "IN" = 2,
                                             "OUT" = 2,
                                             "CROSS" = 2,
                                             "ANGLE" = 3,
                                             "SCREEN" = 3,
                                             "WHEEL" = 3))

#Logistic regression for association between pass result and collision risk
model <- glm(pass_result ~ collision_risk_score, data=combined2, family = binomial)
summary(model)

#Logistic regression for association between route of receiver and collision risk
model <- multinom(route_of_targeted_receiver ~ collision_risk_score, data=combined2)
summary(model)
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Logistic regression for association between down and collision risk
combined2 %>%
  count(down)
model <- multinom(down ~ collision_risk_score, data=combined2)
summary(model)
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Correlation between pass length and collision risk score
with(combined2, plot(pass_length, collision_risk_score))
with(combined2, cor.test(pass_length, collision_risk_score))







