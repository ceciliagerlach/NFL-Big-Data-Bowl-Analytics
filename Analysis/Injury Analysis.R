#Read in data sets
setwd("~/Downloads/NFL Big Data Bowl/Injury Data")
library(tidyverse)
library(dplyr)
library(nnet)
a1 <- read.csv("49ers_IR.csv")
b1 <- read.csv("Bengals_IR.csv")
b2 <- read.csv("Bills_IR.csv")
b3 <- read.csv("Bears_IR.csv")
b4 <- read.csv("Broncos_IR.csv")
b5 <- read.csv("Browns_IR.csv")
b6 <- read.csv("Buccaneers_IR.csv")
c1 <- read.csv("Cardinals_IR.csv")
c2 <- read.csv("Chargers_IR.csv")
c3 <- read.csv("Chiefs_IR.csv")
c4 <- read.csv("Colts_IR.csv")
c5 <- read.csv("Commanders_IR.csv")
c6 <- read.csv("Cowboys_IR.csv")
d1 <- read.csv("Dolphins_IR.csv")
e1 <- read.csv("Eagles_IR.csv")
f1 <- read.csv("Falcons_IR.csv")
g1 <- read.csv("Giants_IR.csv")
j1 <- read.csv("Jaguars_IR.csv")
j2 <- read.csv("Jets_IR.csv")
l1 <- read.csv("Lions_IR.csv")
p1 <- read.csv("Packers_IR.csv")
p2 <- read.csv("Panthers_IR.csv")
p3 <- read.csv("Patriots_IR.csv")
r1 <- read.csv("Raiders_IR.csv")
r2 <- read.csv("Rams_IR.csv")
r3 <- read.csv("Ravens_IR.csv")
s1 <- read.csv("Saints_IR.csv")
s2 <- read.csv("Seahawks_IR.csv")
s3 <- read.csv("Steelers_IR.csv")
t1 <- read.csv("Texans_IR.csv")
t2 <- read.csv("Titans_IR.csv")
v1 <- read.csv("Vikings_IR.csv")

#Helper function to sum number of weeks each player is on injury report
summarize_ir <- function(df) {
  df %>%
    mutate(across(-Player, ~ ifelse(. != "" & !is.na(.), 1, 0))) %>%
    rowwise() %>%
    mutate(Weeks_on_Injury_Report = sum(c_across(-Player))) %>%
    ungroup() %>%
    select(Player, Weeks_on_Injury_Report)
}

#Calculate summaries for each team
sf_sum  <- summarize_ir(a1)
ben_sum <- summarize_ir(b1)
buf_sum <- summarize_ir(b2)
bea_sum <- summarize_ir(b3)
brc_sum <- summarize_ir(b4)
brn_sum <- summarize_ir(b5)
buc_sum <- summarize_ir(b6)
car_sum <- summarize_ir(c1)
cha_sum <- summarize_ir(c2)
chi_sum <- summarize_ir(c3)
col_sum <- summarize_ir(c4)
com_sum <- summarize_ir(c5)
cow_sum <- summarize_ir(c6)
dol_sum <- summarize_ir(d1)
eag_sum <- summarize_ir(e1)
fal_sum <- summarize_ir(f1)
gia_sum <- summarize_ir(g1)
jag_sum <- summarize_ir(j1)
jet_sum <- summarize_ir(j2)
lio_sum <- summarize_ir(l1)
pac_sum <- summarize_ir(p1)
pan_sum <- summarize_ir(p2)
pat_sum <- summarize_ir(p3)
sai_sum <- summarize_ir(s1)
sea_sum <- summarize_ir(s2)
ste_sum <- summarize_ir(s3)
rai_sum <- summarize_ir(r1)
ram_sum <- summarize_ir(r2)
rav_sum <- summarize_ir(r3)
tex_sum <- summarize_ir(t1)
tit_sum <- summarize_ir(t2)
vik_sum <- summarize_ir(v1)

#Combine all injury data sets
injury_all <- bind_rows(sf_sum, ben_sum, buf_sum, bea_sum, brc_sum, buc_sum, car_sum,
                        cha_sum, cow_sum, chi_sum, col_sum, com_sum, dol_sum, eag_sum,
                        fal_sum, gia_sum, jag_sum, jet_sum, lio_sum, pac_sum, pan_sum,
                        pat_sum, sai_sum, sea_sum, ste_sum, rai_sum, ram_sum, rav_sum,
                        tex_sum, tit_sum, vik_sum, brn_sum)

injury_all <- injury_all %>%
  rename("player_name" = "Player") %>%
  group_by(player_name) %>%
  summarize(Weeks_on_Injury_Report = sum(Weeks_on_Injury_Report))

#Read in result data sets
results_all <- read.csv("results.csv")

#Add injury data to results by player
combined <- left_join(results_all, injury_all, by="player_name")

#Correlation test using Spearman rank correlation testing
combined1 <- combined %>%
  group_by(player_name) %>%
  mutate(high_severe = sum(collision_risk_score > 82.88703),
         total_crs = n(),
         prop = high_severe / total_crs)

cor.test((combined1$prop), combined1$Weeks_on_Injury_Report, method="spearman")

#Scatter plot
combined1 %>%
  ggplot(aes(x=prop, y=Weeks_on_Injury_Report)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Relationship Between Risk Exposure and Injury Weeks",
    x = "Proportion of Plays With High-Severe Collision Risk",
    y = "Weeks on Injury Report")
