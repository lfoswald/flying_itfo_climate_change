#### DATA FOR FLYERS IDENTITY ###############

setwd("~/R")
rm(list = ls())

# ------------------ packages -----------------------

library(readxl)
library(psych)
library(sjPlot)

# ---------------- (almost) raw data -----------------------

library(readxl)
source <- read_excel("~/R/data/data_flyid_2019-04-08_clean.xlsx")
#View(source)
attach(source)

# ---------------- outliers ------------------------

mean(TIME_SUM)/60
sd(TIME_SUM)/60

data <- source[source$TIME_SUM > 300,] # remove all participants with total time < 5 min
detach(source)
attach(data)


# ---------------- control variable ----------------

# Flugzeit 1 h --> umgepolt (--> high = aufmerksam)
hist(IV01_15) 
data <- data[data$IV01_15 > 30,]
detach(data)
attach(data)


# ---------------- item analysis --------------------


# original 
PBC <- data.frame(IV03_01,IV03_02,IV03_03,IV03_04,IV03_05)
finance_barrier <- data.frame(IV03_06,IV03_13,IV03_14)
avoiding_problems <- data.frame(IV07_25,IV07_26,IV07_27)
negative_effects <- data.frame(IV03_15,IV03_16,IV03_17,IV03_18,IV03_19,IV03_20,IV03_21)
image_travel <- data.frame(IV06_01,IV06_02,IV06_03,IV06_04,IV06_05)
image_environment <- data.frame(IV06_06,IV06_07,IV06_09,IV06_10,IV06_11)
openness_modes <- data.frame(IV07_28,IV07_29,IV07_30)
openness_alternatives <- data.frame(IV07_31,IV07_33,IV07_34,IV07_32)
attitude_fly <- data.frame(IV01_01,IV01_02,IV01_03,IV01_04,IV01_05,IV01_06)
problem_perception <- data.frame(IV01_07,IV01_09,IV01_10,IV01_11)
dilemma <- data.frame(IV01_12,IV01_13,IV01_14)
offsets <- data.frame(FL05_01,FL05_02,FL05_03)
social_media <- data.frame(SD09_01,SD09_02,SD09_03)


sjt.itemanalysis(PBC, show.shapiro = TRUE)
sjt.itemanalysis(finance_barrier, show.shapiro = TRUE)
sjt.itemanalysis(avoiding_problems, show.shapiro = TRUE)
sjt.itemanalysis(negative_effects, show.shapiro = TRUE)
sjt.itemanalysis(image_travel, show.shapiro = TRUE)
sjt.itemanalysis(image_environment, show.shapiro = TRUE)
sjt.itemanalysis(openness_modes, show.shapiro = TRUE)
sjt.itemanalysis(openness_alternatives, show.shapiro = TRUE)
sjt.itemanalysis(attitude_fly, show.shapiro = TRUE)
sjt.itemanalysis(dilemma, show.shapiro = TRUE)
sjt.itemanalysis(problem_perception, show.shapiro = TRUE)
sjt.itemanalysis(offsets, show.shapiro = TRUE)
sjt.itemanalysis(social_media, show.shapiro = TRUE)


### test modified data.frames


PBC <- data.frame(IV03_01,IV03_02,IV03_04,IV03_05)
negative_effects <- data.frame(IV03_15,IV03_16,IV03_17,IV03_19,IV03_21)
openness_alternatives <- data.frame(IV07_31,IV07_32)
problem_perception <- data.frame(IV01_07,IV01_10,IV01_11)

sjt.itemanalysis(PBC, show.shapiro = TRUE)
sjt.itemanalysis(negative_effects, show.shapiro = TRUE)
sjt.itemanalysis(openness_alternatives, show.shapiro = TRUE)
sjt.itemanalysis(problem_perception, show.shapiro = TRUE)


# ----------------- recode --------------------------

#daten$R_E103_07 <- 102 -(daten$E103_07)

# ---------------- save new scales -------------------

### modified scales 

# PBC --> IV03_03 raus
data$control <- (IV03_01+IV03_02+IV03_04+IV03_05)/4

# negative effects --> IV03_20, IV03_18	raus
data$negative_effects <- (IV03_15+IV03_16+IV03_17+IV03_19+IV03_21)/5

# openness alternatives --> IV07_33, IV07_34 raus (einzeln anschauen)
data$openness_alternatives <- (IV07_31+IV07_32)/3

# problem perception --> IV01_09 raus
data$problem_perception <- (IV01_07+IV01_10+IV01_11)/3


### okay scales 

# avoiding problems 
data$avoiding_problems <- (IV07_25+IV07_26+IV07_27)/3

# fianancial barrier 
data$finance_barrier <- (IV03_06+IV03_13+IV03_14)/3

# image management travel 
data$image_travel <- (IV06_01+IV06_02+IV06_03+IV06_04+IV06_05)/5

# image management environment 
data$image_environment <- (IV06_06+IV06_07+IV06_09+IV06_10+IV06_11)/5

# opennes alternative travel modes 
data$openness_modes <- (IV07_28+IV07_29+IV07_30)/3

# attitude flying 
data$attitude_fly <- (IV01_01+IV01_02+IV01_03+IV01_04+IV01_05+IV01_06)/6
data$problem_perception <- (IV01_07+IV01_09+IV01_10+IV01_11)/4
data$dilemma <- (IV01_12+IV01_13+IV01_14)/3
data$offsets <- (FL05_01+FL05_02+FL05_03)/3
data$social_media <- (SD09_01+SD09_02+SD09_03)/3

### One Item measures ###

data$environmental_attitude <- IV04
data$social_status <- IV07_23
data$success <- IV07_24
data$fear <- IV07_22
data$desire <- IV03_11  
data$age <- 2019-(SD02_01)
data$VFR <- FL07_01
data$openness_skype <- IV07_34
data$openness_video_conference <- IV07_33
  
### nominal answers ###
  
data$flyer_cathegory <- FL06
data$less_flights <- IV04_09
data$gender <- SD01
data$education <- SD03
data$relationship <- SD05
data$finance <- SD07
data$major <- SD04_01
data$studied_abroad <- SD08
data$lived_abroad <- SD11
data$country <- SD06_01
data$city <- SD06_03
  
  

# ----------------- save data ------------------------

save(data, file = "data_fly_id.RData")




