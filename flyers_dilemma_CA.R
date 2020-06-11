#### Analysis - The Flyers' Dilemma ####

# set working directory
setwd("~/R")

# clear working environment
rm(list = ls())

# load packages
library(ggplot2) # graphics
library(sjPlot) # itemanalysis
library(psych) # EFA
library(GPArotation) # EFA 
library(lavaan) # CFA
library(Hmisc) # correlation matrix 
library(qgraph) # gaussian graphical model
library(relaimpo) # estimation of relative importance 

#### 1. loading data files ####

data_fly <- read.csv("~/R/data/data_fly_105.csv")
dist_fly <- read.csv("~/R/data/dist_data_105.csv")

# check attention items (did participants disagree with (obviously?) false statements)

table(data_fly$IV01_15 > 50)
table(data_fly$IV02_21 > 50)
table(data_fly$IV03_12 > 50)

# check consent

table(data_fly$CO01 == 1)

#### descriptives ####

# gender
# 1 = male
# 2 = female
# 3 = other

table(data_fly$SD01)

# age

age <- 2018 - data_fly$SD02_01
table(age <= 35)
mean(age)
sd(age)
min(age)
max(age)

# what to do with 39 and 43 years old? ... ethics?

# education
# 1 = Completed grade school
# 2 = Completed secondary school
# 3 = Some university or college
# 4 = Bachelors degree
# 5 = Masters or professional degree 
# 6 = PhD

table(data_fly$SD03)

# relationship
# 1 = Single
# 2 = Partner, but not sharing a residence 3 = Live together, not married
# 4 = Married, no children
# 5 = Married with children
# 6 = Other

table(data_fly$SD05)

# location / residence

for (i in data_fly$SD06_01){
  if (data_fly$SD06_01 == "Canada" || data_fly$SD06_01 == "canada" || data_fly$SD06_01 == "CANADA") {
    print(TRUE) 
    }
    else {
      print(FALSE)
    }
}

# finance
# 1 = Deep in debt, and cannot pay it until later, if at all
# 2 = Mildly in debt, and almost no disposable income
# 3 = No serious debt, but living month-to-month with no other resources
# 4 = Can cover my expenses, and have some resources of my own, including some accessible resources from others (e.g., partner, friend, parents)
# 5 = Financially comfortable; not unlimited, but no real worries for now
# 6 = Very fortunate; I don’t really have to think much about any costs or expenses

table(data_fly$SD07)

#### global social network ####

mean(data_fly$NE01_01) # number of friends/family 
mean(na.omit(as.numeric(data_fly$NE01_02))) # countries
mean(data_fly$NE01_03) # number of business contacts
mean(na.omit(as.numeric(data_fly$NE01_04))) # countries

contacts <- lm(data_fly$NE01_01 ~ data_fly$NE01_03)
summary(contacts)
summary(contacts)$adj.r.squared
p_val <- summary(contacts)$coefficients[,4] 
p_val[2]

contacts.plot <- ggplot(data_fly, mapping=aes(x=NE01_01,y=NE01_03)) +
  labs(x="friends and family members abroad",y="international business contacts") +
  geom_point(color="dark blue", size=2) +
  geom_smooth(method=lm)
contacts.plot

countries <- lm(data_fly$NE01_02 ~ data_fly$NE01_04)
summary(countries)
summary(countries)$adj.r.squared
p_val <- summary(countries)$coefficients[,4] 
p_val[2]

countries.plot <- ggplot(data_fly, mapping=aes(x=NE01_02,y=NE01_04)) +
  labs(x="number of different countries with friends and family members",y="number of countries where business contacts are located") +
  geom_point(color="dark blue", size=2) +
  geom_smooth(method=lm)
countries.plot 

#### environmental attitude ####

ggplot(data_fly,aes(IV04)) + 
  geom_histogram(binwidth = 1, color = 'white', fill = 'aquamarine4') +
  labs(title = "environmental attitude", x = "environmental actions (out of 15)", y = "freqency") +
  scale_fill_gradient(low = "blue", high = "green")
# pretty 

#### dilemma ####

ggplot(data_fly,aes(IV01_12)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "dilemma - perceived mismatch", x = "extend of agreement", y = "freqency")

ggplot(data_fly,aes(IV01_13)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "dilemma - negative emotion", x = "extend of agreement", y = "freqency") 

ggplot(data_fly,aes(IV01_14)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "dilemma - guilt", x = "extend of agreement", y = "freqency") 



#### frequency of travel modes ####

travel.mode <- data.frame(data_fly$DV20, data_fly$DV21, data_fly$DV22, data_fly$DV23,
                          data_fly$DV24, data_fly$DV25, data_fly$DV26, data_fly$DV27,
                          data_fly$DV28, data_fly$DV29, data_fly$DV30
                          )

travel.mode.1 <- travel.mode[travel.mode != -9 ] # drop na
table(travel.mode.1)

data_mode <- data.frame(travel.mode[travel.mode != -9 ]) # we need data frame for ggplot

mode.lables <- c("plane", "train", "bus", "car", "ship", "bike/foot")

ggplot(data_mode,aes(travel.mode.1)) + 
  geom_bar(color = 'white', fill = 'steelblue') +
  labs(title = "Travel Modes", y = "Frequency") + 
  scale_x_discrete(name = "", limits = mode.lables) +
  theme(axis.text=element_text(size=12))

table(travel.mode.1)


#### travel pattern ####

# routes (qualitatively)

routes <- data.frame(dist_fly$route1, dist_fly$route2, dist_fly$route3, dist_fly$route4, 
                     dist_fly$route5, dist_fly$route6, dist_fly$route7, dist_fly$route8, 
                     dist_fly$route9, dist_fly$route10, dist_fly$route11
                     )

#table(unlist(routes))

## elegante solution? ---> new python project?  

# canada
# america
# europe
# asia

#### distance descriptive plot ####

# past travel distance
past <- (dist_fly$distance1 + dist_fly$distance2 + dist_fly$distance3 + 
                      dist_fly$distance4 + dist_fly$distance5 + dist_fly$distance6)
mean(past)
sd(past)

# circumference of the earth (40.000 km) 
sum(past)/40000
# all 97 participants together have circuit the earth about 28.7 times 
mean(past)/40000
# the average participants circuits the earth by 1/3

# future travel distance
future <- (dist_fly$distance7 + dist_fly$distance8 + dist_fly$distance9 +
                        dist_fly$distance10)
mean(past)
sd(past)

# fictive
fictive <- dist_fly$distance11
mean(fictive)
sd(fictive)

# past travel distance
past.df <- data.frame(dist_fly$distance1, dist_fly$distance2, dist_fly$distance3, 
           dist_fly$distance4, dist_fly$distance5, dist_fly$distance6)
past.df.nna <- data.frame(past.df[past.df != 0])
past.nna <- past.df[past.df != 0]


# future travel distance
future.df <- data.frame(dist_fly$distance7, dist_fly$distance8, dist_fly$distance9, 
             dist_fly$distance10)
future.df.nna <- data.frame(future.df[future.df != 0])
future.nna <- future.df[future.df != 0]


# fictive
fictive.df <- dist_fly$distance11
fictive.df.nna <- data.frame(fictive.df[fictive.df != 0])
fictive.nna <- fictive.df[fictive.df != 0]


# distances total
total.df <- data.frame(past.df, future.df, fictive.df)
total.nna <- total.df[total.df != 0]
total.df.nna <- data.frame(total.df[total.df != 0])


#### Plots ####

# Plotting all trip distances together

ggplot(total.df.nna, aes(x = total.nna)) + 
  geom_dotplot( binwidth = 250, color = "white", fill = "indianred") +
  labs(title = "Distances for all trips", x = "Distance in kilometers") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# groups in different colours:

# past
ggplot(past.df.nna, aes(x = past.nna)) + 
  geom_dotplot( binwidth = 280, color = "white", fill = "goldenrod") +
  labs(title = "Distances for past trips", x = "Distance in kilometers") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# future
ggplot(future.df.nna, aes(x = future.nna)) + 
  geom_dotplot( binwidth = 280, color = "white", fill = "mediumpurple4") +
  labs(title = "Distances for future trips", x = "Distance in kilometers") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
# fictive
ggplot(fictive.df.nna, aes(x = fictive.nna)) + 
  geom_dotplot( binwidth = 280, color = "white", fill = "aquamarine4") +
  labs(title = "Distances for ficitive trips", x = "Distance in kilometers") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#### travel emissions ####


# set travel mode weights

# 1 - airplane - 241
# 2 - train - 38
# 3 - bus - 32
# 4 - car - 105
# 5 - ship - 390
# 6 - bike/foot - 5 

index <- c(1, 2, 3, 4, 5, 6, -9, NA, 0)
values <- c(241, 38, 32, 105, 390, 5, 0, 0, 0)

data_fly$F1 <- values[match(data_fly$DV20, index)]
data_fly$F2 <- values[match(data_fly$DV21, index)]
data_fly$F3 <- values[match(data_fly$DV22, index)]
data_fly$F4 <- values[match(data_fly$DV23, index)]
data_fly$F5 <- values[match(data_fly$DV24, index)]
data_fly$F6 <- values[match(data_fly$DV25, index)]
data_fly$F7 <- values[match(data_fly$DV26, index)]
data_fly$F8 <- values[match(data_fly$DV27, index)]
data_fly$F9 <- values[match(data_fly$DV28, index)]
data_fly$F10 <- values[match(data_fly$DV29, index)]
data_fly$F11 <- values[match(data_fly$DV30, index)]

# replace 'return' -9 with 2 (two ways instead of 1)

index1 <- c(1,-9)
values1 <- c(1, 2)

# here: overwrite variable

data_fly$DV31 <- values1[match(data_fly$DV31, index1)]
data_fly$DV32 <- values1[match(data_fly$DV32, index1)]
data_fly$DV33 <- values1[match(data_fly$DV33, index1)]
data_fly$DV34 <- values1[match(data_fly$DV34, index1)]
data_fly$DV35 <- values1[match(data_fly$DV35, index1)]
data_fly$DV37 <- values1[match(data_fly$DV37, index1)]
data_fly$DV38 <- values1[match(data_fly$DV38, index1)]
data_fly$DV39 <- values1[match(data_fly$DV39, index1)]
data_fly$DV40 <- values1[match(data_fly$DV40, index1)]
data_fly$DV41 <- values1[match(data_fly$DV41, index1)]
data_fly$DV42 <- values1[match(data_fly$DV42, index1)]


# Emissions = Distance (calculated in python, saved in separate csv) * travel mode factor * one way / return

data_fly$E1 <- dist_fly$distance1 * data_fly$F1 * data_fly$DV31
data_fly$E2 <- dist_fly$distance2 * data_fly$F2 * data_fly$DV32
data_fly$E3 <- dist_fly$distance3 * data_fly$F3 * data_fly$DV33
data_fly$E4 <- dist_fly$distance4 * data_fly$F4 * data_fly$DV34
data_fly$E5 <- dist_fly$distance5 * data_fly$F5 * data_fly$DV35
data_fly$E6 <- dist_fly$distance6 * data_fly$F6 * data_fly$DV37
data_fly$E7 <- dist_fly$distance7 * data_fly$F7 * data_fly$DV38
data_fly$E8 <- dist_fly$distance8 * data_fly$F8 * data_fly$DV39
data_fly$E9 <- dist_fly$distance9 * data_fly$F9 * data_fly$DV40
data_fly$E10 <- dist_fly$distance10 * data_fly$F10 * data_fly$DV41
data_fly$E11 <- dist_fly$distance11 * data_fly$F11 * data_fly$DV42

# comment: not a mistake - there is no variable DV36

# past travel emissions
past_e <- (data_fly$E1 + data_fly$E2 + data_fly$E3 + data_fly$E4 + 
             data_fly$E5 + data_fly$E6)
mean(past_e)
sd(past_e)

# future travel emissions
future_e <- (data_fly$E7 + data_fly$E8 + data_fly$E9 + data_fly$E10)
mean(future_e)
sd(future_e)

fictive_e <- data_fly$E11
mean(fictive_e)
sd(fictive)

# total (real) travel distance
total <- past+future
mean(total)
sd(total)

# total emissions
total_e <- past_e+future_e
mean(total_e)
sd(total_e)

#### itemanalysis ####

attach(data_fly)

attitude.df <- data.frame(IV01_01,IV01_02,IV01_03,IV01_04,IV01_05,IV01_06)
awareness.df <- data.frame(IV01_07,IV01_08,IV01_09,IV01_10,IV01_11)
dilemma.df <- data.frame(IV01_12,IV01_13,IV01_14)
soc_norm.df <- data.frame(IV02_01,IV02_02,IV02_03,IV02_04,IV02_05)
prof_norm.df <- data.frame(IV02_06,IV02_07,IV02_08,IV02_09,IV02_10)
sub_norm.df <- data.frame(IV02_11,IV02_12,IV02_13,IV02_14,IV02_15)
identity.df <- data.frame(IV02_16,IV02_17,IV02_18,IV02_19,IV02_20)
control.df <- data.frame(IV03_01,IV03_02,IV03_03,IV03_04,IV03_05)
barriers.df <- data.frame(IV03_06,IV03_07,IV03_08)
desire.df <- data.frame(IV03_09,IV03_10,IV03_11)

global.df <- data.frame(IV02_01,IV02_02,IV02_03,IV02_04,IV02_05,
                        IV02_06,IV02_07,IV02_08,IV02_09,IV02_10,
                        IV02_16,IV02_17,IV02_18,IV02_19,IV02_20)

dragons.df <- data.frame(IV05_01,IV05_02,IV05_03,IV05_04,IV05_05,IV05_06,IV05_07,
                         IV05_08,IV05_09,IV05_10,IV05_11,IV05_12,IV05_13,IV05_14,
                         IV05_15,IV05_16,IV05_17,IV05_18,IV05_19,IV05_20,IV05_21)

need.change.df <- data.frame(IV05_01,IV05_02,IV05_03,IV05_04)
conf.goals.df <- data.frame(IV05_05,IV05_06,IV05_07,IV05_08)
interpersonal.inf.df <- data.frame(IV05_09,IV05_10,IV05_11,IV05_12)
gov.ind.df <- data.frame(IV05_13,IV05_14)
tokenism.df <- data.frame(IV05_15,IV05_16,IV05_17,IV05_18)
lack.knowledge.df <- data.frame(IV05_19,IV05_20,IV05_21)

sjt.itemanalysis(attitude.df, show.shapiro = TRUE) 
sjt.itemanalysis(awareness.df, show.shapiro = TRUE)
sjt.itemanalysis(dilemma.df, show.shapiro = TRUE)
sjt.itemanalysis(soc_norm.df, show.shapiro = TRUE)
sjt.itemanalysis(prof_norm.df, show.shapiro = TRUE)
sjt.itemanalysis(sub_norm.df, show.shapiro = TRUE)  
sjt.itemanalysis(identity.df, show.shapiro = TRUE)
sjt.itemanalysis(control.df, show.shapiro = TRUE)
sjt.itemanalysis(barriers.df, show.shapiro = TRUE)
sjt.itemanalysis(desire.df, show.shapiro = TRUE)
sjt.itemanalysis(dragons.df, show.shapiro = TRUE)
sjt.itemanalysis(global.df, show.shapiro = TRUE)
sjt.itemanalysis(need.change.df, show.shapiro = TRUE)
sjt.itemanalysis(conf.goals.df, show.shapiro = TRUE)
sjt.itemanalysis(interpersonal.inf.df, show.shapiro = TRUE)
sjt.itemanalysis(gov.ind.df, show.shapiro = TRUE)
sjt.itemanalysis(tokenism.df, show.shapiro = TRUE)
sjt.itemanalysis(lack.knowledge.df, show.shapiro = TRUE)

# modify some scales:
attitude.df <- data.frame(IV01_01,IV01_02,IV01_03,IV01_04,IV01_05) # nr 6
prof_norm.df <- data.frame(IV02_06,IV02_07,IV02_08,IV02_09) # nr 10
sub_norm.df <- data.frame(IV02_12,IV02_13,IV02_14,IV02_15) # nr 11
need.change.df <- data.frame(IV05_02,IV05_03,IV05_04) # nr 1

sjt.itemanalysis(attitude.df, show.shapiro = TRUE) 
sjt.itemanalysis(prof_norm.df, show.shapiro = TRUE)
sjt.itemanalysis(sub_norm.df, show.shapiro = TRUE) 
sjt.itemanalysis(need.change.df, show.shapiro = TRUE)

#### scales ####

data_fly$attitude <- (IV01_01+IV01_02+IV01_03+IV01_04+IV01_05)/5
data_fly$awareness <- (IV01_07+IV01_08+IV01_09+IV01_10+IV01_11)/5
data_fly$dilemma <- (IV01_12+IV01_13+IV01_14)/3
data_fly$prof_norm <- (IV02_06+IV02_07+IV02_08+IV02_09+IV02_01)/5
data_fly$sub_norm <- (IV02_13+IV02_14+IV02_15)/3
data_fly$identity <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20)/5
data_fly$control <- (IV03_01+IV03_02+IV03_03+IV03_04+IV03_05)/5
data_fly$barriers <- IV03_06
data_fly$desire <- (IV03_09+IV03_10+IV03_11)/3
data_fly$env_att <- IV04

data_fly$dragons <- (IV05_01+IV05_02+IV05_03+IV05_04+IV05_05+IV05_06+IV05_07+
                       IV05_08+IV05_09+IV05_10+IV05_11+IV05_12+IV05_13+IV05_14+
                       IV05_15+IV05_16+IV05_17+IV05_18+IV05_19+IV05_20+IV05_21)/21

data_fly$need.change <- (IV05_02+IV05_03+IV05_04)/3
data_fly$conf.goals <- (IV05_05+IV05_06+IV05_07+IV05_08)/4
data_fly$interpersonal.inf <- (IV05_09+IV05_10+IV05_11+IV05_12)/4
data_fly$gov.ind <- (IV05_13+IV05_14)/2
data_fly$tokenism <- (IV05_15+IV05_16+IV05_17+IV05_18)/4
data_fly$lack.knowledge <- (IV05_19+IV05_20+IV05_21)/3

data_fly$global <- (IV02_01+IV02_03+IV02_04+IV02_05+
                      IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+
                      IV02_16+IV02_17+IV02_18+IV02_19+IV02_20)/14

data_fly$network <- NE01_01 * NE01_02 + NE01_03 * NE01_04

ggplot(data_fly,aes(dilemma)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "Dilemma", x = "extend of agreement", y = "freqency") 


detach(data_fly)
attach(data_fly)

mean(attitude)
sd(attitude)

mean(awareness)
sd(awareness)

mean(network)
sd(network)
#### CIS factor analysis #####

#EFA

global.df <- data.frame(IV02_01,IV02_02,IV02_03,IV02_04,IV02_05,
                        IV02_06,IV02_07,IV02_08,IV02_09,IV02_10,
                        IV02_16,IV02_17,IV02_18,IV02_19,IV02_20)
sjt.itemanalysis(global.df)

# Parallel analysis
PFX <- fa.parallel(global.df,fa="fa")

# Scree Plot
fit <- princomp(global.df, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

# Force 3 factor solution
PFA2 <- fa(global.df,3) 
PFA2
fa.diagram(PFA2)

# Force 4 factor solution
PFA3 <- fa(global.df,4) 
PFA3
fa.diagram(PFA3)


global.df.n <- data.frame(IV02_01,IV02_03,IV02_04,IV02_05,
                          IV02_06,IV02_07,IV02_08,IV02_09,IV02_10,
                          IV02_16,IV02_17,IV02_18,IV02_19,IV02_20)

PFX <- fa.parallel(global.df.n,fa="fa")
fit <- princomp(global.df.n, cor=TRUE)
plot(fit,type="lines") 

PFA2 <- fa(global.df.n,3) # three factors - as theory suggests & parallel analysis suggests!!! 
fa.diagram(PFA2)

# building scales for further analysis 

data_fly$soc_norm <- (IV02_10+IV02_03+IV02_04+IV02_05)/4
data_fly$prof_norm <- (IV02_06+IV02_07+IV02_08+IV02_09)/4
data_fly$CHI <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20)/5
data_fly$soc_pressure <- (IV02_01+IV02_02)/2

# CFA (will be repeated with other sample)

# general factor

global_cfa <- "
global =~ IV02_01+IV02_02+IV02_03+IV02_04+IV02_05+IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_16+IV02_17+IV02_18+IV02_19+IV02_20"

fit1 <- sem(global_cfa, data = data_fly)
summary(fit1, fit.measures = TRUE)
fitMeasures(fit1, fit.measures = c("chisq", "cfi", "rmsea", "aic"))


# theoretical model 

global_cfa_theoretical <-"
f1 =~ IV02_01+IV02_02+IV02_03+IV02_04+IV02_05
f2 =~ IV02_06+IV02_07+IV02_08+IV02_09+IV02_10
f3 =~ IV02_16+IV02_17+IV02_18+IV02_19+IV02_20"

fit2 <- cfa(global_cfa_theoretical, data = data_fly)
summary(fit2, fit.measures=TRUE)
fitMeasures(fit2, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

# empirical model with 4 factors (including social pressure & judgement)

# try to make the model easier without changing something??

global_cfa_empirical <-"
f1 =~ IV02_16+IV02_17+IV02_18+IV02_19+IV02_20
f2 =~ IV02_06+IV02_07+IV02_08+IV02_09
f3 =~ IV02_03+IV02_04+IV02_05+IV02_10
f4 =~ IV02_01+IV02_02
"

fit3 <- cfa(global_cfa_empirical, data = data_fly)
summary(fit3, fit.measures=TRUE)
fitMeasures(fit3, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

#### correlation & regression ####

data_fly$emissions <- total_e
data_fly$attitude <- attitude
data_fly$awareness <- awareness
data_fly$dilemma <- dilemma
data_fly$social <- soc_norm
data_fly$professional <- prof_norm
data_fly$norm <- sub_norm
data_fly$CHI <- CHI
data_fly$pressure <- soc_pressure
data_fly$barrier <- barriers
data_fly$desire <- desire
data_fly$age <- age
data_fly$network <- network
data_fly$environment <- IV04
data_fly$finance <- SD07
data_fly$relationship <- SD05
data_fly$education <- SD03
data_fly$gender <- SD01

detach(data_fly)
attach(data_fly)

korr_data_pretty <- data.frame(emissions,
                               attitude,
                               awareness,
                               dilemma,
                               social,
                               professional,
                               pressure,
                               norm,
                               CHI,
                               barrier, # financial barrier
                               desire,
                               age,
                               network,
                               environment, # environmental attitude
                               finance, # finance 
                               relationship, # relationship
                               education, # education
                               gender) # gender


rcorr(as.matrix(korr_data_pretty),type = "pearson")
sjt.corr(korr_data_pretty) 
cor.plot(korr_data_pretty, main = "Correlation Plot - total",
         cex = 0.8, numbers = TRUE, stars = TRUE, cex.axis = 0.8)

#### testing hypotheses ####

# positive relationship between environmental attitude and total emissions - yes
cor.test(IV04,total_e)

# relationship between environmental attitude and cosmopolitan identity
cor.test(IV04, global)

# relationship between total emissions and cosmopolitan identity
cor.test(total_e, global)

cor.test(total_e, SD07) # finance 
cor.test(total_e, SD05) # relationship
cor.test(total_e, SD03) # education
cor.test(total_e, SD01) # gender

mean(IV03_06) # financial barriers
sd(IV03_06)


#### gaussian graphical model ####

fly_cors <- cor_auto(korr_data_pretty, detectOrdinal = FALSE)

FlyingGraph <- qgraph(cor(korr_data_pretty),minimum=0.25,
                      legend=TRUE,borders=FALSE, title = "Flying Partial Correlations")

# Same graphs with spring layout:
FlyingGraph <- qgraph(cor(korr_data_pretty),
                      legend=TRUE,borders=FALSE, title = "Flying Correlations",layout="spring")


#### estimation of relative importance of regressors ####

linmod <- lm(emissions ~ attitude + awareness + dilemma + social + professional + norm + CHI + barrier + desire +
             age + network + environment + finance + relationship + education + gender , data = data_fly)


metrics <- calc.relimp(linmod , type = c("lmg"))
metrics
anova(linmod)
par(cex.axis = 0.8,mfrow= c(1,1) )
plot(metrics, names.abbrev = 1)

# bootstrapping of results

randomlmg <- booteval.relimp(boot.relimp(linmod, b = 500), bty = "perc",level = 0.95)
output <- rbind(randomlmg$lmg.lower, randomlmg$lmg.upper)
output <- as.matrix(t(output))
colnames(output) <- c("random.lower", "random.upper")
rownames(output) <- c(fixedlmg$namen[2:6])
output

# plot boot resutls

bootresult <- boot.relimp(linmod, b = 500, type = "lmg", fixed = FALSE)
par(cex.axis = 0.9)
plot(booteval.relimp(bootresult, typesel = "lmg", level = 0.95),names.abbrev = 1, bty = "perc")

#### linear equation modelling / mediation ####

# fictive emissions as intention
med11.txt <- 'fictive_e ~ attitude + sub_norm + control + desire'

med11.fit<- sem(med11.txt,data=korr_data_small, se="bootstrap")
summary(med11.fit)

library(semPlot) 

semPaths(med11.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
         sizeMan = 18, edge.label.cex = 2.2, residuals = FALSE, color = "indianred",
         nodeLabels = c("adrenalin","politics","openness"))

parameterEstimates(med11.fit, ci=TRUE, boot.ci.type="bca.simple") 


# future emissions as intention
med21.txt <- 'future_e ~ attitude + sub_norm + control + desire'

med21.fit <-sem(med21.txt, data=korr_data_small, se="bootstrap")
summary(med21.fit)

semPaths(med21.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
         sizeMan = 18, edge.label.cex = 2.2, residuals = FALSE, color = "indianred",
         nodeLabels = c("adrenalin","politics","openness"))

parameterEstimates(med21.fit, ci=TRUE, boot.ci.type="bca.simple") 








