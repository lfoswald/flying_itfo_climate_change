####### FLYER'S IDENTITY ######

setwd("~/R")
rm(list = ls())

# -------------- packages --------------------------

library(readr) # read csv
library(ggplot2) # graphics
library(sjPlot) # corr table
library(psych) # corr plot
library(lavaan) # CFA
library(Hmisc) # correlation matrix 
library(qgraph) # gaussian graphical model
library(depmixS4) # dependent mixture model (LCA)
library(tidyLPA) # latent profile analysis (LPA)
library(mclust) # hierarchical clustering
library(flexmix) # finite mixture model
library(semPlot) # structural equation model


# ------------- load data --------------------------

# load R prep data 
load("~/R/data_fly_id.RData")
#View(data) # name = data 
attach(data)

# load python csv document

network_data <- read_csv("~/R/data/network_data_final.csv")

flight_data_final <- read_csv("~/R/data/flight_data_final.csv", na = "0")
fd <- flight_data_final



# ------------- descriptives ----------------------

# DEMOGRAPHY

# Language Version

table(LANGUAGE)

# Age

mean(age)
sd(age)
min(age)
max(age)

# Gender
table(gender) # 2 = female

# Education 
table(education)

# Financial Situation
table(finance)

# Relationship 
table(relationship)

# OTHER NOMINAL ANSWERS 

table(flyer_cathegory)
table(less_flights)
list(major)
table(studied_abroad)
table(lived_abroad)
table(country)
list(city)

# FLIGHTS advanced

mean(past.flightkm)
sd(past.flightkm)
mean(planned.flightkm)
sd(planned.flightkm)

mean(no_flights_total)
sd(no_flights_total)

mean(VFR)
sd(VFR)

# did fly at all
did.fly.dat <- data[data$past.flightkm > 0,] 
didnot.fly.dat <- data[data$past.flightkm == 0,] 


# graphical representation

planned.flightdf <- data.frame(fd$distance1 , fd$distance2 , fd$distance3 , fd$distance4 , fd$distance5 , fd$distance6
                          , fd$distance7 , fd$distance8 , fd$distance9 , fd$distance10 , fd$distance11 , fd$distance12
                          , fd$distance13 , fd$distance14 , fd$distance15 , fd$distance16 , fd$distance17
                          , fd$distance18 , fd$distance19 , fd$distance20 , fd$distance21)
past.flightdf <- data.frame(fd$distance26 , fd$distance27 , fd$distance28 , fd$distance29 , fd$distance30 , fd$distance31
                       , fd$distance32 , fd$distance33 , fd$distance34 , fd$distance35 , fd$distance36 , fd$distance37
                       , fd$distance38 , fd$distance39 , fd$distance40 , fd$distance41 , fd$distance42
                       , fd$distance43 , fd$distance44 , fd$distance45 , fd$distance46
                       , fd$distance47 , fd$distance48 , fd$distance49 , fd$distance50)


total.df <- data.frame(past.flightdf, planned.flightdf)
total.nna <- total.df[total.df != 0]
total.df.nna <- data.frame(total.df[total.df != 0])

ggplot(total.df.nna, aes(x = total.nna)) + 
  geom_dotplot( binwidth = 250, color = "white", fill = "indianred") +
  labs(title = "Distances for all trips", x = "Distance in kilometers") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


# types of flights


flight.types <- total.nna
flight.types <- ifelse((total.nna >=0 & total.nna <= 1000) , 'national', flight.types)
flight.types <- ifelse((total.nna>1000 & total.nna<=5000) , 'continental', flight.types)
flight.types <- ifelse((total.nna>5000 & total.nna<=20000) , 'intercontinental', flight.types)


ft.df <- as.data.frame(flight.types)
table(ft.df)

table(FL06)

# NETWORK advanced

mean(no_contacts)
sd(no_contacts)

mean(data$networkkm)
sd(data$networkkm)

# hub contains closeness score (sum of all closeness values of contacts living in this hub --> calc manually in Excel)

hub1 <- NE19_01
hub2 <- NE19_02
hub3 <- NE19_03
hub4 <- NE19_04
hub5 <- NE19_05
hub6 <- NE19_06
hub7 <- NE19_07
hub8 <- NE19_08
hub9 <- NE19_09
hub10 <- NE19_10
hub11 <- NE19_11
hub12 <- NE19_12
hub13 <- NE19_13

data$GSIS <- (hub1*network_data$distance1+
                hub2*network_data$distance2+
                hub3*network_data$distance3+
                hub4*network_data$distance4+
                hub5*network_data$distance5+
                hub6*network_data$distance6+
                hub7*network_data$distance7+
                hub8*network_data$distance8+
                hub9*network_data$distance9+
                hub10*network_data$distance10+
                hub11*network_data$distance11+
                hub12*network_data$distance12+
                hub13*network_data$distance13)/100000


mean(data$GSIS)
sd(data$GSIS)

# graphical representation 

ggplot(data,aes(GSIS)) + 
  geom_histogram(binwidth = 2, color = 'white', fill = 'plum4') +
  labs(title = "GSIS", x = "Global Social Interconnectedness Score", y = "Freqency") 

ggplot(data,aes(GSIS)) + 
  geom_dotplot(binwidth = 2, color = 'white', fill = 'plum4') +
  labs(title = "GSIS", x = "Global Social Interconnectedness Score", y = "Freqency") 

data$studied_abroad_f <- as.factor(studied_abroad)
t.test(GSIS, studied_abroad, na.rm = T)

AV <- GSIS
UV <- as.factor(studied_abroad)

summary(aov(AV~UV))
tapply(AV, UV, mean, na.rm = T)
pairwise.t.test(AV,UV, p.adj="bonferroni")

detach(data)
attach(data)

df <- data.frame(GSIS,studied_abroad)
### barplot GSIS, studied abroad

my.mean1<-tapply(df[,"GSIS"],df[,"studied_abroad"],mean, na.rm =T)
my.sd1<-tapply(df[,"GSIS"],df[,"studied_abroad"],sd)
table(df[,"studied_abroad_f"])
my.nr1<-as.vector(table(df[,"studied_abroad"]))

my.se1<-my.sd1/sqrt(my.nr1)

my.df1<-data.frame(groups=factor(names(my.mean1),labels =c("studied abroad","did not study abroad")),
                   mean=my.mean1,
                   sd=my.sd1,
                   se=my.se1)
my.df1

# start with barplot
my.box1 <-ggplot(my.df1 ,aes(x=groups, y=mean)) + 
  lims(y=c(0,15)) +
  geom_bar(stat="identity", aes(x=groups, y=mean),color="white",size=0.1,
           width = 0.6, fill=c("plum4")) + 
  theme(
    axis.text.x = element_text(colour="black", size =12),
    axis.text.y = element_text(colour="black", size =10), 
    axis.title.x = element_text( colour="black", size=15),
    axis.title.y = element_text( colour="black", size=15))+
  labs(x="",y="Global Social Interconnectedness Score")
my.box1

# add errorbars / Standardfehler
graph <- my.box1 +
  geom_errorbar(aes(ymin= mean - my.se1, ymax = mean + my.se1),
                width=.1)
graph



cor.test(GSIS, VFR)

cor.test(GSIS, CIS)

# environmental attitude 
mean(environmental_attitude)
sd(environmental_attitude)

ggplot(data,aes(environmental_attitude)) + 
  geom_histogram(binwidth = 1, color = 'white', fill = 'aquamarine4') +
  labs(title = "Environmental attitude", x = "Environmental actions (out of 15)", y = "Freqency") 

# dilemma
mean(dilemma)
sd(dilemma)

ggplot(data,aes(dilemma)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "Dilemma", x = "Extent of agreement", y = "Freqency")


# ------------- CFA CIS ----------------------------

CIS.df <- data.frame(IV02_01,IV02_02,IV02_03,IV02_05,IV02_06,IV02_07,IV02_08,IV02_09,
                  IV02_10,IV02_11,IV02_12,IV02_16,IV02_17,IV02_18,IV02_19,IV02_20,
                  IV02_22,IV02_23,IV02_26,IV02_27,IV02_28,IV02_31,IV02_13,IV02_14,
                  IV02_15)




#EFA
PFX <- fa.parallel(CIS.df,fa="fa") # 6 factors
fit <- princomp(CIS.df, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

PFA3 <- fa(CIS.df,4) # force 4 factor solution
PFA3
fa.diagram(PFA3)


# CFA
cosmo_cfa <- "
cosmo =~ IV02_01+IV02_02+IV02_03+IV02_05+IV02_06+IV02_07+IV02_08+IV02_09+
                  IV02_10+IV02_11+IV02_12+IV02_16+IV02_17+IV02_18+IV02_19+IV02_20+
                  IV02_22+IV02_23+IV02_26+IV02_27+IV02_28+IV02_31+IV02_13+IV02_14+
                  IV02_15"

fit1 <- sem(cosmo_cfa, data = data)
summary(fit1, fit.measures = TRUE)
fitMeasures(fit1, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

cosmo_cfa_theoretical_4 <-"
f1 =~ IV02_16+IV02_17+IV02_18+IV02_19+IV02_20 # CHI
f2 =~ IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02+IV02_31 # pessure / emotional norm
f3 =~ IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_01  # prof norm
f4 =~ IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_11 # social norm (incl. subjective norm)
"
fit2 <- cfa(cosmo_cfa_theoretical_4, data = data)
summary(fit2, fit.measures=TRUE)
fitMeasures(fit2, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

cosmo_cfa_empirical_4 <-"
f1 =~ IV02_16+IV02_17+IV02_18+IV02_19+IV02_20
f2 =~ IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02
f3 =~ IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_11+IV02_01
f4 =~ IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05
"
fit3 <- cfa(cosmo_cfa_empirical_4, data = data)
summary(fit3, fit.measures=TRUE)
fitMeasures(fit3, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

# --------> build scale + subscales! (based on theoretical 4 factor model)

data$CIS <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20+ # CHI
        IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02+ # pessure / emotional norm
        IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_01+  # prof norm
        IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_11)/25 # social norm
data$social_norm <- (IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_11)/7
data$prof_norm <- (IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_01)/6
data$CHI <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20)/5
data$social_pressure <- (IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02)/6

data$total.flightkm <- (planned.flightkm + past.flightkm)

CIS.df.fin <- data.frame(IV02_01,IV02_02,IV02_03,IV02_05,IV02_06,IV02_07,IV02_08,IV02_09,
                     IV02_10,IV02_11,IV02_12,IV02_16,IV02_17,IV02_18,IV02_19,IV02_20,
                     IV02_22,IV02_23,IV02_26,IV02_27,IV02_28,IV02_13,IV02_14,
                     IV02_15)


sjt.itemanalysis(CIS.df.fin)


detach(data)
attach(data)

# --------------- correlation table ---------------

#--------> include flights, voluntariness



korr_data_pretty <- data.frame(  total.flightkm,
  attitude_fly,
  social_norm,
  prof_norm,
  CHI,
  social_pressure,
  VFR,
  fear,
  studied_abroad,
  problem_perception,
  GSIS,
  dilemma,
  success,
  social_media,
  social_status,
  control,
  negative_effects,
  openness_alternatives,
  avoiding_problems,
  image_travel,
  image_environment,
  finance_barrier, 
  desire,
  age,
  environmental_attitude,
  gender
) 



rcorr(as.matrix(korr_data_pretty),type = "pearson")
sjt.corr(korr_data_pretty) 
cor.plot(korr_data_pretty, main = "Correlation Plot - total",
         cex = 0.6, numbers = TRUE, stars = TRUE, cex.axis = 0.8)

# GGM
fly_cors <- cor_auto(korr_data_pretty, detectOrdinal = FALSE)

FlyingGraph <- qgraph(cor(korr_data_pretty),minimum=0.25,
                      legend=TRUE,borders=FALSE, title = "Flying Partial Correlations",layout="spring")


# Same graphs with spring layout:
FlyingGraph <- qgraph(cor(korr_data_pretty),
                      legend=TRUE,borders=FALSE, title = "Flying Correlations",layout="spring", negCol = "purple")


# -------------------------- K-means Clustering -------------------------


# Including all CIS variables

which(colnames(data)=='CIS')
which(colnames(data)=='social_pressure')
which(colnames(data)=='social_norm')
which(colnames(data)=='prof_norm')
which(colnames(data)=='CHI')

kmeans.clusters <- kmeans(data[,c(395:399)], 3)
# Save the cluster number in the dataset as column
data$clusters <- as.factor(kmeans.clusters$cluster)
data$clusters

ggplot(data, aes(CIS, no_flights_total)) + geom_point(aes(colour = clusters) ) +
  ggtitle("CHI and Flights") + labs (x = "Cosmopolitan Identity", y = "Total Number of Flights")




# -------------- latent profile analysis  ------------

detach(data)
attach(data)

# latent profile analysis 

devtools::install_github("data-edu/tidyLPA")
library(tidyLPA)
library(dplyr)

# short version
data %>%
  select(problem_perception, CIS, social_media, age, finance ,VFR)%>%
  scale() %>%
  estimate_profiles(n_profiles = 4, models = 3) 
data %>%
  select(problem_perception, CIS, social_media, age, finance ,VFR)%>%
  scale() %>%
  estimate_profiles(n_profiles = 3, models = 3) 
data %>%
  select(problem_perception, CIS, social_media, age, finance ,VFR)%>%
  scale() %>%
  estimate_profiles(n_profiles = 2, models = 3) 
data %>%
  select(problem_perception, CIS, social_media, age, finance ,VFR)%>%
  scale() %>%
  estimate_profiles(n_profiles = 1, models = 3) 

data %>%
  select(problem_perception, CIS, social_media, age, finance ,VFR)%>%
  scale() %>%
  estimate_profiles(n_profiles = 3, models = 3) %>%
  plot_profiles()



# my example in long format

table(is.na(VFR))

prof_variables <- subset(data,select = c(problem_perception, age, CIS, social_media, finance ,VFR))

prof_variables <- scale(prof_variables)

exp.profile_estimates <- estimate_profiles(prof_variables, n_profiles = 3 , models = 3)
get_fit(exp.profile_estimates)
fits <- get_fit(exp.profile_estimates)

profiles <- get_data(exp.profile_estimates)

#View(profiles) # search for id (right) where 1 starts again
fin.profiles <- subset(profiles[1:393,])
#View(fin.profiles)

data$profile <- fin.profiles$Class

str(fin.profiles) #393 x 12
str(data) # 393 x 403

data <- cbind(data,fin.profiles)
str(data) # 393 x 415

col_by_value(x, col, range = NA, breaks = NA, showColorBar = T)
plot_profiles(exp.profile_estimates, rawdata = T, sd = T, alpha_range = c(0, 0.5))
plot_profiles(exp.profile_estimates, rawdata = T, sd = F, alpha_range = c(0, 0.5))
plot_profiles(exp.profile_estimates, rawdata = F, sd = F, alpha_range = c(0, 0.5))



# n_profiles: Anzahl der Klassen
# Varying means, equal variances, and covariances fixed to 0 (model 1)
# Varying means, equal variances, and equal covariances (model 2)
# Varying means, varying variances, and covariances fixed to 0 (model 3)
# Varying means, varying variances, and varying covariances (model 6)


# -------------- ANOVA betw. groups ------
detach(data)
attach(data)

# ANOVA univariate
#Variablen definieren:
UV = as.factor(data$profile)

AV = total.flightkm
AV = no_flights_total
AV = studied_abroad
AV = dilemma
AV = negative_effects
AV = openness_modes
AV = avoiding_problems
AV = voluntariness

table(profile)

summary(aov(AV~UV))
tapply(AV, UV, mean, na.rm = T)
pairwise.t.test(AV,UV, p.adj="bonferroni")

detach(data)
attach(data)

profile <- as.factor(profile)

ggplot(korr_data_pretty, aes(CIS, no_flights_total)) + 
  geom_point(aes(colour = profile), size = 2) +
  ggtitle("Flights and Profile") + 
  labs (x = "Cosmopolitan Identity", y = "Total Number of Flights") + 
  scale_color_brewer(palette="Set1")

ggplot(korr_data_pretty, aes(GSIS, VFR)) + 
  geom_point(aes(colour = profile), size = 2) +
  ggtitle("Global Social Network") + 
  labs (x = "Global Social Interconnectedness", y = "Visiting Friends and Relatives") + 
  scale_color_brewer(palette="Paired")


ggplot(korr_data_pretty, aes(environmental_attitude, dilemma)) + 
  geom_point(aes(colour = profile), size = 2) +
  ggtitle("The Flyers' Dilemma") + 
  labs (x = "Environmental Attitude", y = "Dilemma") + 
  scale_color_brewer(palette="Accent")



# BARPLOT

my.mean1<-tapply(data[,"total.flightkm"],data[,"profile"],mean)
my.sd1<-tapply(data[,"total.flightkm"],data[,"profile"],sd)
table(data[,"profile"])
my.nr1<-as.vector(table(data[,"profile"]))

my.se1<-my.sd1/sqrt(my.nr1)

my.df1<-data.frame(groups=factor(names(my.mean1),labels =c("cosmopolitan","mainstream","settled")),
                   mean=my.mean1,
                   sd=my.sd1,
                   se=my.se1)
my.df1

# start with barplot
my.box1 <-ggplot(my.df1 ,aes(x=groups, y=mean)) + 
  lims(y=c(0,30000)) +
  geom_bar(stat="identity", aes(x=groups, y=mean),color="white",size=0.1,
           width = 0.6, fill=c("steelblue3")) + 
  theme(
        axis.text.x = element_text(colour="black", size =12),
        axis.text.y = element_text(colour="black", size =10), 
        axis.title.x = element_text( colour="black", size=15),
        axis.title.y = element_text( colour="black", size=15))+
  labs(x="Profile",y="Total flight kilometers taken and planned")
my.box1

# add errorbars / Standardfehler
graph <- my.box1 +
  geom_errorbar(aes(ymin= mean - my.se1, ymax = mean + my.se1),
                width=.1)
graph


#--------- ANOVA betw. language sub samples---------

UV = as.factor(data$LANGUAGE)

AV =VFR
AV =GSIS
AV =dilemma
AV =negative_effects


summary(aov(AV~UV))
tapply(AV, UV, mean, na.rm = T)
pairwise.t.test(AV,UV, p.adj="bonferroni")

# --------------- SEM ------------------------------

# work and education & political and social engagement --> business

flyer_cathegory
values <- c(1, 0, 0, 0, 0, 1)
index <- c(5, 6, 7, 8, 9, 10)
data$flyer_cathegory_business  <- values[match(data$flyer_cathegory, index)]
data$flyer_cathegory_business 

detach(data)
attach(data)

# standardize variables first (to allow for comparable beta coefficients)
data$sc.attitude_fly <- scale(attitude_fly)
data$sc.offsets <- scale(offsets)
data$sc.dilemma <- scale(dilemma)
data$sc.problem_perception <- scale(problem_perception)
data$sc.environmental_attitude<- scale(environmental_attitude)
data$sc.total.flightkm<- scale(total.flightkm)
data$sc.CIS<- scale(CIS)
data$sc.image_travel<- scale(image_travel)
data$sc.social_media <- scale(social_media)
data$sc.avoiding_problems <- scale(avoiding_problems)
data$sc.age <- scale(age)
data$sc.control<- scale(control)
data$sc.fear<- scale(fear)
data$sc.finance_barrier<- scale(finance_barrier)
data$sc.openness_modes<- scale(openness_modes)
data$sc.negative_effects<- scale(negative_effects)
data$sc.VFR<- scale(VFR)
data$sc.GSIS <- scale(GSIS)
data$sc.studied_abroad<- scale(studied_abroad)
data$sc.flyer_cathegory<- scale(flyer_cathegory)
data$sc.social_norm<- scale(social_norm)
data$sc.desire <- scale(desire)
data$sc.voluntariness <- scale(voluntariness)
data$sc.flyer_cathegory_business <- scale(flyer_cathegory_business )

detach(data)
attach(data)


##### 1. Eco-dilemma model

mod1.txt <- '
sc.offsets ~ sc.dilemma
sc.dilemma ~ sc.problem_perception 
sc.problem_perception ~ sc.environmental_attitude
' 

#moderation
summary(lm(sc.dilemma ~ sc.problem_perception * sc.total.flightkm))


mod1.fit <-sem(mod1.txt, data=data, se="bootstrap")
summary(mod1.fit)

semPaths(mod1.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
         sizeMan = 18, edge.label.cex = 2.2, residuals = FALSE, color = "green",
         nodeLabels = c("offsets","dilemma","problem","environment"))

parameterEstimates(mod1.fit, ci=TRUE, boot.ci.type="bca.simple") 


##### 2. Cosmo-identity model

mod2.txt <- '
sc.total.flightkm ~ sc.CIS
sc.CIS ~ sc.image_travel + sc.social_media + sc.avoiding_problems + sc.age 
' 

mod2.fit <-sem(mod2.txt, data=data, se="bootstrap")
summary(mod2.fit)

semPaths(mod2.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
sizeMan = 10, edge.label.cex = 1, residuals = FALSE, color = "lightblue",
nodeLabels = c("flights","CIS","image_travel","image_env", "media", "no_problems"))

parameterEstimates(mod2.fit, ci=TRUE, boot.ci.type="bca.simple") 


##### 3. TBC model

mod3.txt <- '
sc.total.flightkm ~ sc.control + sc.attitude_fly + sc.social_norm + sc.desire + sc.fear + sc.finance_barrier
sc.control ~ sc.openness_modes 
sc.desire ~ sc.CIS
'
              
mod3.fit <-sem(mod3.txt, data=data, se="bootstrap")
summary(mod3.fit)

semPaths(mod3.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
sizeMan = 7, edge.label.cex = 1, residuals = FALSE, color = "lightblue",
nodeLabels = c("flights","control","attitude","sub_norm", "desire", "fear", "finance",
               "openness", "CIS"))

parameterEstimates(mod3.fit, ci=TRUE, boot.ci.type="bca.simple") 

mean(fear)
sd(fear)

##### 4. Negative business model


mod4.txt <- '
sc.negative_effects ~ sc.voluntariness
sc.voluntariness ~ sc.flyer_cathegory_business
'

mod4.fit <-sem(mod4.txt, data=data, se="bootstrap")
summary(mod4.fit)

semPaths(mod4.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
sizeMan = 18, edge.label.cex = 2.2, residuals = FALSE, color = "purple",
nodeLabels = c("negative","voluntariness","business"))

parameterEstimates(mod4.fit, ci=TRUE, boot.ci.type="bca.simple") 

# Differences in voluntariness
tapply(voluntariness, flyer_cathegory_business, mean, na.rm = T)
tapply(voluntariness, flyer_cathegory_business, sd, na.rm = T)
summary(aov(voluntariness~flyer_cathegory_business))


##### 5. Social network model

mod5.txt <- '
sc.total.flightkm ~ sc.VFR
sc.VFR ~ sc.GSIS 
sc.GSIS ~ sc.studied_abroad
'

mod5.fit <-sem(mod5.txt, data=data, se="bootstrap")
summary(mod5.fit)

semPaths(mod5.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
         sizeMan = 10, edge.label.cex = 1, residuals = FALSE, color = "pink",
         nodeLabels = c("flights","VFR","GSIS","abroad"))

parameterEstimates(mod5.fit, ci=TRUE, boot.ci.type="bca.simple") 




# ----------------- save data ------------------------

save(data, file = "data_fly_id_end.RData")
write.csv(data, file = "data_fly_id_end.csv")



# ------------------- t-test between study one and two --------

# run flyer_dilemma_CA.R script without first lines (rm lists ...)

t.test(data$environmental_attitude, data_fly$env_att, alternative = "two.sided", var.equal = FALSE)

t.test(data$dilemma, data_fly$dilemma, alternative = "two.sided", var.equal = FALSE)


