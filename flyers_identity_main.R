####### FLYER'S IDENTITY ######

setwd("~/R")
rm(list = ls())

# -------------- packages --------------------------

library(ggplot2) # graphics
library(lavaan) # CFA
library(Hmisc) # correlation matrix 
library(qgraph) # gaussian graphical model
library(relaimpo) # estimation of relative importance 



# ------------- load data --------------------------

# load R prep data 
load("~/R/data_fly_id.RData")
#View(data) # name = data 
attach(data)

# load Phython prep data



# ------------- descriptives ----------------------

# DEMOGRAPHY

# Age

mean(age)
sd(age)
min(age)
max(age)

# Gender
table(gender) # 2 = male

# Education 
table(education)

# Financial Situation
table(finance)

# Relationship 
table(relationship)

# OTHER NOMINAL ANSWERS 

table(flyer_cathegory)
hist(less_flights)
list(major)
table(studied_abroad)
table(lived_abroad)
table(country)
list(city)

# FLIGHTS 

# NETWORK

# environmental attitude 
mean(environmental_attitude)
sd(environmental_attitude)

ggplot(data,aes(environmental_attitude)) + 
  geom_histogram(binwidth = 1, color = 'white', fill = 'aquamarine4') +
  labs(title = "environmental attitude", x = "environmental actions (out of 15)", y = "freqency") 

# dilemma
mean(dilemma)
sd(dilemma)

ggplot(data,aes(dilemma)) + 
  geom_histogram(binwidth = 5, color = 'white', fill = 'goldenrod') +
  labs(title = "dilemma - perceived mismatch", x = "extend of agreement", y = "freqency")


# ------------- CFA CIS ----------------------------

CIS <- data.frame(IV02_01,IV02_02,IV02_03,IV02_05,IV02_06,IV02_07,IV02_08,IV02_09,
                  IV02_10,IV02_11,IV02_12,IV02_16,IV02_17,IV02_18,IV02_19,IV02_20,
                  IV02_22,IV02_23,IV02_26,IV02_27,IV02_28,IV02_31,IV02_13,IV02_14,
                  IV02_15)

#EFA
PFX <- fa.parallel(CIS,fa="fa") # 5 factors
fit <- princomp(CIS, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

PFA3 <- fa(CIS,4) # force 4 factor solution
PFA3
fa.diagram(PFA3)

PFA4 <- fa(CIS,5) 
PFA4
fa.diagram(PFA4)

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
f4 =~ IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_31
"
fit3 <- cfa(cosmo_cfa_empirical_4, data = data)
summary(fit3, fit.measures=TRUE)
fitMeasures(fit3, fit.measures = c("chisq", "cfi", "rmsea", "aic"))

# --------> build scale + subscales! (based on theoretical 4 factor model)

data$CIS <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20+ # CHI
        IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02+IV02_31+ # pessure / emotional norm
        IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_01+  # prof norm
        IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_11)/25 # social norm
data$social_norm <- (IV02_15+IV02_03+IV02_12+IV02_14+IV02_13+IV02_05+IV02_11)/7
data$prof_norm <- (IV02_06+IV02_07+IV02_08+IV02_09+IV02_10+IV02_01)/6
data$CHI <- (IV02_16+IV02_17+IV02_18+IV02_19+IV02_20)/5
data$social_pressure <- (IV02_28+IV02_22+IV02_27+IV02_23+IV02_26+IV02_02+IV02_31)/7

detach(data)
attach(data)

# --------------- correlation table ---------------

#--------> include flights, voluntariness

korr_data_pretty <- data.frame(
  attitude_fly,
  social_norm,
  prof_norm,
  CHI,
  VFR,
  fear,
  desire,
  less_flights,
  flyer_cathegory,
  studied_abroad,
  lived_abroad,
  social_pressure,
  problem_perception,
  dilemma,
  success,
  social_media,
  social_status,
  control,
  negative_effects,
  openness_alternatives,
  openness_modes,
  openness_skype,
  openness_video_conference,
  avoiding_problems,
  image_travel,
  image_environment,
  finance_barrier, 
  desire,
  age,
  environmental_attitude,
  finance, 
  relationship,
  education, 
  gender
) 


rcorr(as.matrix(korr_data_pretty),type = "pearson")
sjt.corr(korr_data_pretty) 
cor.plot(korr_data_pretty, main = "Correlation Plot - total",
         cex = 0.6, numbers = TRUE, stars = TRUE, cex.axis = 0.8)

# GGM
fly_cors <- cor_auto(korr_data_pretty, detectOrdinal = FALSE)

FlyingGraph <- qgraph(cor(korr_data_pretty),minimum=0.25,
                      legend=TRUE,borders=FALSE, title = "Flying Partial Correlations")

# Same graphs with spring layout:
FlyingGraph <- qgraph(cor(korr_data_pretty),
                      legend=TRUE,borders=FALSE, title = "Flying Correlations",layout="spring")


# -------------- latent class analysis // dependent mixture models ------------

# one group
install.packages("depmixS4")
library(depmixS4)

mod <- mix(list(emissions~1,attitude~1,awareness~1,dilemma~1,global~1,desire~1,environment~1,age~1,gender~1), data=data_fly, nstates=3,
           family=list(gaussian(),gaussian(),gaussian(),gaussian(), gaussian(),gaussian(),gaussian(),gaussian(),multinomial()),
           respstart=runif(57))

fmod<-fit(mod)
summary(fmod)
fmod@response

# two groups

# three groups 

# ...

# ---------------> define factor: class 



# -------------- ANOVA / t-test betw. groups ------

# testing correlations
cor.test(past_e, global)

#t-Test (mit Welch-Korrektur):
t.test(x$Frauen,x$Maenner)
Daten = c(1,2,4,3,4,2,3,0,2,5,4,0,2,1,2,3,1,4,1,3)
Gruppierung = c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)
t.test (Daten~Gruppierung)
t.test (Daten~Gruppierung,var.equal=T) # ohne Welch-Korrektur

# ANOVA univariate
#Variablen definieren:
AV = x$Matheleistung
UV = as.factor(x$Schulform)

#Optional: Bennenen der Faktorstufen:
UV = factor(UV, label=c("Hauptschule","Realschule","Gymnasium"))
model.tables(aov(AV~UV),"means")
summary(aov(AV~UV))

pairwise.t.test(AV,UV, p.adj="bonferroni")

# ANOVA multivariate 
AV = x$Fahrfehler
Faktor.A.Musik = as.factor(x$Musik)
Faktor.B.Teststrecke = as.factor(x$Teststrecke)

ANOVA =aov(AV~Faktor.A.Musik*Faktor.B.Teststrecke) 
summary(ANOVA)

# BARPLOT

library(ggplot2)

# how to use means and error bars
my.mean1<-tapply(NnaD[,"environment_sc"],NnaD[,"SP02"],mean,na.rm=T)
my.median1<-tapply(NnaD[,"environment_sc"],NnaD[,"SP02"],median,na.rm=T)
my.sd1<-tapply(NnaD[,"environment_sc"],NnaD[,"SP02"],sd,na.rm=T)

table(NnaD[,"SP02"])
my.sub1<-subset(NnaD,is.na(environment_sc)==F)
table(my.sub1[,"SP02"])
table(subset(NnaD, is.na(environment_sc)==F)[,"SP02"])

my.nr1<-as.vector(table(my.sub1[,"SP02"]))

my.se1<-my.sd1/sqrt(my.nr1)
my.se1

my.df1<-data.frame(groups=factor(names(my.mean1),labels =c("indoor","field","outdoor")),
                   mean=my.mean1,
                   median=my.median1,
                   sd=my.sd1,
                   se=my.se1)
my.df1

# start with barplot
my.box1<-ggplot(my.df1,aes(x=groups,y=mean)) + 
  lims(y=c(0,4.5)) +
  geom_bar(stat="identity", aes(fill=groups),color="white",size=0.1,
           width = 0.6, fill=c("grey")) + 
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        axis.text.x = element_text(colour="black", size =15),
        axis.text.y = element_text(colour="black", size =15), 
        axis.title.x = element_text( colour="black", size=18),
        axis.title.y = element_text( colour="black", size=18),
        panel.grid.major.y = element_line(colour="grey55"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  labs(x="location of sport",y="pro-environmental behavior")
my.box1

# add errorbars / Standardfehler
sport.graph <- my.box1 +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.2)
sport.graph

colours()

## anderer Graph

library(sciplot)
bargraph.CI(x.factor=NnaD$SP02,
            response=NnaD$environment_sc ,
            ylim=c(3,4),
            axes=FALSE, 
            xlab= 'location of sport activity', 
            ylab='pro-environmental behavior',
            legend=TRUE, names.arg = c("indoor", "field", "outdoor"))
axis(2,at=c(3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8),
     labels=c(0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8))



# ------------- Regression models ------------------

reg.1 <- lm(fictive_e ~ attitude + sub_norm + control + desire)
summary(reg.1)

# nice plot 
contacts.plot <- ggplot(data_fly, mapping=aes(x=NE01_01,y=NE01_03)) +
  labs(x="friends and family members abroad",y="international business contacts") +
  geom_point(color="dark blue", size=2) +
  geom_smooth(method=lm)
contacts.plot


# --------------- SEM ------------------------------

med11.txt <- 'fictive_e ~ attitude + sub_norm + control + desire'

med11.fit<- sem(med11.txt,data=korr_data_small, se="bootstrap")
summary(med11.fit)

library(semPlot) 

semPaths(med11.fit, whatLabels="est", layout="spring", layoutSplit=TRUE, rotation=2,
         sizeMan = 18, edge.label.cex = 2.2, residuals = FALSE, color = "indianred",
         nodeLabels = c("adrenalin","politics","openness"))

parameterEstimates(med11.fit, ci=TRUE, boot.ci.type="bca.simple") 


