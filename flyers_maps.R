
####### MA - MAPS #####

setwd("~/R")
rm(list = ls())

# ---------------------- packages -----------------

library(readr)
library(maptools)
library(maps)
library(tidyverse)
library(ggmap)


#------------------------- Data ---------------------------------

# google API key
register_google(key = "")


# load python csv documents

network_data <- read_csv("~/R/data/network_data_final.csv")
flight_data <- read_csv("~/R/data/flight_data_final_smart.csv", na = "0")


#--------------------------- Flight Maps --------------------------------

# destinations 

View(flight_data)

attach(flight_data)

visited <- c(destination, destination_1, destination_2, destination_3
                 , destination_4, destination_5, destination_6, destination_7, destination_8
                 , destination_9, destination_10, destination_11, destination_12, destination_13
                 , destination_14, destination_15, destination_16, destination_17, destination_18,
                 destination_19, destination_20, destination_21, destination_22, destination_23
                 , destination_24, destination_25, destination_26, destination_27, destination_28
                 , destination_29, destination_30, destination_31, destination_32, destination_33
                 , destination_34, destination_35, destination_36, destination_37, destination_38,
                 destination_39, destination_40, destination_41, destination_42, destination_43
                 , destination_44, destination_45)

# test
#visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury, SA")

#ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat


#Using GGPLOT, plot the Base World Map

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld 

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="orange1", size=3) 
mp



#--------------------------- Flight Maps --------------------------------

# destinations 

View(network_data)

attach(network_data)

visitednet <- na.omit(c(destination1, destination2, destination3
             , destination4, destination5, destination6, destination7, destination8
             , destination9, destination10, destination11, destination12, destination13))

ll.visited <- geocode(visitednet)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat


#Using GGPLOT, plot the Base World Map

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="orange2", size=2) 
mp


