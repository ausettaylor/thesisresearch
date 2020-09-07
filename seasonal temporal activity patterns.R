# install necessary packages from R
install.packages('overlap')
install.packages('lubridate')
install.packages('tidyverse')
#call in the necessary libraries needed for this R session 
library(overlap)
library(lubridate)
library(mgcv)
library(plyr)
library(dplyr)

#set working directory to the folder with the metadata csv file
setwd("~/Desktop/thesis")

#read in the data 
data <- read.csv("thesis_data.csv")

#start data analysis with first species: Baboon separate pre/post wild dog & season data
#separate the seasons by: wet, early dry, and late dry since the dry season begins with leftover vegetation from wet season then becomes
#much more barren
#for pre-release I'm using .b to indicate "before" wild dogs, for post-release I'm using .a to indicate "after" wild dogs 

#Baboon activity for the wet season pre-wild dog release
baboon_wet.b <- data[data$season == "Wet" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_wet.b <- baboon_wet.b$Time.Radians
#plot the temporal activity of baboons in the wet season pre wild dog release
densityPlot(plot_baboon_wet.b, rug=TRUE, main ="Baboon Activity in the Wet Season Pre Wild Dog Release")


#Baboon activity for the early dry season pre-wild dog release
baboon_early_dry.b <- data[data$season == "Early Dry" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_early_dry.b <- baboon_early_dry.b$Time.Radians
#plot the temporal activity of baboons in the early dry season pre wild dog release
densityPlot(plot_baboon_early_dry.b, rug=TRUE, main = "Baboon Activity in the Early Dry Season Pre Wild Dog Release")

#Baboon activity for the late dry season pre-wild dog release
baboon_late_dry.b <- data[data$season == "Late Dry" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the TimeRadians column to use in the temporal activity plot
plot_baboon_late_dry.b <- baboon_late_dry.b$Time.Radians
#plot the temporal activity of baboons in the late dry season pre wild dog release
densityPlot(plot_baboon_late_dry.b, rug=TRUE, main = "Baboon Activity in the Late Dry Season")


#Baboon activity for the wet season post-wild dog release
baboon_wet.a <- data[data$season == "Wet" &
                     data$TimePeriod == "PostRelease" & 
                     data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_wet.a <- baboon_wet.a$Time.Radians
#plot the temporal activity of baboons in the wet season post wild dog release
densityPlot(plot_baboon_wet.a, rug = TRUE, main = "Baboon Activity in the Wet Season Post Wild Dog Release")

#Baboon activity for the early dry season post-wild dog release
baboon_early_dry.a <- data[data$season == "Early Dry" &
                           data$TimePeriod == "PostRelease" & 
                           data$Species == "Baboon",]

plot_baboon_early_dry.a <- baboon_early_dry.a$Time.Radians
#plot the temporal activity of baboons in the early dry season post wild dog release
densityPlot(plot_baboon_early_dry.a, rug = TRUE, main = "Baboon Activity in the Early Dry Season Post Wild Dog Release")

#Baboon activity for the late dry season post-wild dog release
baboon_late_dry.a <- data[data$season == "Late Dry" &
                          data$TimePeriod == "PostRelease" & 
                          data$Species == "Baboon",]

plot_baboon_late_dry.a <- baboon_late_dry.a$Time.Radians
#plot the temporal activity of baboons in the late dry season post wild dog release
densityPlot(plot_baboon_late_dry.a, rug = TRUE, main = "Baboon Activity in the Late Dry Season Post Wild Dog Release")

#####################################
#do the same for Civets

#Civet activity for the wet season pre-wild dog release
civet_wet.b <- data[data$season == "Wet" &
                       data$TimePeriod == "PreRelease" & 
                       data$Species == "Civet",]
#pick the Time Radians column to use in the temporal activity plot
plot_civet_wet.b <- civet_wet.b$Time.Radians
#plot the temporal activity of civets in the wet season pre wild dog release
densityPlot(plot_civet_wet.b, rug=TRUE, main ="Civet Activity in the Wet Season Pre Wild Dog Release")


#civet activity for the early dry season pre-wild dog release
civet_early_dry.b <- data[data$season == "Early Dry" &
                             data$TimePeriod == "PreRelease" & 
                             data$Species == "Civet",]
#pick the Time Radians column to use in the temporal activity plot
plot_civet_early_dry.b <- civet_early_dry.b$Time.Radians
#plot the temporal activity of civets in the early dry season pre wild dog release
densityPlot(plot_civet_early_dry.b, rug=TRUE, main = "Civet Activity in the Early Dry Season Pre Wild Dog Release")

#Civet activity for the late dry season pre-wild dog release
civet_late_dry.b <- data[data$season == "Late Dry" &
                            data$TimePeriod == "PreRelease" & 
                            data$Species == "Civet",]
#pick the TimeRadians column to use in the temporal activity plot
plot_civet_late_dry.b <- civet_late_dry.b$Time.Radians
#plot the temporal activity of civet in the late dry season pre wild dog release
densityPlot(plot_civet_late_dry.b, rug=TRUE, main = "Civet Activity in the Late Dry Season")


#Civet activity for the wet season post-wild dog release
civet_wet.a <- data[data$season == "Wet" &
                       data$TimePeriod == "PostRelease" & 
                       data$Species == "Civet",]
#pick the Time Radians column to use in the temporal activity plot
plot_civet_wet.a <- civet_wet.a$Time.Radians
#plot the temporal activity of civets in the wet season post wild dog release
densityPlot(plot_civet_wet.a, rug = TRUE, main = "Civet Activity in the Wet Season Post Wild Dog Release")

#Civet activity for the early dry season post-wild dog release
civet_early_dry.a <- data[data$season == "Early Dry" &
                             data$TimePeriod == "PostRelease" & 
                             data$Species == "Civet",]

plot_civet_early_dry.a <- civet_early_dry.a$Time.Radians
#plot the temporal activity of civets in the early dry season post wild dog release
densityPlot(plot_civet_early_dry.a, rug = TRUE, main = "Civet Activity in the Early Dry Season Post Wild Dog Release")

#Civet activity for the late dry season post-wild dog release
civet_late_dry.a <- data[data$season == "Late Dry" &
                            data$TimePeriod == "PostRelease" & 
                            data$Species == "Civet",]

plot_civet_late_dry.a <- civet_late_dry.a$Time.Radians
#plot the temporal activity of civets in the late dry season post wild dog release
densityPlot(plot_civet_late_dry.a, rug = TRUE, main = "Civet Activity in the Late Dry Season Post Wild Dog Release")

##################################
#do the same for Genets

#Genet activity for the wet season pre-wild dog release
genet_wet.b <- data[data$season == "Wet" &
                      data$TimePeriod == "PreRelease" & 
                      data$Species == "Genet",]
#pick the Time Radians column to use in the temporal activity plot
plot_genet_wet.b <- genet_wet.b$Time.Radians
#plot the temporal activity of genets in the wet season pre wild dog release
densityPlot(plot_genet_wet.b, rug=TRUE, main ="Genet Activity in the Wet Season Pre Wild Dog Release")


#genet activity for the early dry season pre-wild dog release
genet_early_dry.b <- data[data$season == "Early Dry" &
                            data$TimePeriod == "PreRelease" & 
                            data$Species == "Genet",]
#pick the Time Radians column to use in the temporal activity plot
plot_genet_early_dry.b <- genet_early_dry.b$Time.Radians
#plot the temporal activity of genets in the early dry season pre wild dog release
densityPlot(plot_genet_early_dry.b, rug=TRUE, main = "Genet Activity in the Early Dry Season Pre Wild Dog Release")

#Genet activity for the late dry season pre-wild dog release
genet_late_dry.b <- data[data$season == "Late Dry" &
                           data$TimePeriod == "PreRelease" & 
                           data$Species == "Genet",]
#pick the TimeRadians column to use in the temporal activity plot
plot_genet_late_dry.b <- genet_late_dry.b$Time.Radians
#plot the temporal activity of genet in the late dry season pre wild dog release
densityPlot(plot_genet_late_dry.b, rug=TRUE, main = "Genet Activity in the Late Dry Season")


#Genet activity for the wet season post-wild dog release
genet_wet.a <- data[data$season == "Wet" &
                      data$TimePeriod == "PostRelease" & 
                      data$Species == "Genet",]
#pick the Time Radians column to use in the temporal activity plot
plot_genet_wet.a <- genet_wet.a$Time.Radians
#plot the temporal activity of genets in the wet season post wild dog release
densityPlot(plot_genet_wet.a, rug = TRUE, main = "Genet Activity in the Wet Season Post Wild Dog Release")

#genet activity for the early dry season post-wild dog release
genet_early_dry.a <- data[data$season == "Early Dry" &
                            data$TimePeriod == "PostRelease" & 
                            data$Species == "Genet",]

plot_genet_early_dry.a <- genet_early_dry.a$Time.Radians
#plot the temporal activity of genets in the early dry season post wild dog release
densityPlot(plot_genet_early_dry.a, rug = TRUE, main = "Genet Activity in the Early Dry Season Post Wild Dog Release")

#Genet activity for the late dry season post-wild dog release
genet_late_dry.a <- data[data$season == "Late Dry" &
                           data$TimePeriod == "PostRelease" & 
                           data$Species == "Genet",]

plot_genet_late_dry.a <- genet_late_dry.a$Time.Radians
#plot the temporal activity of genets in the late dry season post wild dog release
densityPlot(plot_genet_late_dry.a, rug = TRUE, main = "Genet Activity in the Late Dry Season Post Wild Dog Release")

