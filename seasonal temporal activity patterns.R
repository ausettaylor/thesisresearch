# install necessary packages from R
install.packages('overlap')
install.packages('lubridate')
install.packages('tidyverse')
install.packages('activity')
install.packages('CRAN')
#call in the necessary libraries needed for this R session 
library(overlap)
library(lubridate)
library(mgcv)
library(plyr)
library(dplyr)
library(activity)

#set working directory to the folder with the metadata csv file
setwd("~/Desktop/thesis")

#read in the data 
data <- read.csv("thesis_data.csv")

#start data analysis with first species: Baboon separate pre/post wild dog & season data
#separate the seasons by: wet, early dry, and late dry since the dry season begins with leftover vegetation from wet season then becomes
#much more barren
#for pre-release I'm using .b to indicate "before" wild dogs, for post-release I'm using .a to indicate "after" wild dogs 

#these are the minimum and maximum y axis values for the graphs below
ymin <- 0 
ymax <- 0.12

#this turns on the graphing device to save automatically
png("baboonactivity.png", width = 300, height = 700)

#this helps save the defalt parameters for the graphs
def.par <- par(no.readonly = TRUE)

par(mfrow = c(3,2))

#Baboon activity for the wet season pre-wild dog release
baboon_wet.b <- data[data$season == "Wet" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_wet.b <- baboon_wet.b$Time.Radians
#plot the temporal activity of baboons in the wet season pre wild dog release
densityPlot(plot_baboon_wet.b, rug=TRUE, main ="Baboon Activity in the Wet Season Pre Wild Dog Release", ylim = c(ymin, ymax))

fit.bwb <- fitact(plot_baboon_wet.b, wt=NULL, reps = 1000)

#Baboon activity for the wet season post-wild dog release
baboon_wet.a <- data[data$season == "Wet" &
                             data$TimePeriod == "PostRelease" & 
                             data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_wet.a <- baboon_wet.a$Time.Radians
#plot the temporal activity of baboons in the wet season post wild dog release
densityPlot(plot_baboon_wet.a, rug = TRUE, main = "Baboon Activity in the Wet Season Post Wild Dog Release", ylim = c(ymin, ymax))

fit.bwa <- fitact(plot_baboon_wet.a, wt=NULL, reps = 1000)

#Baboon activity for the early dry season pre-wild dog release
baboon_early_dry.b <- data[data$season == "Early Dry" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the Time Radians column to use in the temporal activity plot
plot_baboon_early_dry.b <- baboon_early_dry.b$Time.Radians
#plot the temporal activity of baboons in the early dry season pre wild dog release
densityPlot(plot_baboon_early_dry.b, rug=TRUE, main = "Baboon Activity in the Early Dry Season Pre Wild Dog Release", ylim = c(ymin, ymax))

fit.beb <- fitact(plot_baboon_early_dry.b, reps = 1000)

#Baboon activity for the early dry season post-wild dog release
baboon_early_dry.a <- data[data$season == "Early Dry" &
                                   data$TimePeriod == "PostRelease" & 
                                   data$Species == "Baboon",]

plot_baboon_early_dry.a <- baboon_early_dry.a$Time.Radians
#plot the temporal activity of baboons in the early dry season post wild dog release
densityPlot(plot_baboon_early_dry.a, rug = TRUE, main = "Baboon Activity in the Early Dry Season Post Wild Dog Release", ylim = c(ymin, ymax))

fit.bea <- fitact(plot_baboon_early_dry.a, reps = 1000)

#Baboon activity for the late dry season pre-wild dog release
baboon_late_dry.b <- data[data$season == "Late Dry" &
                     data$TimePeriod == "PreRelease" & 
                     data$Species == "Baboon",]
#pick the TimeRadians column to use in the temporal activity plot
plot_baboon_late_dry.b <- baboon_late_dry.b$Time.Radians
#plot the temporal activity of baboons in the late dry season pre wild dog release
densityPlot(plot_baboon_late_dry.b, rug=TRUE, main = "Baboon Activity in the Late Dry Season Before Wild Dog Release", ylim = c(ymin, ymax))

fit.blb <- fitact(plot_baboon_late_dry.b, reps = 1000)

#Baboon activity for the late dry season post-wild dog release
baboon_late_dry.a <- data[data$season == "Late Dry" &
                          data$TimePeriod == "PostRelease" & 
                          data$Species == "Baboon",]

plot_baboon_late_dry.a <- baboon_late_dry.a$Time.Radians
#plot the temporal activity of baboons in the late dry season post wild dog release
densityPlot(plot_baboon_late_dry.a, rug = TRUE, main = "Baboon Activity in the Late Dry Season Post Wild Dog Release", ylim = c(ymin, ymax))

fit.bla <- fitact(plot_baboon_late_dry.a, reps = 1000)

#turn off graphics device
dev.off()
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

fit.cwb <- fitact(plot_civet_wet.b, reps = 1000)

#civet activity for the early dry season pre-wild dog release
civet_early_dry.b <- data[data$season == "Early Dry" &
                             data$TimePeriod == "PreRelease" & 
                             data$Species == "Civet",]
#pick the Time Radians column to use in the temporal activity plot
plot_civet_early_dry.b <- civet_early_dry.b$Time.Radians
#plot the temporal activity of civets in the early dry season pre wild dog release
densityPlot(plot_civet_early_dry.b, rug=TRUE, main = "Civet Activity in the Early Dry Season Pre Wild Dog Release")

fit.ceb <- fitact(plot_civet_early_dry.b, reps = 1000)

#Civet activity for the late dry season pre-wild dog release
civet_late_dry.b <- data[data$season == "Late Dry" &
                            data$TimePeriod == "PreRelease" & 
                            data$Species == "Civet",]
#pick the TimeRadians column to use in the temporal activity plot
plot_civet_late_dry.b <- civet_late_dry.b$Time.Radians
#plot the temporal activity of civet in the late dry season pre wild dog release
densityPlot(plot_civet_late_dry.b, rug=TRUE, main = "Civet Activity in the Late Dry Season Pre Wild Dog Release")

fit.clb <- fitact(plot_civet_late_dry.b, reps = 1000)

#Civet activity for the wet season post-wild dog release
civet_wet.a <- data[data$season == "Wet" &
                       data$TimePeriod == "PostRelease" & 
                       data$Species == "Civet",]
#pick the Time Radians column to use in the temporal activity plot
plot_civet_wet.a <- civet_wet.a$Time.Radians
#plot the temporal activity of civets in the wet season post wild dog release
densityPlot(plot_civet_wet.a, rug = TRUE, main = "Civet Activity in the Wet Season Post Wild Dog Release")

fit.cwa <- fitact(plot_civet_wet.a, reps = 1000)

#Civet activity for the early dry season post-wild dog release
civet_early_dry.a <- data[data$season == "Early Dry" &
                             data$TimePeriod == "PostRelease" & 
                             data$Species == "Civet",]

plot_civet_early_dry.a <- civet_early_dry.a$Time.Radians
#plot the temporal activity of civets in the early dry season post wild dog release
densityPlot(plot_civet_early_dry.a, rug = TRUE, main = "Civet Activity in the Early Dry Season Post Wild Dog Release")

fit.cea <- fitact(plot_civet_early_dry.a, reps = 1000)

#Civet activity for the late dry season post-wild dog release
civet_late_dry.a <- data[data$season == "Late Dry" &
                            data$TimePeriod == "PostRelease" & 
                            data$Species == "Civet",]

plot_civet_late_dry.a <- civet_late_dry.a$Time.Radians
#plot the temporal activity of civets in the late dry season post wild dog release
densityPlot(plot_civet_late_dry.a, rug = TRUE, main = "Civet Activity in the Late Dry Season Post Wild Dog Release")

fit.cla <- fitact(plot_civet_late_dry.a, reps = 1000)

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

fit.gwb <- fitact(plot_genet_wet.b, reps = 1000)

#genet activity for the early dry season pre-wild dog release
genet_early_dry.b <- data[data$season == "Early Dry" &
                            data$TimePeriod == "PreRelease" & 
                            data$Species == "Genet",]
#pick the Time Radians column to use in the temporal activity plot
plot_genet_early_dry.b <- genet_early_dry.b$Time.Radians
#plot the temporal activity of genets in the early dry season pre wild dog release
densityPlot(plot_genet_early_dry.b, rug=TRUE, main = "Genet Activity in the Early Dry Season Pre Wild Dog Release")

fit.geb <- fitact(plot_genet_early_dry.b, reps = 1000)

#Genet activity for the late dry season pre-wild dog release
genet_late_dry.b <- data[data$season == "Late Dry" &
                           data$TimePeriod == "PreRelease" & 
                           data$Species == "Genet",]
#pick the TimeRadians column to use in the temporal activity plot
plot_genet_late_dry.b <- genet_late_dry.b$Time.Radians
#plot the temporal activity of genet in the late dry season pre wild dog release
densityPlot(plot_genet_late_dry.b, rug=TRUE, main = "Genet Activity in the Late Dry Season")

fit.glb <- fitact(plot_genet_late_dry.b, reps = 1000)

#Genet activity for the wet season post-wild dog release
genet_wet.a <- data[data$season == "Wet" &
                      data$TimePeriod == "PostRelease" & 
                      data$Species == "Genet",]
#pick the Time Radians column to use in the temporal activity plot
plot_genet_wet.a <- genet_wet.a$Time.Radians
#plot the temporal activity of genets in the wet season post wild dog release
densityPlot(plot_genet_wet.a, rug = TRUE, main = "Genet Activity in the Wet Season Post Wild Dog Release")

fit.gwa <- fitact(plot_genet_wet.a, reps = 1000)

#genet activity for the early dry season post-wild dog release
genet_early_dry.a <- data[data$season == "Early Dry" &
                            data$TimePeriod == "PostRelease" & 
                            data$Species == "Genet",]

plot_genet_early_dry.a <- genet_early_dry.a$Time.Radians
#plot the temporal activity of genets in the early dry season post wild dog release
densityPlot(plot_genet_early_dry.a, rug = TRUE, main = "Genet Activity in the Early Dry Season Post Wild Dog Release")

fit.gea <- fitact(plot_genet_early_dry.a, reps = 1000)

#Genet activity for the late dry season post-wild dog release
genet_late_dry.a <- data[data$season == "Late Dry" &
                           data$TimePeriod == "PostRelease" & 
                           data$Species == "Genet",]

plot_genet_late_dry.a <- genet_late_dry.a$Time.Radians
#plot the temporal activity of genets in the late dry season post wild dog release
densityPlot(plot_genet_late_dry.a, rug = TRUE, main = "Genet Activity in the Late Dry Season Post Wild Dog Release")

fit.gla <- fitact(plot_genet_late_dry.a, reps = 1000)

###################################################

#To do comparisons of pre wild dog release and post wild dog release
#will use overlapPlot function to plot them on the same graph
# AWD = African Wild Dog

#these are the minimum and maximum y axis values for the graphs below
ymin <- 0 
ymax <- 0.12

#this turns on the graphing device to save automatically
png("babooncompactivity.png", width = 300, height = 700)

#this helps save the defalt parameters for the graphs
def.par <- par(no.readonly = TRUE)

par(mfrow = c(3,1))

#Baboons Wet Season comparison pre/post AWD release
overlapPlot(plot_baboon_wet.b, plot_baboon_wet.a, main = "Baboon Wet Season Activity, pre and post AWD Release")
legend('topright', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Calculate the 'Coefficient of Overlap' to see temporal overlap for Baboon wet season
#I'm going to use delta_hat4 estimator because Baboons have thousands of samples
(BaboonWet_Release_Comparison <- overlapEst(plot_baboon_wet.b, plot_baboon_wet.a, type="Dhat4"))
#BaboonWet_Release_Comparison = 0.9321808

bootstrap.bwb <- resample(plot_baboon_wet.b, 1000)
bootstrap.bwa <- resample(plot_baboon_wet.a, 1000)

baboon.wet_boot <- bootEst(bootstrap.bwb, bootstrap.bwa, type = "Dhat4")
(BSmean <- mean(baboon.wet.before_boot))
#0.9325368

compareCkern(fit.bwb, fit.bwa, reps = 999)

#Baboons Early Dry Season comparison pre/post AWD release
overlapPlot(plot_baboon_early_dry.b, plot_baboon_early_dry.a, main = "Baboon Early Dry Season Activity, pre and post AWD Release")
legend('topright', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Baboon early dry season
(BaboonEarlyDry_Release_Comparison <- overlapEst(plot_baboon_early_dry.b, plot_baboon_early_dry.a, type="Dhat4"))
#BaboonEarlyDry_Release_Comparison = 0.9419022

bootstrap.beb <- resample(plot_baboon_early_dry.b, 1000)
bootstrap.bea <- resample(plot_baboon_early_dry.a, 1000)

baboon.edry_boot <- bootEst(bootstrap.beb, bootstrap.bea, type = "Dhat4")
(BSmean <- mean(baboon.edry_boot))
#0.9442342

compareCkern(fit.beb, fit.bea, reps = 999)

#Baboons Late Dry Season comparison pre/post AWD release
overlapPlot(plot_baboon_late_dry.b, plot_baboon_late_dry.a, main = "Baboon Late Dry Season Activity, pre and post AWD Release")
legend('topright', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Baboon late dry season
(BaboonLateDry_Release_Comparison <- overlapEst(plot_baboon_late_dry.b, plot_baboon_late_dry.a, type="Dhat4"))
#BaboonLateDry_Release_Comparison = 0.9646155

bootstrap.blb <- resample(plot_baboon_late_dry.b, 1000)
bootstrap.bla <- resample(plot_baboon_late_dry.a, 1000)

baboon.ldry_boot <- bootEst(bootstrap.blb, bootstrap.bla, type = "Dhat4")
(BSmean <- mean(baboon.ldry_boot))
#0.9569782

compareCkern(fit.blb, fit.bla, reps = 999)

#turn off graphics device
dev.off()
################################################
#these are the minimum and maximum y axis values for the graphs below
ymin <- 0 
ymax <- 0.12

#this turns on the graphing device to save automatically
png("civetacompctivity.png", width = 300, height = 700)

#this helps save the defalt parameters for the graphs
def.par <- par(no.readonly = TRUE)

par(mfrow = c(3,1))
#Civets Wet Season Comparison pre/post AWD release
overlapPlot(plot_civet_wet.b, plot_civet_wet.a, main = "Civet Wet Season Activity, pre and post AWD Release")
legend('topleft', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Civet wet season
(CivetWet_Release_Comparison <- overlapEst(plot_civet_wet.b, plot_civet_wet.a, type="Dhat4"))
#CivetWet_Release_Comparison = 0.8738868

bootstrap.cwb <- resample(plot_civet_wet.b, 1000)
bootstrap.cwa <- resample(plot_civet_wet.a, 1000)

civet.wet_boot <- bootEst(bootstrap.cwb, bootstrap.cwa, type = "Dhat4")
(BSmean <- mean(civet.wet_boot))
#0.8537129

compareCkern(fit.cwb, fit.cwa, reps = 999)

#Civets Early Dry Season Comparison pre/post AWD release
overlapPlot(plot_civet_early_dry.b, plot_civet_early_dry.a, main = "Civet Early Dry Season Activity, pre and post AWD Release")
legend('topleft', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Civet early dry season
(CivetEarlyDry_Release_Comparison <- overlapEst(plot_civet_early_dry.b, plot_civet_early_dry.a, type="Dhat4"))
#CivetEarlyDry_Release_Comparison = 0.9180619

bootstrap.ceb <- resample(plot_civet_early_dry.b, 1000)
bootstrap.cea <- resample(plot_civet_early_dry.a, 1000)

civet.edry_boot <- bootEst(bootstrap.ceb, bootstrap.cea, type = "Dhat4")
(BSmean <- mean(civet.edry_boot))
#0.8701923

compareCkern(fit.ceb, fit.cea, reps = 999)


#Civets Late Dry Season Comparison pre/post AWD release
overlapPlot(plot_civet_late_dry.b, plot_civet_late_dry.a, main = "Civet Late Dry Season Activity, pre and post AWD Release")
legend('topleft', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Civet late dry season
(CivetLateDry_Release_Comparison <- overlapEst(plot_civet_late_dry.b, plot_civet_late_dry.a, type="Dhat4"))
#CivetLateDry_Release_Comparison = 0.9042455

bootstrap.clb <- resample(plot_civet_late_dry.b, 1000)
bootstrap.cla <- resample(plot_civet_late_dry.a, 1000)

civet.ldry_boot <- bootEst(bootstrap.clb, bootstrap.cla, type = "Dhat4")
(BSmean <- mean(civet.ldry_boot))
#0.892224

compareCkern(fit.clb, fit.cla, reps = 999)

#turn off graphics device
dev.off()
####################################
png("genetcompactivity.png", width = 300, height = 700)

#this helps save the defalt parameters for the graphs
def.par <- par(no.readonly = TRUE)

par(mfrow = c(3,1))

#Genets Wet Season comparison pre/post AWD release
overlapPlot(plot_genet_wet.b, plot_genet_wet.a, main = "Genet Wet Season Activity, pre and post AWD Release")
legend('topright', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Genet wet season
(GenetWet_Release_Comparison <- overlapEst(plot_genet_wet.b, plot_genet_wet.a, type="Dhat4"))
#GenetWet_Release_Comparison = 0.8979296

bootstrap.gwb <- resample(plot_genet_wet.b, 1000)
bootstrap.gwa <- resample(plot_genet_wet.a, 1000)

genet.wet_boot <- bootEst(bootstrap.gwb, bootstrap.gwa, type = "Dhat4")
(BSmean <- mean(genet.wet_boot))
#0.8659621

compareCkern(fit.gwb, fit.gwa, reps = 999)

#Genets Early Dry Season Comparison pre/post AWD release
overlapPlot(plot_genet_early_dry.b, plot_genet_early_dry.a, main = "Genet Early Dry Season Activity, pre and post AWD Release")
legend('topleft', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Genet early dry season
(GenetEarlyDry_Release_Comparison <- overlapEst(plot_genet_early_dry.b, plot_genet_early_dry.a, type="Dhat4"))
#GenetEarlyDry_Release_Comparison = 0.9189269

bootstrap.geb <- resample(plot_genet_early_dry.b, 1000)
bootstrap.gea <- resample(plot_genet_early_dry.a, 1000)

genet.edry_boot <- bootEst(bootstrap.geb, bootstrap.gea, type = "Dhat4")
(BSmean <- mean(genet.edry_boot))
#0.9074405

compareCkern(fit.geb, fit.gea, reps = 999)

#Genets Late Dry Season Comparison pre/post AWD release
overlapPlot(plot_genet_late_dry.b, plot_genet_late_dry.a, main = "Genet Late Dry Season Activity, pre and post AWD Release")
legend('topleft', c("Pre AWD Release", "Post AWD Release"), 
       lty=c(1,2), col=c(1,4), bty='n')

#Coefficient of Overlap for Genet late dry season
(GenetLateDry_Release_Comparison <- overlapEst(plot_genet_late_dry.b, plot_genet_late_dry.a, type="Dhat4"))
#GenetLateDry_Release_Comparison = 0.9346674

bootstrap.glb <- resample(plot_genet_late_dry.b, 1000)
bootstrap.gla <- resample(plot_genet_late_dry.a, 1000)

genet.ldry_boot <- bootEst(bootstrap.glb, bootstrap.gla, type = "Dhat4")
(BSmean <- mean(genet.ldry_boot))
#0.9074405

compareCkern(fit.glb, fit.gla, reps = 999)

#turn off graphics device
dev.off()

