# =========== #
# Homework #6 #
# =========== #

# ==========#
# Problem 1 #
# ==========#

install.packages("faraway")    # Installs the farway package
# Select local mirror like USA(PA 1) 
library(faraway)               # Opens the faraway library
data(seatpos)                  # Loads the seat position data
attach(seatpos)                # Allow to quote var names without
                               # need of dataset name 
# Part (a)
# ========
reg1 <- lm(hipcenter~.,data=seatpos)      # Multiple linear regression fit
summary(reg1)                             # Regression summary

# Part (b)
# ========
round(cor(seatpos[,1:8]),digits=2)        # Correlation matrix for x-variables

# Part (c)
# ========
vif(reg1)                                 # Variance inflation factors

# Part (d)
# ========
reg2 <- lm(hipcenter~Age+Weight+Ht+       # Fits model for hipcenter on age,
             I(Age*Weight),data=seatpos)  #   weight, ht, and age*weight
summary(reg2)                             # Regression summary


# ========================= #
# Problem 2 # evolution
# ========================= #
evol <- read.csv("evolution.txt",    # Reads in the evolution data
  header=T)
attach(evol)                              # Promotes the dataset

# Part (a)
# ========
plot(latitude,wingsize,cex.axis=1.5,      # Labeled scatterplot of wing size vs.
  cex=1.5,xlab="Latitude",ylab=           #   latitude for both NAM and EUR flies
  "Wing Size (thous.log mm)",cex.lab=1.6,
  pch=c(1,16)[unclass(as.factor(continent))],
  mgp=c(2.7,1,0))
reg1 <- lm(wingsize[continent=="NAM"]~    # Regression of wing size on latitude
  latitude[continent=="NAM"])             #   for NAM flies
reg2 <- lm(wingsize[continent=="EUR"]~    # Regression of wing size on latitude
  latitude[continent=="EUR"])             #   for EUR flies
abline(reg1,lwd=2)                        # Plots fitted line for surface
abline(reg2,lwd=2,lty=2)                  # Plots fitted line for deep
legend(36,850,c("NAM Flies","EUR Flies"), # Places a legend on the plot for
  lwd=c(2,2),lty=c(1,2),pch=c(16,1),      #   NAM and EUR flies at (36,850)
  cex=1.5)

# Parts (c),(d)
# =============
reg3 <- lm(wingsize~latitude*continent)   # Interaction model with latitude, continent
summary(reg3)
confint(reg3)                             # Confidence intervals for model parameters


# ============================================= #
# Problem 3: Bat data #
# ============================================= #
bats <- read.csv("Data/bats.txt",header=T)     # Reads in the bat data
attach(bats)                                   # Promotes the data set
plot(logmass,logenergy,xlab="Log Mass",ylab=   # Scatterplot of log energy expend.
       "Log Energy Expenditure",cex.axis=1.5,       #   vs. log body mass with a separate
     pch=c(4,16,1)[unclass(type)],cex=1.5,        #   plotting symbol for the three
     cex.lab=1.6,mgp=c(2.7,1,0))                  #   types of species
reg1.out <- lm(logenergy[type=="Non-echolocating bats"]~
                 logmass[type=="Non-echolocating bats"])      # Regression for non-echo bats
abline(reg1.out,lwd=2,lty=3)                   # Plots the regression line
reg2.out <- lm(logenergy[type=="Non-echolocating birds"]~
                 logmass[type=="Non-echolocating birds"])     # Regression for non-echo birds
abline(reg2.out,lwd=2,lty=1)                   # Plots the regression line
reg3.out <- lm(logenergy[type=="Echolocating bats"]~
                 logmass[type=="Echolocating bats"])          # Regression for echo bats
abline(reg3.out,lwd=2,lty=2)                   # Plots the regression line
legend(1.8,0.5,c("Non-echo bats",              # Places a legend on the plot to
                 "Non-echo birds","Echo bats"),pch=c(4,16,1), #   distinguish the three species
       lwd=c(2,2,2),lty=c(3,1,2),cex=1.3)           #   types, at (1.8,0.5)

reg4.out <- lm(logenergy~logmass*type)         # Interaction model
summary(reg4.out)                              # Summary of interaction model
anova(reg4.out)                                # ANOVA for interaction model


