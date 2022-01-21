# ========== #
# Homework 1 #
# ========== #

# ==================== #
# Problem 1#
# ==================== #
sizes <- read.csv("sizes.txt",header=T)   # Reads in sizes data
attach(sizes)                             # Allows to access variable names 
                                          #   without calling the data frame

# Parts (a,b): Scatterplot of height vs. armspan for females and males,
#              Regression equations for both females and males,
# ==================================================================
plot(armspan,height,pch=1,cex=1.5,             # Plots height vs. armspan with open circles
     xlab="Armspan (cm)",ylab="Height (cm)",      #   (pch=1), axis labels, and text sizes set
     cex.lab=1.5,cex.axis=1.5,mgp=c(2.7,1,0),     #   to 1.5 times the default size
     main="Height vs. Armspan",cex.main=1.8)
points(armspan[sex=="F"],height[sex=="F"],     # Overlays female points with a closed
       pch=16,cex=1.5)                              #   circle (pch=16)
regF <- lm(height[sex=="F"]~armspan[sex=="F"]) # LS regression for females only
regM <- lm(height[sex=="M"]~armspan[sex=="M"]) # LS regression for males only
abline(regF,lwd=2)                             # Regression line for females
abline(regM,lwd=2,lty=2)                       # Regression line (dashed) for males
legend(183,170,legend=c("Female","Male"),      # Places a legend at (180,170) to distinguish
       lty=c(1,2),pch=c(16,1),lwd=c(2,2),cex=1.6)   #   female and male line types
summary(regF)
summary(regM)

# Part (c): Prediction of Dr Zhang's height
# =========================================
Zhangpred <- regF$coef[1]+regF$coef[2]*165       # Dr. Zhang's predicted height
resid(regF)                                      # Residuals from the female regression

# Part (d): Plot, regression of height on foot length for both sexes
# ==================================================================
plot(footlen,height,pch=1,cex=1.5,             # Plots height vs. foot length with open circles
     xlab="Foot Length (cm)",ylab="Height (cm)",  #   (pch=1), axis labels, and text sizes set
     cex.lab=1.5,cex.axis=1.5,mgp=c(2.7,1,0),     #   to 1.5 times the default size
     main="Height vs. Foot Length",cex.main=1.8)
points(footlen[sex=="F"],height[sex=="F"],     # Overlays female points with a closed
       pch=16,cex=1.5)                              #   circle (pch=16)
regF <- lm(height[sex=="F"]~footlen[sex=="F"]) # LS regression for females only
regM <- lm(height[sex=="M"]~footlen[sex=="M"]) # LS regression for males only
abline(regF,lwd=2)                             # Regression line for females
abline(regM,lwd=2,lty=2)                       # Regression line for males
legend(26.2,168,legend=c("Female","Male"),     # Places a legend at (26.2,168) to distinguish
       lty=c(1,2),pch=c(16,1),lwd=c(2,2),cex=1.6)   #   female and male line types
summary(regF)
summary(regM)

# Part (e): Prediction of Dr Zhang's height
# =========================================
Zhangpred <- regF$coef[1]+regF$coef[2]*24        # Dr. Zhang's predicted height
resid(regF)                                      # Residuals from the male regression




# ==================== #
# Problem 2 #
# ==================== #
recov <- read.csv("recovery.txt",       # Reads in the biological recovery data
  header=T)
attach(recov)                                # Promotes the biological recovery data

# Part (a): Scatterplot of Biological Recovery vs. Time
# =====================================================
plot(time,recovery,pch=16,cex.axis=1.5,      # Plots the recovery percentages vs. time
  xlab="Time (minutes)",mgp=c(2.7,1,0),      #   with x- and y-axis labels and a title
  ylab="Biological Recovery (percent)",
  main="Biological Recovery vs. Time",
  cex.lab=1.6,cex.main=1.8,cex=1.5)

# Part (b): Scatterplot of Log Biological Recovery vs. Time
# =========================================================
logrec <- log(recov$recovery)                # Computes the log recovery percentages
plot(time,logrec,pch=16,cex.axis=1.5,        # Plots the log recovery percentages vs.
  xlab="Time (minutes)",mgp=c(2.7,1,0),      #   time with axis labels and a title
  ylab="Log Biological Recovery (percent)",
  main="Log Biological Recovery vs. Time",
  cex.lab=1.6,cex.main=1.8,cex=1.5)

# Part (c): Regression of Log Recovery on Time
# ============================================
reg.out <- lm(logrec~time)                   # Regression of log recovery on time
summary(reg.out)                             # Prints regression summary

# Part (d): ANOVA Table
# =====================
anova(reg.out)                               # Prints the ANOVA table

