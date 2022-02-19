# =========== #
# Homework #4 #
# =========== #

# ==================== #
# Problem 1 #
# ==================== #
drug <- read.csv("drug.txt",header=T)
attach(drug)

# Part (a)
# ========
plot(dose,response,xlab="Dose Level",pch=16,  # Plots response vs. dose with
  ylab="Response (drug strength)",cex=1.5,    #   axis labels and a closed
  cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),    #   circle
  main="Response vs. Dose",cex.main=1.8)

# Part (b)
# ========
reg <- lm(response~dose)                      # Regression of response on dose
summary(reg)                                  # Regression summary

# Part (c)
# ========
plot(dose,reg$resid,xlab="Dose Level",pch=16, # Makes a residual plot of
  ylab="Residuals",cex=1.5,cex.lab=1.6,       #   the residuals vs. the
  cex.axis=1.5,mgp=c(2.7,1,0),main=           #   x-values (doses)
  "Residual Plot",cex.main=1.8)
abline(h=0,lwd=2,lty=2)                       # Plots the zero-line

# Part (d)
# ========
purelof(dose,response)              # Conducts a model lack of fit test

# Part (e)
# ========
ldose <- log(dose)                            # Log of the doses
plot(ldose,response,xlab="Log Dose",pch=16,   # Plots response vs. dose with
  ylab="Response (drug strength)",cex=1.5,    #   axis labels and a closed
  cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),    #   circle
  main="Response vs. Log Dose",cex.main=1.8)
reg <- lm(response~ldose)                     # Regresses y on log dose
abline(reg,lwd=2)                             # Plots the regression line
summary(reg)                                  # Regression summary
plot(ldose,reg$resid,xlab="Log Dose",         # Makes a residual plot of
  ylab="Residuals",cex=1.5,cex.lab=1.6,       #   the residuals vs. the
  cex.axis=1.5,mgp=c(2.7,1,0),main=           #   log doses
  "Residual Plot",cex.main=1.8,pch=16)
abline(h=0,lwd=2,lty=2)                       # Plots the zero-line
purelof(log(dose),response)                   # Model LOF test


# ==================== #
# Problem 2 #
# ==================== #
wheatears <- read.csv("wheatears.txt",      # Reads in the wheatears data
                      header=T)
attach(wheatears)                           


plot(mass,tcell,xlab="Mean Stone Mass (g)", # Scatterplot of T-cell Response
     ylab="T-cell Response (mm)",cex.lab=1.6,  #    vs. Stone Mass
     cex.axis=1.5,pch=16,cex=1.5,
     main="T-cell Response vs. Stone Mass",
     cex.main=1.6,mgp=c(2.7,1,0))

# Attempt transformation
exptcell=exp(tcell)
expmass=exp(mass)
reg <- lm(exptcell~expmass,data=wheatears)      
summary(reg)      
abline(reg,lwd=2)                         


reg <- lm(tcell~mass,data=wheatears)      # Regression of T-cell on stone mass
summary(reg)      
abline(reg,lwd=2)                         # Plots regression line


# Residual Analysis 
plot(reg$fitted,reg$resid,ylab="Residuals", 	# Residual plot
     xlab="Predicted Values",cex.lab=1.6,cex=1.5,
     pch=16,main="Residuals vs. Predicted Values",
     cex.main=1.8,cex.axis=1.5,mgp=c(2.7,1,0))
abline(h=0,lty=2,lwd=2)                       	# Plots the y=0 line

qqnorm(reg$resid,pch=16,ylab="Residuals",	# Normal quantile plot of model residuals
       xlab="Standard Normal Quantiles",       
       cex.lab=1.6,cex.axis=1.5,cex.main=1.8,
       main="Normal Quantile Plot",cex=1.5,mgp=c(2.7,1,0))

studres <- rstudent(reg)             # Vector of Studentized deleted residuals
wheatears[abs(studres)>2.1,]         # Display outlying obs


