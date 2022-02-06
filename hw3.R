# ========== #
# Homework 3 #
# ========== #

# ==================== #
# Problem 1 #
# ==================== #
recov <- read.csv("recovery.txt",       # Reads in the biological recovery data
                  header=T)
attach(recov)                               

# Part (a): 
# =========
logrec <- log(recov$recovery)                # Computes the log recovery percentages
reg.out <- lm(logrec~time)                   # Regression of log recovery on time
summary(reg.out)                             # Prints regression summary

sum(time^2)                    # Computes sum of x-squared
sum((time-mean(time))^2)       # Computes Sxx
confint(reg.out, level=0.99)   # Computes 99% CI's for slope and intercept

# ==================== #
#     Problem 2        #
# ==================== #

big<-read.csv("bigbang.txt")
attach(big)

# Part (b): 
# =========
reg1<-lm(distance~velocity, data=big)
summary(reg1)

# Part (c): 
# =========
reg<-lm(distance~velocity-1, data=big)
summary(reg)
confint(reg, "velocity")   # Computes CI for slope of velocity


# ==================== #
#     Problem 3        #
# ==================== #
horse <- read.csv("horsepower.txt",header=T)
attach(horse)

# Part (a): Scatterplot of price vs. horsepower
# =============================================
plot(horsepower,price,cex.axis=1.5,cex=1.5,     # Plots price vs. horsepower
     xlab="Horsepower",pch=16,cex.lab=1.6,ylab=    #   with axis labels, and a
       "Price (thousands of $)",cex.main=1.8,main=   #   title.
       "Price vs. Horsepower",mgp=c(2.7,1,0))

# Part (b): Regression of price and horsepower with plotted line
# ==============================================================
reg <- lm(price~horsepower)                     # Regresses price on horsepower
abline(reg,lwd=2)                               # Plots regression line

# Part (c): Prediction interval for the average price 
# ==============================================================
xval <- data.frame(horsepower=280)              # Specify an x value
pred <- predict(reg,xval,interval=              # Computes prediction interval for y at x
                  "prediction")


# Part (e): Residual plot
# =======================
plot(horsepower,reg$resid,cex.axis=1.5,cex=1.5, # Plots residuals vs. horsepower
     xlab="Horsepower",pch=16,cex.lab=1.6,ylab=    #   with axis labels, and a
       "Residuals",cex.main=1.8,main=                #   title.
       "Residual Plot",mgp=c(2.7,1,0))


