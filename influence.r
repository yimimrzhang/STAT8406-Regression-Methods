
sat <- read.csv("sat.txt",      # Reads in the SAT data
                header=T)
sat2 <- data.frame(sat[,1:2],logtak= # New SAT dataset with log(takers)
                     log(sat[,3]),sat[,4:8])            #   instead of TAKERS
attach(sat2)                         # Attaches the dataset
reg3 <- lm(sat~logtak+years+expend,  # Fits the 4-parameter model
           data=sat2)
jack <- rstudent(reg3)               # Computes studentized deleted residuals

par(mfrow=c(2,2))                    # Creates a 2x2 graphics window
plot(years,jack,xlab="Years",ylab=   # Residual plot of studentized deleted
       "Stddeleted Residuals",cex.lab=1.6, #   residuals vs. year
     cex.axis=1.5,cex=1.5,pch=16,cex.main=1.4,
     main="Stddeleted Residuals vs. Year",mgp=c(2.7,1,0))
plot(expend,jack,xlab="Expenditures",# Residual plot of studentized deleted
     ylab="Stddeleted  Residuals",cex.lab=#   residuals vs. expenditures
       1.6,cex.axis=1.5,cex=1.5,pch=16,cex.main=1.4,
     main="Stddeleted  Residuals vs. Expenditures",mgp=c(2.7,1,0))
plot(logtak,jack,xlab="Log(Takers)", # Residual plot of studentized deleted
     ylab="Stddeleted  Residuals",cex.lab=#   residuals vs. log(takers)
       1.6,cex.axis=1.5,cex=1.5,pch=16,cex.main=1.4,
     main="Stddeleted  Residuals vs. Log(Takers)",mgp=c(2.7,1,0))
plot(reg3$fitted,jack,xlab=          # Residual plot of studentized deleted
       "Predicted SAT",ylab=              #   residuals vs. predicted values
       "Stddeleted  Residuals",cex.lab=1.6,
     cex.axis=1.5,cex=1.5,pch=16,cex.main=1.4,
     main="Stddeleted  Residuals vs. Predicted SAT",mgp=c(2.7,1,0))

hatvalues(reg3)        # Computes leverages
cooks.distance(reg3)   # Computes Cook's D values
dffits(reg3)           # Computes DfFits values
dfbetas(reg3)          # Computes DfBetas values

plot(reg3) # Plots studentized deleted residuals vs leverage

