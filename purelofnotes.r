plasma <- c(13.44,12.84,11.91,20.09,15.60,  # Defines the plasma levels
            10.11,11.38,10.28,8.96,8.59,
            9.83,9.00,8.65,7.85,8.88,
            7.94,6.01,5.14,6.90,6.77,
            4.86,5.10,5.67,5.75,6.23)
age <- rep(0:4,each=5)                      # Defines the ages
par(mfrow=c(1,2))                           # Creates a 1x2 graphics window
plot(age,plasma,xlab="Age in Years",ylab=   # Plot of plasma level vs. age
  "Plasma Level",cex.lab=1.6,cex.axis=1.5,  #   with axis labels
  pch=16,cex=1.5,mgp=c(2.7,1,0))
lplas <- log(plasma)                        # Log plasma levels
plot(age,lplas,xlab="Age in Years",ylab=    # Plot of log plasma vs. age
  "Log Plasma Level",cex.lab=1.6,cex.axis=  #   with axis labels
  1.5,pch=16,cex=1.5,mgp=c(2.7,1,0))
reg.out <- lm(lplas~age)                    # Fits an SLR model
abline(reg.out,lwd=2)                       # Plots regression line
purelof(age,lplas)                          # Runs a test of LOF

# Second Way to Run Lack of Fit Test
# ==================================
reg.out <- lm(lplas~age)                    # Fits an SLR model
rega.out <- lm(lplas~factor(age))           # Model with age as a factor
anova(reg.out,rega.out)                     # ANOVA LOF test
