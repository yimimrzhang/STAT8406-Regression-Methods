sat <- read.csv("sat.txt",header=T)
source("pairs.panels.r")

# Raw scatterplot pairs
# =====================
lab <- c("SAT","TAKERS","INCOME","YEARS","PUBLIC","EXPEND","RANK")
pairs.panels(sat[,2:8],labels=lab)

# Log scatterplot pairs
# =====================
lab[2] <- "LOGTAK"
sat2 <- data.frame(sat[,1:2],logtak=log(sat[,3]),sat[,4:8])
pairs.panels(sat2[,2:8],labels=lab)



# install.packages("olsrr")
library(olsrr)

# All Possible Regression 
# =======================
reg <- lm(sat~logtak+income+years+public+expend+rank,data=sat2)
allsubsets <- ols_step_all_possible(reg)
allsubsets
plot(allsubsets)


bestsubsets <- ols_step_best_subset(reg)  # Select best subset regression
bestsubsets
plot(bestsubsets)


# Mallow's Cp Plot
plot((allsubsets$n+1),allsubsets$cp, # Plots Cp v.s p
     main="Cp Plot",xlab="p", ylab="Cp",
     xlim=c(2,7), ylim=c(0,15),
     pch=19, cex=1.4) 
abline(0, 1,lwd=2)                   # Add reference line y=x


# Subset Selection for Regression Models
# ======================================

# Based on p-values
ols_step_backward_p(reg, prem = 0.05, details = TRUE)    # Backward Elimination
ols_step_forward_p(reg, penter = 0.05, details = TRUE)   # Forward Selection
ols_step_both_p(reg, pent = 0.05, prem = 0.1)            # Stepwise Selection


# Based on AIC
ols_step_backward_aic(reg)
ols_step_forward_aic(reg)
ols_step_both_aic(reg)


# Final selected model 
reg1<-lm(sat~logtak+years+expend+rank,data=sat2)
summary(reg1)          # Regression summary
ols_press(reg1)        # Computes PRESS statistic
