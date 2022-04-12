# Reads in the Pima Indian diabetes data
# ======================================
library(faraway)                      # Loads faraway library
data(pima)                            # Loads pima data

# Scatterplot of diabetes incidence vs. glucose concentration
# ===========================================================
plot(pima$glucose,pima$test,          # Plots test outcome vs. glucose
  xlab="Glucose Concentration",ylab=  #   concentration
  "Diabetes Test Outcome",cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),cex=0.7,
  pch=16,main="Scatterplot of Test Outcome vs. Glucose",cex.main=1.5)
abline(lm(test~glucose,pima),lwd=3)

# Computes decile group proportions and plots them on scatterplot
# ===============================================================
brk <- quantile(pima$glucose,         # Finds the deciles of the glucose
  probs=seq(.1,.9,.1))                #   distribution.
grp <- rep(1,length(pima$test))       # Sets all glucose values to group 1
for (i in 1:length(brk)) grp <- grp + # Determines the group for each
  as.numeric(pima$glucose>brk[i])     #   glucose value
diabcount <- tapply(pima$test,grp,sum)# Counts diabetes cases in groups
grpprops <- diabcount/table(grp)      # Proportion of diabetes in groups
grpmeans <- c(42.5,90.5,99,106,113.5, # Glucose midpoints for each group
  121.5,130,141,157.5,184)
points(grpmeans,grpprops,cex=2)       # Overlays group proportions

# Fitting the logistic regression model
# =====================================
logitmod <- glm(test~glucose,         # Fits a logistic regression model
  data=pima,family=binomial)          #   of diabetes presence vs. glucose
summary(logitmod)                     # Summary of logistic fit

# Prediction of diabetes prob. for glucose = 100
# ==============================================
x0 <- c(1,100)                             # X-vector for prediction
eta0 <- sum(x0*coef(logitmod))             # Computes eta for prediction
ilogit(eta0)                               # Predicted prob. at x=100; ilogit function is available in library(faraway)
varmat <- summary(logitmod)$cov.unscaled   # Extracts betahat var matrix
se <- sqrt(t(x0)%*%varmat%*%x0)            # SE of prediction
ilogit(c(eta0-1.96*se,eta0+1.96*se))       # CI for prediction

# Predictions at group means
# ==========================
grouppred <- ilogit(logitmod$coef[1]+ # Logistic predictions at the glucose
  logitmod$coef[2]*grpmeans)          #   group means

# Plots fitted logistic curve
# ===========================
plot(grpmeans,grpprops,cex=2,xlab=    # Scatterplot of proportion of diabetes
  "Mean Glucose Concentration",       #   cases per group plotted against the
  ylab="Proportion with Diabetes",    #   median glucose concentration for that
  cex.lab=1.6,cex.axis=1.5,pch=16,    #   group
  main="Fitted Logistic Model",cex.main=1.8,mgp=c(2.7,1,0),ylim=c(0,1))
axis(2,at=seq(0,0.8,0.2),cex.axis=1.5)# Specific axis values
val <- 0:200                          # Sequence of values 0,1,...,200
logitpred <- ilogit(logitmod$coef[1]+ # Computes predicted diabetes probs
  logitmod$coef[2]*val)               #   at glucose values 0-200
lines(val,logitpred,lwd=3)            # Overlays logistic curve on plot

# How well does the model predict?
# ================================
ptest <- as.numeric(logitmod$fitted>  # Binary vector set to 1 if the prob.
  0.5)                                #   exceeds 0.50 and 0 otherwise
table(pima$test,ptest)                # Contingency table of test outcomes

# Testing b1=0 with a likelihood ratio test
# =========================================
1-pchisq(184.76,1)                    # Chi-square p-value for LRT test


