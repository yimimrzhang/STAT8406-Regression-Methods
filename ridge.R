# Code for Inference in Multiple Regression Handout
# =================================================
mfoam <- read.csv("meadowfoam.txt",     # Reads in meadowfoam data
  header=T)
attach(mfoam)                                # Promotes the meadowfoam dataset

reg4.out <- lm(flowers~intensity+timing)     # Fits first-order model
summary(reg4.out)                            # Regression summary
anova(reg4.out)                              # Regression ANOVA table

# Fit the ridge regression model
# ============================================
library(glmnet)                      # Load package 'glmnet'
x<-cbind(intensity,timing)           # Combine variables by column
y<-flowers
grid<-10^seq(10,-2, length=100)        # Create a grid of lambda values
ridge.mod=cv.glmnet(x,y,lambda=grid,   # Build a CV ridge regression          
nfold=length(y),                       # nfold=sample size, leave-one-out CV
alpha=0)                               # alpha=0, ridge reg is fit

## Ignore the received warning which recommends leaving 3-or-more out in CV ## 
#Warning message:
#Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold 

plot(log10(ridge.mod$lambda), ridge.mod$cvm,      # Plot average CV error versus log(lambda)
xlab="log10(Lambda)", ylab="CV Error")                     
abline(v = log10(ridge.mod$lambda.min), lty = 3)

(lambda=ridge.mod$lambda.min)        # The lambda that minimizes CV error
predict(ridge.mod,s=lambda,          # Obtain ridge reg coefs
type="coefficients")

