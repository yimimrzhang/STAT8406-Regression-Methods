# ============ #
# Homework #9  #
# ============ #

# ==================== #
# Problem 1 #
# ==================== #
library(faraway)
library(pls)
attach(seatpos)     

# Part (a)
# ========
library(glmnet)     
grid<-10^seq(10,-2, length=100)                        # Create a grid of lambda values
x<-as.matrix(seatpos[,1:8])
ridge.mod=cv.glmnet(x,seatpos$hipcenter,lambda=grid,   # Build a CV ridge regression          
                    nfold=length(seatpos$hipcenter),                       
                    alpha=0)

ridge.mod=cv.glmnet(x,seatpos$hipcenter,   # Build a CV ridge regression          
                    nfold=length(seatpos$hipcenter),                       
                    alpha=0)    

plot(log10(ridge.mod$lambda), ridge.mod$cvm,           # Plot average CV error versus log(lambda)
     xlab="log10(Lambda)", ylab="CV Error")                     
abline(v = log10(ridge.mod$lambda.min), lty = 3)

(lambda=ridge.mod$lambda.min)                          # The lambda that minimizes CV error
predict(ridge.mod,s=lambda,                            # Fit ridge reg using minimizing lambda
        type="coefficients")

ridge.mod$cvm[ridge.mod$lambda==ridge.mod$lambda.min]  # Obtain the minimized CV error

# Part (b)
# ========
pcr.cv=pcr(hipcenter ~., data=seatpos,
scale=TRUE, validation ="LOO")              # Perform leave-one-out CV in PCR
summary(pcr.cv)                             # report CV error and cumulative PVE
validationplot(pcr.cv,val.type="MSEP")      # Plot CV MSE vs # of components



# Part (c)
# ========
reg <- lm(hipcenter ~ .,data=seatpos)       # Least squares regression  
summary(reg)
library(MPV)
PRESS(reg)/nrow(seatpos)                    # Obtain CV error from OLS



# ==================== #
# Problem 2 #
# ==================== #

nordic <- read.csv("nordic.txt",header=T) # Reads in the nordic data

# Part (b)
# ========
pr.out=prcomp(nordic[,3:4],scale=TRUE)    # Perform PCA on SkiJump and CrossCountry, 
pr.out$rotation					# Identify first component loadings
min3=sort(pr.out$x[,1],partial=3)[3]      # Return 3rd smallest score of 1st component
nordic$Name[pr.out$x[,1]==min3]           # Report name associated with 3rd smallest score

 

# Part (c)
# ========
pr.var=pr.out$sdev^2                  # Variance explained by each principal component
pve=pr.var/sum(pr.var)                # PVE by each principal component

par(mfrow=c(1,2))                           # Creates a 1x2 graphics window
plot(pve,xlab="Principal Component",            # Scree plot
ylab="Proportion of Variance Explained",
ylim=c(0,1),type='b')

plot(cumsum(pve),xlab=" Principal Component",   # Cumulative PVE      
ylab="Cumulative Proportion of Variance Explained",
ylim=c(0,1),type='b')



