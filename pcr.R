# Principal Component Regression
# ======================================
sizes <- read.csv("sizes.txt",header=T) # Reads in the class data
cor(sizes[,1:3])                             # Correlations for 1st 3 var's

attach(sizes)
pr.out=prcomp(sizes[,1:3],scale=TRUE)        # Perform PCA on xsizes, 
						  # By default, xsizes is column centered
						  # scale=TRUE, scale the variables to sd=1
                           
pr.out$center 				  # Variable means for scaling before PCA
pr.out$scale                          # Variable sd for scaling before PCA

pr.out$rotation       			  # Provide principal component loadings
pr.out$x                              # Output principal component score vector
pr.out$sdev                           # Sd of each principal component
pr.var=pr.out$sdev^2                  # Variance explained by each principal component
pve=pr.var/sum(pr.var)                # PVE by each principal component

par(mfrow=c(1,2))                           # Creates a 1x2 graphics window
plot(pve,xlab="Principal Component",            # Scree plot
ylab="Proportion of Variance Explained",
ylim=c(0,1),type='b')

plot(cumsum(pve),xlab=" Principal Component",   # Cumulative PVE      
ylab="Cumulative Proportion of Variance Explained",
ylim=c(0,1),type='b')



# ======================================================== 
library(ISLR)					  # Hitters data saved in ISLR library
library(pls)					 
pcr.cv=pcr(Salary~., data=Hitters,
scale=TRUE, validation ="LOO")              # Perform leave-one-out CV
summary(pcr.cv)                             # report CV error and cumulative PVE
par(mfrow=c(1,1))                           # Recover 1x1 graphics window
validationplot(pcr.cv,val.type="MSEP")      # Plot CV MSE vs # of components
