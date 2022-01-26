biom <- c(16.6,49.1,121.7,219.6,375.5,570.8,  # Vector of biomass values
          648.2,755.6)
solrad <- c(29.7,68.4,120.7,217.2,313.5,      # Vector of solar radiation values
            419.1,535.9,641.5)
plot(solrad,biom,xlab="Solar Radiation (x)",  # Plots biomass vs. solar radiation
  ylab="Plant Biomass (y)",cex.axis=1.5,pch=  #   with axis labels and a title,
  16,cex.lab=1.6,cex=1.5,cex.main=1.6,main=   #   and closed circles (pch=16)
  "Biomass vs. Radiation with Regression Line",
  mgp=c(2.7,1,0))
reg.out <- lm(biom~solrad)                    # Regression of biomass on solar rad.
summary(reg.out)                              # Regression summary
abline(reg.out,lwd=2)                         # Plots the fitted regression line
anova.out <- anova(reg.out)                   # Computes the ANOVA table
anova.out                                     # Prints the ANOVA table

qqnorm(reg.out$resid, xlab="Normal Quantiles",# Constructs a normal quantile plot
  ylab="Residuals",cex.lab=1.6,cex.axis=1.5,  #   of the residuals
  cex=1.5,pch=16,cex.main=1.6,mgp=c(2.7,1,0),
  main="Normal Quantile Plot for Solar Radiation Data")

cor(biom,solrad)                              # Correlation between biomass and solar

