brainbody <- read.csv("brainbody.txt",header=T)
source("Scripts/pairs.panels.r") # Execute the commands from a file, as if they are typed into R

# Raw scatterplot pairs
# =====================
lab <- c("Brain\nWeight (g)","Body\nWeight (kg)",
  "Gestation\nPeriod (days)","Litter\nSize")
pairs.panels(brainbody[,2:5],labels=lab)

# Log scatterplot pairs
# =====================
attach(brainbody)
logdat <- data.frame(species=species,logbrain=log(brain),logbody=log(body),
  loggest <- log(gestation),loglitt=log(litter))
lab <- c("Log Brain\nWeight (g)","Log Body\nWeight (kg)",
  "Log Gestation\nPeriod (days)","Log Litter\nSize")
pairs.panels(logdat[,2:5],labels=lab)

# First model fit 
# ==========================
reg.out <- lm(logbrain~logbody+loggest+loglitt+I(logbody*loggest)+
  I(logbody*loglitt)+I(loggest*loglitt),data=logdat)
summary(reg.out)

# Second model fit
# ===========================
reg2.out <- lm(logbrain~logbody+loggest+loglitt,data=logdat) 
summary(reg2.out)

# Compare first and second models
# ===========================
anova(reg2.out, reg.out)  # place reduced model ahead of complete model in arguments 

# Third model fit
# ===========================
reg3.out <- lm(logbrain~logbody,data=logdat)
summary(reg3.out)

# Compare second and third models
# ===========================
anova(reg3.out, reg2.out) 

