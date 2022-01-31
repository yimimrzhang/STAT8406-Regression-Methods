purelof <- function(x,y){
  dat <- data.frame(x[order(x)],y[order(x)])
  run1.reg <- lm(y ~ x)
  run2.aov <- aov(run1.reg$resid ~ ordered(x))
  sspe <- sum(run2.aov$resid**2)
  sslof <- sum(run1.reg$resid**2) - sspe
  df.sspe <- run2.aov$df.residual
  df.sslof <- run1.reg$df.residual - df.sspe
  fstat <- (sslof/df.sslof)/(sspe/df.sspe)
  pvalue <- 1-pf(fstat,df.sslof,df.sspe)
  digits <- 6-trunc(log10(sspe))
  cat(" Test for Pure Error/Lack of Fit","\n","-------------------------------","\n",
    "Source of Variance",rep("",digits-2),"df",rep("",digits),"SS",rep("",digits+3),
    "MS",rep("",digits+3),"F",rep("",digits+1),"P-value","\n")
  cat(" Lack of Fit            ",df.sslof,"  ",format(round(sslof,digits)),"  ",format(round(sslof/df.sslof,digits)),
    "  ",format(round(fstat,digits)),"  ",format(round(pvalue,digits)),"\n")
  cat(" Pure Error            ",df.sspe,"  ",format(round(sspe,digits)),"  ",format(round(sspe/df.sspe,digits)),
    "\n")
}
