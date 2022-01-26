options pageno=1;

*------------------------------------------------------------------*;
* For a new predictiion at x=300, leave the response value missing *;
*------------------------------------------------------------------*;
data biomass;
input biomass radiation @@;
datalines;
16.6 29.7 49.1 68.4 121.7 120.7 219.6 217.2
375.5 313.5 570.8 419.1 648.2 535.9 755.6 641.5
. 300
;  

*----------------------------------------------------------*;
*  The regression analysis procedure (PROC REG) is run.    *;
*   We ask for a printout of predicted values (p),         *;
*   the parameter confidence intervals (clb),              *;
*   confidence intervals for E(y) (clm),                   *;
*   prediction intervals (cli),                            *;
*   studentized residual values (r),                       *;
*   and studentized deleted residual values (influence)    *;
*----------------------------------------------------------*;

proc reg  data=biomass alpha=0.05;
  model biomass=radiation/ p clb r cli clm influence;
title ' Simple Linear Regression Biomass Example' ;
run;
quit;
