ODS HTML CLOSE;
ODS HTML;
options pageno=1;

*Import a local data set into SAS.;
data brainweight0;
length species $24. ; *read up to 24 character values;
infile 'C:\Users\yzhang09\Documents\CourseWork\MAT8406_sp17\Data\brainbody.txt' FIRSTOBS=2 DLM=','; *DLM=','indicates a comma is the delimiter;
input species$ brain body gestation litter ; *indicate $ if a categorical variable;
run;

*Scatterpot Matrix;
proc sgscatter data=brainweight0; 
matrix brain body gestation litter / diagonal=(histogram);
run;

*Creat a new dataset with log transformed variables;
data brainweight;
set brainweight0;
lbrain=log(brain);
lbody=log(body);
lgestation=log(gestation);
llitter=log(litter);
run;

proc sgscatter data=brainweight; 
matrix lbrain lbody lgestation llitter / diagonal=(histogram);
run;

proc glm data=brainweight;  
model lbrain=lbody lgestation llitter lbody*lgestation lbody*llitter lgestation*llitter/solution; *'solution' gives us the parameter estimates;
title ' Multiple Regression with interaction'; 
run;


proc glm data=brainweight;  
model lbrain=lbody lgestation llitter/solution;
title ' Multiple Regression without interaction'; 
run;


quit;

