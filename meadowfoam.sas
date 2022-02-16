ODS HTML CLOSE;
ODS HTML;
options pageno=1;

*Import a local data set into SAS.;
data meadowfoam;
length obs $24. flowers 8. intensity 8. timing $24.; *read up to 24 character values;
infile 'C:\Users\yzhang09\Documents\CourseWork\STAT8406_sp22\Data\meadowfoam.txt' FIRSTOBS=2 DLM=','; *DLM=','indicates a comma is the delimiter;
input obs$ flowers intensity timing$ ; *indicate $ if a categorical variable;
run;


*using the proc glm without manually coding the dummy variables.;
proc glm data=meadowfoam;  
class timing; *indicate categorical variables in the class statement;
model flowers=intensity timing intensity*timing/solution; *'solution' gives us the parameter estimates;
title ' Multiple Regression with interaction'; 
run;

ods graphics on; *use ods graphics to get assorted diagnostic plots;
proc glm data=meadowfoam plots=diagnostics;  
class timing;
model flowers=intensity timing/solution CLI; 
*CLM:confidence intervals for E(y), CLI:prediction intervals;
*The CLI option is ignored if the CLM option is also specified ; 
title ' Multiple Regression without interaction'; 
run;
ods graphics off;

*Add a new observation to data;
proc sql;
insert into  meadowfoam values ( '' , ., 400,'"At PFI"');
run;

proc glm data=meadowfoam;  
class timing;
model flowers=intensity timing/solution CLM CLI; 
title 'Prediction for a new observation';  
run;

quit;

