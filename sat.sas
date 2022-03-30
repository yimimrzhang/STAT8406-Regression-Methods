ODS HTML CLOSE;
ODS HTML;
options pageno=1;

*Import a local data set into SAS.;
data sat;
length state $24. ; *read up to 24 character values;
infile 'C:\Users\yzhang09\Documents\CourseWork\MAT8406_sp17\Data\sat.txt' FIRSTOBS=2 DLM=','; *DLM=','indicates a comma is the delimiter;
input state$ sat takers  income years public expend rank ; *indicate $ if a categorical variable;
run;

*Creat a new dataset with log transformed variable;
data sat1;
set sat;
ltakers=log(takers);
run;

ods graphics on; *use ods graphics to get assorted diagnostic plots;
proc reg data=sat1	 plots=diagnostics;  
model sat=years expend ltakers /influence ; 
*influence: produce several influence statistics;
title 'Influential Statistics'; 
run;
ods graphics off;

quit;

