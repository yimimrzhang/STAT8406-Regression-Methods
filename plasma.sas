options pageno=1;

data plasma;
input year plasma;
cards;
0	13.44
0	12.84
0	11.91
0	20.09
0	15.6
1	10.11
1	11.38
1	10.28
1	8.96
1	8.59
2	9.83
2	9
2	8.65
2	7.85
2	8.88
3	7.94
3	6.01
3	5.14
3	6.9
3	6.77
4	4.86
4	5.1
4	5.67
4	5.75
4	6.23
run;

*Transform the variable plasma by taking natural logarithm;
data logplasma;
set plasma;
logy=log(plasma);

proc reg data=logplasma;
  model logy=year/ lackfit;   *display lack of fit test results;
title ' Simple Linear Regression Plasma Example' ; 
run;
quit;
