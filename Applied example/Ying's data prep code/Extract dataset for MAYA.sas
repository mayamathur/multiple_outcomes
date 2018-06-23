libname  yc    "C:\Users\MSONG\manuscripts\Flourishing" ; *my home directory;

ods html file="C:\Users\MSONG\manuscripts\Flourishing\programs\Third version\sample.xls";

data complete;
set yc.flourishing;

if A1STATUS=2 and B1STATUS=2; *restrict to those who participated in both SAQ and phone interview;
if nmiss (of A1SEPA A1PAGE_M2 sibling A1SE9 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1PC1 CEDUC4cat A1SE7 A1SE8c A1PC14 mom_smk dad_smk B1PA58 A1SE6)>0 then delete; 

proc means N; 
var M2ID;
title "max sample size for complete case analysis";
run;

proc sort data=complete; by M2FAMNUM; run;

data sample1;
set complete;

proc freq data=sample1;
tables M2FAMNUM /out=countfreq;
run;

proc freq data=countfreq;
tables count;
title 'check on clustering by siblings';
run;

*extract participants without siblings included in the sample;
data group1; 
set complete;
by M2FAMNUM;
if first.M2FAMNUM and last.M2FAMNUM;
run;

*extract participants with siblings included in the sample;
data group2;
set complete;
by M2FAMNUM;
if first.M2FAMNUM and last.M2FAMNUM then delete;
run;

*randomly selected one participant within each family that had siblings included;
proc surveyselect data=group2 method=srs n=1 seed=100 out=group3;
strata M2FAMNUM;
run;

proc sort data=group3; by M2FAMNUM; run;
proc freq data=group3;
tables M2FAMNUM /out=countfreq;
run;

proc freq data=countfreq;
tables count;
title 'check whether siblings were removed';
run;

proc means N data=group3;
var M2ID;
title 'sample size of the extracted data';
run;

proc sort data=group1; by M2ID; run;
proc sort data=group3; by M2ID; run;

*merge two groups;
data flourish_new;
merge group1 group3;
by M2ID;
run;

proc contents data=flourish_new; run;

*output the extracted dataset*;
data yc.flourish_new;
set flourish_new;
run;

ods html close;
