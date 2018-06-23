libname  yc    "C:\Users\MSONG\manuscripts\Flourishing" ; *my home directory;
*%inc 'C:\Users\MSONG\table1.sas' ;


****************************************************IMPORT DATASET*************************************************************************************;
DATA b;  *Import MIDUS 1 Data;
set Da2760p1(keep= M2ID A1STATUS A1SBMI A1SA23 A1PRSEX A1SS7 A1PB17 A1PB1 A1PC2 A1PC8 A1SHHTOT A1SE2 A1SE3 A1SE4 A1SE5 A1SE7 A1PI_YR A1PI_MO A1PB17
A1PTSEI A1PTSEIP A1PDEPRE A1PA43 A1PA40 A1PA53 A1SVIGOR A1SMODER SAMPLMAJ M2FAMNUM A1SE13 A1SE14A A1SE14B A1SE14C A1SE14D A1SE14E A1SE14K
A1SE14F A1SE14G A1SE14H A1SE14I A1SE15 A1SE16A A1SE16B A1SE16C A1SE16D A1SE16E A1SE16K A1SE16F A1SE16G A1SE16H A1SE16I A1PA36 A1SA9C A1SA10E A1SCHRON 
A1SA27 A1SA9B A1SA9A A1SA9G A1PAGE_M2 A1PA11 A1PA29CC A1SE17A A1SE17B A1SE17C A1SE17D A1SE17E A1SE17F A1SE17G A1SE17H A1SE17I A1SE17J A1SE17K A1SE17L A1SE17M 
A1PDEPDX A1SE17N A1SE17O A1PA12 A1SA9X A1SA9Z A1SA10B A1SA10D A1PC1 A1SE18A A1SE18B A1SE18C A1SE19A A1SE19B A1SE19C A1PC3 A1PC9 A1PC14 A1SE9 A1SE8 A1SD1 A1SD3 
A1SE6 A1PA41 A1PA52 A1PB36B1 A1PB35 A1SKINPO A1SFDSPO A1SSPEMP A1SM2 A1SM3 A1SM4 A1SM5 A1SM11 A1SM12 A1SM13 A1SM14 A1SP11 A1SP12 A1SP13 A1SP14 A1SP15 A1SP16 A1SA25 A1SA27 SAMPLMAJ A1STATUS);
run;

data a;	*Import MIDUS 2 Data ;
set DA4652P1(keep=M2ID B1STATUS B1SBMI B1SA35 B1PRSEX B1PF7A B1PB1 B1PAGE_M2 B1SMPQSR B1PA26 B1SA11C B1SA12E B1SA39 B1STATUS B1SA37A M2FAMNUM B1PA45A B1PA45B B1PA45C B1PA58 B1PBYEAR 
B1SFDCOP B1SE13G B1SE13N B1PDEPDX B1PDEPRE B1SA39 B1SA37B B1SA37A B1PDEPDX B1PDEPRE B1PANXIE B1PANXTD B1SKINPO B1SFDSPO B1SSPEMP B1SSATIS B1SORIEN B1SPOSAF B1SPWBA2
B1SPWBE2 B1SPWBG2 B1SPWBR2 B1SPWBU2 B1SPWBS2 B1SESTEE B1SNEURO B1SCONS2 B1SSPIRI B1SMNDFU 
B1SA62A B1SA62B B1SA62C B1SA62D B1SA62E B1SA62F B1SA62G B1SA62H B1SA62I B1SA62J B1SBMI B1PA39
B1SA30E B1SA30F B1SA31E B1SA31F B1SA32E B1SA32F B1PA1 B1SA11A B1SA11S B1SA12A B1SA11X B1SA12B B1SA11Z B1SA12C B1PA8 B1PA26 B1SA12B B1PIDATE
B1SA26C B1SA26I B1SA26E B1SH16O B1SH16B B1SH16E B1SH16N B1SH16H B1SE1X B1SE1T B1SE1HH B1SE1AA B1SE1S B1SE1K B1PA52 B1PA53
B1SPOSAF B1SQ1 B1SSWBMS B1SSWBSI B1SSWBAO B1SSWBSC B1SSWBSA B1SPWBA1 B1SPWBE1 B1SPWBG1 B1SPWBR1 B1SPWBU1 B1SPWBS1 B1SN11A B1SN11B B1SN11C B1SN11D B1SN11E B1SN11F B1SN11G 
B1SN11H B1SN11I B1SN2B B1SN2D B1SL11A B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SJ2A B1SJ2B B1SJ2C B1SJ2D B1SE10A B1SE10B B1SE10C B1SE10D 
B1SE10E B1SE10F B1STINC1 B1STATUS B1PB1 B1STINC1 B1PB19 B1POCPMJ B1POCMAJ B1PBWORK B1SN3D
B1SA26A B1SA26B B1SA26C B1SA26D B1SA26E B1SA26F B1SE1M B1SE1S B1SE1KK B1SE1B B1SE1H B1SE1T B1SE1I B1SE1AA B1SE1GG B1SE1J B1SE1BB B1SE1HH B1SE1E B1SE1OO B1SE1QQ B1SE1F B1SE1X B1SE1DD
B1SH16A B1SH16H B1SH16B B1SH16F B1SH16K B1SH16C B1SH16J B1SH16N B1SH16D B1SH16G B1SH16O B1SH16E B1SH16I B1SH16M);
run;

data c;  *Import MIDUS2 Biomarker Project Data;
set Da29282p1;
run;

proc sort data=b; by M2ID; run;
proc sort data=a;; by M2ID; run;
proc sort data=c;by M2ID; run;

DATA MERGED;*Merge datasets;
MERGE b c a;
by M2ID; 
if SAMPLMAJ=13 then delete; /*Remove Miwauwakee participants from the merged sample since they didn't participate in MIDUS 1*/
run;


************ RECODE VARIABLES RELATED TO PARENTAL WARMTH******************************;
data parenting;
set Merged;

*maternal wamrth;
if A1SE14A<1 or A1SE14A>4 then A1SE14Ac=.; *recode maternal warmth - understand problems;
else A1SE14Ac=5-A1SE14A;
label A1SE14Ac='maternal warmth-understand problems' ;

if A1SE14B<1 or A1SE14B>4 then A1SE14Bc=.; *recode maternal warmth - confide;
else A1SE14Bc=5-A1SE14B;
label A1SE14Bc='maternal warmth-confide' ;

if A1SE14C<1 or A1SE14C>4 then A1SE14Cc=.; *recode maternal warmth - love;
else A1SE14Cc=5-A1SE14C;
label A1SE14Cc='maternal warmth-love' ;

if A1SE14D<1 or A1SE14D>4 then A1SE14Dc=.; *recode maternal warmth - attention;
else A1SE14Dc=5-A1SE14D;
label A1SE14Dc='maternal warmth-attention' ;

if A1SE14E<1 or A1SE14E>4 then A1SE14Ec=.; *recode maternal warmth - upbring;
else A1SE14Ec=5-A1SE14E;
label A1SE14Ec='maternal warmth-upbrining' ;

if A1SE14K<1 or A1SE14K>4 then A1SE14Kc=.; *recode maternal warmth - teach;
else A1SE14Kc=5-A1SE14K;
label A1SE14Kc='maternal warmth-teach' ;

if nmiss (of A1SE14Ac A1SE14Bc A1SE14Cc A1SE14Dc A1SE14Ec A1SE14Kc)>3 then A1SE14MA=.;
else A1SE14MA= 6* mean (of A1SE14Ac A1SE14Bc A1SE14Cc A1SE14Dc A1SE14Ec A1SE14Kc);
label A1SE14MA	='maternal warmth' ;


*maternal discipline;
if A1SE14F<1 or A1SE14F>4 then A1SE14Fc=.; *recode maternal discipline - strict;
else A1SE14Fc=5-A1SE14F;
label A1SE14Fc='maternal discipline-strict' ;

if A1SE14G<1 or A1SE14G>4 then A1SE14Gc=.; *recode maternal discipline - consistent;
else A1SE14Gc=5-A1SE14G;
label A1SE14Gc='maternal discipline-consistent' ;

if A1SE14I<1 or A1SE14I>4 then A1SE14Ic=.; *recode maternal discipline - stop;
else A1SE14Ic=5-A1SE14I;
label A1SE14Ic='maternal discipline-stop' ;

if A1SE14H<1 or A1SE14H>4 then A1SE14Hc=.; *recode maternal discipline - punish;
else A1SE14Hc=5-A1SE14H;
label A1SE14Hc='maternal discipline-punish' ;

if nmiss (of A1SE14Fc A1SE14Gc A1SE14Ic)>1 then A1SE14MD=.; 
else A1SE14MD= 3* mean(of A1SE14Fc A1SE14Gc A1SE14Ic);
label A1SE14MD='maternal discipline' ;

if nmiss (of A1SE14Fc A1SE14Gc A1SE14Ic A1SE14H)>2 then A1SE14MDc=.; *an alternative measure including a fourth item on punishing;
else A1SE14MDc= 4* mean (of A1SE14Fc A1SE14Gc A1SE14Ic A1SE14Hc);
label A1SE14MDc='maternal discipline (alternative)' ;

*paternal wamrth;
if A1SE16A<1 or A1SE16A>4 then A1SE16Ac=.; *recode paternal warmth - understand problems;
else A1SE16Ac=5-A1SE16A;
label A1SE16Ac='paternal warmth-understand problems' ;

if A1SE16B<1 or A1SE16B>4 then A1SE16Bc=.; *recode paternal warmth - confide;
else A1SE16Bc=5-A1SE16B;
label A1SE16Bc='paternal warmth-confide' ;

if A1SE16C<1 or A1SE16C>4 then A1SE16Cc=.; *recode paternal warmth - love;
else A1SE16Cc=5-A1SE16C;
label A1SE16Cc='paternal warmth-love' ;

if A1SE16D<1 or A1SE16D>4 then A1SE16Dc=.; *recode paternal warmth - attention;
else A1SE16Dc=5-A1SE16D;
label A1SE16Dc='paternal warmth-attention' ;

if A1SE16E<1 or A1SE16E>4 then A1SE16Ec=.; *recode paternal warmth - upbring;
else A1SE16Ec=5-A1SE16E;
label A1SE16Ec='paternal warmth-upbrining' ;

if A1SE16K<1 or A1SE16K>4 then A1SE16Kc=.; *recode paternal warmth - teach;
else A1SE16Kc=5-A1SE16K;
label A1SE16Kc='paternal warmth-teach' ;

if nmiss (of A1SE16Ac A1SE16Bc A1SE16Cc A1SE16Dc A1SE16Ec A1SE16Kc)>3 then A1SE16FA=.;
else A1SE16FA= 6* mean (of A1SE16Ac A1SE16Bc A1SE16Cc A1SE16Dc A1SE16Ec A1SE16Kc);
label A1SE16FA= 'paternal warmth' ;

*paternal discipline;
if A1SE16F<1 or A1SE16F>4 then A1SE16Fc=.; *recode paternal discipline - strict;
else A1SE16Fc=5-A1SE16F;
label A1SE16Fc='paternal discipline-strict' ;

if A1SE16G<1 or A1SE16G>4 then A1SE16Gc=.; *recode paternal discipline - consistent;
else A1SE16Gc=5-A1SE16G;
label A1SE16Gc='paternal discipline-consistent' ;

if A1SE16I<1 or A1SE16I>4 then A1SE16Ic=.; *recode paternal discipline - stop;
else A1SE16Ic=5-A1SE16I;
label A1SE16Ic='paternal discipline-stop' ;

if A1SE16H<1 or A1SE16H>4 then A1SE16Hc=.; *recode paternal discipline - punish;
else A1SE16Hc=5-A1SE16H;
label A1SE16Hc='paternal discipline-punish' ;

if nmiss (of A1SE16Fc A1SE16Gc A1SE16Ic)>1 then A1SE16FD=.;
else A1SE16FD= 3* mean (of A1SE16Fc A1SE16Gc A1SE16Ic);
label A1SE16FD= 'paternal discipline' ;

if nmiss (of A1SE16Fc A1SE16Gc A1SE16Ic A1SE16Hc)>2 then A1SE16FDc=.;*alternative: including a fourth item on punish;
else A1SE16FDc= 4* mean (of A1SE16Fc A1SE16Gc A1SE16Ic A1SE16Hc);
label A1SE16FDc= 'paternal discipline' ;

A1SEPA= mean (of A1SE14MA A1SE16FA);
label A1SEPA='parental warmth' ;

A1SEPD= mean (of A1SE14MD A1SE16FD);
label A1SEPD='parental discipline' ;

A1SEPDc= mean (of A1SE14MDc A1SE16FDc);
label A1SEPDc='parental discipline (alternative)' ;

A1SEPA_z=A1SEPA;

proc standard mean=0 std=1 out=parenting;
var A1SEPA_z;
run;

/*
******Check on the distribution and factor analyses of warmth and control******************;
data check;
set parenting;

*Maternal warmth*;

proc gchart;
vbar A1SE14Ac A1SE14Bc A1SE14Cc A1SE14Dc A1SE14Ec A1SE14Kc/discrete;
title;
run;

proc univariate;
var A1SE14MA;
histogram/midpoints=(1 to 4 by 1);
title 'Distribution of maternal warmth' ;
run;

proc factor method=ml n=1 scree;
var A1SE14Ac A1SE14Bc A1SE14Cc A1SE14Dc A1SE14Ec A1SE14Kc;
title 'Factor analysis of maternal warmth scale (N=1)' ;
run;

*Maternal discipline;
proc gchart;
vbar A1SE14Fc A1SE14Gc A1SE14Ic A1SE14Hc/discrete;
title '' ;
run;

proc univariate;
var A1SE14MD;
histogram/midpoints=(1 to 4 by 1);;
title 'Distribution of maternal discipline' ;
run;

proc univariate;
var A1SE14MDc;
histogram/midpoints=(1 to 4 by 1);;
title 'Distribution of maternal discipline(alternative)' ;
run;

proc factor method=ml n=1 scree;
var A1SE14Fc A1SE14Gc A1SE14Ic;
title 'Factor analysis of maternal discipline scale (N=1)' ;
run;

proc factor method=ml n=1 scree;
var A1SE14Fc A1SE14Gc A1SE14Ic A1SE14Hc;
title 'Factor analysis of maternal discipline scale (alternative)' ;
run;

*paternal warmth;
proc gchart;
vbar A1SE16Ac A1SE16Bc A1SE16Cc A1SE16Dc A1SE16Ec A1SE16Kc/discrete;
title'' ;
run;

proc univariate;
var A1SE16FA;
histogram/midpoints=(1 to 4 by 1);
title 'Distribution of paternal warmth' ;
run;

proc factor method=ml n=1 scree;
var A1SE16Ac A1SE16Bc A1SE16Cc A1SE16Dc A1SE16Ec A1SE16Kc;
title 'Factor analysis of paternal warmth scale (N=1)' ;
run;

*paternal control;
proc gchart;
vbar A1SE16Fc A1SE16Gc A1SE16Ic A1SE16Hc/discrete;
title '' ;
run;

proc univariate;
var A1SE16FD;
histogram/midpoints=(1 to 4 by 1);
title 'Distribution of paternal control' ;
run;

proc univariate;
var A1SE16FDc;
histogram/midpoints=(1 to 4 by 1);
title 'Distribution of paternal control(alternative)' ;
run;

proc factor method=ml n=1 scree;
var A1SE16Fc A1SE16Gc A1SE16Ic ;
title 'Factor analysis of the paternal control scale' ;
run;

proc factor method=ml n=1 scree;
var A1SE16Fc A1SE16Gc A1SE16Ic A1SE16Hc;
title 'Factor analysis of the paternal control scale (alternative)' ;
run;

***Parental warmth****;
proc univariate;
var A1SEPA;
histogram/midpoints=(1 to 4 by 1);;
title 'Distribution of parental warmth' ;
run;

proc univariate;
var A1SEPD;
histogram/midpoints=(1 to 4 by 1);;
title 'Distribution of parental discipline' ;
run;

proc univariate;
var A1SEPDc;
histogram;
title 'Distribution of parental discipline (alternative)' ;
run;

*/


****Parenting style _ Median and tertile split************;
data parenting;
set parenting;

proc rank groups=2 out=parenting; *median parental warmth, for creating parenting style;
var A1SEPA;
ranks A1SEPAd;
run;

proc rank groups=2 out=parenting; *median parental control, for creating parenting style;
var A1SEPDc;
ranks A1SEPDcd;
run;

proc rank groups=3 out=parenting; *tertile parental warmth, for creating parenting style;
var A1SEPA;
ranks A1SEPAt;
run;

proc rank groups=3 out=parenting; *tertile parental control, for creating parenting style;
var A1SEPDc;
ranks A1SEPDct;
run;

proc rank groups=3 out=parenting; *tertile maternal warmth;
var A1SE14MA;
ranks A1SE14MAt;
run;

proc rank groups=3 out=parenting; *tertile maternal control;
var A1SE16FA;
ranks A1SE16FAt;
run;

data par_stl;
set parenting;

if A1SEPAd=1 and A1SEPDcd=1 then style=1; *create parenting style using median split;
else if A1SEPAd=0 and A1SEPDcd=1 then style=2;
else if A1SEPAd=1 and A1SEPDcd=0 then style=3;
else if A1SEPAd=0 and A1SEPDcd=0 then style=4;
label style='parenting style (median split)' ;

if A1SEPAt=2 and A1SEPDct=2 then stylet=1; *create parenting style using top tertile split;
else if A1SEPAt in (0, 1) and A1SEPDct=2 then stylet=2;
else if A1SEPAt=2 and A1SEPDct in (0, 1) then stylet=3;
else if A1SEPAt in (0, 1) and A1SEPDct in (0, 1) then stylet=4;
label stylet='parenting style (tertile split)' ;

proc format;
value style  1='authoritative'
             2='authoritarian'
			 3='permissive'
			 4='neglectful' ;

value stylet 1='authoritative'
             2='authoritarian'
			 3='permissive'
			 4='neglectful' ;
run;


/***********Covariates*********************/
data control; 
set par_stl;

*age;
if A1PAGE_M2<20 or A1PAGE_M2>75 then A1PAGE_M2=.; *age, impute missing with MIDUS2 age-9;
if B1PAGE_M2<28 or B1PAGE_M2>84 then B1PAGE_M2=.;
if A1PAGE_M2=. then A1PAGE_M2=B1PAGE_M2-9;
label A1PAGE_M2='MIDUS1 Age' ;

A1PAGE_sqr=A1PAGE_M2* A1PAGE_M2;
label A1PAGE_sqr='quardtratic age' ;

*sex;
if A1PRSEX>2 or A1PRSEX<1 then A1PRSEX=.;  *sex: 1=male 2=female, impute missing with MIDUS2 sex;
if B1PRSEX>2 or B1PRSEX<1 then B1PRSEX=.;  
if A1PRSEX=. then A1PRSEX=B1PRSEX; 
LABEL A1PRSEX = 'sex' ;

*race;
if A1SS7>6 or A1SS7<1 then A1SS7=.;	*race: 1=white, 2=black, 3= others, impute missing with MIDUS2 race;
if B1PF7A>6 or B1PF7A<1 then B1PF7A=.;
if A1SS7=. then A1SS7=B1PF7A;
if A1SS7=. then raceA=.;
else if A1SS7=1 then raceA=1;
else if A1SS7=2 then raceA=2;
else raceA=3;
LABEL raceA = 'race' ;

*participant nativity;
If A1SE2<1 or A1SE2>2 then A1SE2=.;

*parents' nativity;
If A1SE3<1 or A1SE3>2 then A1SE3=.;
If A1SE4<1 or A1SE4>2 then A1SE4=.;

*language in childhood;
if A1SE5<1 or A1SE5>4 then A1SE5=.;

* lived with both parents in childhood;
if A1PC1<1 or A1PC1>2 then A1PC1=.;

*number of siblings;
if A1SE18A<0 or A1SE18A>19 then A1SE18A=.;
if A1SE18B<0 or A1SE18B>11 then A1SE18B=.;
if A1SE18C<0 or A1SE18C>2 then A1SE18C=.;
if A1SE19A<0 or A1SE19A>23 then A1SE19A=.;
if A1SE19B<0 or A1SE19B>13 then A1SE19B=.;
if A1SE19C<0 or A1SE19C>4 then A1SE19C=.;

sibling= sum (of A1SE18A A1SE18B A1SE18C A1SE19A A1SE19B A1SE19C);
label sibling='number of siblings' ;

if sibling>5 then sibling=5; *winsorize;

*parental education;
IF (A1PC2 = 1) OR (A1PC2 = 2) OR (A1PC2 = 3) THEN FEDUC4cat = 1;  *childhood SES - fathers'education: less than HS=1, HS=2, Some college=3, College degree or more=4 ;
ELSE IF (A1PC2 = 4) OR (A1PC2 = 5) THEN FEDUC4cat = 2;
ELSE IF (A1PC2 = 6) OR (A1PC2 = 7) OR (A1PC2 = 8) THEN FEDUC4cat = 3;
ELSE IF (A1PC2 = 9) OR (A1PC2 = 10) OR (A1PC2 = 11) OR (A1PC2 = 12) THEN FEDUC4cat = 4;
ELSE FEDUC4cat = .;
LABEL FEDUC4cat = 'Fathers Education' ;

IF (A1PC8 = 1) OR (A1PC8 = 2) OR (A1PC8 = 3) THEN MEDUC4cat = 1;  *childhood SES - mothers'education: less than HS=1, HS=2, Some college=3, College degree or more=4 ;
ELSE IF (A1PC8 = 4) OR (A1PC8 = 5) THEN MEDUC4cat = 2;
ELSE IF (A1PC8 = 6) OR (A1PC8 = 7) OR (A1PC8 = 8) THEN MEDUC4cat = 3;
ELSE IF (A1PC8 = 9) OR (A1PC8 = 10) OR (A1PC8 = 11) OR (A1PC8 = 12) THEN MEDUC4cat = 4;
ELSE MEDUC4cat = .;
LABEL MEDUC4cat = 'Mothers Education' ;

CEDUC4cat= max (of MEDUC4cat FEDUC4cat);* childhood SES is defined as the highest level of parental education;
LABEL CEDUC4cat = 'Childhood SES' ;

if CEDUC4cat=. then CEDUC4catc=.;
else if CEDUC4cat=4 then CEDUC4catc=1;
else CEDUC4catc=0;
label CEDUC4catc='parental education as college or more' ;

* on welfare in childhood;
if A1PC14<1 or A1PC14>2 then A1PC14=.;

* subjective SES;
if A1SE9<1 or A1SE9>7 then A1SE9=.;
A1SE9=8-A1SE9;

* residential area;
if A1SE7<1 or A1SE7>6 then A1SE7=.;

*residential mobility;
if A1SE8<0 or A1SE8>60 then A1SE8=.;
label A1SE8='times of moving to a new neighborhood' ;

if A1SE8=. then A1SE8c=.;
else if A1SE8<3 then A1SE8c=1;
else A1SE8c=0;
label A1SE8c='residentail stability' ;

*parents smoking in childhood;
if B1PA45B<1 or B1PA45B>4 then B1PA45B=.; 
if B1PA45B in (2, 4) then mom_smk=0;
else if B1PA45B=1 then mom_smk=1;
else mom_smk=.;
label mom_smk='mom smoking in childhood' ;

if B1PA45A<1 or B1PA45A>4 then B1PA45A=.; 
if B1PA45A in (2, 4) then dad_smk=0;
else if B1PA45A=1 then dad_smk=1;
else dad_smk=.;
label dad_smk='dad smoking in childhood' ;

*lived with alcoholics in childhood;
if B1PA58<1 or B1PA58>2 then B1PA58=.;

*importance of religion in family;
if A1SE6<1 or A1SE6>4 then A1SE6=.;

*adulthood education;
if A1PB1<1 or A1PB1>12 then A1PB1=.;
if A1PB1=. then A1PB1=B1PB1; *education: less than HS=1, HS=2, Some college=3, College degree or more=4, impute missing with MIDUS2 education;
IF (A1PB1 = 1) OR (A1PB1 = 2) OR (A1PB1 = 3) THEN AEDUC4catA = 1; 
else IF (A1PB1 = 4) OR (A1PB1 = 5) THEN AEDUC4catA = 2;
else IF (A1PB1 = 6) OR (A1PB1 = 7) OR (A1PB1 = 8) THEN AEDUC4catA = 3;
else IF (A1PB1 = 9) OR (A1PB1 = 10) OR (A1PB1 = 11) OR (A1PB1 = 12) THEN AEDUC4catA = 4;
else AEDUC4catA = .;
LABEL AEDUC4catA = 'Education' ;

*adulthood household income;
if A1SHHTOT>300000 or A1SHHTOT<0 then A1SHHTOT=.; *income;
label A1SHHTOT = 'Income' ;

*marital status;
if A1PB17=1 then marryA=1;
else if A1PB17 in (2, 3) then marryA=2;
else if A1PB17=4 then marryA=3;
else if A1PB17=5 then marryA=4;

*************M2 covairates for running the distribution analyses********;
*M2 education;
if B1PB1<1 or B1PB1>12 then B1PB1=.;
if B1PB1=. then B1PB1=B1PB1; *education: less than HS=1, HS=2, Some college=3, College degree or more=4, impute missing with MIDUS2 education;
IF (B1PB1 = 1) OR (B1PB1 = 2) OR (B1PB1 = 3) THEN BEDUC4catA = 1; 
else IF (B1PB1 = 4) OR (B1PB1 = 5) THEN BEDUC4catA = 2;
else IF (B1PB1 = 6) OR (B1PB1 = 7) OR (B1PB1 = 8) THEN BEDUC4catA = 3;
else IF (B1PB1 = 9) OR (B1PB1 = 10) OR (B1PB1 = 11) OR (B1PB1 = 12) THEN BEDUC4catA = 4;
else BEDUC4catA = .;
LABEL BEDUC4catA = 'Education' ;

if BEDUC4catA=. then BEDUC4catA=AEDUC4catA;

*M2 adulthood household income;
if B1STINC1>300000 or B1STINC1<0 then B1STINC1=.; *M2 income;
label B1STINC1= 'M2 income';

*M2 marital status;
if B1PB19=1 then marry=1;
else if B1PB19 in (2, 3) then marry=2;
else if B1PB19=4 then marry=3;
else if B1PB19=5 then marry=4;

*M2 Occupational Groups and working status*/;
if B1POCMAJ>9 or B1POCMAJ<1 then B1POCMAJ=.; *remove invalid values on major occupational groups;
if B1POCPMJ<1 or B1POCPMJ>9 then B1POCPMJ=.; *remove invalid values on previous occupational groups among those not currently employed or self-employed;

if B1POCMAJ in (1, 2) then B1POCMAJci= 1;  *code major occupational category of current job;
else if B1POCMAJ in (3,4,5,6) then B1POCMAJci=2;
else if B1POCMAJ in (7,8,9) then B1POCMAJci=3;

if B1POCMAJ=. then do; 
	 if B1PBWORK=5 then B1POCMAJci=12;
end;

B1POCMAJcc=B1POCMAJci; *code occupation groups for those currently retired;
if B1POCMAJci=12 then do;
    if B1POCPMJ in (1, 2) then B1POCMAJcc=1;
    else if B1POCPMJ in (3, 4, 5, 6) then B1POCMAJcc=2;
    else if B1POCPMJ in (7, 8, 9) then B1POCMAJcc=3;
	else B1POCMAJcc=.;
end;

if B1POCMAJcc=. then B1POCMAJcc=4; *4=not currently working;
label B1POCMAJcc= 'Major occupational category of current job or previous job before retirement';

*M2 religious service attendance;
if B1SN3D in (1, 2, 3) then religion=4;
else if B1SN3D=4 then religion=3; 
else if B1SN3D=5 then religion=2; 
else if B1SN3D=6 then religion=1; 

proc rank groups=4 out=control; *M1 income;
var A1SHHTOT;
ranks A1SHHTOTq;
run;

proc rank groups=4 out=control;*M2 income;
var B1STINC1;
ranks B1STINC1q;
run;

data control;
set control;

if B1STINC1q=. then B1STINC1q=A1SHHTOTq;

proc format;
value A1PRSEX 1= 'male' 2='female' ;

value raceA 
           1='White'
           2='Black'
           3='others' ;

value AEDUC4catA 
           1='less than HS'
           2='HS'
           3='college'
           4='college or more' ;

value BEDUC4catA 
           1='less than HS'
           2='HS'
           3='college'
           4='college or more' ;

Value CEDUC4cat 
           1= '<HS'
           2='HS'
           3='Some college'
           4='College or more' ;

value A1SE8c
           1='stable'
		   0='unstable' ;

value dad_smk
           1='smoker'
		   0='non-smoker' ;

value sibling
		   1='1'
		   2='2'
		   3='3'
		   4='4'
		   5='5 or more' ;

value marry
           1='married'
		   2='divorce/separated'
		   3='widowed'
		   4='never married';

value B1POCMAJcc
      1= 'Managerial/professional'
	  2= 'Technical/Sales/Clerical/Service'
	  3= 'Manual'
      4= 'not currently working';

value religion
     1= 'never'
	 2= '<1/month'
	 3= '1-3 times/month'
	 4= 'at least 1/week';

format raceA raceA.;
format A1PRSEX A1PRSEX.;
format AEDUC4catA AEDUC4catA.;
format CEDUC4cat CEDUC4cat.;
format marry marry.;
format B1POCMAJcc B1POCMAJcc.;
format religion;
run;


/********CODING THE OUTCOME VARIABLES (wave 2)**********/

data outcome;
set control;

****CREATE THE FLOURISHING SCORE********;

*1. Emotional well-being;
if B1SQ1<0 or B1SQ1>10 then B1SQ1=.; *satisfaction with life overall;

   if B1SA26A<1 or B1SA26A>5 then B1SA26Ac=.; else B1SA26Ac=6-B1SA26A;
   if B1SA26B<1 or B1SA26B>5 then B1SA26Bc=.; else B1SA26Bc=6-B1SA26B;
   if B1SA26C<1 or B1SA26C>5 then B1SA26Cc=.; else B1SA26Cc=6-B1SA26C;
   if B1SA26D<1 or B1SA26D>5 then B1SA26Dc=.; else B1SA26Dc=6-B1SA26D;
   if B1SA26E<1 or B1SA26E>5 then B1SA26Ec=.; else B1SA26Ec=6-B1SA26E;
   if B1SA26F<1 or B1SA26F>5 then B1SA26Fc=.; else B1SA26Fc=6-B1SA26F;

if nmiss (of B1SA26Ac B1SA26Bc B1SA26Cc B1SA26Dc B1SA26Ec B1SA26Fc)>3 then B1SPOSAF=.; *positive affect;
else B1SPOSAF= 6* mean (of B1SA26Ac B1SA26Bc B1SA26Cc B1SA26Dc B1SA26Ec B1SA26Fc);
label B1SPOSAF='positive affect' ;

*2. Social well-being;

array soc[15] B1SH16A B1SH16B B1SH16C B1SH16D B1SH16E B1SH16F B1SH16G B1SH16H B1SH16I B1SH16J B1SH16K B1SH16L B1SH16M B1SH16N B1SH16O;
array soc_c[15] B1SH16A_c B1SH16B_c B1SH16C_c B1SH16D_c B1SH16E_c B1SH16F_c B1SH16G_c B1SH16H_c B1SH16I_c B1SH16J_c B1SH16K_c B1SH16L_c B1SH16M_c B1SH16N_c B1SH16O_c;

do i=1 to 15;
   soc_c[i]=soc[i];
   if soc_c[i]<1 or soc_c[i]>7 then soc_c[i]=.;
end;
   
array soc_r[6] B1SH16F_c B1SH16K_c B1SH16C_c B1SH16N_c B1SH16D_c B1SH16E_c;

do i=1 to 6;
   soc_r[i]=8-soc_r[i]; *reverse code some items as necessary, so that higher scores indicate greater social well-being ;
end;

*Meaningfulness of society;
if nmiss (of B1SH16A_c B1SH16H_c)>1 then B1SSWBMS=.;
else B1SSWBMS=2*mean (of B1SH16A_c B1SH16H_c);

*Social integration;
if nmiss (of B1SH16B_c B1SH16F_c B1SH16K_c)>1 then B1SSWBSI=.;
else B1SSWBSI=3*mean (of B1SH16B_c B1SH16F_c B1SH16K_c);

*Social acceptance;
if nmiss (of B1SH16C_c B1SH16J_c B1SH16N_c)>1 then B1SSWBAO=.;
else B1SSWBAO=3*mean (of B1SH16C_c B1SH16J_c B1SH16N_c);

*Social contribution;
if nmiss (of B1SH16D_c B1SH16G_c B1SH16O_c)>1 then B1SSWBSC=.;
else B1SSWBSC=3*mean (of B1SH16D_c B1SH16G_c B1SH16O_c);

*Social actualization;
if nmiss (of B1SH16E_c B1SH16I_c B1SH16M_c)>1 then B1SSWBSA=.;
else B1SSWBSA=3*mean (of B1SH16E_c B1SH16I_c B1SH16M_c);


*3. Psychological well-being;
array pwb [18] B1SE1M B1SE1S B1SE1KK B1SE1B B1SE1H B1SE1T B1SE1I B1SE1AA B1SE1GG B1SE1J B1SE1BB B1SE1HH B1SE1E B1SE1OO B1SE1QQ B1SE1F B1SE1X B1SE1DD;
array pwb_c [18] B1SE1M_c B1SE1S_c B1SE1KK_c B1SE1B_c B1SE1H_c B1SE1T_c B1SE1I_c B1SE1AA_c B1SE1GG_c B1SE1J_c B1SE1BB_c B1SE1HH_c B1SE1E_c B1SE1OO_c B1SE1QQ_c B1SE1F_c B1SE1X_c B1SE1DD_c;

do i=1 to 18;
   pwb_c[i]=pwb[i];
   if pwb_c[i]<1 or pwb_c[i]>7 then pwb_c[i]=.;
end;

array pwb_r[10] B1SE1S_c B1SE1KK_c B1SE1B_c B1SE1T_c B1SE1I_c B1SE1AA_c B1SE1BB_c B1SE1OO_c B1SE1F_c B1SE1X_c;

do i=1 to 10;
   pwb_r[i]=8-pwb_r[i]; *reverse code some items as necessary, so that higher scores indicate greater psychological well-being ;
end;

*autonomy;
if nmiss (of B1SE1M_c B1SE1S_c B1SE1KK_c)>1 then B1SPWBA1=.;
else B1SPWBA1= 3* mean (of B1SE1M_c B1SE1S_c B1SE1KK_c);

*environmental mastery;
if nmiss (of B1SE1B_c B1SE1H_c B1SE1T_c)>1 then B1SPWBE1=.;
else B1SPWBE1= 3* mean (of B1SE1B_c B1SE1H_c B1SE1T_c);

*personal growth;
if nmiss (of B1SE1I_c B1SE1AA_c B1SE1GG_c)>1 then B1SPWBG1=.;
else B1SPWBG1= 3* mean (of B1SE1I_c B1SE1AA_c B1SE1GG_c);

*positive relation;
if nmiss (of B1SE1J_c B1SE1BB_c B1SE1HH_c)>1 then B1SPWBR1=.;
else B1SPWBR1= 3* mean (of B1SE1J_c B1SE1BB_c B1SE1HH_c);

*purpose in life;
if nmiss (of B1SE1E_c B1SE1OO_c B1SE1QQ_c)>1 then B1SPWBU1=.;
else B1SPWBU1= 3* mean (of B1SE1E_c B1SE1OO_c B1SE1QQ_c);

*self acceptance;
if nmiss (of B1SE1F_c B1SE1X_c B1SE1DD_c)>1 then B1SPWBS1=.;
else B1SPWBS1= 3* mean (of B1SE1F_c B1SE1X_c B1SE1DD_c);

*standardize life satisfaction and positive affect scores; 
B1SPOSAFz=B1SPOSAF; B1SQ1z=B1SQ1;
proc standard mean=0 std=1 out=outcome;
var B1SPOSAFz B1SQ1z;
run;

*1. the continous measure (standardized);
data outcome;
set outcome;

emotion= B1SPOSAFz + B1SQ1z;
psych=   B1SPWBA1 + B1SPWBE1 + B1SPWBG1 + B1SPWBR1 + B1SPWBU1 + B1SPWBS1;
social=  B1SSWBMS + B1SSWBSI + B1SSWBAO + B1SSWBSC + B1SSWBSA;

emotion_z=emotion; psych_z=psych; social_z=social; 

proc standard mean=0 std=1 out=outcome;
var emotion_z psych_z social_z;
run;

data outcome;
set outcome;

if nmiss (of emotion_z psych_z social_z)>0 then flourish=.;
else flourish= sum (of emotion_z psych_z social_z);

flourish_z=flourish;

proc standard mean=0 std=1 out=outcome;
var flourish_z;
run;

*2. the dichotomized measure (Keyes approach);
proc rank groups=3 out=outcome; 
var B1SPOSAF;
ranks B1SPOSAFt;
run;

proc rank groups=3 out=outcome; 
var B1SQ1;
ranks B1SQ1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBA1;
ranks B1SPWBA1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBE1;
ranks B1SPWBE1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBG1;
ranks B1SPWBG1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBR1;
ranks B1SPWBR1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBU1;
ranks B1SPWBU1t;
run;

proc rank groups=3 out=outcome; 
var B1SPWBS1;
ranks B1SPWBS1t;
run;

proc rank groups=3 out=outcome; 
var B1SSWBMS;
ranks B1SSWBMSt;
run;

proc rank groups=3 out=outcome; 
var B1SSWBSI;
ranks B1SSWBSIt;
run;

proc rank groups=3 out=outcome; 
var B1SSWBAO;
ranks B1SSWBAOt;
run;

proc rank groups=3 out=outcome; 
var B1SSWBSC;
ranks B1SSWBSCt;
run;

proc rank groups=3 out=outcome; 
var B1SSWBSA;
ranks B1SSWBSAt;
run;

data outcome;
set outcome;

if B1SPOSAFt=2 or B1SQ1t=2 then high_emo=1;

if B1SPOSAFt=2 then B1SPOSAF_h=1;
else if B1SPOSAFt in (0, 1)then B1SPOSAF_h=0;

if B1SQ1t=2 then B1SQ1_h=1;
else if B1SQ1t in (0, 1) then B1SQ1_h=0;

array origin [11] B1SPWBA1t B1SPWBE1t B1SPWBG1t B1SPWBR1t B1SPWBU1t B1SPWBS1t B1SSWBMSt B1SSWBSIt B1SSWBAOt B1SSWBSCt B1SSWBSAt;
array high  [11] B1SPWBA1_h B1SPWBE1_h B1SPWBG1_h B1SPWBR1_h B1SPWBU1_h B1SPWBS1_h B1SSWBMS_h B1SSWBSI_h B1SSWBAO_h B1SSWBSC_h B1SSWBSA_h;  

do i=1 to 11;
   if origin[i]=2 then high[i]=1;
   else if origin[i] in (0, 1) then high[i]=0;
end;

score=sum (of B1SPWBA1_h B1SPWBE1_h B1SPWBG1_h B1SPWBR1_h B1SPWBU1_h B1SPWBS1_h B1SSWBMS_h B1SSWBSI_h B1SSWBAO_h B1SSWBSC_h B1SSWBSA_h);
if score GE 6 then high_psso=1;

if flourish=. then flourish_c=.; *keyes' dichotomized measure;
else if high_emo=1 and high_psso=1 then flourish_c=1;
else flourish_c=0;

*3. our count measure (the number of subdomains with a score in the top tertile (standardized);
flourish_d= B1SPOSAF_h+ B1SQ1_h+ B1SPWBA1_h+ B1SPWBE1_h+ B1SPWBG1_h+ B1SPWBR1_h+ B1SPWBU1_h+ B1SPWBS1_h+ B1SSWBMS_h+ B1SSWBSI_h+ B1SSWBAO_h+ B1SSWBSC_h+ B1SSWBSA_h; *the count measure of flourishing;

flourish_dz=flourish_d;

array psw [11] B1SPWBA1 B1SPWBE1 B1SPWBG1 B1SPWBR1 B1SPWBU1 B1SPWBS1 B1SSWBMS B1SSWBSI B1SSWBAO B1SSWBSC B1SSWBSA;
array pwsz[11] B1SPWBA1z B1SPWBE1z B1SPWBG1z B1SPWBR1z B1SPWBU1z B1SPWBS1z B1SSWBMSz B1SSWBSIz B1SSWBAOz B1SSWBSCz B1SSWBSAz;

do i=1 to 11; pwsz[i]=psw[i]; end;

proc standard mean=0 std=1 out=outcome;
var flourish_dz B1SPWBA1z B1SPWBE1z B1SPWBG1z B1SPWBR1z B1SPWBU1z B1SPWBS1z B1SSWBMSz B1SSWBSIz B1SSWBAOz B1SSWBSCz B1SSWBSAz;
run;


/*****Start coding of other outcome variables******/
data outcome;
set outcome;

/*Mental illness*/

*depression;
if B1PDEPDX<0 or B1PDEPDX>1 then B1PDEPDX=.;*dummy variable;
if B1PDEPRE<0 or B1PDEPRE>7 then B1PDEPRE=.; *continuous score;

*Generalized anxiety disorder;
if B1PANXIE<0 or B1PANXIE>10 then B1PANXIE=.;*continuous score;
if B1PANXTD<0 or B1PANXTD>1 then B1PANXTD=.; *dummy variable;

/*Unhealthy behaviors*/
*marijuana use; 
if B1SA62G=1 then B1SA62G=1;
else if B1SA62G=2 then B1SA62G=0;
else B1SA62G=.;

*other drug use; 
if B1SA62A=1 then B1SA62A=1; *sedatives;
else if B1SA62A=2 then B1SA62A=0;
else B1SA62A=.;

if B1SA62B=1 then B1SA62B=1; *tranquilizers;
else if B1SA62B=2 then B1SA62B=0;
else B1SA62B=.;

if B1SA62C=1 then B1SA62C=1; *stimulants;
else if B1SA62C=2 then B1SA62C=0;
else B1SA62C=.;

if B1SA62D=1 then B1SA62D=1; *pain killers;
else if B1SA62D=2 then B1SA62D=0;
else B1SA62D=.;

if B1SA62E=1 then B1SA62E=1; *depress meds;
else if B1SA62E=2 then B1SA62E=0;
else B1SA62E=.;

if B1SA62F=1 then B1SA62F=1; *inhalants meds;
else if B1SA62F=2 then B1SA62F=0;
else B1SA62F=.;

if B1SA62H=1 then B1SA62H=1; *Cocaine;
else if B1SA62H=2 then B1SA62H=0;
else B1SA62H=.;

if B1SA62I=1 then B1SA62I=1; *LSD;
else if B1SA62I=2 then B1SA62I=0;
else B1SA62I=.;

if B1SA62J=1 then B1SA62J=1; *Heroin;
else if B1SA62J=2 then B1SA62J=0;
else B1SA62J=.;

other= sum (of B1SA62A B1SA62B B1SA62C B1SA62D B1SA62E B1SA62F B1SA62H B1SA62I B1SA62J); *2. any other drugs;
if other GE 1 then oth_sub=1;
else if other=0 then oth_sub=0;
label oth_sub='any other drugs' ;

*obesity;
if B1SBMI<14.23 or B1SBMI>82.307 then B1SBMI=.; *B1SBMI is the BMI variable in MIDUS2 project 1;
if B4PBMI<14.986 or B4PBMI>65.088 then B4PBMI=.;*B1SBMI is the BMI variable in MIDUS2 project 4;
if B1SBMI=. then B1SBMI=B4PBMI;

if B1SBMI=. then B1SBMIc=.; *define overweight and obesity in MIDUS2;
else if B1SBMI LT 25 then B1SBMIc=0; *normal weight;
else B1SBMIc=1; *overweight or obese;
label B1SBMIc='overweight/obese' ;

*smoking;
if B1PA39<1 or B1PA39 in (7, 8) then smoke=.;
else if B1PA39=1 then smoke=1; *current smoker;
else if B1PA39=2 then smoke=1; *former smoker;
else if B1PA39=9 then smoke=0; *never smoker;
label smoke='smoking' ;

*binge drinking;
if B1PA53=97 or B1PA53=98 then binge=.;
else if B1PA53=99 then binge=0;
else binge=B1PA53;

if binge=. then binge_c=.;
else if binge=0 then binge_c=0;
else if binge>0 then binge_c=1;

*physical activity;
array phy_act[6] B1SA30E B1SA30F B1SA31E B1SA31F B1SA32E B1SA32F;

DO i=1 to 6;
   if phy_act[i]<1 or phy_act[i]>6 then phy_act[i]=.;
   phy_act[i]=7-phy_act[i]; *reverse coding so that higher scores reporesent more activity;
end; drop i;

vigor= mean (of B1SA30E B1SA30F); label vigor='vigorous physical activity' ;
moder= mean (of B1SA31E B1SA31F); label moder='moderate physical activity' ;
light= mean (of B1SA32E B1SA32F); label light='light physical activity' ;

vigor_z=vigor;

proc standard mean=0 std=1 out=outcome;
var vigor_z;
run;

data yc.flourishing;
set outcome;
run;

/*distribution of all substantive variables*/
proc means n nmiss mean std median min max data=outcome;
var A1PAGE_M2 sibling 
    flourish flourish_d emotion psych social B1SPOSAF B1SQ1 B1SPWBA1 B1SPWBE1 B1SPWBG1 B1SPWBR1 B1SPWBU1 B1SPWBS1 B1SSWBMS B1SSWBSI B1SSWBAO B1SSWBSC B1SSWBSA
    emotion_z psych_z social_z flourish_z flourish_dz M2ID M2FAMNUM
    A1SE14MA A1SE16FA A1SE14MDc A1SE16FDc A1SEPA A1SEPA_z A1SEPAt A1SE9 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1PC1 CEDUC4cat A1SE7 A1SE8c A1PC14 mom_smk dad_smk B1PA58 A1SE6 flourish_c;
where A1STATUS=2 and B1STATUS=2;
run;

proc freq data=outcome;
tables A1SE9 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1PC1 CEDUC4cat A1SE7 A1SE8c A1PC14 mom_smk dad_smk B1PA58 A1SE6 flourish_c BEDUC4catA B1STINC1q marry B1POCMAJcc religion;
run;


data sample0; set outcome; if A1STATUS=2 and B1STATUS=2;run; /*only keep those who partipated in SAQ at both waves*/

proc means N NMISS;
var BEDUC4catA B1STINC1 marry B1POCMAJcc religion;
run;

proc freq;
tables BEDUC4catA B1STINC1q marry B1POCMAJcc religion;
run;

data sibling;
set sample0 (keep=M2FAMNUM SAMPLMAJ M2ID);
if SAMPLMAJ in (2, 3);
run;

proc freq data=sibling;
tables M2FAMNUM /out=countfreq;
run;

proc freq data=countfreq;
tables count;
run;


/***********FOR SAMPLE SIZE FLOW CHART (parental warmth)************/
ods html file="C:\Users\MSONG\manuscripts\Flourishing\programs\Third version\missing.xls";

data sample0; set outcome; if A1STATUS ne 2 then delete; if B1STATUS ne 2 then delete; run;
data sample1; set sample0; if A1SEPA=. then delete; run;
data sample2; set sample1; if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58)>0 then delete; run;                                      
data sample3; set sample2; proc means N NMISS; var B1SBMIc smoke binge_c B1SA62G oth_sub flourish ;run;
ods html close;

/***********FOR SAMPLE SIZE FLOW CHART (maternal warmth)************/
data sample0; set outcome; if B1STATUS in (., 8) then delete; run;
data sample1; set sample0; if A1SE14MA=. then delete; run;
data sample2; set sample1; if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58)>0 then delete; run;                                      
data sample3; set sample2; proc means N NMISS; var B1SBMIc smoke binge_c B1SA62G oth_sub flourish ;run;

/***********FOR SAMPLE SIZE FLOW CHART (paternal warmth)************/

data sample0; set outcome; if B1STATUS in (., 8) then delete; run;
data sample1; set sample0; if A1SE16FA=. then delete; run;
data sample2; set sample1; if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58)>0 then delete; run;                                      
data sample3; set sample2; proc means N NMISS; var B1SBMIc smoke binge_c B1SA62G oth_sub flourish ;run;

/***********FOR SAMPLE SIZE FLOW CHART (parenting style)************/

data sample0; set outcome; if B1STATUS in (., 8) then delete; run;
data sample1; set sample0; if style=. then delete; run;
data sample2; set sample1; if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58)>0 then delete; run;                                      
data sample3; set sample2; proc means N NMISS; var B1SBMIc smoke binge_c B1SA62G oth_sub flourish ;run;

/***********FOR SAMPLE SIZE FLOW CHART (maternal and paternal warmth)************/
data sample0; set outcome; if B1STATUS in (., 8) then delete; run;
data sample1; set sample0; if A1SE14MA=. or A1SE16FA=. then delete; run;
data sample2; set sample1; if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58)>0 then delete; run;                                      
data sample3; set sample2; proc means N NMISS; var B1SBMIc smoke binge_c B1SA62G oth_sub flourish ;run;


/***********CHECK SCALE MISSING************/
data check1;set sample0; nmiss_ma= nmiss (of A1SE14Ac A1SE14Bc A1SE14Cc A1SE14Dc A1SE14Ec A1SE14Kc); proc freq; tables nmiss_ma; run;
data check2;set sample0; nmiss_pa= nmiss (of A1SE16Ac A1SE16Bc A1SE16Cc A1SE16Dc A1SE16Ec A1SE16Kc); proc freq; tables nmiss_pa; run;

ods html file="C:\Users\MSONG\manuscripts\Flourishing\programs\Third version\missing.xls";
data check3;set sample0; nmiss_ps= nmiss (of B1SA26Ac B1SA26Bc B1SA26Cc B1SA26Dc B1SA26Ec B1SA26Fc); proc freq; tables nmiss_ps; run;
data check4;set sample0; nmiss_pb1= nmiss (of B1SE1M_c B1SE1S_c B1SE1KK_c); proc freq; tables nmiss_pb1; run;
data check5;set sample0; nmiss_pb2= nmiss (of B1SE1B_c B1SE1H_c B1SE1T_c); proc freq; tables nmiss_pb2; run;
data check6;set sample0; nmiss_pb3= nmiss (of B1SE1I_c B1SE1AA_c B1SE1GG_c); proc freq; tables nmiss_pb3; run;
data check7;set sample0; nmiss_pb4= nmiss (of B1SE1J_c B1SE1BB_c B1SE1HH_c); proc freq; tables nmiss_pb4; run;
data check8;set sample0; nmiss_pb5= nmiss (of B1SE1E_c B1SE1OO_c B1SE1QQ_c); proc freq; tables nmiss_pb5; run;
data check9;set sample0; nmiss_pb6= nmiss (of B1SE1F_c B1SE1X_c B1SE1DD_c); proc freq; tables nmiss_pb6; run;
data check10;set sample0; nmiss_sb1= nmiss (of B1SH16A_c B1SH16H_c); proc freq; tables nmiss_sb1; run;
data check11;set sample0; nmiss_sb2= nmiss (of B1SH16B_c B1SH16F_c B1SH16K_c); proc freq; tables nmiss_sb2; run;
data check12;set sample0; nmiss_sb3= nmiss (of B1SH16C_c B1SH16J_c B1SH16N_c); proc freq; tables nmiss_sb3; run;
data check13;set sample0; nmiss_sb4= nmiss (of B1SH16D_c B1SH16G_c B1SH16O_c); proc freq; tables nmiss_sb4; run;
data check14;set sample0; nmiss_sb5= nmiss (of B1SH16E_c B1SH16I_c B1SH16M_c); proc freq; tables nmiss_sb5; run;
ods html close;

/***********COMPARE INCLUDED PARTICIPANTS WITH EXCLUDED************/
data equality;
set sample0;

if A1STATUS=2 and B1STATUS=2; *restrict to those who participated in both SAQ and phone interview;
if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58 A1SEPA flourish)>0 then include=0;
else if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58 A1SEPA flourish)=0 then include=1;

proc freq;
tables include;
run;

proc sort; by include;
proc freq;
tables include*(A1PRSEX raceA AEDUC4catA A1SHHTOTq MarryA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 CEDUC4cat A1PC14 A1SE7 A1SE8c mom_smk dad_smk B1PA58 )/chisq;
run; 

proc glm;
class include;
model A1PAGE_M2=include;
means include;
run;

proc glm;
class include;
model A1SE9 =include;
means include;
run;

proc glm;
class include;
model sibling =include;
means include;
run;


/***********COMPARE PARTICIPANTS WHO STAYED IN THE CORHOT TO PARTICIPANTS LOST TO FOLLOWED UP IN M2************/

data attrition;
set outcome;

if A1STATUS ne 2 then delete;
if B1STATUS in (., 8) then lost=1;
else lost=0;

if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58 A1SEPA flourish)>0 then include=0;
else if nmiss (of A1PAGE_M2 A1PRSEX raceA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 sibling CEDUC4cat A1PC14 A1SE9 A1SE7 A1SE8c mom_smk dad_smk B1PA58 A1SEPA flourish)=0 then include=1;

proc freq;
tables lost;
run;

proc sort; by lost;
proc freq;
tables lost*(A1PRSEX raceA AEDUC4catA A1SHHTOTq MarryA A1SE2 A1SE3 A1SE4 A1SE6 A1PC1 CEDUC4cat A1PC14 A1SE7 A1SE8c mom_smk dad_smk )/chisq;
run; 

proc glm;
class lost;
model A1PAGE_M2=lost;
means lost;
run;

proc glm;
class lost;
model A1SE9 =lost;
means lost;
run;

proc glm;
class lost;
model sibling =lost;
means lost;
run;

proc glm;
class lost;
model A1SEPA =lost;
means lost;
run;



