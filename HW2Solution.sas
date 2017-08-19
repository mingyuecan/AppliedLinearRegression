/*STAT 448 Spring 2017
Homework2 Solution
*/
ods html close; 
options nodate nonumber;
title;
ods rtf file='C:\Stat 448 spring 2017\HW2Solution.rtf' nogtitle startpage=no;
ods noproctitle;



/*data for exercise 1*/
data peanut;
 input treat $13. allergy $1. count;
cards;
control      N 87
control      Y 43
intervention N 109
intervention Y 8
; run;

/*data for exercises 2 and 3*/
data fish; set sashelp.fish;
format weight_cat $30.;
if species in ('Bream','Perch','Roach');
if weight=. then delete;
if weight<=135 then weight_cat='1-Light';
else if 135<weight<=500 then weight_cat='2-Medium';
else if weight>500 then weight_cat='3-Heavy';
run;

proc univariate data=fish;
var weight; 
run;

proc freq data=fish;
tables weight_cat; run;

/* Exercise 1*/
proc freq data=peanut;
	tables treat*allergy/ nocol nopercent chisq expected;
	weight count;
run;
*part c;
proc freq data=peanut;
	tables treat*allergy/ nocol nopercent chisq expected riskdiff;
	weight count;
run;


/* Exercise 2 */
proc freq data=fish;
	tables species*weight_cat/expected chisq norow nocol nopercent;
run;

proc freq data=fish;
	tables species*weight_cat/expected chisq nocol nopercent riskdiff;
	where weight_cat in ('1-Light','3-Heavy') and species in ('Bream','Perch');
run;


/* Exercise 3 */

data fish; set fish;
lweight=log(weight);
run;

proc anova data=fish;
	class species;
	model lweight = species;
	means species/ hovtest tukey  welch cldiff;
	ods select HOVFTest ModelANOVA OverallANOVA Welch FitStatistics CLDiffs;
run;

ods rtf close;
