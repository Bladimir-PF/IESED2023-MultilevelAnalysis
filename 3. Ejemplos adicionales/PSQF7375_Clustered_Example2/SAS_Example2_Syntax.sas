* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; ODS LISTING; ODS HTML CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 2 EXAMPLE     *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use TotalR2 macro;
* DV =        Case-sensitive name of dependent variable;
* PredFewer = Name of OUTPM= data file of predicted outcomes for nested model;
* PredMore =  Name of OUTPM= data file of predicted outcomes for comparison model;
%MACRO TotalR2(DV=,PredFewer=,PredMore=);
PROC CORR NOPRINT NOSIMPLE DATA=&PredFewer. OUTP=CorrFewer; VAR pred &DV.; RUN;
PROC CORR NOPRINT NOSIMPLE DATA=&PredMore.  OUTP=CorrMore;  VAR pred &DV.; RUN;
DATA CorrFewer; LENGTH Name $30.; SET CorrFewer; Name="&PredFewer."; RUN;
DATA CorrMore;  LENGTH Name $30.; SET CorrMore;  Name="&PredMore.";  RUN;
DATA CorrCompare; LENGTH Name $30.; SET CorrFewer CorrMore; 
     PredCorr=Pred; TotalR2=PredCorr*PredCorr; 
     IF _NAME_="Pred" OR MISSING(_NAME_)=1 THEN DELETE; DROP Pred; RUN;
DATA CorrCompare; SET CorrCompare; TotalR2Diff=TotalR2-LAG1(TotalR2);
     KEEP Name PredCorr TotalR2 TotalR2Diff; RUN; 
TITLE9 "Total R2 (% Reduction) for &PredFewer. vs. &PredMore.";
PROC PRINT NOOBS DATA=CorrCompare; RUN; TITLE9; 
%MEND TotalR2;

* To use Regions Macro, enter;
* FixData =   Name of ODS SolutionF table that stores fixed effects for model;
* CovBData =  Name of ODS CovB table that stores XTX inv matrix for model;
* Pred =      Case-sensitive name of predictor effect regions are for; 
* Mod =       Case-sensitive name of moderator effect (for region values);
* ModCenter = Centering point of moderator predictor;
* Interact =  Case-sensitive name of interaction effect; 
* Order =     Order of entry of interaction in MODEL statement;
%MACRO Regions(FixData=,CovBData=,Pred=,Mod=,ModCenter=,Interact=,Order=);
DATA _NULL_; SET &FixData.; WHERE Effect="&Pred.";
     CALL SYMPUT('Bpred', Estimate); 
     CALL SYMPUT('SEpred', StdErr); RUN; 
DATA _NULL_; SET &FixData.; WHERE Effect="&Interact.";
     CALL SYMPUT('Binter', Estimate); 
     CALL SYMPUT('SEinter', StdErr); RUN; 
%LET order=%EVAL(&order.+1);
DATA _NULL_; SET &CovBData.; 
     WHERE INDEX(Effect,"&Pred.")>0 AND INDEX(Effect,"*")=0;
     CALL SYMPUT('CovPredInt', ROUND(Col&order.,.0001)); RUN;    
%PUT Bpred=&Bpred. SEpred=&SEpred. Binter=&Binter. 
     SEinter=&SEinter. CovPredInt=&CovPredInt.;
DATA Regions;
     A=(1.96*1.96)*(&SEinter.*&SEinter.)-(&Binter.*&Binter.);
     B=2*((1.96*1.96)*&CovPredInt.-(&Bpred.*&Binter.));
     C=(1.96*1.96*&SEpred.*&SEpred.)-(&Bpred.*&Bpred.);
     CenteredLower=((-1*B)+SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenlower',ROUND(CenteredLower,.001));
     CenteredUpper=((-1*B)-SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenupper',ROUND(CenteredUpper,.001));
     UncenteredLower=CenteredLower+&ModCenter.; 
        CALL SYMPUT('uncenlower',ROUND(UncenteredLower,.001));
     UncenteredUpper=CenteredUpper+&ModCenter.; 
        CALL SYMPUT('uncenupper',ROUND(UncenteredUpper,.001));
RUN;
TITLE7 "Regions of significance for &interact. interaction:";
TITLE8 "The effect of &pred. will be significant at centered values of &mod. BELOW the lower bound"; 
TITLE9 "and ABOVE the upper bound, which translate to these uncentered lower and upper bounds."; 
PROC PRINT DATA=Regions NOOBS; VAR CenteredLower--UncenteredUpper; RUN; TITLE7; TITLE8; TITLE9;
%MEND Regions;

***********************************************************************************;
*******                 BEGIN DATA MANIPULATION FOR EXAMPLE 2               *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
  %LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example2;
* Location for SAS files for these models (uses macro variable filesave);
  LIBNAME filesave "&filesave.";

* Import chapter 2 example data into work library;
DATA work.Chapter2; SET filesave.SAS_Chapter2;
* Centering continuous predictors;
age85 = age - 85;
grip9 = grip - 9;
* Creating manual contrasts for dementia groups (to be treated as continuous);
     IF demgroup=1 THEN DO; demNF=0; demNC=0; END; * None group;
ELSE IF demgroup=2 THEN DO; demNF=1; demNC=0; END; * Future group;
ELSE IF demgroup=3 THEN DO; demNF=0; demNC=1; END; * Current group;
* Labeling new variables – note semi-colon is only at the end of ALL labels;
LABEL   
age85=  "age85: Age in Years (0=85)"
grip9=  "grip9: Grip Strength in Pounds (0=9)"
sexMW=  "sexMW: Sex (0=M, 1=W)"
demNF=  "demNF: Dementia Contrast for None=0 vs Future=1"
demNC=  "demNC: Dementia Contrast for None=0 vs Current=1"
cognition= "Cognition Outcome"
demgroup=  "Dementia Group 1N 2F 3C";
RUN;

* Creating value labels to use as needed (not stored in data);
PROC FORMAT; VALUE FDemGroup 1="1None" 2="2Future" 3="3Current"; 
		   VALUE FSex 0="0Men" 1="1Women"; 
RUN;

***********************************************************************************;
*******                       BEGIN EXAMPLE 2A MODELS                       *******;
***********************************************************************************;

* Open output directory to save results to;
ODS RTF FILE="&filesave.\SAS_Example2a_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

TITLE1 "Chapter 2: Descriptive Statistics for Example Variables";
PROC MEANS NOLABEL NONOBS NDEC=2 DATA=work.Chapter2; VAR age grip cognition; RUN;
PROC FREQ DATA=work.Chapter2; 
	TABLE sexMW*demgroup / NOROW NOCOL; 
	FORMAT sexMW Fsex. demgroup FDemGroup.; RUN;
TITLE1;

TITLE1 'Eq 2.3: Empty Means Model';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
     MODEL cognition =  / SOLUTION DDFM=BW;
RUN; TITLE1;
 
TITLE1 'Eq 2.7: Age + Grip + Sex (0=M 1=W, as continuous predictor)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
     MODEL cognition = age85 grip9 sexMW 
					/ SOLUTION DDFM=BW OUTPM=PredAgeGripSex;
     CONTRAST "Model R2 F-Test" age85 1, grip9 1, sexMW 1;
RUN; TITLE1;
PROC CORR NOSIMPLE DATA=work.PredAgeGripSex; 
	VAR cognition; WITH pred; RUN;  


TITLE1 'Eq 2.8: Adding Dementia Group';
TITLE2 'Using Manual Group Contrasts so Reference=None';
TITLE3 'sexMW, demNF, and demNC are all treated as continuous predictors';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC 
					/ SOLUTION DDFM=BW OUTPM=PredAgeGripSexDem;
	CONTRAST "Model R2 F-Test" age85 1, grip9 1, sexmw 1, demNF 1, demNC 1;
     CONTRAST "Omnibus F-Test for Dementia Group" demNF 1, demNC 1;
* Request conditional (adjusted) group means (hold age=85, grip=9, men because not there);
  ESTIMATE "Intercept for None Group"    intercept 1 demNF 0 demNC 0; * Given as B0;
  ESTIMATE "Intercept for Future Group"  intercept 1 demNF 1 demNC 0; * Not given (B0+B4); 
  ESTIMATE "Intercept for Current Group" intercept 1 demNF 0 demNC 1; * Not given (B0+B5); 
  ESTIMATE "Intercept for Ever Group"    intercept 1 demNF .5 demNC .5; * B0 + (B4+B5)/2;
* Request group differences (unconditional because there are no interactions);
  ESTIMATE "None   vs. Future Group"   demNF  1 demNC 0; * Given as B4;
  ESTIMATE "None   vs. Current Group"  demNF  0 demNC 1; * Given as B5;
  ESTIMATE "Future vs. Current Group"  demNF -1 demNC 1; * Not given (B5-B4); 
  ESTIMATE "None vs. Ever Group"       demNF .5 demNC .5; * Not given (B5+B4)/2;
RUN; TITLE1; TITLE2; TITLE3;
PROC CORR NOSIMPLE DATA=work.PredAgeGripSexDem; VAR cognition; WITH pred; RUN; 

TITLE1 'Eq 2.8: Adding Dementia Group';
TITLE2 'Categorical Predictor for Dementia Group on CLASS statement';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
     CLASS demgroup; * CLASS statement demgroup replaces previous dem contrasts;
	FORMAT demgroup Fdemgroup.; * Use value labels defined earlier in output;
     MODEL cognition = age85 grip9 sexMW demgroup / SOLUTION DDFM=BW;
     CONTRAST "Model R2 F-Test with df=5" age85 1, grip9 1, sexmw 1, 
                                          demgroup -1 1 0, demgroup -1 0 1;
* Request conditional (adjusted) group means (hold age=85, grip=9, men) and all diffs;
     LSMEANS demgroup / DIFF=ALL AT(age85 grip9 sexMW) = (0 0 0);
* Request conditional (adjusted) group means and all differences for demonstration;
     LSMEANS demgroup / DIFF=ALL;
*** All of the code below is redundant with LSMEANS, but here is how you get all the info;
*** The exceptions are the lines that refer to the "ever" group, which is not a default;
  CONTRAST "Omnibus F-Test for Dementia Group with df=2"  demgroup -1 1 0, demgroup -1 0 1;
* Request conditional (adjusted) group means (hold age=85, grip=9, men);
  ESTIMATE "Intercept for None Group"    intercept 1 demgroup 1 0 0; * Not given (B0+B4);
  ESTIMATE "Intercept for Future Group"  intercept 1 demgroup 0 1 0; * Not given (B0+B5);
  ESTIMATE "Intercept for Current Group" intercept 1 demgroup 0 0 1; * Given as B0;
  ESTIMATE "Intercept for Ever Group"    intercept 1 demgroup 0 .5 .5; * (B0+B5)/2;
* Request group differences (unconditional because there are no interactions);
  ESTIMATE "None   vs. Future Group"   demgroup -1  1  0; * Not given (B5-B4);
  ESTIMATE "None   vs. Current Group"  demgroup -1  0  1; * Given as B5;
  ESTIMATE "Future vs. Current Group"  demgroup  0 -1  1; * Given as B4;
  ESTIMATE "None   vs. Ever Group"     demgroup -1 .5 .5; * (B0+B5)/2 - B4;
RUN; TITLE1; TITLE2;

* Close output;
ODS RTF CLOSE;

***********************************************************************************;
*******                       BEGIN EXAMPLE 2B MODELS                       *******;
***********************************************************************************;

* Open output directory to save results to;
ODS RTF FILE="&filesave.\SAS_Example2b_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

* Creating 'fake people' to show age*grip interaction;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeAgeGrip; INPUT PersonID grip9 age85 sexMW demNF demNC; 
DATALINES;
-99  3 -5  0  0  0 
-99  3  0  0  0  0 
-99  3  5  0  0  0 
-99  0 -5  0  0  0 
-99  0  0  0  0  0 
-99  0  5  0  0  0 
-99 -3 -5  0  0  0 
-99 -3  0  0  0  0 
-99 -3  5  0  0  0 
; RUN;
* Merge with real data;
DATA work.PlotAgeGrip; MERGE work.Chapter2 work.FakeAgeGrip; BY PersonID; RUN;

TITLE1 'Eq 2.9: Adding Age by Grip Interaction';
TITLE2 'Age 0=85 Grip 0=9';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotAgeGrip COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
MODEL cognition = age85 grip9 sexMW demNF demNC age85*grip9
                       / SOLUTION DDFM=BW COVB OUTPM=PredAgebyGrip;
CONTRAST "Model R2 F-Test" age85 1, grip9 1, sexmw 1, demNF 1, demNC 1, age85*grip9 1;
ODS OUTPUT SolutionF=FixAgeGrip COVB=CovBAgeGrip;
* Simple slopes of each main effect;
ESTIMATE 'Age Slope at Grip Strength =  6' age85 1 age85*grip9 -3;
ESTIMATE 'Age Slope at Grip Strength =  9' age85 1 age85*grip9  0;
ESTIMATE 'Age Slope at Grip Strength = 12' age85 1 age85*grip9  3;
ESTIMATE 'Grip Strength Slope at Age = 80' grip9 1 age85*grip9 -5;
ESTIMATE 'Grip Strength Slope at Age = 85' grip9 1 age85*grip9  0;
ESTIMATE 'Grip Strength Slope at Age = 90' grip9 1 age85*grip9  5; 
* If you are NOT using fake people, you have to write these to create predicted outcomes;
ESTIMATE 'Cognition at Grip = 12 Age = 80' intercept 1 age85 -5 grip9  3 age85*grip9 -15;
ESTIMATE 'Cognition at Grip = 12 Age = 85' intercept 1 age85  0 grip9  3 age85*grip9   0;
ESTIMATE 'Cognition at Grip = 12 Age = 90' intercept 1 age85  5 grip9  3 age85*grip9  15;
ESTIMATE 'Cognition at Grip =  9 Age = 80' intercept 1 age85 -5 grip9  0 age85*grip9   0;
ESTIMATE 'Cognition at Grip =  9 Age = 85' intercept 1 age85  0 grip9  0 age85*grip9   0;
ESTIMATE 'Cognition at Grip =  9 Age = 90' intercept 1 age85  5 grip9  0 age85*grip9   0;
ESTIMATE 'Cognition at Grip =  6 Age = 80' intercept 1 age85 -5 grip9 -3 age85*grip9  15;
ESTIMATE 'Cognition at Grip =  6 Age = 85' intercept 1 age85  0 grip9 -3 age85*grip9   0;
ESTIMATE 'Cognition at Grip =  6 Age = 90' intercept 1 age85  5 grip9 -3 age85*grip9 -15;
RUN; TITLE1; TITLE2;
PROC CORR NOSIMPLE DATA=work.PredAgebyGrip; VAR cognition; WITH pred; RUN; 

* Call macro for regions of significance for main effects of interaction;
%Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=grip9, Mod=age85,
         ModCenter=85, Interact=age85*grip9, Order=6);
%Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=age85, Mod=grip9,
         ModCenter=9, Interact=age85*grip9, Order=6)

* Subset predicted outcomes data to fake people;
DATA work.PredAgeGrip; SET work.PredAgeGrip; WHERE PersonID=-99; RUN;
TITLE9 'Predicted Outcomes for Fake People';
PROC PRINT NOOBS DATA=work.PredAgeGrip; VAR age85 grip9 sexMW demNF demNC pred; RUN;

* Creating 'fake people' to show age*grip*sex interaction;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeAgeGripSex; INPUT PersonID grip9 age85 sexMW demNF demNC; 
DATALINES;
-99   3  -5  0  0  0 
-99   3   0  0  0  0 
-99   3   5  0  0  0 
-99   0  -5  0  0  0 
-99   0   0  0  0  0 
-99   0   5  0  0  0 
-99  -3  -5  0  0  0 
-99  -3   0  0  0  0 
-99  -3   5  0  0  0 
-99   3  -5  1  0  0 
-99   3   0  1  0  0 
-99   3   5  1  0  0 
-99   0  -5  1  0  0 
-99   0   0  1  0  0 
-99   0   5  1  0  0 
-99  -3  -5  1  0  0 
-99  -3   0  1  0  0 
-99  -3   5  1  0  0 
; RUN;
* Merge with real data;
DATA work.PlotAgeGripSex; MERGE work.Chapter2 work.FakeAgeGripSex; BY PersonID; RUN;

TITLE1 'Eq 2.18: Adding Sex by Age by Grip Three-Way Interaction';
TITLE2 'Age 0=85 Grip 0=9 Sex 0=Men Dementia 0=None';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotAgeGripSex COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC age85*grip9 sexMW*demNF sexMW*demNC
                       age85*sexMW grip9*sexMW age85*grip9*sexMW 
						/ SOLUTION DDFM=BW OUTPM=Pred3AgeSexGrip;
	CONTRAST "Model R2 F-Test" age85 1, grip9 1, sexmw 1, demNF 1, demNC 1, age85*grip9 1,
			sexMW*demNF 1, sexMW*demNC 1, age85*sexMW 1, grip9*sexMW 1, age85*grip9*sexMW 1;
	CONTRAST "Model R2 Change F-Test from Sex*Demgroup" sexMW*demNF 1, sexMW*demNC 1;
	CONTRAST "Model R2 Change F-Test from Age*Grip*Sex" age85*sexMW 1, grip9*sexMW 1, age85*grip9*sexMW 1; 
* Request simple effects of Age;  
ESTIMATE "Age for Grip=6,  Men"	age85 1 age85*grip9 -3 age85*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Age for Grip=9,  Men"	age85 1 age85*grip9  0 age85*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Age for Grip=12, Men"	age85 1 age85*grip9  3 age85*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Age for Grip=6,  Women"	age85 1 age85*grip9 -3 age85*sexMW  1 age85*grip9*sexMW -3;
ESTIMATE "Age for Grip=9,  Women"	age85 1 age85*grip9  0 age85*sexMW  1 age85*grip9*sexMW  0;
ESTIMATE "Age for Grip=12, Women"	age85 1 age85*grip9  3 age85*sexMW  1 age85*grip9*sexMW  3;
* Request simple effects of Grip; 
ESTIMATE "Grip for Age=80, Men"	grip9 1 age85*grip9 -5 grip9*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Grip for Age=85, Men"	grip9 1 age85*grip9  0 grip9*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Grip for Age=90, Men"	grip9 1 age85*grip9  5 grip9*sexMW  0 age85*grip9*sexMW  0;
ESTIMATE "Grip for Age=80, Women"	grip9 1 age85*grip9 -5 grip9*sexMW  1 age85*grip9*sexMW -5;
ESTIMATE "Grip for Age=85, Women"	grip9 1 age85*grip9  0 grip9*sexMW  1 age85*grip9*sexMW  0;
ESTIMATE "Grip for Age=90, Women"	grip9 1 age85*grip9  5 grip9*sexMW  1 age85*grip9*sexMW  5;
* Request simple effects of Sex (hold demgroup 0=None); 
ESTIMATE "Sex: Age=80, Grip=6" 	sexMW 1 age85*sexMW -5 grip9*sexMW -3 age85*grip9*sexMW  15;
ESTIMATE "Sex: Age=85, Grip=6" 	sexMW 1 age85*sexMW  0 grip9*sexMW -3 age85*grip9*sexMW   0;
ESTIMATE "Sex: Age=90, Grip=6" 	sexMW 1 age85*sexMW  5 grip9*sexMW -3 age85*grip9*sexMW -15;
ESTIMATE "Sex: Age=80, Grip=9" 	sexMW 1 age85*sexMW -5 grip9*sexMW  0 age85*grip9*sexMW   0;
ESTIMATE "Sex: Age=85, Grip=9" 	sexMW 1 age85*sexMW  0 grip9*sexMW  0 age85*grip9*sexMW   0;
ESTIMATE "Sex: Age=90, Grip=9" 	sexMW 1 age85*sexMW  5 grip9*sexMW  0 age85*grip9*sexMW   0;
ESTIMATE "Sex: Age=80,Grip=12" 	sexMW 1 age85*sexMW -5 grip9*sexMW  3 age85*grip9*sexMW -15;
ESTIMATE "Sex: Age=85,Grip=12" 	sexMW 1 age85*sexMW  0 grip9*sexMW  3 age85*grip9*sexMW   0;
ESTIMATE "Sex: Age=90,Grip=12" 	sexMW 1 age85*sexMW  5 grip9*sexMW  3 age85*grip9*sexMW  15;
* Request simple two-way interactions of age*grip;
ESTIMATE "Age by Grip: Men"		age85*grip9 1 age85*grip9*sexMW  0;
ESTIMATE "Age by Grip: Women"	 	age85*grip9 1 age85*grip9*sexMW  1;
* Request simple two-way interactions of age*sex;
ESTIMATE "Age by Sex:  Grip=6"	age85*sexMW 1 age85*grip9*sexMW -3;
ESTIMATE "Age by Sex:  Grip=9"	age85*sexMW 1 age85*grip9*sexMW  0;
ESTIMATE "Age by Sex:  Grip=12"	age85*sexMW 1 age85*grip9*sexMW  3;
* Request simple two-way interactions of grip*sex;
ESTIMATE "Grip by Sex: Age=80"	grip9*sexMW 1 age85*grip9*sexMW -5;
ESTIMATE "Grip by Sex: Age=85"	grip9*sexMW 1 age85*grip9*sexMW  0;
ESTIMATE "Grip by Sex: Age=90"	grip9*sexMW 1 age85*grip9*sexMW  5;
RUN; TITLE1; TITLE2;

* Subset predicted outcomes data to fake people;
DATA work.Pred3AgeSexGrip; SET work.Pred3AgeSexGrip; WHERE PersonID=-99; RUN;
TITLE9 'Predicted Outcomes for Fake People';
PROC PRINT NOOBS DATA=work.Pred3AgeSexGrip; VAR age85 grip9 sexMW demNF demNC pred; RUN;

* Close output;
ODS RTF CLOSE;

***********************************************************************************;
*******                       BEGIN EXAMPLE 2C MODELS                       *******;
***********************************************************************************;

* Open output directory to save results to;
ODS RTF FILE="&filesave.\SAS_Example2c_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

TITLE1 'Eq 2.13: Adding Sex by Dementia Interaction';
TITLE2 'Continuous Sex (0=Men), Continuous Dementia (0=None)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 METHOD=REML;
MODEL cognition = age85 grip9 age85*grip9 sexMW demNF demNC
                  sexMW*demNF sexMW*demNC
                   / SOLUTION CHISQ DDFM=BW OUTPM=PredSexbyDem;
CONTRAST 'Model R2 Test' age85 1, grip9 1, age85*grip9 1, sexMW 1, demNF 1, demNC 1, 
				     sexMW*demNF 1, sexMW*demNC 1;
CONTRAST 'Omnibus Dementia*Sex Interaction Test' sexMW*demNF 1, sexMW*demNC 1;
* Cell means (predicted intercepts);
ESTIMATE 'Cognition for Men   None'       intercept 1 sexMW 0 demNF  0 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE 'Cognition for Women None'       intercept 1 sexMW 1 demNF  0 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE 'Cognition for Men   Future'     intercept 1 sexMW 0 demNF  1 demNC 0 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE 'Cognition for Women Future'     intercept 1 sexMW 1 demNF  1 demNC 0 sexMW*demNF 1 sexMW*demNC 0;
ESTIMATE 'Cognition for Men   Current'    intercept 1 sexMW 0 demNF  0 demNC 1 sexMW*demNF 0 sexMW*demNC 0;
ESTIMATE 'Cognition for Women Current'    intercept 1 sexMW 1 demNF  0 demNC 1 sexMW*demNF 0 sexMW*demNC 1;
* Simple effects of sex and dementia group;
ESTIMATE 'Sex Difference for No Dementia'             sexMW 1 demNF  0 demNC 0 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE 'Sex Difference for Future Dementia'         sexMW 1 demNF  0 demNC 0 sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE 'Sex Difference for Current Dementia'        sexMW 1 demNF  0 demNC 0 sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE 'None-Future Difference for Men'             sexMW 0 demNF  1 demNC 0 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE 'None-Future Difference for Women'           sexMW 0 demNF  1 demNC 0 sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE 'None-Current Difference for Men'            sexMW 0 demNF  0 demNC 1 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE 'None-Current Difference for Women'          sexMW 0 demNF  0 demNC 1 sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE 'Future-Current Difference for Men'          sexMW 0 demNF -1 demNC 1 sexMW*demNF  0 sexMW*demNC 0;
ESTIMATE 'Future-Current Difference for Women'        sexMW 0 demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1;
* Differences in simple effects = interactions;
ESTIMATE 'A: Sex Effect differ between None and Future?'        sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE 'A: None-Future Effect differ by Sex?'                 sexMW*demNF  1 sexMW*demNC 0;
ESTIMATE 'B: Sex Effect differ between None and Current?'       sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE 'B: None-Current Effect differ by Sex?'       	    sexMW*demNF  0 sexMW*demNC 1;
ESTIMATE 'C: Sex Effect differ between Future and Current?'     sexMW*demNF -1 sexMW*demNC 1;
ESTIMATE 'C: Future-Current Effect differ by Sex?'              sexMW*demNF -1 sexMW*demNC 1;
RUN; TITLE1; TITLE2;
PROC CORR NOSIMPLE DATA=work.PredSexbyDem; VAR cognition; WITH pred; RUN; 

TITLE1 'Eq 2.13: Adding Sex by Dementia Interaction';
TITLE2 'Continuous Sex (0=Men) and Categorical Dementia (Ref=Current)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
CLASS demgroup;              * Demgroup is categorical;
FORMAT demgroup Fdemgroup.;  * Use my value label in the output;
MODEL cognition = age85 grip9 age85*grip9 sexMW demgroup
                       sexMW*demgroup / SOLUTION CHISQ DDFM=BW;
CONTRAST 'Model R2 Test' age85 1, grip9 1, age85*grip9 1, sexMW  1, demgroup -1 1 0, demgroup -1 0 1, 
					sexMW*demgroup -1  1  0, sexMW*demgroup -1  0  1 / CHISQ;
* Marginal dementia group mean differences;
LSMEANS demgroup / DIFF=ALL AT(age85 grip9 sexMW) = (0 0 .5); * Averged across sex means (what F-tests give);
* Simple effects of dementia group diffs per sex;
LSMEANS demgroup / DIFF=ALL AT(age85 grip9 sexMW) = (0 0 0); * For men;
LSMEANS demgroup / DIFF=ALL AT(age85 grip9 sexMW) = (0 0 1); * For women;
* Simple effects of sex per dementia group;
ESTIMATE 'Sex Difference for None'             demgroup  0  0  0 sexMW 1 sexMW*demgroup  1  0  0;
ESTIMATE 'Sex Difference for Future'           demgroup  0  0  0 sexMW 1 sexMW*demgroup  0  1  0;
ESTIMATE 'Sex Difference for Current'          demgroup  0  0  0 sexMW 1 sexMW*demgroup  0  0  1;
* Simple effects of dementia group per sex (redundant with LSMEANS);
ESTIMATE 'None-Future Difference for Men'      demgroup -1  1  0 sexMW 0 sexMW*demgroup  0  0  0;
ESTIMATE 'None-Future Difference for Women'    demgroup -1  1  0 sexMW 0 sexMW*demgroup -1  1  0;
ESTIMATE 'None-Current Difference for Men'     demgroup -1  0  1 sexMW 0 sexMW*demgroup  0  0  0;
ESTIMATE 'None-Current Difference for Women'   demgroup -1  0  1 sexMW 0 sexMW*demgroup -1  0  1;
ESTIMATE 'Future-Current Difference for Men'   demgroup  0 -1  1 sexMW 0 sexMW*demgroup  0  0  0;
ESTIMATE 'Future-Current Difference for Women' demgroup  0 -1  1 sexMW 0 sexMW*demgroup  0 -1  1;
* Differences in simple effects = interactions;
ESTIMATE 'A: Sex Effect differ between None and Future?'                 sexMW*demgroup -1  1  0;
ESTIMATE 'A: None-Future Effect differ by Sex?'     		             sexMW*demgroup -1  1  0;
ESTIMATE 'B: Sex Effect differ between None and Current?'                sexMW*demgroup -1  0  1;
ESTIMATE 'B: None-Current Effect differ by Sex?'    		             sexMW*demgroup -1  0  1;
ESTIMATE 'C: Sex Effect differ between Future and Current?'              sexMW*demgroup  0 -1  1;
ESTIMATE 'C: Future-Current Effect differ by Sex?'  		             sexMW*demgroup  0 -1  1;
RUN; TITLE1; TITLE2;

TITLE1 'Eq 2.13: Adding Sex by Dementia Interaction';
TITLE2 'Categorical Sex (Ref=Women) and Categorical Dementia (Ref=Current)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
CLASS sexMW demgroup; * SexMW and Demgroup are categorical;
FORMAT sexMW Fsex. demgroup Fdemgroup.; * Use my value labels in the output;
MODEL cognition = age85 grip9 age85*grip9 sexMW demgroup
                  sexMW*demgroup / SOLUTION DDFM=BW;
CONTRAST 'Model R2 Test' age85 1, grip9 1, age85*grip9 1, sexMW -1 1, demgroup -1 1 0, demgroup -1 0 1,  
					sexMW*demgroup -1  1  0  1 -1  0, sexMW*demgroup -1  0  1  1  0 -1;
* Marginal sex means and tests diffs averaged over demgroup (F-test below);
LSMEANS sexMW    / DIFF=ALL AT(age85 grip9) = (0 0); 
* Marginal demgroup means and tests of diffs averaged over sex (F-test below);
LSMEANS demgroup / DIFF=ALL AT(age85 grip9) = (0 0);
* Cell means and simple effect tests (SLICE means per);
LSMEANS sexMW*demgroup / SLICE=demgroup SLICE=sexMW DIFF=ALL AT(age85 grip9) = (0 0); 
* Order by CLASS statement: MN MF MC WN WF WC -- check order with / E;
* Simple effects of sex per dementia group (redundant with LSMEANS);
ESTIMATE 'Sex Difference for None'             demgroup  0  0  0 sexMW -1 1 sexMW*demgroup -1  0  0  1  0  0;
ESTIMATE 'Sex Difference for Future'           demgroup  0  0  0 sexMW -1 1 sexMW*demgroup  0 -1  0  0  1  0;
ESTIMATE 'Sex Difference for Current'          demgroup  0  0  0 sexMW -1 1 sexMW*demgroup  0  0 -1  0  0  1;
* Simple effects of dementia group per sex (redundant with LSMEANS);
ESTIMATE 'None-Future Difference for Men'      demgroup -1  1  0 sexMW  0 0 sexMW*demgroup -1  1  0  0  0  0;
ESTIMATE 'None-Future Difference for Women'    demgroup -1  1  0 sexMW  0 0 sexMW*demgroup  0  0  0 -1  1  0;
ESTIMATE 'None-Current Difference for Men'     demgroup -1  0  1 sexMW  0 0 sexMW*demgroup -1  0  1  0  0  0;
ESTIMATE 'None-Current Difference for Women'   demgroup -1  0  1 sexMW  0 0 sexMW*demgroup  0  0  0 -1  0  1;
ESTIMATE 'Future-Current Difference for Men'   demgroup  0 -1  1 sexMW  0 0 sexMW*demgroup  0 -1  1  0  0  0;
ESTIMATE 'Future-Current Difference for Women' demgroup  0 -1  1 sexMW  0 0 sexMW*demgroup  0  0  0  0 -1  1;
* Simple effect differences = interactions;
ESTIMATE "A: Sex Effect differ between None and Future?"                    sexMW*demgroup -1  1  0  1 -1  0;
ESTIMATE "A: None-Future Effect differ by Sex?"                             sexMW*demgroup -1  1  0  1 -1  0;
ESTIMATE "B: Sex Effect differ between None and Current?"                   sexMW*demgroup -1  0  1  1  0 -1;
ESTIMATE "B: None-Current Effect differ by Sex?"                            sexMW*demgroup -1  0  1  1  0 -1;
ESTIMATE "C: Sex Effect differ between Future and Current?"                 sexMW*demgroup  0 -1  1  0  1 -1;
ESTIMATE "C: Future-Current Effect differ by Sex?"                          sexMW*demgroup  0 -1  1  0  1 -1;
RUN; TITLE1; TITLE2;

* Close output;
ODS RTF CLOSE;
