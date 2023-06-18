* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Open datasets even if the formats are not available;
  OPTIONS NOfmterr; 
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; 
* Turn on old-school output in addition to HTML output;
  ODS LISTING; 

***********************************************************************************;
*******     MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR THIS EXAMPLE        *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use TotalR2 macro;
* DV =        Case-sensitive name of dependent variable;
* PredFewer = Name of OUTPM= data file of predicted outcomes for nested model;
* PredMore =  Name of OUTPM= data file of predicted outcomes for comparison model;
%MACRO TotalR2(DV, PredFewer, PredMore);
PROC CORR NOPRINT NOSIMPLE DATA=&PredFewer. OUTP=CorrFewer; VAR pred &DV.; RUN;
PROC CORR NOPRINT NOSIMPLE DATA=&PredMore.  OUTP=CorrMore;  VAR pred &DV.; RUN;
DATA CorrFewer; LENGTH Name $30.; SET CorrFewer; Name="&PredFewer."; RUN;
DATA CorrMore;  LENGTH Name $30.; SET CorrMore;  Name="&PredMore.";  RUN;
DATA CorrCompare; LENGTH Name $30.; SET CorrFewer CorrMore; 
     IF Pred=. THEN Pred=0;
     PredCorr=Pred; TotalR2=PredCorr*PredCorr; 
     IF _NAME_="Pred" OR MISSING(_NAME_)=1 THEN DELETE; DROP Pred; RUN;
DATA CorrCompare; SET CorrCompare; TotalR2Diff=TotalR2-LAG1(TotalR2);
     KEEP Name PredCorr TotalR2 TotalR2Diff; RUN; 
TITLE2 "Total R2 (% Reduction) for &PredFewer. vs. &PredMore.";
PROC PRINT NOOBS DATA=CorrCompare; RUN; TITLE2;   
%MEND TotalR2;

* To use PseudoR2 macro;
* Ncov =     TOTAL # entries in covariance parameter estimates table;
* CovFewer = Name of ODS CovParms table for nested model;
* CovMore =  Name of ODS CovParms table for comparison model;
%MACRO PseudoR2(NCov=,CovFewer=,CovMore=);
DATA &CovFewer.; LENGTH Name $30.; SET &CovFewer.; Name="&CovFewer."; RUN;
DATA &CovMore.;  LENGTH Name $30.; SET &CovMore.;  Name="&CovMore.";  RUN;
DATA CovCompare; LENGTH Name $30.; SET &CovFewer. &CovMore.; RUN;
DATA CovCompare; SET CovCompare; 
     PseudoR2=(LAG&Ncov.(Estimate)-Estimate)/LAG&Ncov.(Estimate); RUN;
DATA CovCompare; SET CovCompare; 
     IF CovParm IN("UN(2,1)","UN(3,1)","UN(3,2)",
             "UN(4,1)","UN(4,2)","UN(4,3)",
             "UN(5,1)","UN(5,2)","UN(5,3)","UN(5,4)",
             "UN(6,1)","UN(6,2)","UN(6,3)","UN(6,4)","UN(6,5)") THEN DELETE; RUN;
TITLE9 "PseudoR2 (% Reduction) for &CovFewer. vs. &CovMore.";
PROC PRINT NOOBS DATA=CovCompare; RUN; TITLE9;
%MEND PseudoR2;

* To use FitTest macro;
* FitFewer = name of infocrit table for nested model;
* FitMore  = name of infocrit table for comparison model;
%MACRO FitTest(FitFewer, FitMore);
DATA &FitFewer.; LENGTH Name $30.; SET &FitFewer.; Name="&FitFewer."; RUN;
DATA &FitMore.;  LENGTH Name $30.; SET &FitMore.;  Name="&FitMore.";  RUN;
DATA FitCompare; LENGTH Name $30.; SET &FitFewer. &FitMore.; RUN;
DATA FitCompare; SET FitCompare; DevDiff=Lag1(Neg2LogLike)-Neg2LogLike;
     DFdiff=Parms-LAG1(Parms); Pvalue=1-PROBCHI(DevDiff,DFdiff);
     DROP AICC HQIC CAIC; RUN;
TITLE2 "Likelihood Ratio Test for &FitFewer. vs. &FitMore.";
PROC PRINT NOOBS DATA=FitCompare; RUN; TITLE2;
%MEND FitTest;

***********************************************************************************;
*******         BEGIN IMPORT AND MANIPUATION OF CROSSED EXAMPLE DATA        *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example4;
LIBNAME example "&filesave.";

* Open output directory to save results to;
ODS RTF FILE="&filesave.\PSQF7375_Clustered_Example4_SAS_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

* Import example 4 SAS dataset;
DATA work.pupcross; SET example.pupcross; 
     LABEL pupSES="Student SES"
	      achiev="Student Achievement in Secondary"; RUN;

/* Export to STATA;
PROC EXPORT DATA=work.pupcross OUTFILE="&filesave.\pupcross.dta" DBMS=STATA REPLACE;
RUN; */

* Get means per primary school and secondary school of kid variables;
PROC SORT DATA=work.pupcross; BY pschool pdenom; RUN;
PROC MEANS NOPRINT DATA=work.pupcross; BY pschool pdenom; VAR pupSES achiev; 
     OUTPUT OUT=work.Primary MEAN(pupSES achiev)=pmSES pmAchiev; RUN;
PROC SORT DATA=work.pupcross; BY sschool sdenom; RUN;
PROC MEANS NOPRINT DATA=work.pupcross; BY sschool sdenom; VAR pupSES achiev; 
     OUTPUT OUT=Secondary MEAN(pupSES achiev)=smSES smAchiev; RUN;

* Label new variables;
DATA work.Primary; SET work.Primary;
	LABEL pmSES=    "Primary Mean Student SES"
	      pmAchiev= "Primary Mean Student Achievement";
     DROP _TYPE_ _FREQ_; RUN;
DATA work.Secondary; SET work.Secondary;
	LABEL smSES=    "Secondary Mean Student SES"
           smAchiev= "Secondary Mean Student Achievement";
     DROP _TYPE_ _FREQ_; RUN;

* Merge back into individual data;
PROC SORT DATA=work.pupcross; BY pschool pdenom; RUN;
DATA work.pupcross; MERGE work.pupcross work.Primary; BY pschool pdenom; RUN;
PROC SORT DATA=work.pupcross; BY sschool sdenom; RUN;
DATA work.pupcross; MERGE work.pupcross work.Secondary; BY sschool sdenom; RUN;

* Center predictors;
DATA work.pupcross; SET work.pupcross;
	pupSES4 = pupSES - 4; LABEL pupSES4= "Student SES (0=4)";
	pmSES4  = pmSES - 4;  LABEL pmSES4=  "Primary Mean Student SES (0=4)";
	smSES4  = smSES - 4;  LABEL smSES4=  "Secondary Mean Student SES (0=4)";
RUN;

***********************************************************************************;
*******                 BEGIN ANALYSES FOR EXAMPLE 4                        *******;
***********************************************************************************;

TITLE "SAS Primary School Descriptives";
PROC MEANS NDEC=2 DATA=work.Primary; 
     VAR pdenom pmSES pmAchiev; RUN;

TITLE "SAS Secondary School Descriptives";
PROC MEANS NDEC=2 DATA=work.Secondary; 
     VAR sdenom smSES smAchiev; RUN;

TITLE "SAS Student Descriptives";
PROC MEANS NDEC=2 DATA=work.pupcross; 
     VAR pupSES achiev; RUN;

TITLE "SAS Empty Means Model 1a: Secondary Random Intercept Only";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev =   / SOLUTION DDFM=Satterthwaite; 
	RANDOM INTERCEPT / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary school; 
	ODS OUTPUT InfoCrit=FitNested; * Save output for LRT;
 RUN; TITLE;

TITLE "SAS Empty Means Model 1b: Primary by Secondary Crossed Random Intercepts";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev =   / SOLUTION DDFM=Satterthwaite OUTPM=PredEmpty; 
	RANDOM INTERCEPT / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	ODS OUTPUT InfoCrit=FitCrossed CovParms=CovEmpty; * Save output for LRT and pseudo-R2;
 RUN; TITLE;
* Calculate difference in model fit relative to nested model 1a;
  %FitTest(FitFewer=FitNested, FitMore=FitCrossed);

TITLE "SAS Model 2: Add School Denomination Variables";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev = pdenom sdenom / SOLUTION DDFM=Satterthwaite OUTPM=PredDenom;
	RANDOM INTERCEPT / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	CONTRAST "Joint Test of Denomination" pdenom 1, sdenom 1; 
	ODS OUTPUT CovParms=CovDenom; * Save output for pseudo-R2;
 RUN; TITLE;
* Calculate PseudoR2 relative to empty means model 1b;
  %PseudoR2(NCov=3, CovFewer=CovEmpty, CovMore=CovDenom);
* Calculate TotalR2 relative to empty means model 1b;
  %TotalR2(DV=achiev, PredFewer=PredEmpty, PredMore=PredDenom);

TITLE "SAS Model 3a: Add Student SES";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev = pdenom sdenom pupSES4 / SOLUTION DDFM=Satterthwaite;
	RANDOM INTERCEPT / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	ODS OUTPUT CovParms=CovPup1; * Save output for pseudo-R2;
 RUN; TITLE;
* Calculate PseudoR2 relative to denom model 2;
  %PseudoR2(NCov=3, CovFewer=CovDenom, CovMore=CovPup1);

TITLE "SAS Model 3b: Add Student SES Contextual Effects";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev = pdenom sdenom pupSES4 pmSES4 smSES4  
                    / SOLUTION DDFM=Satterthwaite OUTPM=PredSES;
	RANDOM INTERCEPT / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	CONTRAST "Joint Test of SES" pupSES4 1, pmSES4 1, smSES4 1; 
	ODS OUTPUT InfoCrit=FitFixSES CovParms=CovPup2; * Save output for LRT and pseudo-R2;
 RUN; TITLE;
* Calculate PseudoR2 relative to smushed model 3a;
  %PseudoR2(NCov=3, CovFewer=CovPup1, CovMore=CovPup2);
* Calculate PseudoR2 relative to denom model 2;
  %PseudoR2(NCov=3, CovFewer=CovDenom, CovMore=CovPup2);
  * Calculate TotalR2 relative to denom model 2;
  %TotalR2(DV=achiev, PredFewer=PredDenom, PredMore=PredSES);

TITLE "SAS Model 3c: Add Random Pupil SES across Secondary Schools";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev = pdenom sdenom pupSES4 pmSES4 smSES4 / SOLUTION DDFM=Satterthwaite;
	RANDOM INTERCEPT pupSES4 / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT         / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	ODS OUTPUT InfoCrit=FitRandSESsec; * Save output for LRT;
 RUN; TITLE;
* Calculate difference in model fit relative to fixed SES model 3b;
  %FitTest(FitFewer=FitFixSES, FitMore=FitRandSESsec);

TITLE "SAS Model 3d: Add Random Pupil SES across Primary Schools";
PROC MIXED DATA=work.pupcross COVTEST NOCLPRINT IC NAMELEN=100 METHOD=REML;
	CLASS pupil pschool sschool;
	MODEL achiev = pdenom sdenom pupSES4 pmSES4 smSES4 / SOLUTION DDFM=Satterthwaite;
	RANDOM INTERCEPT pupSES4 / SUBJECT=sschool TYPE=UN; * Level 2 variance for secondary; 
	RANDOM INTERCEPT pupSES4 / SUBJECT=pschool TYPE=UN; * Level 2 variance for primary; 
	ODS OUTPUT InfoCrit=FitRandSESprim; * Save output for LRT;
 RUN; TITLE;
* Calculate difference in model fit relative to random SES model 3c;
  %FitTest(FitFewer=FitRandSESsec, FitMore=FitRandSESprim);

* Close output;
ODS RTF CLOSE;
