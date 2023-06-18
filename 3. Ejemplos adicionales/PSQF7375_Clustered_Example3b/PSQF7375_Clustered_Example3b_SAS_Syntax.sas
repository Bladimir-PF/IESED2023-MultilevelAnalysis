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
*******                     BEGIN IMPORT FOR EXAMPLE DATA                   *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example3b;
LIBNAME example "&filesave.";
     
* Save SAS syntax and output to permanent text file;
PROC PRINTTO PRINT="&filesave.\PSQF7375_Clustered_Example3b_SAS_Output.txt" 
               LOG="&filesave.\PSQF7375_Clustered_Example3b_SAS_Output.txt"; RUN;

* Import data into work library;
DATA work.grade10; SET example.grade10school; 
     LABEL studentID= "studentID: Student ID number"
           schoolID=  "schoolID: School ID number"
           frlunch=   "frlunch: 0=No, 1=Free/Reduced Lunch"
           math=      "math: Math Test Score Outcome";
     * Selecting cases that are complete for analysis variables;
     IF NMISS(studentID, schoolID, frlunch, math)>0 THEN DELETE;
RUN;

* Get school means;
PROC SORT DATA=work.grade10; BY schoolID studentID; RUN;
PROC MEANS NOPRINT N DATA=work.grade10; 
      BY schoolID;
      VAR frlunch math;
      OUTPUT OUT=work.SchoolMeans 
              MEAN(frlunch math)= SMfrlunch SMmath; 
RUN;

* Label new school mean variables;
DATA work.SchoolMeans; SET work.SchoolMeans;
     Nperschool = _FREQ_; * Saving N per school;
     DROP _TYPE_ _FREQ_;  * Dropping unneeded SAS-created variables;
     LABEL Nperschool= "Nperschool: # Students Contributing Data"
           SMfrlunch=  "SMfrlunch: School Mean 0=No, 1=Free/Reduced Lunch"
           SMmath=     "SMmath: School Mean Math Outcome";
     * Center school mean predictor;
     SMfrlunch30 = SMfrlunch - .30; 
     LABEL SMfrlunch30= "SMfrlunch30: 0=.30)";
     * Arbitrarily select only schools with at least 30 students;
     IF Nperschool < 31 THEN DELETE; 
RUN;

* Merge school means back with individual data;
DATA work.grade10; MERGE work.grade10 work.SchoolMeans; BY schoolID;
     * Arbitrarily selecting only schools with at least 30 students;
     IF Nperschool < 31 THEN DELETE;
RUN;

* Sort in order of ID variables;
PROC SORT DATA=work.grade10; BY schoolID studentID; RUN;

TITLE "School-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.SchoolMeans; 
      VAR Nperschool SMmath SMfrlunch; 
RUN; TITLE;

TITLE "Student-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.grade10; 
      VAR math frlunch; 
RUN; TITLE;

***********************************************************************************;
*******                       BEGIN ANALYSES FOR EXAMPLE 3B                 *******;
***********************************************************************************;

TITLE "SAS Model 1: 2-Level Empty Means, Random Intercept for Math Outcome";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID; * Put nesting variable on CLASS to speed estimation;
     MODEL math = / SOLUTION DDFM=Satterthwaite OUTPM=PredEmpty;
     * Asking for G for all, V and VCORR for first school in order of schoolID;
     RANDOM INTERCEPT / G V=1 VCORR=1 TYPE=UN SUBJECT=schoolID; * VCORR gives ICC;
     * Asking for R for first school in order of schoolID;
     REPEATED / R=1 TYPE=VC SUBJECT=schoolID;  * Default R matrix is diagonal (VC);
     * ODS saves results for pseudo-R2 macro;
     ODS OUTPUT CovParms=CovEmpty; RUN; TITLE;

TITLE "SAS Model 2: Add Fixed Effect of Student Free/Reduced Lunch";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID;
     MODEL math = frlunch / SOLUTION DDFM=Satterthwaite; 
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     * ODS saves results for pseudo-R2 and LRT macros;
     ODS OUTPUT CovParms=CovFR1 InfoCrit=FitFR1; RUN; TITLE; 
* Calculate PseudoR2 relative to empty model 1;
  %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovFR1);

TITLE "SAS Model 3: Add Fixed Effect of School Proportion Free/Reduced Lunch";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID;
     MODEL math = frlunch SMfrlunch30 / SOLUTION DDFM=Satterthwaite OUTPM=work.PredLunch;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     ODS OUTPUT CovParms=CovFR2 InfoCrit=FitFR2;
     CONTRAST "Test of Model R2 (Omnibus FR Lunch)" frlunch 1, SMfrlunch30 1;
     ESTIMATE "FR Lunch Between-School Effect"  frlunch 1 SMfrlunch30 1; RUN; TITLE;
* Calculate PseudoR2 relative to previous model 2;
  %PseudoR2(NCov=2, CovFewer=CovFR1, CovMore=CovFR2);
* Calculate PseudoR2 relative to empty model 1 (total for FRlunch);
  %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovFR2);
* Calculate TotalR2 relative to empty model 1 (total for FRlunch);
  %TotalR2(DV=math, PredFewer=PredEmpty, PredMore=PredLunch);

TITLE "SAS Model 4: Add Random Effect of Student Free/Reduced Lunch";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID;
     MODEL math = frlunch SMfrlunch30 / SOLUTION DDFM=Satterthwaite;
     * Asking for G and GCORR for all, V and VCORR for first school in order of schoolID;
     RANDOM INTERCEPT frlunch / G GCORR V=1 VCORR=1 TYPE=UN SUBJECT=schoolID; 
     * Asking for R for first school in order of schoolID;
     REPEATED / R=1 TYPE=VC SUBJECT=schoolID;  * Default R matrix is diagonal (VC);
     ODS OUTPUT CovParms=CovFR2RandFR1 InfoCrit=FitFR2RandFR1; RUN; TITLE;
* Calculate difference in model fit relative to fixed-FRlunch-only model 3;
  %FitTest(FitFewer=FitFR2, FitMore=FitFR2RandFR1);

TITLE "SAS Model 5: Add Cross-Level Interaction of Student by School Free/Reduced Lunch";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID;
     MODEL math = frlunch SMfrlunch30 frlunch*SMfrlunch30 
                   / SOLUTION DDFM=Satterthwaite;
     RANDOM INTERCEPT frlunch / G TYPE=UN SUBJECT=schoolID; 
     ODS OUTPUT CovParms=CovInt1 InfoCrit=FitInt1; RUN; TITLE;
* Calculate PseudoR2 for both FRlunch effects relative to random FRlunch model 4;
  %PseudoR2(NCov=4, CovFewer=CovFR2RandFR1, CovMore=CovInt1);

TITLE "SAS Model 6: Add Level-2 Interaction of Quadratic School Free/Reduced Lunch";
PROC MIXED DATA=work.grade10 NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID;
     MODEL math = frlunch SMfrlunch30 frlunch*SMfrlunch30 SMfrlunch30*SMfrlunch30
                   / SOLUTION DDFM=Satterthwaite OUTPM=work.PredTotal;
     RANDOM INTERCEPT frlunch / G TYPE=UN SUBJECT=schoolID; 
     ODS OUTPUT CovParms=CovInt2 InfoCrit=FitInt2;
     CONTRAST "Test of Omnibus FR Lunch Interaction" frlunch*SMfrlunch30 1, SMfrlunch30*SMfrlunch30 1;
     ESTIMATE "FR Lunch Between-School Simple Main Effect"  frlunch 1 SMfrlunch30 1;
     ESTIMATE "FR Lunch Between-School Interaction"  frlunch*SMfrlunch30 1 SMfrlunch30*SMfrlunch30 1;
RUN; TITLE;
* Calculate PseudoR2 relative to level-1 lunch interaction only model 5;
  %PseudoR2(NCov=4, CovFewer=CovInt1, CovMore=CovInt2);
* Calculate PseudoR2 relative to random FRlunch model 4 (total for FRlunch interactions);
  %PseudoR2(NCov=4, CovFewer=CovFR2RandFR1, CovMore=CovInt2);
* Calculate TotalR2 relative to random FRlunch model 4 (total for FRlunch interactions);
  %TotalR2(DV=math, PredFewer=PredLunch, PredMore=PredTotal);

* Close output;
PROC PRINTTO; RUN;
