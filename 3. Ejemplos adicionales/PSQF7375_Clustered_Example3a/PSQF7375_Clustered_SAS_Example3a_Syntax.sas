* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Open datasets even if the formats are not available;
  OPTIONS NOfmterr; 
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; 
* Turn on old-school output and new HTML graphics;
  ODS LISTING; ODS GRAPHICS ON; 

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
*******                     BEGIN IMPORT FOR EXAMPLE DATA                   *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example3a;

* Import example data excel data file into SAS;
PROC IMPORT DATAFILE="&filesave.\Multilevel_Analysis_Missing.xlsx" 
     OUT=work.Students DBMS=XLSX REPLACE; 
     SHEET="book_data"; 
     GETNAMES=YES; RUN;

* Data processing;
DATA work.Students; SET work.Students;
* Label variables;
  LABEL schoolID=  "School ID"
        studentID= "Student ID"
        langpost=  "Student Language"
        IQverb=    "Student Verbal IQ"
        IQperf=    "Student Performance IQ"
        denomina=  "School Denomination"
        homework=  "School Homework"
        mixedgra=  "School Mixed Grade";
* Select variables to keep; 
  KEEP schoolID studentID langpost IQverb IQperf denomina homework mixedgra;
* Select complete cases;
  IF NMISS(schoolID,studentID,langpost,IQverb,IQperf,denomina,homework,mixedgra)>0 THEN DELETE;
* Remove denomina=4,5 for small sample sizes;
  IF denomina IN(4,5) THEN DELETE; RUN;

* Z-score IQ variables for comparability as predictors;
PROC STANDARD DATA=work.Students OUT=work.StudentsZ MEAN=0 STD=1;
     VAR IQverb IQperf; RUN;
* Rename z-scored variables;
DATA work.StudentsZ; SET work.StudentsZ;
     RENAME IQverb=IQverbz IQperf=IQperfz; RUN;

* Create school-level dataset;
PROC MEANS NOPRINT DATA=work.StudentsZ; By schoolID;
     VAR langpost IQverbz IQperfz homework mixedgra;
     OUTPUT OUT=work.Schools N(langpost)=Nperschool
     MEAN(langpost IQverbz IQperfz denomina homework mixedgra)=
          SMlangpost SMIQverbz SMIQperfz denomina homework mixedgra; RUN;
* Label new school variables;
DATA work.Schools; SET work.Schools;
  LABEL SMlangpost= "School Mean Language"
        SMIQverbz=  "School Mean Verbal IQ Z" 
        SMIQperfz=  "School Mean Performance IQ Z";
  DROP _TYPE_ _FREQ_; * Remove unneeded vars; RUN; 
* Merge school-level data back into student data;
DATA work.StudentsSchools; MERGE StudentsZ Schools; BY schoolID; 
* Contrasts for school denomination -- initialize to missing;
  den1v2=.; den1v3=.;
  IF denomina=1 THEN DO; den1v2=0; den1v3=0; END;
  IF denomina=2 THEN DO; den1v2=1; den1v3=0; END;
  IF denomina=3 THEN DO; den1v2=0; den1v3=1; END;
  LABEL den1v2= "Denom 1 vs 2"
        den1v3= "Denom 1 vs 3";
* Center school predictors near sample mean;
  hw2=homework-2; 
  SMIQverbz0=SMIQverbz-0; * Reminder to center all L2 predictors;
  SMIQperfz0=SMIQperfz-0; 
  LABEL hw2= "School Homework (0=2)";
* Create group-MC student-level variables using original L2 means;
  WSlangpost=langpost-SMlangpost;
  WSIQverbz=IQverbz-SMIQverbz;
  WSIQperfz=IQperfz-SMIQperfz;
  LABEL WSlangpost= "Student Language (0=School Mean)"
        WSIQverbz=  "Student Verbal IQ Z (0=School Mean)"
        WSIQperfz=  "Student Performance IQ Z (0=School Mean)";
RUN;
     

***********************************************************************************;
*******         BEGIN ANALYSES FOR EXAMPLE 3A: FIXED EFFECTS ONLY           *******;
***********************************************************************************;

* Open output directory to save results to;
ODS RTF FILE="&filesave.\SAS_Example3a_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

TITLE "SAS School-Level Descriptives";  
PROC FREQ DATA=work.Schools;
     TABLE denomina mixedgra; RUN;
PROC MEANS NDEC=2 DATA=work.Schools;
     VAR SMlangpost SMIQverbz SMIQperfz homework; RUN; TITLE;
TITLE "SAS Student-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.StudentsSchools;
     VAR langpost WSlangpost IQverbz WSIQverbz IQperfz WSIQperfz; RUN; TITLE;

TITLE "SAS Model 1a for Language: Single-Level Empty Means, No Random Intercept";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID; 
     MODEL langpost = / SOLUTION DDFM=BW; RUN; TITLE;

TITLE "SAS Model 1b for Language: Two-Level Empty Means, Random Intercept";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID; * OUTPM=Save predicted outcomes;
     MODEL langpost = / SOLUTION DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=schoolID; * VCORR gives ICC;
     ODS OUTPUT CovParms=CovEmpty; * Save variances; RUN; TITLE;

TITLE "SAS Model 2a: Add 3 School Predictors";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID;               
     MODEL langpost = hw2 mixedgra den1v2 den1v3 
                      / SOLUTION DDFM=Satterthwaite OUTPM=PredHMD;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     ESTIMATE "Denomination 2 vs 3"                       den1v2 -1  den1v3 1;
     CONTRAST "Test of Model Total-R2" hw2 1, mixedgra 1, den1v2  1, den1v3 1;
     CONTRAST "Test of Change in R2 after HW" mixedgra 1, den1v2  1, den1v3 1;
     CONTRAST "Test of Omnibus Denomination"              den1v2  1, den1v3 1;
     ODS OUTPUT CovParms=CovHMD; RUN; TITLE;
* Calculate PseudoR2 relative to two-level empty means model 1b;
  %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovHMD);
* Calculate TotalR2 relative to two-level empty means model 1b;
  %TotalR2(DV=langpost, PredFewer=PredEmpty, PredMore=PredHMD);

TITLE "SAS Model 2b: Add Interaction of Mixed Grade by Denomination";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID;  
     MODEL langpost = hw2 mixedgra den1v2 den1v3 mixedgra*den1v2 mixedgra*den1v3 
                      / SOLUTION DDFM=Satterthwaite OUTPM=PredMGxD;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     CONTRAST "Omnibus Test of MixedGra by Den Interaction (Change in R2)" 
               mixedgra*den1v2 1, mixedgra*den1v3 1;
     ESTIMATE "Denomination 2 vs 3" den1v2 -1 den1v3 1;
     * Simple effects of mixed grade per denomination;
     ESTIMATE "MixedGra Effect for Den=1" mixedgra 1 mixedgra*den1v2 0 mixedgra*den1v3 0;
     ESTIMATE "MixedGra Effect for Den=2" mixedgra 1 mixedgra*den1v2 1 mixedgra*den1v3 0;
     ESTIMATE "MixedGra Effect for Den=3" mixedgra 1 mixedgra*den1v2 0 mixedgra*den1v3 1;
     * Simple effects of denomination per MixedGra;
     ESTIMATE "Den 1v2 for MixedGra=0" den1v2  1 den1v3 0 mixedgra*den1v2  0 mixedgra*den1v3 0;
     ESTIMATE "Den 1v3 for MixedGra=0" den1v2  0 den1v3 1 mixedgra*den1v2  0 mixedgra*den1v3 0;
     ESTIMATE "Den 2v3 for MixedGra=0" den1v2 -1 den1v3 1 mixedgra*den1v2  0 mixedgra*den1v3 0;
     ESTIMATE "Den 1v2 for MixedGra=1" den1v2  1 den1v3 0 mixedgra*den1v2  0 mixedgra*den1v3 0;
     ESTIMATE "Den 1v3 for MixedGra=1" den1v2  0 den1v3 1 mixedgra*den1v2  0 mixedgra*den1v3 1;
     ESTIMATE "Den 2v3 for MixedGra=1" den1v2 -1 den1v3 1 mixedgra*den1v2 -1 mixedgra*den1v3 1;
     * Differences in simple effects -- interaction contrasts;
     ESTIMATE "MixedGra Effect for Den=1 vs 2" mixedgra*den1v2  1 mixedgra*den1v3 0;
     ESTIMATE "MixedGra Effect for Den=1 vs 3" mixedgra*den1v2  0 mixedgra*den1v3 1;
     ESTIMATE "MixedGra Effect for Den=2 vs 3" mixedgra*den1v2 -1 mixedgra*den1v3 1;
     ODS OUTPUT CovParms=CovMGbxD; RUN; TITLE;
* Calculate PseudoR2 relative to two-level empty means model 1b;
  %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovMGbxD);
* Calculate PseudoR2 relative to L2 main effects model 2a;
  %PseudoR2(NCov=2, CovFewer=CovHMD, CovMore=CovMGbxD);
* Calculate TotalR2 relative to L2 main effects model 2a;
  %TotalR2(DV=langpost, PredFewer=PredHMD, PredMore=PredMGxD);

TITLE "SAS Model 1b for IQverb: Two-Level Empty Means, Random Intercept";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID; 
     MODEL IQverbz = / SOLUTION DDFM=Satterthwaite;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=schoolID; RUN; TITLE;

TITLE "SAS Model 1b for IQperf: Two-Level Empty Means, Random Intercept";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID; 
     MODEL IQperfz = / SOLUTION DDFM=Satterthwaite;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=schoolID; RUN; TITLE;

TITLE "SAS Model 3a: Remove Mixed Grade by Denom, Add L2 and L1 Student Verbal IQ";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID;  
     MODEL langpost = hw2 mixedgra den1v2 den1v3 SMIQverbz0 WSIQverbz 
                      / SOLUTION DDFM=Satterthwaite OUTPM=PredIQverb;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     CONTRAST "Omnibus Test of Verbal IQ (Change in R2)" SMIQverbz0 1, WSIQverbz 1;
     ESTIMATE "Denomination 2 vs 3" den1v2 -1 den1v3 1;
     ESTIMATE "L2 Contextual Effect of Verbal IQ" SMIQverbz0 1 WSIQverbz -1;
     ODS OUTPUT CovParms=CovIQverb; RUN; TITLE;
* Calculate PseudoR2 relative to L2 main effects model 2a;
  %PseudoR2(NCov=2, CovFewer=CovHMD, CovMore=CovIQverb);
* Calculate TotalR2 relative to L2 main effects model 2a;
  %TotalR2(DV=langpost, PredFewer=PredHMD, PredMore=PredIQverb);

TITLE "SAS Model 3b: Add L2 and L1 Student Performance IQ";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID;  
     MODEL langpost = hw2 mixedgra den1v2 den1v3 SMIQverbz0 WSIQverbz SMIQperfz0 WSIQperfz
                      / SOLUTION DDFM=Satterthwaite OUTPM=PredIQperf;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     CONTRAST "Omnibus Test of Verbal IQ (Change in R2)"      SMIQverbz0 1, WSIQverbz 1;
     CONTRAST "Omnibus Test of Performance IQ (Change in R2)" SMIQperfz0 1, WSIQperfz 1;
     ESTIMATE "Denomination 2 vs 3" den1v2 -1 den1v3 1;
     ESTIMATE "L2 Contextual Effect of Verbal IQ" SMIQverbz0 1 WSIQverbz -1;
     ESTIMATE "L2 Contextual Effect of Performance IQ" SMIQperfz0  1 WSIQperfz -1;
     ESTIMATE "L2 Verbal vs Performance IQ effects"  SMIQverbz0 -1 SMIQperfz0 1;
     ESTIMATE "L1 Verbal vs Performance IQ effects"  WSIQverbz  -1 WSIQperfz  1;
     ODS OUTPUT CovParms=CovIQperf; RUN; TITLE;
* Calculate PseudoR2 relative to verbal IQ model 3a;
  %PseudoR2(NCov=2, CovFewer=CovIQverb, CovMore=CovIQperf);
* Calculate TotalR2 relative to verbal IQ model 3a;
  %TotalR2(DV=langpost, PredFewer=PredIQverb, PredMore=PredIQperf);

TITLE "SAS Model 3c: Add L2 and L1 IQ Interactions";
PROC MIXED DATA=work.StudentsSchools NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS schoolID studentID; 
     MODEL langpost = hw2 mixedgra den1v2 den1v3 SMIQverbz0 WSIQverbz SMIQperfz0 WSIQperfz
                      SMIQverbz0*SMIQperfz0 WSIQverbz*WSIQperfz 
                      / SOLUTION DDFM=Satterthwaite OUTPM=PredIQxIQ COVB;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; * COVB for regions of significance macro;
     CONTRAST "Omnibus Test of IQ Interactions (Change in R2)" SMIQverbz0*SMIQperfz0 1, WSIQverbz*WSIQperfz 1;
     ESTIMATE "Denomination 2 vs 3" den1v2 -1 den1v3 1;
     ESTIMATE "L2 Simple Contextual Effect of Verbal IQ"      SMIQverbz0 1 WSIQverbz -1;
     ESTIMATE "L2 Simple Contextual Effect of Performance IQ" SMIQperfz0 1 WSIQperfz -1;
     * Get simple slopes of L2 IQ perf at representative values of L2 IQ verb;
     ESTIMATE "L2 IQ perf effect at L2 IQ verb z = -1" SMIQperfz0 1 SMIQverbz0*SMIQperfz0 -1;
     ESTIMATE "L2 IQ perf effect at L2 IQ verb z =  0" SMIQperfz0 1 SMIQverbz0*SMIQperfz0  0;
     ESTIMATE "L2 IQ perf effect at L2 IQ verb z =  1" SMIQperfz0 1 SMIQverbz0*SMIQperfz0  1;
     * Get simple slopes of L1 IQ perf at representative values of L1 IQ verb;
     ESTIMATE "L1 IQ perf effect at L1 IQ verb z = -1" WSIQperfz 1 WSIQverbz*WSIQperfz -1;
     ESTIMATE "L1 IQ perf effect at L1 IQ verb z =  0" WSIQperfz 1 WSIQverbz*WSIQperfz  0;
     ESTIMATE "L1 IQ perf effect at L1 IQ verb z =  1" WSIQperfz 1 WSIQverbz*WSIQperfz  1;
     * Save info for R2 and regions of significance macro; 
     ODS OUTPUT CovParms=CovIQxIQ SolutionF=FixIQxIQ CovB=CovBIQxIQ; RUN; TITLE;
* Calculate PseudoR2 relative to empty means model 1b;
  %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovIQxIQ);
* Calculate PseudoR2 relative to main effects IQ model 3b;
  %PseudoR2(NCov=2, CovFewer=CovIQperf, CovMore=CovIQxIQ);
* Calculate TotalR2 relative to main effects IQ model 3b;
  %TotalR2(DV=langpost, PredFewer=PredIQperf, PredMore=PredIQxIQ);
* Calculate regions of significance for effect of L2 IQperf;
  %Regions(FixData=FixIQxIQ,CovBData=CovBIQxIQ,Pred=SMIQperfz0,
           Mod=SMIQverbz0,ModCenter=0,Interact=SMIQverbz0*SMIQperfz0,Order=9);
* Calculate regions of significance for effect of L1 IQperf;
  %Regions(FixData=FixIQxIQ,CovBData=CovBIQxIQ,Pred=WSIQperfz,
           Mod=WSIQverbz,ModCenter=0,Interact=WSIQverbz*WSIQperfz,Order=10);

  * Close output;
ODS RTF CLOSE;
