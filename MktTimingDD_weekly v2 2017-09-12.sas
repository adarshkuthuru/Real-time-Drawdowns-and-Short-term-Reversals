proc datasets lib=work kill nolist memtype=data;
quit;

libname SP500 'E:\Drive\Local Disk F\Realtime_DD\22Nov'; run;


***IMPORT DATA;
PROC IMPORT OUT= WORK.MktDD
            DATAFILE= "E:\Drive\Local Disk F\Realtime_DD\DD_repeated_thrice\Realtime_DD_RepeatedThrice.xlsx"
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$";
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

*converting Candidate_trough's date format;
data MktDD;
	set MktDD;
	date1=date;
	Candidate_trough1=input(trim(Candidate_trough),mmddyy10.);
	format Candidate_trough1 date9. date1 best8.;
	drop Candidate_trough;
run;

*creating count variable based on repetition of the candidate trough;
data MktDD1;
	set MktDD;
	if missing(Candidate_trough1)=0;
	drop count;
run;

data MktDD1;
	set MktDD1;
	date1=date;
	format date1 best8.;
	nrow=_N_;
	by Candidate_trough1 notsorted;
	if first.Candidate_trough1=1 then count=0;
	count+1;
run;

**adding count variable to original dataset;
proc sql;
	create table MktDD as
	select distinct a.*,b.count
	from MktDD as a left join MktDD1 as b
	on a.Date=b.date;
quit;



**instead of printing log, it saves log file at mentioned location;
proc printto log="E:\Drive\Realtime_DD_iteration1\filename.log";
run;

proc sql noprint;
        select count(*) into :trough from MktDD1;
quit;

options symbolgen;
%macro doit;
%do i=160 %to 160; *&trough;

	proc sql noprint;
     select Date1 into :dt from MktDD1 where nrow=&i;
    quit;

	data MktDD2;
	    set MktDD;
		if Date1<=&dt;  *13788;
    run;

	**Checking if the candidate troughs in the above dataset are repeated twice or more;
	**If yes, replace their drawdowns with real-time DDs;
	data MktDD2a;
    	set MktDD2;
		if missing(count)=0;
	run;


/*	proc sql;*/
/*		create table MktDD2a as*/
/*		select distinct *, max(count) as max_count*/
/*		from MktDD2a*/
/*		group by Candidate_trough1;*/
/*	quit;*/

	proc sql;
		create table MktDD2b as
		select distinct Candidate_trough1,max(count) as max_count
		from MktDD2a
		group by Candidate_trough1;
	quit;

	*Merging with original dataset;
	proc sql;
		create table MktDD2a as
		select distinct a.*,b.max_count
		from MktDD2a as a left join MktDD2b as b
		on a.Candidate_trough1=b.Candidate_trough1;
	quit;

	proc sql;
		drop table MktDD2b;
	quit;

	**only taking last observation of each candidatetrough;
	data MktDD2a;
		set MktDD2a;
		by Candidate_trough1;
		if last.Candidate_trough1=1;
		if count>2;  *trough repeated thrice is taken as candidate trough;
		date2=Candidate_trough1;
		format date2 best12.;
	run;

	data MktDD2a;
		set MktDD2a;
		nrow=_N_;
	run;

	**counting number of rows in MktDD2a;
	proc sql noprint;
	        select count(*) into :nr from MktDD2a;
	quit;

	**macro which calculated realtime DDs;
	%macro doit1;
	%do j=1 %to &nr;

	proc sql noprint;
     select Date1 into :dti from MktDD2a where nrow=&j;
    quit;

	proc sql noprint;
     select Date2 into :dtj from MktDD2a where nrow=&j;
    quit;


	data MktDD2;
	    set MktDD2;
		if Date1>=&dtj and Date1<=&dti and missing(DD_new)=1 then DD_new=Real_time_Drawdowns; 
    run;
	%end;
	%mend doit1;


	**macros2;
	**Just to get week for which we are predicting ret;
	options symbolgen;
	%macro doit2;

	proc sql noprint;
     select Date1 into :dt from MktDD2 
	 having date=max(date);
    quit;

	%end;
	%mend doit2;




	%if &nr>0 %then %doit1;
	%else %doit2;


	**filling the rest of the column with peak-to-peak drawdowns;
	data MktDD2;
	    set MktDD2;
		if missing(DD_new)=1 then DD_new=Peak_to_peak_Drawdowns;
	run;



	**********************************************;

	***DD ON LAST TRADING DAY (Wednesday) OF THE WEEK;


	data DDLastDayWeekx;
    	set MktDD2;
		year=year(date);
		week=week(date);
		if missing(Candidate_trough1)=0;
		keep date date1 year week DD_new;
	run;

		*RANGE IN WHICH DD LIES;
	data DDLastDayWeekx;
	  set DDLastDayWeekx;
	  DDL = floor(DD_new*100)/100;
	  DDH = DDL + 0.01;
	  date1 = date; *Previous week end date;
	  format date1 best12.;
	  keep year week date date1 DD_new DDL DDH;
	  if year>=1955;  *Note that the starting year of prediction = 1955;
	run;


	**reading the values of DD_new;
	proc sql noprint;
     select DD_new into :dd from DDLastDayWeekx 
	 having date1=max(date1);
    quit;
		***IDENTIFY SIMILAR DDs IN HISTORICAL DATA
	HISTORICAL DATA = DATA UNTIL ONE WEEK BEFORE;

	data HistDD;
		set DDLastDayWeekx;
		if date1 < &dt and DDL <= &dd <= DDH;
	run;

	***CALCULATE NEXT 7 TRADING DAY CUMULATIVE RETURN FOLLOWING HISTORICAL DDs;
	data HistDD;
	  set HistDD;
	  dateh1 = intnx("day",date,7);
	  format dateh1 best12.;
	run;

	*CUMULATIVE RETURN FOR THE NEXT 1 MONTH;
	proc sql;
	  create table Next7DayReturn as
	  select distinct a.date1, a.dateh1, b.date1 as date_dd, b.log_ret
	  from HistDD as a, MktDD as b
	  where a.date1 < b.date1 <= a.dateh1;
	quit;

	proc sql;
	  create table Next7DayReturn as
	  select distinct date1, dateh1, sum(log_ret) as Log7DayRet
	  from Next7DayReturn
	  group by date1, dateh1;
	quit;

	data HistDD;
	  set HistDD;
	  Pred_week=&dt;
	  DD=&dd;
	run;

	*INCLUDE Next7DayReturn IN HistDD;
	proc sql;
	  create table HistDD_Next7DayReturn as
	  select distinct a.*, b.Log7DayRet
	  from HistDD as a, Next7DayReturn as b
	  where a.date1=b.date1 and a.dateh1=b.dateh1;
	quit;

	*CALCULATE ESTIMATED RETURN FOR THE NEXT WEEK;

	proc sql;
	  create table PredRet as
	  select distinct Pred_week format=date9., dd, mean(Log7DayRet) as PredRet
	  from HistDD_Next7DayReturn
	  group by Pred_week;
	quit;

	**Appending to the final dataset;
	proc append data=PredRet base=final; run;


%end;
%mend doit;
%doit



*re-enabling the log;
PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;

/*data sp500.final;*/
/*	set final;*/
/*run;*/

/* data final;*/
/*	set final;*/
/*	date1=intck("day",pred_week,lag(pred_week));*/
/* run;*/

***Adding mkt_ret variable to final dataset;
data final;
	set final;
	date1=intnx("day",pred_week,7);
	format date1 date9.;
run;

proc sql;
  create table DDLastDayWeek2 as
  select distinct a.pred_week as date, a.date1, b.date as dateff, b.rf, b.mktrf
  from final as a left join SP500.Daily_Rf_Mktrf as b
  /*group by a.date, b.date;*/
  on a.pred_week<b.date<=a.date1;
quit;

proc sql;
  create table DDLastDayWeek3 as
  select distinct date, date1, sum(rf) as Sum_rf, sum(Mktrf) as Sum_mktrf
  from DDLastDayWeek2
  group by date, date1;
quit;


/****************************************/
/********** TRADING STRATEGY ************/
/****************************************/


***Predicted and Actual returns;
proc sql;
  create table Pred_Actual_Ret as
  select distinct a.date, a.DD, a.Pred_Month, a.PredRet, b.sum_rf, b.sum_mktrf
  from PredRet as a,DDLastDayWeek3 as b
  where a.Pred_Month = b.date;
quit;

***Trading strategy returns;
data TradingStrategy;
  set Pred_Actual_Ret;
  *Strategy-1 (unlevered);
  if PredRet > 0 then Strategy1 = sum_mktrf;
  else Strategy1 = sum_rf;

  *Strategy-2 (levered);
  if PredRet > 0 then Strategy2 = (1.5 * sum_mktrf) - (0.5 *sum_rf);
  else Strategy2 = (-0.5 * sum_mktrf) + (1.5 *sum_rf);
run;

***Strategy Performance;
proc sql;
  create table MktTimingDD_Perf as
  select distinct mean(Strategy1)*52*100 as Strategy1_Mean, std(Strategy1*100)*sqrt(52) as Strategy1_Vol,
                  (mean(Strategy1)*52*100)/(std(Strategy1*100)*sqrt(52)) as Strategy1_Sharpe,
                  mean(Strategy2)*52*100 as Strategy2_Mean, std(Strategy2*100)*sqrt(52) as Strategy2_Vol,
                          (mean(Strategy2)*52*100)/(std(Strategy2*100)*sqrt(52)) as Strategy2_Sharpe
  from TradingStrategy;
quit;


***Copy results to SP500 library;
/*data SP500.MktDD; set MktDD; run;*/
/*data SP500.MktTimingDD_Perf; set MktTimingDD_Perf; run;*/


**Skewness & kurtosis;
proc sort data=TradingStrategy; by Date; run;
proc means data=TradingStrategy noprint;
/*  by AnomalyVar Rank_Anomaly;*/
  var Strategy1 Strategy2;
  output out=sk Skewness=Strategy1 Strategy2;
quit;

proc means data=TradingStrategy noprint;
/*  by AnomalyVar Rank_Anomaly;*/
  var Strategy1 Strategy2;
  output out=kt Kurtosis=Strategy1 Strategy2;
quit;
