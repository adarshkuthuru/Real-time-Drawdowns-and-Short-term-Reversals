
proc datasets lib=work kill nolist memtype=data;
quit;

libname SP500 'E:\Drive\Realtime_DD_iteration1\22Nov'; run;

***IMPORT DATA;
PROC IMPORT OUT= WORK.MktDD
            DATAFILE= "E:\Drive\Realtime_DD_iteration1\DD_repeated_thrice\Realtime_DD_RepeatedThrice_fut.xlsx"
            DBMS=EXCEL REPLACE;
     RANGE="Realtime_DD_RepeatedThrice_fut$";
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

data MktDD;
	set MktDD;
	if missing(Date)=1 then delete;
run;

***DD ON LAST TRADING DAY OF THE WEEK;
proc sql;
  create table DDLastDayMonth as
  select distinct year(date) as year, week(date) as week, date   /* month(date) as month, */
  from MktDD
  group by year(date),week(date)
  having date=max(date);
quit;

proc sql;
  create table DDLastDayMonthx as
  select a.year, a.week, intnx("day",a.date,-2) as date format=date9.,b.Real_time_Drawdowns as DD,b.ADD
  from DDLastDayMonth as a, MktDD as b
  /*group by year(date),week(date) */
  where intnx("day",a.date,-2)=b.date;  /* Checking on Wednesdays */
quit;

*RANGE IN WHICH DD LIES;
data DDLastDayMonthx;
  set DDLastDayMonthx;
  DDL = floor(DD*100)/100;
  DDH = DDL + 0.01;
  date_1 = intnx("day",date,-7); *Previous month end date;
  format date_1 date9.;
  keep year week date DD DDL DDH ADD date_1;
  if year>=1955;  *Note that the starting year of prediction = 1955;
run;

****************************************************************************
(1)Real-time DD;
***************************************************************************;

***IDENTIFY SIMILAR DDs IN HISTORICAL DATA
HISTORICAL DATA = DATA UNTIL ONE MONTH BEFORE;
proc sql;
  create table HistDD as
  select distinct a.*, b.Real_time_Drawdowns as DD_Hist, b.date as dateh
  from DDLastDayMonthx as a, MktDD as b
  where b.date < a.date_1 and a.DDL <= b.Real_time_Drawdowns<= a.DDH;
  *Note that DD_hist is the DD on dateh;
quit;


***CALCULATE NEXT 22 TRADING DAY CUMULATIVE RETURN (THAT IS, NEXT 30 CALENDAT DAY RETURN) FOLLOWING HISTORICAL DDs;
*NEXT 22 TRADING DAYS (OR NEXT 30 CALENDAR DAYS);
data HistDD;
  set HistDD;
  dateh1 = intnx("day",dateh,7);
  format dateh1 date9.;
run;

*CUMULATIVE RETURN FOR THE NEXT 1 MONTH;
proc sql;
  create table Next22DayReturn as
  select distinct a.dateh, a.dateh1, b.date as date_dd, b.log_ret
  from HistDD as a, MktDD as b
  where a.dateh < b.date <= a.dateh1;
quit;

proc sql;
  create table Next22DayReturn as
  select distinct dateh, dateh1, sum(log_ret) as Log22DayRet
  from Next22DayReturn
  group by dateh, dateh1;
quit;

*INCLUDE Next22DayReturn IN HistDD;
proc sql;
  create table HistDD_Next22DayReturn as
  select distinct a.*, b.Log22DayRet
  from HistDD as a, Next22DayReturn as b
  where a.dateh=b.dateh and a.dateh1=b.dateh1;
quit;

*CALCULATE ESTIMATED RETURN FOR THE NEXT MONTH (THAT IS, FOR THE month = month + 1);

proc sql;
  create table PredRet as
  select distinct year, week, date, intnx("day",date,7) as Pred_Month format=date9., DD, mean(Log22DayRet) as PredRet
  from HistDD_Next22DayReturn
  group by year, week, date, DD;
quit;


proc sql;
  create table DDLastDayMonth1 as
  select distinct date, intnx("day",date,7) as date1 format=date9.
  from DDLastDayMonthx
  group by date;
quit;

proc sql;
  create table DDLastDayMonth2 as
  select distinct a.date, a.date1, b.date as dateff, b.rf, b.mktrf
  from DDLastDayMonth1 as a, SP500.Daily_Rf_Mktrf as b
  /*group by a.date, b.date;*/
  where a.date<b.date<=a.date1;
quit;

proc sql;
  create table DDLastDayMonth3 as
  select distinct date, date1, sum(rf) as Sum_rf, sum(Mktrf) as Sum_mktrf
  from DDLastDayMonth2
  group by date, date1;
quit;


****************************************************************************
(2) Actual DD;
***************************************************************************;

***IDENTIFY SIMILAR DDs IN HISTORICAL DATA
HISTORICAL DATA = DATA UNTIL ONE MONTH BEFORE;
proc sql;
  create table HistDDA as
  select distinct a.*, b.ADD as DD_Hist, b.date as dateh
  from DDLastDayMonthx as a, MktDD as b
  where b.date < a.date_1 and a.DDL <= b.ADD<= a.DDH;
  *Note that DD_hist is the DD on dateh;
quit;


***CALCULATE NEXT 22 TRADING DAY CUMULATIVE RETURN (THAT IS, NEXT 30 CALENDAT DAY RETURN) FOLLOWING HISTORICAL DDs;
*NEXT 22 TRADING DAYS (OR NEXT 30 CALENDAR DAYS);
data HistDDA;
  set HistDDA;
  dateh1 = intnx("day",dateh,7);
  format dateh1 date9.;
run;

*CUMULATIVE RETURN FOR THE NEXT 1 MONTH;
proc sql;
  create table Next22DayReturnA as
  select distinct a.dateh, a.dateh1, b.date as date_dd, b.log_ret
  from HistDDA as a, MktDD as b
  where a.dateh < b.date <= a.dateh1;
quit;

proc sql;
  create table Next22DayReturnA as
  select distinct dateh, dateh1, sum(log_ret) as Log22DayRet
  from Next22DayReturnA
  group by dateh, dateh1;
quit;

*INCLUDE Next22DayReturn IN HistDD;
proc sql;
  create table HistDD_Next22DayReturnA as
  select distinct a.*, b.Log22DayRet
  from HistDDA as a, Next22DayReturnA as b
  where a.dateh=b.dateh and a.dateh1=b.dateh1;
quit;

*CALCULATE ESTIMATED RETURN FOR THE NEXT MONTH (THAT IS, FOR THE month = month + 1);

proc sql;
  create table PredRetA as
  select distinct year, week, date, intnx("day",date,7) as Pred_Month format=date9., ADD, mean(Log22DayRet) as PredRet
  from HistDD_Next22DayReturnA
  group by year, week, date,ADD;
quit;



/****************************************/
/********** TRADING STRATEGY ************/
/****************************************/

*(1) Real-time DD;

***Predicted and Actual returns;
proc sql;
  create table Pred_Actual_Ret as
  select distinct a.date, a.DD, a.Pred_Month, a.PredRet, b.sum_rf, b.sum_mktrf
  from PredRet as a,DDLastDayMonth3 as b
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

*(2) Actual DD;

***Predicted and Actual returns;
proc sql;
  create table Pred_Actual_RetA as
  select distinct a.date, a.ADD, a.Pred_Month, a.PredRet, b.sum_rf, b.sum_mktrf
  from PredRetA as a,DDLastDayMonth3 as b
  where a.Pred_Month = b.date;
quit;

***Trading strategy returns;
data TradingStrategyA;
  set Pred_Actual_RetA;
  *Strategy-1 (unlevered);
  if PredRet > 0 then Strategy1 = sum_mktrf;
  else Strategy1 = sum_rf;

  *Strategy-2 (levered);
  if PredRet > 0 then Strategy2 = (1.5 * sum_mktrf) - (0.5 *sum_rf);
  else Strategy2 = (-0.5 * sum_mktrf) + (1.5 *sum_rf);
run;

***Strategy Performance;
proc sql;
  create table MktTimingDD_PerfA as
  select distinct mean(Strategy1)*52*100 as Strategy1_Mean, std(Strategy1*100)*sqrt(52) as Strategy1_Vol,
                  (mean(Strategy1)*52*100)/(std(Strategy1*100)*sqrt(52)) as Strategy1_Sharpe,
                  mean(Strategy2)*52*100 as Strategy2_Mean, std(Strategy2*100)*sqrt(52) as Strategy2_Vol,
                          (mean(Strategy2)*52*100)/(std(Strategy2*100)*sqrt(52)) as Strategy2_Sharpe
  from TradingStrategyA;
quit;


***Copy results to SP500 library;
data SP500.MktDD; set MktDD; run;
data SP500.MktTimingDD_Perf; set MktTimingDD_Perf; run;



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
