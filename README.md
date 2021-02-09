Covid R scripts: README
======
This folder contains a collection of scripts for loading and analysing Covid Business Restriction Survey data.
The initial commits are on CBAS in gitlab repository "Covid": this git repo now takes the roll of master repository. 
I've not created R projects so to run one of the "end use" scripts you'll have to put all relevant precedents into your working directory or set up a project yourself
I've listed some sets of files for different tasks below...

fernley Symons 23:46 21/01/2021

*General*

*    CovidLoadDataFunctions.r<br>Functions to load data and aggregate it; includes functions to load lookups like geography lookups, field description [whether number/text] lookups. 
							NB this includes lists with sets of fields to add together to get totals etc. Edit this if these change [e.g. when new options added to questions]
*    CovidLoadPublicData.r<br>Load [concatenate] all the public "br*.xlsx" files listed in the README. Assumes README exists and has named range with public "br*" files listed
*    CovidLoadWorkingData.r<br>Load any working files listed in a separate input file [edit the code to reflect your input file with its list of files to load, and keep the input file as audit trail]
*    CovidByWeek.r<br>Basic aggregation script doing aggregation by week etc. NB default in underlying function is to group by week, but should work for any supplied grouping variable
							[e.g. group by q2 to get total checks by each respondent since survey start etc]. Call this script with grouping variables as in CovidByWeekEng.r,CovidByWeekRegion.r

*QA scripts*

*    CovidNoFTEsByWeek.r<br>Crosstab giving those respondents who've not recorded any FTE resource by week.
*    CovidResponsesByWeek.r<br>Create a matrix of response received [=1] or not [=0] by potential respondents / weeks. Works on public data

*Data summary*

*    CovidByWeekEng.r<br>aggregation by week for England
*    CovidByWeekRegion.r<br>aggregation by week for regions

*Create public dataset*

*    CovidKentResponses.r<br>Extract of overall data for Kent respondents only. ***Check this carefully for disclosive info before making available ***

**Tasks:**

### 1) Extracting Kent data [subset of all data]

CovidKentResponses.r calls:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadPublicData.r,<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[optional: CovidLoadWorkingData.r]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadDataFunctions.r

### 2) Summarising who responded in which week

CovidResponsesByWeek.r calls:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadPublicData.r,<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[optional: CovidLoadWorkingData.r]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadDataFunctions.r<br>

### 3) Determining who recorded no FTEs [but did respond] by week

CovidNoFTEsByWeek.r calls:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadPublicData.r,<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[optional: CovidLoadWorkingData.r]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadDataFunctions.r<br>

### 4) Summarising data by week [England]

CovidByWeekEng.r calls:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidByWeek.r<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadPublicData.r,<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[optional: CovidLoadWorkingData.r]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadDataFunctions.r<br>

### 5) Summarising data by week and region

CovidByWeekRegion.r calls:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidByWeek.r<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadPublicData.r,<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[optional: CovidLoadWorkingData.r]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CovidLoadDataFunctions.r<br>
