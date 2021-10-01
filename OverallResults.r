# *** Covid: Time series of key metrics by week [all England] *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# Then take selected variables from this and output to XL

#TestCommentforPush

# *** Optional:
rm(list=ls()) # remove anything in memory

# Working directory ====
# *****************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# aggregate data to week across regions [i.e. for England]
# run this first because it removes anything in memory

source('CovidByWeek.r') # this includes a clear memory statement so library calls must come afterwards

# Libraries
library('stringr')
library('knitr')
library('scales')

# NB sources have to be in this order because 'CovidByWeek.r' starts by removing everything from memory 
source("Captioner package.R") # This contains the "meat" of the captioner package otherwise available from github

# Constants for just this script ====
# ******************************
# NB other constants are in CovidConstants.r
resultsDir=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/") # For convenience, use the scripts directory
# resultsDir="C:/Users/fsymons/Downloads/Annexes/" # or uncomment and change this...

# Date from which LAs have said we can share (=publish) the survey data
publishDataDate="2020-11-23"

# In this DF, list the variables and labels that accompany them in charts
# use this DF construction form to make the pairs more intelligible
charts=data.frame(var='q24_',varName='week ending')
charts=rbind(charts,c('q2__cnt','responses'))
charts=rbind(charts,c('q4__sum','requests for advice'))
charts=rbind(charts,c('q5__sum','requests responded within agreed time'))
charts=rbind(charts,c('q6__sum','complaints received'))
charts=rbind(charts,c('q6__sum','complaints weekly'))
charts=rbind(charts,c('q8_1.0_sum','checks by week end'))
charts=rbind(charts,c('q8_2.0_sum','checks due to complaint by week end'))
charts=rbind(charts,c('q8_3.0_sum','non-compliant businesses found by checks'))
charts=rbind(charts,c('q9__.__sum','full-time equivalent employees'))
charts=rbind(charts,c('q9__.__cnt','fte'))
charts=rbind(charts,c('q12_1.0__sum','businesses failing to remain closed'))
charts=rbind(charts,c('q12_1.1__sum','businesses failing to remain closed (all tiers)'))
charts=rbind(charts,c('q12_1.2__sum','businesses failing to remain closed (tier specific)'))
charts=rbind(charts,c('q12_2.0__sum','self-isolating workers working outside home'))
charts=rbind(charts,c('q12_3.0__sum','failing to comply with a Local Authority direction'))
charts=rbind(charts,c('q12_4.0__sum','not collecting test and trace information'))
charts=rbind(charts,c('q12_5.0__sum','not protecting staff and customers'))
charts=rbind(charts,c('q12_6y7.0__sum','other breaches'))
charts=rbind(charts,c('q12_8.0__sum','exceeding opening hours rules'))
charts=rbind(charts,c('q12_9.0__sum','not providing table service only'))
charts=rbind(charts,c('q12_10.0__sum','breaches of the rule of 6'))
charts=rbind(charts,c('q12__.__sum','all breach types'))
charts=rbind(charts,c('q12__.__a_sum','verbal advice'))
charts=rbind(charts,c('q12__.__b_sum','letter/email'))
charts=rbind(charts,c('q12__.__c_sum','direction notice'))
charts=rbind(charts,c('q12__.__d_sum','fixed penalty'))
charts=rbind(charts,c('q12__.__e_sum','prohibition notice'))
charts=rbind(charts,c('q12__.__f_sum','prosecutions'))
charts=rbind(charts,c('q12__.__g_sum','hsawa improvement notice'))
charts=rbind(charts,c('q12__.__h_sum','coronavirus improvement notice'))
charts=rbind(charts,c('q12__.__i_sum','coronavirus immediate restriction'))
charts=rbind(charts,c('q12__.__j_sum','coronavirus restriction'))
charts=rbind(charts,c('q12_1.1y2__sum','failing to remain closed'))
charts=rbind(charts,c('other_breaches','all other breaches'))
charts=rbind(charts,c('q12__.__others_sum','all other actions'))

## colours ====
# These are the OPSS branding colours. Below, commented out, are BEIS colours
colour1="#003366" # OPSS dark blue
colour2="#0099cc" # cyan
colour3="#99cc00" # lime green
colour4="#ffcc33" # gold
colour5="#ff3399" # magenta
colour6="#66cc33" # green
colour7="#ff9933" # orange
colour8="#99adc2" # grey blue [tint of OPSS blue]

# colour1="#00AEEF" # "BEIS" blue
# colour2="#005674" # darker teal blue
# colour3="#000000" # black
# colour4="#A6A6A6" # mid-light grey
# colour5="#74ac00" # lime green
# colour6="#8080ff" # mid-light blue
# colour7="#404040" # v dark grey
# colour8="#73A1C4" # grey blue

## main chart theme ====
# set the base theme for all sub charts:
th=theme(
  panel.border = element_blank(), # no panel border
  panel.background = element_rect(fill = "white"), # white rectangular background
  panel.grid.major = element_blank(), # no grid lines
  panel.grid.minor = element_blank(),
  legend.position= "none", # no legend
  axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
  axis.title.x = element_text(hjust = 1, size=15),
  axis.text.y = element_text(hjust = 1, size=15, margin = margin(r=5)),
  axis.text.x = element_text(hjust = 0.5, size=15, margin = margin(t=10)), # centre align x axis labels
  plot.title = element_text(hjust = 0, size=15)
)

#specify geom to update, and list attibutes you want to change appearance of
update_geom_defaults("line", list(size = 1.6))

# date of switch in administrative boundaries to new geography--at least as far as survey results concerned [actual switch = 1/4/2021]
geoChangeDate=as.Date("2021-03-29")

## list of breaches ====
# These are breaches of the Covid business restrictions regulations
# This 'detailed' list used for the table. Note still includes amalgamations:--1.1 & 1.2, 6 & 7.
detailedBreachesVars=c('q12_1.1y2__sum_csum','q12_2.0__sum_csum','q12_3.0__sum_csum','q12_4.0__sum_csum',
                       'q12_5.0__sum_csum','q12_6y7.0__sum_csum','q12_8.0__sum_csum','q12_9.0__sum_csum','q12_10.0__sum_csum')
detailedBreaches=filter(charts,var %in% detailedBreachesVars)
# First choose the basic variable list:
breachVars=c('q12__.__sum','q12_1.1y2__sum','q12_5.0__sum','other_breaches')
breaches=filter(charts,var %in% sub('_csum','',breachVars))

## list of actions by LAs ====
# Create a vector of total actions for each LA action [intervention] type. 
actionVars=c('q12__.__a_sum','q12__.__b_sum','q12__.__c_sum','q12__.__d_sum',
             'q12__.__e_sum','q12__.__f_sum','q12__.__g_sum','q12__.__h_sum','q12__.__i_sum','q12__.__j_sum')

# Note that the magnitude of the first 2 actions, verbal advice and emails/letters, is much greater than
# any other. So makes sense to do 2 presentations: one chart with verbal + emails + others [grouped] and another
# [perhaps table] showing the lesser categories of actions individually.
# First, create an appropriate pair of cols for "others [grouped]" by week and cumulatively
otherActionVars=actionVars[-c(1:2)] # create a vector of "others"

## List of minor actions for separate chart ====
otherActions=filter(charts,var %in% otherActionVars)
otherActions$var=paste0(otherActions$var,"_csum") # but those are per week so convert names to cumulative

## Get the verbal/email bits of actionVars + other ====
mainActionVars=c(actionVars[1:2],'q12__.__others_sum')
mainActions=filter(charts,var %in% mainActionVars) # get variables from list above
mainActions$var=paste0(mainActions$var,"_csum") # but those are per week so convert names to cumulative
# For [experimental] proportions of [cum.] main actions through time:
mainActionsPw=mainActions %>% 
  mutate(var=paste0(var,"_prop")) # here we're calculating proportions [of total] for [cumm.] main actions

# Functions for just this script ====
# ******************************
# function for labelling ticks as e.g. 10k instead of 10,000
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   # suffix = "k",
                                   big.mark = ",")(x) }

# This function is the only way I could dynamically supply variables names (strings) to rename statements
# from lists. Used when creating version of chart DF for spreadsheet
udf_getVar=function(x){
  variable=sym(x)
  return(variable)
}

# ====Processing code ====
#setup to use captions in Rmd
fig_nums <- captioner()
tab_nums <- captioner(prefix="Table")

# Calculate the base no [denominator] of respondents. This changed on 1/4/2021 but we applied it 
# from survey w/c 29/3/2021, w/e 4/4/2021--even though this spans the boundary change.
udf_geog_lookups() # get the basic geog lookups for the survey
# count before boundary change
initialLaCnt=nrow(distinct(select(filter(surveyRespondents, Valid.from=='05/11/2020' & (Valid.to=='31/03/2021' | Valid.to=='20/06/2021')),Name.of.reporting.entity)))
# count after boundary change; 
finalLaCnt=nrow(distinct(select(filter(surveyRespondents, (Valid.from=='05/11/2020' | Valid.from=='01/04/2021') &  Valid.to=='20/06/2021'),Name.of.reporting.entity)))

# only include the data LAs have allowed us to share
rptDataByWeek = filter(pubDataByWeek, q24_>= publishDataDate) %>% 
  # Compute no. of businesses which should be closed [all tiers+tier specific]
  mutate(q12_1.1y2__sum=q12_1.1__sum+q12_1.2__sum) %>% 
  # For main chart, add up everything which is not 'business which should be closed' or 'not protecting staff and customers'
  mutate(other_breaches=select(., c('q12_2.0__sum','q12_3.0__sum','q12_4.0__sum','q12_6y7.0__sum','q12_8.0__sum','q12_9.0__sum','q12_10.0__sum')) 
         %>% rowSums())

# We want some cumulative sums for charts...
# select w/e, total checks/wk, total FTE/wk, total actions/wk
standard_charts=rptDataByWeek %>% 
  # use intersect formulation because some variables in the vector don't [yet] exist
  # on the dataset: only take the ones which do !
  select(., intersect(names(.), as.vector(charts$var))) %>% 
  # calculate the cumulative sums for these
  mutate(across(-q24_, list(csum=cumsum))) %>% 
  # add the cols for "others grouped" breach types:
  mutate(q12__.__others_sum=rowSums(.[,c(otherActionVars)], na.rm=TRUE)) %>% 
  mutate(q12__.__others_sum_csum=cumsum(q12__.__others_sum)) %>% 
  # add in some proportions for main action types by week [these for cumulative values]
  mutate(q12__.__a_sum_csum_prop=q12__.__a_sum_csum/q12__.__sum_csum, # verbal
         q12__.__b_sum_csum_prop=q12__.__b_sum_csum/q12__.__sum_csum, # letters/emails
         q12__.__others_sum_csum_prop=q12__.__others_sum_csum/q12__.__sum_csum) %>% # others
  # calculate the per-week response rate, using the correct denominator for each week
  mutate(q2__rate=case_when(q24_< geoChangeDate~q2__cnt/initialLaCnt,
                            q24_>=geoChangeDate~q2__cnt/finalLaCnt
                            ))

# get dataset to use for embedded kable table of breaches.
breachesTable=standard_charts %>% 
  select(., intersect(names(.), detailedBreachesVars)) %>% 
  .[nrow(.),] %>% # get the last row of data. This has total number of breaches recorded
  pivot_longer(.,everything()) %>% # transpose
  filter(., value>0) %>% # 12_3 = 0 for this time frame: remove
  arrange(.,value) %>% # put in ascending order
  rbind(.,c('Total',colSums(.[,-1]))) %>% # add a total row
  mutate(name=sub('_csum','',name)) %>% # to change to form to match on "charts" DF
  left_join(charts, by = c("name" = "var")) %>% 
  select(breach=varName,number=value) %>% 
  mutate(breach=ifelse(is.na(breach),'Any breach',breach)) %>% 
  mutate(`%`=round(as.numeric(`number`)/as.numeric(select(filter(., breach=='Any breach'),`number`))*100,1))

# Standard charts ====
# ***************
## weekly survey response rate chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# abstract the variables and their English translations to a list we can reuse. variables first, then translations
ChartVars=list() # initialise as we will reuse this variable later
ChartVars=list('q24_','q2__rate','Week ending','Response rate, %') # abstract the variables to a list we can reuse
names(ChartVars)[1]="Weekly response rates" # this is the chart name for the XL
  
# create DF for the chart for XL output
# The udf_getVar() function approach is the only way I could get rename() to accept the list elements
responsesChartDF=select(standard_charts, one_of(c(ChartVars[[1]],ChartVars[[2]]))) %>%
  # the label says % so have to convert the figures to percentages. Use this [awkward] method; round to 1 dp
  mutate(!!(variable=udf_getVar(ChartVars[[2]])):=round(!!(variable=udf_getVar(ChartVars[[2]]))*100,1)) %>% 
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]], !!(variable=udf_getVar(ChartVars[[4]])):=ChartVars[[2]])

# In the following we have to use the complicated !!(variable=udf_getVar())) formulation instead of [say] get(). The latter
# fails in the RMD. This might be because the environment isn't clear
responsesChart = ggplot2:: ggplot(data = standard_charts,aes(x = !!(variable=udf_getVar(ChartVars[[1]])), y = !!(variable=udf_getVar(ChartVars[[2]])))) +
  geom_line(color = colour1) + th + #,size=1.4 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(labels = function(x) x*100,limits = c(0,NA)) + #scales::percent
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
responsesChart

# create a list. Keep in it chart, chart name and chart DF
# we'll use this to create the spreadsheet output later [=another script]
covidFigs=list()
covidFigs[[length(covidFigs)+1]] = list(responsesChart,responsesChartDF) # chart, DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## cumulative requests for advice chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# NB given that previous weeks' requests may be responded to within a subsequent week and count as "win agreed time"
# the "win agreed time" series can be greater than the requests

ChartVars=list()
ChartVars=list('q24_','q4__sum_csum','Week ending','Cumulative requests to survey respondents, thousands') # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative requests for advice from businesses" # this is the chart name for the XL

requestsChart = ggplot2:: ggplot(data = standard_charts,aes(x = !!(variable=udf_getVar(ChartVars[[1]])), y = !!(variable=udf_getVar(ChartVars[[2]])))) +
  geom_line(color = colour1 ) + th + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(labels = ks,limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
requestsChart

# create DF for the chart for XL output
# The udf_getVar() function approach is the only way I could get rename() to accept the list elements
requestsChartDF=select(standard_charts, one_of(c(ChartVars[[1]],ChartVars[[2]]))) %>% 
  # remove the "thousands" from the column heading [which is based on the y axis title---that _is_ in 000s]
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]], !!(variable=udf_getVar(sub(', thousands','',ChartVars[[4]]))):=ChartVars[[2]])

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(requestsChart,requestsChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] =  sub(', thousands','', names(ChartVars)[1]) # chart name

## cumulative complaints chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ChartVars=list()
ChartVars=list('q24_','q6__sum_csum','Week ending','Cumulative complaints to survey respondents, thousands') # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative complaints about COVID-19 non-compliance" # this is the chart name for the XL

complaintsChart = ggplot2:: ggplot(data = standard_charts,aes(x = !!(variable=udf_getVar(ChartVars[[1]])), y = !!(variable=udf_getVar(ChartVars[[2]])))) +
  geom_line(color = colour1 ) + th + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
         scale_y_continuous(labels = ks,limits = c(0,NA)) +
         scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
complaintsChart

# create DF for the chart for XL output
# The udf_getVar() function approach is the only way I could get rename() to accept the list elements
complaintsChartDF=select(standard_charts, one_of(c(ChartVars[[1]],ChartVars[[2]]))) %>% 
  # again, remove the "thousands" from the col name: not appropriate here
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]], !!(variable=udf_getVar(sub(', thousands','',ChartVars[[4]]))):=ChartVars[[2]])

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(complaintsChart,complaintsChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = sub(', thousands','', names(ChartVars)[1]) # chart name

## weekly (new) complaints chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# add a derived col. to DF standard_charts so we can extract that later when creating the spreadsheet with the chart data
standard_charts=standard_charts %>% 
  mutate('q6__sum/q2__cnt'=`q6__sum`/`q2__cnt`)

ChartVars=list()
ChartVars=list('q24_','q6__sum/q2__cnt','Week ending','Average weekly complaints to survey respondents') # abstract the variables to a list we can reuse
names(ChartVars)[1]="Weekly (new) complaints about COVID-19 non-compliance" # this is the chart name for the XL

complaintsWeeklyChart = ggplot2:: ggplot(data = standard_charts,aes(x = !!(variable=udf_getVar(ChartVars[[1]])), y = !!(variable=udf_getVar(ChartVars[[2]])))) +
  geom_line(color = colour1 ) + th + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
complaintsWeeklyChart

# create DF for the chart for XL output
# The udf_getVar() function approach is the only way I could get rename() to accept the list elementscomplaintsWeeklyChartDF=select(standard_charts, one_of(c(complaintsChartVars[[1]],complaintsChartVars[[2]]))) %>% 
complaintsWeeklyChartDF=select(standard_charts, one_of(c(ChartVars[[1]],ChartVars[[2]]))) %>% 
  # round the figures to 1 dp. Note awkward syntax to avoid hard coding col names.
  mutate(!!(variable=udf_getVar(ChartVars[[2]])):=round(!!(variable=udf_getVar(ChartVars[[2]])),1)) %>%
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]], !!(variable=udf_getVar(ChartVars[[4]])):=ChartVars[[2]])

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(complaintsWeeklyChart,complaintsWeeklyChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## cumulative checks: all, those from complaints and non-compliant businesses found chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ChartVars=list()
# In this more complex one the order is x variable, list of y variables, x variable English name, y axis title, list of y variable English names,
ChartVars=list('q24_',list('q8_1.0_sum_csum','q8_2.0_sum_csum','q8_3.0_sum_csum'),'Week ending',
               'Cumulative checks performed/non-compliant businesses found by survey respondents, thousands',
               list('all checks','checks from complaints','non-compliant businesses')) # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative compliance checks and non-compliant businesses found by authorities" # this is the chart name for the XL

#first gather data into tidy format
checksTidy = standard_charts %>%
  select(`q24_`,`q8_1.0_sum_csum`,`q8_2.0_sum_csum`,`q8_3.0_sum_csum`) %>%
  gather(key = "variable", value = "value", -`q24_`) 

checksChart = ggplot2::ggplot(data = checksTidy,aes(x = `q24_`, y = `value`))+ 
  geom_line(aes(color = `variable`)) + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(labels=ks,limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
  annotate("text", x=as.Date("2021-03-15"), y=90000, label= ChartVars[[5]][2], colour=colour2, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=15000, label= ChartVars[[5]][3], colour=colour3, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=330000, label= ChartVars[[5]][1], colour=colour1, hjust=0, size=8) + 
  scale_color_discrete(labels=c(ChartVars[[5]][1],ChartVars[[5]][2],ChartVars[[5]][3]),
                       type=c(colour1, colour3, colour2)) +
  th 
checksChart

# create DF for the chart for XL output
checksChartDF=select(standard_charts, one_of(c(ChartVars[[1]],unlist(ChartVars[[2]])))) %>% 
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]]) %>% # rename the x axis variable
  # rename the vector of y items; use vector of old and new names
  rename_with(~ as.vector(unlist(ChartVars[5]))[which(as.vector(unlist(ChartVars[2])) == .x)], 
              .cols = as.vector(unlist(ChartVars[2])))

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(checksChart,checksChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## cumulative full-time equivalent resource chart ====
# NB not included in the report
fteChart= ggplot2::ggplot(data = standard_charts, aes(x = `q24_`, y = `q9__.__sum_csum`)) + 
  geom_line(color = colour2 ) + th + 
  labs(title = "Cumulative full-time equivalent resource by survey respondents, thousands",
         x = list(title = "Week ending"),
         y = element_blank()) +
  scale_y_continuous(labels = ks, limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
fteChart

## average weekly full-time equivalent resource chart ====
# NB not included in the report
fteWeeklyChart= ggplot2::ggplot(data = standard_charts, aes(x = `q24_`, y = `q9__.__sum`/`q2__cnt`)) + 
  geom_line(color = colour2 ) + th + 
  labs(title = "Average weekly full-time equivalent resource by survey respondents",
       x = list(title = "Week ending"),
       y = element_blank()) +
  #scale_y_continuous(limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")
fteWeeklyChart

## cumulative major types of breaches chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Determine breaches to plot. In order = all breaches/actions, all other breaches, 
# combined "should be closed" series, not protecting customers
# NB exclude 'q12_1.0__sum' if going from mid-Dec as this option not available

ChartVars=list()
# In this more complex one the order is x variable, list of y variables, x variable English name, y axis title, list of y variable English names,
ChartVars=list('q24_',list('q12__.__sum_csum','other_breaches_csum','q12_1.1y2__sum_csum','q12_5.0__sum_csum'),'Week ending',
               'Covid regulation breaches reported by survey respondents, thousands',
               list('any breach','all other breaches','businesses failing to remain closed','not protecting staff and customers')) # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative breaches of COVID-19 regulations for major breach categories" # this is the chart name for the XL

# First gather into tidy format
breachesTidy = standard_charts %>%
  select(`q24_`,`q12__.__sum_csum`,`other_breaches_csum`,`q12_1.1y2__sum_csum`,`q12_5.0__sum_csum`) %>%
  gather(key = "variable", value = "value", -`q24_`)
str(breachesTidy$variable)
# chart
breachesChart = ggplot2::ggplot(data = breachesTidy,aes(x = `q24_`, y = `value`))+ 
  geom_line(aes(color = `variable`)) + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(labels=ks,limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
  annotate("text", x=as.Date("2021-03-15"), y=100000, label= ChartVars[[5]][1], colour=colour1, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=75000, label= ChartVars[[5]][2], colour=colour2, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=52000, label= ChartVars[[5]][4], colour=colour3, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=8000, label= ChartVars[[5]][3], colour=colour4, hjust=0, size=8) + 
   scale_color_discrete(type=c(colour2, colour1, colour4, colour3)) +
    th 
breachesChart

# create DF for the chart for XL output
breachesChartDF=select(standard_charts, one_of(c(ChartVars[[1]],unlist(ChartVars[[2]])))) %>% 
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]]) %>% # rename the x axis variable
  # rename the vector of y items; use vector of old and new names
  rename_with(~ as.vector(unlist(ChartVars[5]))[which(as.vector(unlist(ChartVars[2])) == .x)], 
              .cols = as.vector(unlist(ChartVars[2])))

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(breachesChart,breachesChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## cumulative main categories of actions undertaken chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# verbal + email + other action types grouped

ChartVars=list()
# In this more complex one the order is x variable, list of y variables, x variable English name, y axis title, list of y variable English names,
ChartVars=list('q24_',list('q12__.__sum_csum','q12__.__a_sum_csum','q12__.__b_sum_csum','q12__.__others_sum_csum'),'Week ending',
               'Number of actions by survey respondents, thousands',
               list('all actions','verbal advice','letter/email','all other actions')) # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative actions against COVID-19 regulation breaches for main action types" # this is the chart name for the XL

# create Actions undertaken tidy format
mainActionsTidy = standard_charts %>%
  select(as.vector(unlist(c(ChartVars[1],unlist(ChartVars[2]))))) %>%
  gather(key = "variable", value = "value", -`q24_`)

mainActionsChart = ggplot2::ggplot(data = mainActionsTidy,aes(x = `q24_`, y = `value`))+ 
  geom_line(aes(color = `variable`)) + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(labels=ks,limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
  annotate("text", x=as.Date("2021-03-15"), y=100000, label= ChartVars[[5]][1], colour=colour1, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=72000, label= ChartVars[[5]][3], colour=colour2, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=36000, label= ChartVars[[5]][2], colour=colour3, hjust=0, size=8) + 
  annotate("text", x=as.Date("2021-03-15"), y=8000, label= ChartVars[[5]][4], colour=colour4, hjust=0, size=8) + 
  scale_color_discrete(labels=c(ChartVars[[4]][2],ChartVars[[5]][3],ChartVars[[5]][4],ChartVars[[5]][1]),
                       type=c(colour3, colour2, colour4, colour1)) +
  th 
mainActionsChart

# create DF for the chart for XL output
mainActionsChartDF=select(standard_charts, one_of(c(ChartVars[[1]],unlist(ChartVars[[2]])))) %>% 
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]]) %>% # rename the x axis variable
  # rename the vector of y items; use vector of old and new names
  rename_with(~ as.vector(unlist(ChartVars[5]))[which(as.vector(unlist(ChartVars[2])) == .x)], 
              .cols = as.vector(unlist(ChartVars[2])))

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(mainActionsChart,mainActionsChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## cumulative "other" [minor] actions chart ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# i.e. those action types making up "other" in main actions chart

ChartVars=list()
# In this more complex one the order is x variable, list of y variables, x variable English name, y axis title, list of y variable English names,
ChartVars=list('q24_',list('q12__.__e_sum_csum','q12__.__d_sum_csum','q12__.__h_sum_csum','q12__.__c_sum_csum',
                           'q12__.__g_sum_csum','q12__.__i_sum_csum','q12__.__f_sum_csum','q12__.__j_sum_csum'),'Week ending',
               'Number of actions by survey respondents',
               list('prohibition notice','fixed penalty','coronavirus improvement notice','direction notice',
                    'HSAWA improvement notice','coronavirus immediate restriction notice','prosecutions',
                    'coronavirus restriction notice')) # abstract the variables to a list we can reuse
names(ChartVars)[1]="Cumulative other actions against COVID-19 regulation breaches" # this is the chart name for the XL

# first tidy format again
otherActionsTidy = standard_charts %>%
  select(as.vector(unlist(c(ChartVars[1],unlist(ChartVars[2]))))) %>%
  gather(key = "variable", value = "value", -`q24_`)
otherActionsTidy = otherActionsTidy %>% 
  mutate(variable=case_when(
    variable==paste0(ChartVars[[2]][1])~1,
    variable==paste0(ChartVars[[2]][2])~2,
    variable==paste0(ChartVars[[2]][3])~3,
    variable==paste0(ChartVars[[2]][4])~4,
    variable==paste0(ChartVars[[2]][5])~5,
    variable==paste0(ChartVars[[2]][6])~6,
    variable==paste0(ChartVars[[2]][7])~7,
    variable==paste0(ChartVars[[2]][8])~8,
    TRUE~-1
  )) %>% 
  mutate(variable=factor(variable,labels=as.vector(unlist(ChartVars[[5]]))))

# chart
otherActionsChart = ggplot2::ggplot(data = otherActionsTidy,aes(x = `q24_`, y = `value`))+ 
  geom_line(aes(color = `variable`)) + 
  labs(title = ChartVars[[4]],y=element_blank(),x = list(title = paste0('\n',ChartVars[[3]]))) + # hack to increase space above title
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month") +
# following are annotations, but they don't work for the lower, more bunched up items  
  # annotate("text", x=as.Date("2021-04-15"), y=950, label= "prohibition notice", colour=colour3, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-04-15"), y=700, label= "fixed penalty", colour=colour2, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-04-15"), y=400, label= "coronavirus improvement notice", colour=colour6, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-04-15"), y=125, label= "direction notice", colour=colour1, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-04-15"), y=80, label= "HSAWA improvement notice", colour=colour5, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-05-15"), y=48, label= "coronavirus immediate restriction", colour=colour7, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-05-15"), y=20, label= "prosecutions", colour=colour4, hjust=0, size=4) + 
  # annotate("text", x=as.Date("2021-04-15"), y=0, label= "coronavirus restriction", colour=colour8, hjust=0, size=4) + 
  # # annotate("text", x=as.Date("2021-04-15"), y=8, label= "direction notices", colour=colour5, hjust=0, size=4) + 
  # scale_color_discrete(labels=c("direction notice","fixed penalty","prohibition notice","prosecutions","hsawa improvement notice","coronavirus improvement notice","coronavirus immediate restriction","coronavirus restriction"),
  scale_color_discrete(type=c(colour1, colour2, colour3, colour4,colour5,colour6,colour7,colour8)) +
  th +
  theme(legend.position=c(0.22,0.74), legend.title=element_blank(), 
        legend.background=element_blank(), legend.key = element_blank(),
        legend.spacing.y=unit(-1, 'cm'), # Note -ve number in call to spacing
        legend.text = element_text(size = 14),
         legend.key.size = unit(2, 'lines')) # increase the vertical space between legend items
otherActionsChart

# create DF for the chart for XL output. In this case, easiest to create from the DF we used for the chart:
otherActionsChartDF=otherActionsTidy %>% 
  pivot_wider(., names_from = variable) %>% 
  rename(!!(variable=udf_getVar(ChartVars[[3]])):=ChartVars[[1]]) # rename the x axis variable

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(otherActionsChart,otherActionsChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = names(ChartVars)[1] # chart name

## frequency histogram of number of weeks respondents responded ====
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# NB This excludes those who never responded.
# Bins run from lower bound < x <= upper bound
hist=filter(data, q24_>= publishDataDate) %>% # apply start date
  distinct(q24_,q2_) %>% # make sure there's no duplication of LA/wk combinations
  group_by(q2_) %>% 
  summarise(cnt=n()) # count them

freqRespsChart=ggplot(data=hist, aes(x=cnt)) +
  geom_histogram(breaks=seq(0, 30, by = 5),fill=colour2,colour=colour1)+
  labs(title = "Number of respondents",
       x = list(title = "\nNumber of weeks response received"), # hack to increase space above title
       y = element_blank()) +
  scale_x_continuous(breaks = seq(2.5, 30, by = 5), 
                     labels=c('1 to 5','6 to 10',
                              '11 to 15','16 to 20','21 to 25','26 to 30')) +
  # scale_x_discrete(breaks=c("0.5","1","2"),
  #                  labels=c("Dose 0.5", "Dose 1", "Dose 2"))
  th  
freqRespsChart

# get the data out of the plot [frequencies] for inclusion in accompanying spreadsheet
freqRespsChartDF=select(ggplot_build(freqRespsChart)$data[[1]],count,xmin,xmax) %>%  # this extracts data underpinning plot
  select(xmin,xmax,count) %>% 
  mutate(`number of weeks response received`=paste0(xmin+1,' to ',xmax, ' inclusive')) %>% 
  select(., -c(xmin,xmax)) %>% 
  rename(`count of respondents`=count) %>% 
  select(2,1)

# add to list for later XL creation
covidFigs[[length(covidFigs)+1]] = list(freqRespsChart,freqRespsChartDF) # DF, variables in chart from DF
names(covidFigs)[length(covidFigs)][[1]] = 'Frequency of responses by respondents' # chart name

# Tables ====
# ******
## list of breaches table ====
# for other, minor breaches
# just rename the dataframe headers to something more Gov.uk compliant
tableOfBreaches=rename(breachesTable,Breach=breach,Number=number, `Per cent`='%') %>% 
  mutate(Number=as.numeric(Number)) %>% 
  kable(format.args = list(big.mark = ","))

# list of legislation in 2020 table A ====
legA=tibble::tribble(~Month,~Day,~Legislation,
                     "March",25,"The Coronavirus Act 2020",
                     "",26,"The Health Protection (Coronavirus, Restrictions) (England) Regulations 2020)",
                     "May",13,"The Health Protection (Coronavirus, Restrictions) (England) (Amendment No. 2) Regulations 2020",
                     "June",1,"The Health Protection (Coronavirus, Restrictions) (England) (Amendment No. 3) Regulations 2020",
                     "",13,"Parts of The Health Protections (Coronavirus, Restrictions) (England) (Amendment No.4) Regulations 2020",
                     "",15,"The remainder of The Health Protections (Coronavirus, Restrictions) (England) (Amendment No.4) Regulations 2020",
                     "July",4,"The Health Protection (Coronavirus, Restrictions) (No. 2) (England) Regulations 2020",
                     "",4,"The Health Protection (Coronavirus, Restrictions) (Leicester) Regulations 2020",
                     "",11,"Parts of The Health Protection (Coronavirus, Restrictions) (No. 2) (England) (Amendment) Regulations 2020",
                     "",13,"The remainder of The Health Protection (Coronavirus, Restrictions) (No. 2) (England) (Amendment) Regulations 2020",
                     "",18,"The Health Protection (Coronavirus, Restrictions) (England) (No.3) Regulations 2020",
                     "August",3,"The Health Protection (Coronavirus, Restrictions) (Leicester) (No. 2) Regulations 2020",
                     "",5,"The Health Protection (Coronavirus, Restrictions on Gatherings) (North of England) Regulations 2020",
                     "September",14,"The Health Protection (Coronavirus, Restrictions) (No. 2) (England) (Amendment) (No. 4) Regulations 2020",
                     "",18,"The Health Protection (Coronavirus, Collection of Contact Details etc and Related Requirements) Regulations 2020",
                     "",24,"The Health Protection (Coronavirus, Restrictions) (No. 2) (England) (Amendment) (No. 5) Regulations 2020",
                     "",28,"The Health Protection (Coronavirus, Restrictions) (Self-Isolation) (England) Regulations 2020",
                     "",28,"The Health Protection (Coronavirus, Restrictions) (Obligations of Undertakings) (England) (Amendment) Regulations 2020",
                     "October",14,"The Health Protection (Coronavirus, Local COVID-19 Alert Level) (Medium) (England) Regulations 2020 (SI. 2020 No. 1103)",
                     "",14,"The Health Protection (Coronavirus, Local COVID-19 Alert Level) (High) (England) Regulations 2020, as amended1 (SI. 2020 No. 1104)",
                     "",14,"The Health Protection (Coronavirus, Local COVID-19 Alert Level) (Very High) (England) Regulations 2020, as amended2 (SI. 2020 No. 1105)",
                     "November",5,"The Health Protection (Coronavirus, Restrictions) (England) (No. 4) Regulations 2020 (SI. 2020: No.1200)",
                     "December",2,"The Health Protection (Coronavirus, Restrictions) (Local Authority Enforcement Powers and Amendment) (England) Regulations 2020")

legislationTableA=kable(legA)

# list of legislation in 2020 table B ====

legB=tibble::tribble(~colA,~colB,~colC,
                     "January",5,"Amendments made to The Health Protection (Coronavirus, Restrictions) (All Tiers) (England) Regulations 2020 (SI 2020 No. 1374)",
                     "March",8,"The Health Protection (Coronavirus) (Wearing of Face Coverings in a Relevant Place and Restrictions: All Tiers) (England) (Amendment) Regulations 2021 (SI 2021. No. 247)",
                     "",29,"The Health Protection (Coronavirus, Restrictions) (Steps) (England) Regulations 2021",
                     "April",12,"The Health Protection (Coronavirus, Restrictions) (Steps) (England) Regulations 2021 amended",
                     "May",17,"The Health Protection (Coronavirus, Restrictions) (Steps and Other Provisions) (England) (Amendment) Regulations 2021 (SI 2021 No. 585)")

legislationTableB=kable(legB, col.names = NULL)

# need to turn the rownames into row titles for screen readers: this isn't working...
# tableOfBreaches=c(tableOfBreaches[1:2],gsub('^\\|','\\|\\# ',tableOfBreaches[3:length(tableOfBreaches)]))
# attr(tableOfBreaches, 'class') = 'knitr_kable'
# attr(tableOfBreaches, 'format') = 'pipe'

## breach types by date possible table ====
# When did different breach types apply in the survey?
# all breaches start with q12_. Exclude q12_6/q12_7 as these are "other" breaches--text entries
ba=select(data,colnames(data[,grep("(q12_*)|q24_",colnames(data))])) %>% # get breaches and dates
  select(., !starts_with(c("q12_6","q12_7"))) %>% # lose the "other" breaches
  # unpivot; will get NAs for weeks where breach not apply:
  pivot_longer(.,-q24_,names_to='col',values_to='count') %>% 
  filter(., !is.na(count))  # remove the NA rows

bt=ba %>% # re-use this DF for the next table [below]
  # reformat the breach col so it sorts properly [10 is last]
  mutate(col=as.numeric(gsub('q12_(.+)_[a-z]','\\1',col))) %>% 
  distinct(., q24_,col) # get distinct combinations
# NB we can't reformat the dates into something more readable here
# because then they sort out of sequence in the next stage

# crosstab the breaches with the weeks [weeks across top]
breachesDates=table(bt[c('col','q24_')])
attributes(breachesDates)$class = "matrix" # convert to matrix from table
breachesDates=as.data.frame(breachesDates) %>% # convert matrix to DF.
  # It has row names, but we want explicit entries their own column  
  # [dplyr equivalent didn't work first time here]  
  mutate(q=rownames(.)) %>% 
  # reorder the cols so breach ID col is first  
  select(q,everything())

# condense this cross tab into a set of "from date x to date y" entries
dates=colnames(breachesDates[,2:ncol(breachesDates)]) # get the list of dates
# process the table a row at a time
dvalid=vector() # initialise empty vector

for( i in 1: nrow(breachesDates)){ # run through, a row at a time
  r = rle(breachesDates[i,2:ncol(breachesDates)]) # first col has ID
  indx=which(r$values[1,]==1) # get the indices of entries which==1
  de=colnames(r$values[indx]) # get the dates; these are the last dates
  dep=which(dates %in% de) # get positions of these dates in global dates list
  dsp=dep-r$lengths[indx]+1 # calculate the start date positions
  ds=dates[dsp] # get start dates using positions
  ds=format(as.Date(ds), "%e %B %Y") # make dates more readable
  de=format(as.Date(de), "%e %B %Y") # ditto
  c=paste0(str_trim(ds),' to ',str_trim(de)) # collapse start and end date to single string
  c=paste0(c,collapse='; ') # if there's more than one start/end pair for a breach type, concatenate
  dvalid=c(dvalid,c) # add to the vector of dates when breach was valid
}

# hand code a more verbose description of each breach and its equivalent category in the report
pubbt=tribble(
  ~q,~Breach,~category,
  '1','Premises failing to close that are required to remain closed by law (that is during national lockdown)','businesses failing to remain closed',
  '1.1','Premises failing to close that are required to remain closed by law in all tiers (nightclubs, dancehalls & sexual entertainment venues) (that is when tiers were in force)','businesses failing to remain closed',
  '1.2','Premises failing to comply with tier specific business closure rules','businesses failing to remain closed',
  '2','Premises failing to take reasonable steps to ensure that workers who must be self-isolating are not working from outside their home','self-isolating workers working outside home',
  '3','Premises failing to comply with a Local Authority direction (to close, or that places certain restrictions upon a premise, event, or public outdoor place) - apart from sector-wide direction given as part of national rules','failing to comply with a Local Authority direction',
  '4','Premises not collecting customer information for NHS Test and Trace or not displaying a QR code','not collecting test and trace information',
  '5','Breaches of COVID-Secure guidance in relation to legal duty of employers to protect staff and customers under Health and Safety legislation (for example, issues with face coverings, social distancing and/or signage)','not protecting staff and customers',
  '8','Premises failing to adhere to restrictions on opening hours and last order times','exceeding opening hours rules',
  '9','Premises failing to comply with requirement to provide table service only','not providing table service only',
  '10','Premises failing to take reasonable steps to prevent bookings of over 6 (or one household in areas of local restrictions), table spacing, and mingling between groups on the premises','breaches of the rule of 6'
)

dvalid=as.data.frame(dvalid) %>% 
  cbind(breachesDates$q) %>% 
  rename('q'='breachesDates$q', `Dates valid`='dvalid') %>% 
  inner_join(., pubbt) %>% 
  rename(`Category in report`='category') %>% 
  select(., Breach,`Dates valid`,`Category in report`)

## action types by date possible table ====
# which types of action were possible when?
# extract the available actions in the same way as the breach types above
actions=ba %>% 
  # reformat the breach col so it sorts properly [10 is last]
  mutate(col=gsub('q12_.+_([a-z])','\\1',col)) %>% 
  distinct(., q24_,col) # get distinct combinations

# crosstab the breaches with the weeks [weeks across top]
actionsDates=table(actions[c('col','q24_')])
attributes(actionsDates)$class = "matrix" # convert to matrix from table
actionsDates=as.data.frame(actionsDates) %>% # convert matrix to DF.
  # It has row names, but we want explicit entries their own column  
  # [dplyr equivalent didn't work first time here]  
  mutate(q=rownames(.)) %>% 
  # reorder the cols so breach ID col is first  
  select(q,everything())

# condense this cross tab into a set of "from date x to date y" entries
# process the table a row at a time
avalid=vector() # initialise empty vector
for( i in 1: nrow(actionsDates)){ # run through, a row at a time
  r = rle(actionsDates[i,2:ncol(actionsDates)]) # first col has ID
  indx=which(r$values[1,]==1) # get the indices of entries which==1
  de=colnames(r$values[indx]) # get the dates; these are the last dates
  dep=which(dates %in% de) # get positions of these dates in global dates list
  dsp=dep-r$lengths[indx]+1 # calculate the start date positions
  ds=dates[dsp] # get start dates using positions
  ds=format(as.Date(ds), "%e %B %Y") # make dates more readable
  de=format(as.Date(de), "%e %B %Y") # ditto
  c=paste0(str_trim(ds),' to ',str_trim(de)) # collapse start and end date to single string
  c=paste0(c,collapse='; ') # if there's more than one start/end pair for a breach type, concatenate
  avalid=c(avalid,c) # add to the vector of dates when breach was valid
}

pubat=tribble(
  ~q,~action,
  'a','verbal advice',
  'b','letter/email',
  'c','direction notice issued',
  'd','fixed penalty notice issued',
  'e','prohibition notice issued',
  'f','prosecutions initiated',
  'g','Health and Safety At Work Act (HSAWA) improvement notice',
  'h','coronavirus improvement notice',
  'i','coronavirus immediate restriction notice',
  'j','coronavirus restriction notice'
)
# actual table [rendered by kable in RMD]
actionsDates=bind_cols(pubat,avalid) %>% 
  rename(.,`dates valid`='...3') %>% 
  select(action,`dates valid`) %>% 
  group_by(`dates valid`) %>% 
  summarise(action = paste(unique(action), collapse = ', '))



