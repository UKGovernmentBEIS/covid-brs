# *** Covid: Time series of key metrics by week [all England] *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# Then take selected variables from this and output to XL

# *** Optional:
rm(list=ls()) # remove anything in memory

# Libraries
library('plotly')
library('stringr')
library('knitr')

# Working directory ====
# *****************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# aggregate data to week across regions [i.e. for England]
# run this first because it removes anything in memory

source('CovidByWeek.r')

# Constants for just this script ====
# ******************************
# NB other constants are in CovidConstants.r
resultsDir=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/") # For convenience, use the scripts directory
# resultsDir="C:/Users/fsymons/Downloads/Annexes/" # or uncomment and change this...

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

# date of switch in administrative boundaries to new geography--at least as far as survey results concerned [actual switch = 1/4/2021]
geoChangeDate=as.Date("2021-03-29")

# ====list of breaches
# These are breaches of the Covid business restrictions regulations
# This 'detailed' list used for the table. Note still includes amalgamations:--1.1 & 1.2, 6 & 7.
detailedBreachesVars=c('q12_1.1y2__sum_csum','q12_2.0__sum_csum','q12_3.0__sum_csum','q12_4.0__sum_csum',
                       'q12_5.0__sum_csum','q12_6y7.0__sum_csum','q12_8.0__sum_csum','q12_9.0__sum_csum','q12_10.0__sum_csum')
detailedBreaches=filter(charts,var %in% detailedBreachesVars)
# First choose the basic variable list:
breachVars=c('q12__.__sum','q12_1.1y2__sum','q12_5.0__sum','other_breaches')
breaches=filter(charts,var %in% sub('_csum','',breachVars))

# ====list of actions by LAs
# Create a vector of total actions for each LA action [intervention] type. 
actionVars=c('q12__.__a_sum','q12__.__b_sum','q12__.__c_sum','q12__.__d_sum',
             'q12__.__e_sum','q12__.__f_sum','q12__.__g_sum','q12__.__h_sum','q12__.__i_sum','q12__.__j_sum')

# Note that the magnitude of the first 2 actions, verbal advice and emails/letters, is much greater than
# any other. So makes sense to do 2 presentations: one chart with verbal + emails + others [grouped] and another
# [perhaps table] showing the lesser categories of actions individually.
# First, create an appropriate pair of cols for "others [grouped]" by week and cumulatively
otherActionVars=actionVars[-c(1:2)] # create a vector of "others"

# List of minor actions for separate chart
otherActions=filter(charts,var %in% otherActionVars)
otherActions$var=paste0(otherActions$var,"_csum") # but those are per week so convert names to cumulative

# Get the verbal/email bits of actionVars + other
mainActionVars=c(actionVars[1:2],'q12__.__others_sum')
mainActions=filter(charts,var %in% mainActionVars) # get variables from list above
mainActions$var=paste0(mainActions$var,"_csum") # but those are per week so convert names to cumulative
# For [experimental] proportions of [cum.] main actions through time:
mainActionsPw=mainActions %>% 
  mutate(var=paste0(var,"_prop")) # here we're calculating proportions [of total] for [cumm.] main actions

# ====Processing code

# Calculate the base no [denominator] of respondents. This changed on 1/4/2021 but we applied it 
# from survey w/c 29/3/2021, w/e 4/4/2021--even though this spans the boundary change.
udf_geog_lookups() # get the basic geog lookups for the survey
# count before boundary change
initialLaCnt=nrow(filter(surveyRespondents, Valid.from=='05/11/2020' & (Valid.to=='31/03/2021' | Valid.to=='20/06/2021')))
# count after boundary change; 
finalLaCnt=nrow(filter(surveyRespondents, (Valid.from=='05/11/2020' | Valid.from=='01/04/2021') &  Valid.to=='20/06/2021'))

# only include the data LAs have allowed us to share
rptDataByWeek = filter(pubDataByWeek, q24_>= "2020-12-13") %>% 
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
# Let's use plotly
# survey responses each week
responsesChart = plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q2__rate`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
  layout(title = "Survey responses",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Response rate",showticklabels = TRUE,showline=TRUE,tickformat='%',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
responsesChart

# Simple line chart of cumulative requests for advice
# NB given that previous weeks' requests may be responded to within a subsequent week and count as "win agreed time"
# the "win agreed time" series can be greater than the requests
requestsChart = plot_ly(data=standard_charts, x = ~`q24_`, y=~`q4__sum_csum`, name='all requests', type='scatter',mode='lines+markers') %>% 
  #add_trace(y = ~	q5__sum_csum, name = 'response within agreed time', mode = 'lines+markers') %>% 
  layout(title = "Cumulative requests for advice",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of requests",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
requestsChart

# cumulative complaints chart  
complaintsChart = plot_ly(data=standard_charts, x = ~`q24_`, y=~`q6__sum_csum`, name='complaints', type='scatter',mode='lines+markers') %>% 
  layout(title = "Cumulative complaints",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of compaints",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
complaintsChart

# weekly (new) complaints
complaintsWeeklyChart = plot_ly(data=standard_charts, x = ~`q24_`, y=~`q6__sum`/`q2__cnt`, name='complaints', type='scatter',mode='lines+markers') %>% 
  layout(title = "Weekly complaints",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of compaints",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
complaintsWeeklyChart

# Simple line chart of cumulative checks with time
checksChart = plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q8_1.0_sum_csum`, name='all checks', type="scatter", mode="lines+markers", line=list(width = 1)) %>% 
  add_trace(y = ~	q8_2.0_sum_csum, name = 'checks from complaints', mode = 'lines+markers') %>% 
  add_trace(y = ~	q8_3.0_sum_csum, name = 'non-compliant businesses', mode = 'lines+markers') %>% 
  layout(title = "Cumulative Covid compliance checks",
        xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
        yaxis = list(title = "Number of checks/businesses",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = TRUE)
checksChart

# full-time equivalent resource graph
fteChart= plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q9__.__sum_csum`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
  layout(title = "Cumulative full-time equivalent resource",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Full-time equivalent resource",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = F)
fteChart

# full-time equivalent resource graph on weekly
fteWeeklyChart= plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q9__.__cnt`/`q2__cnt`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
  layout(title = "Weekly full-time equivalent resource",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Full-time equivalent resource",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = F)
fteWeeklyChart

# ===Breaches by types
# Determine breaches to plot. In order = all breaches/actions, all other breaches, 
# combined "should be closed" series, not protecting customers
# NB exclude 'q12_1.0__sum' if going from mid-Dec as this option not available
breachesChart = plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q12__.__sum_csum`, name='all breach types', type="scatter", mode="lines+markers", line=list(width = 1)) %>% 
  add_trace(y = ~	other_breaches_csum, name = 'all other breaches', mode = 'lines+markers') %>% 
  add_trace(y = ~	q12_1.1y2__sum_csum, name = 'businesses failing to remain closed', mode = 'lines+markers') %>% 
  add_trace(y = ~	q12_5.0__sum_csum, name = 'not protecting staff and customers', mode = 'lines+markers') %>% 
  layout(title = "Cumulative breaches of Covid regulations",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of breaches",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = TRUE)
breachesChart

# NB The following is an alternative, small multis version of the above.
# **As set up, list of variables uses non-cumulative totals. We adjust this below to work with the
# cumulative figures. 
udf_plotf=function(var){
  # adjust the variable name to reflect cumulative version of the variable
  plot_ly(standard_charts, x = ~q24_, y = as.formula(paste0("~", var[1],'_csum'))) %>%
    add_lines(name = var[2])
}
plots=apply(breaches,1, udf_plotf)
breachesMultiChart=subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# In this version, per week pattern [not cumulative]
udf_plotf=function(var){
  # adjust the variable name to reflect cumulative version of the variable
  plot_ly(standard_charts, x = ~q24_, y = as.formula(paste0("~", var[1]))) %>%
    add_lines(name = var[2])
}
plots=apply(breaches,1, udf_plotf)
breachesWklyMultiChart=subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# breaches table
tableOfBreaches=kable(breachesTable)

# ===Actions undertaken
# **Create the plot of main actions [verbal + email + others grouped] **
# create a blank plot
blankPlot = plot_ly()%>%
  layout(title = "Actions undertaken",
         xaxis = list(title = "week ending"),
         yaxis = list (title = "number of actions"),
         type = 'scatter',
         mode = 'line+markers')
mainActionsChart = blankPlot

# run along variable list in actions DF and add these to plot
# match the variable name to get the English entry
for(i in mainActions$var){
  mainActionsChart = mainActionsChart %>% add_trace(x = standard_charts[["q24_"]], y = standard_charts[[i]], name = mainActions$varName[which(mainActions$var==i)],
                       type = 'scatter',
                       mode = 'line+markers',
                       line = list(width = 0.5))
}
# let's add in the total [same as total number of breaches !]
mainActionsChart = mainActionsChart %>% 
  add_trace(x = standard_charts[["q24_"]], y = standard_charts[["q12__.__sum_csum"]], name = 'all actions',
            type = 'scatter',
            mode = 'line+markers',
            line = list(width = 0.5))
mainActionsChart

mainActionsChartPw = plot_ly()%>%
  layout(title = "Weekly actions undertaken",
         xaxis = list(title = "week ending"),
         yaxis = list (title = "proportion of actions",tickformat='%'),
         type = 'scatter',
         mode = 'line+markers'
         )

# run along variable list in actions DF and add these to plot
# match the variable name to get the English entry
for(i in mainActionsPw$var){
  mainActionsChartPw = mainActionsChartPw %>% 
    add_trace(x = standard_charts[["q24_"]], 
              y = standard_charts[[i]], name = mainActions$varName[which(mainActionsPw$var==i)],
                                                    type = 'scatter',
                                                    mode = 'line+markers',
                                                    line = list(width = 0.5))
}
mainActionsChartPw

# **Create the plot of other actions  **
# create a blank plot
otherActionsChart=blankPlot

for(i in otherActions$var){
  otherActionsChart = otherActionsChart %>% 
    add_trace(x = standard_charts[["q24_"]], y = standard_charts[[i]], 
              name = otherActions$varName[which(otherActions$var==i)],
                                                    type = 'scatter',
                                                    mode = 'line+markers',
                                                    line = list(width = 0.5))
}
otherActionsChart

# 
# apply(actions,1,function(var){
#   plot_ly(standard_charts, x = ~q24_, type="scatter", mode = 'lines+markers') %>%
#     add_trace(x = ~q24_, y = as.formula(paste0("~", var[1])), mode = 'lines+markers') %>%
#     add_lines(name = var[2])
# })
# 
# plots=apply(actions,1, udf_plotf)
# actionsChart=subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)
# actionsChart
# 
# breachesChart = plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q12_1.0__sum`, name='businesses failing to remain closed', type="scatter", mode="lines+markers", line=list(width = 1)) %>% 
#   add_trace(y = ~q12_1.1__sum, name = 'businesses failing to remain closed (all tiers)', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_1.2__sum, name = 'businesses failing to remain closed (tier specific)', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_2.0__sum, name = 'self-isolating workers working outside home', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_3.0__sum, name = 'failing to comply with a Local Authority direction', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_4.0__sum, name = 'not collecting test and trace information', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_5.0__sum, name = 'not protecting staff and customers', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_6y7.0__sum, name = 'other breaches', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_8.0__sum, name = 'exceeding opening hours rules', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_9.0__sum, name = 'not providing table service only', mode = 'lines+markers') %>% 
#   add_trace(y = ~q12_10.0__sum, name = 'breaches of the rule of 6', mode = 'lines+markers') %>% 
#   layout(title = "Cumulative Covid compliance checks",
#          xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
#          yaxis = list(title = "Number of checks/businesses",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = TRUE)
# breachesChart
# 
# 
# library(plotly)
# 
# trace_0 <- rnorm(100, mean = 5)
# trace_1 <- rnorm(100, mean = 0)
# trace_2 <- rnorm(100, mean = -5)
# x <- c(1:100)
# 
# data <- data.frame(x, trace_0, trace_1, trace_2)
# 
# fig <- plot_ly(data, x = ~x)
# fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
# fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
# fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
# 
# 
# #,type="scatter", mode="lines+markers"
# # y = ~`requests for advice cum. sum`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
# #   layout(title = "Cumulative requests for advice",
# #          xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
# #          yaxis = list(title = "Number of requests",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
# # requests_chrt=requests_chrt %>% add_trace(y=~`requests responded within agreed time`, name=`requests responded within agreed time`, mode='lines')
#   requests_chrt
# 
# 
# 
# # Simple line chart of cumulative actions with time
# actions_chrt= plot_ly(data = standard_charts, x = ~`week ending`, y = ~`actions by week end cum. sum`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
#   layout(title = "Cumulative Covid enforcement actions",
#          xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
#          yaxis = list(title = "Number of actions",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
# actions_chrt
# 
# # Simple line chart of cumulative FTE with time
# fte_chrt= plot_ly(data = standard_charts, x = ~`week ending`, y = ~`FTE by week end cum. sum`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
#   layout(title = "Cumulative Full-Time Equivalent (FTE) resource for Covid activities",
#          xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
#          yaxis = list(title = "FTE",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
# fte_chrt
# 
# # Types of action ====
# # ***************
# action_charts=select(rptDataByWeek, actions$var) %>% 
#   # calculate the cumulative sums for these
#   mutate(across(-q24_, list(csum=cumsum)))
# # Regex-replace variable with English names [see above]
# names(action_charts) = stringi::stri_replace_all_regex(names(action_charts), actions$var, actions$varName, vectorize_all = FALSE)
# names(action_charts) = stringi::stri_replace_all_regex(names(action_charts),'_csum',' cum. sum', vectorize_all = FALSE)
# 
# plots <- lapply(breachVars, function(i) {
#   plot_ly(standard_charts, x = ~q24_, y = as.formula(paste0("~", var))) %>%
#     add_lines(name = var)
# })

# use the vector of variable names $var and regular expression-replace with the English variable name
# Do in 2 operations because we will still have '_csum' suffixes
# names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts), charts$var, charts$varName, vectorize_all = FALSE)
# names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts),'_csum',' cum. sum', vectorize_all = FALSE)