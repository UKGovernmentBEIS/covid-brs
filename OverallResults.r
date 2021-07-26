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

 charts=data.frame(var=c('q24_','q2__cnt','q4__sum','q5__sum','q6__sum','q6__sum','q8_1.0_sum','q8_2.0_sum',
                         'q8_3.0_sum','q9__.__sum','q9__.__cnt','q12_1.0__sum','q12_1.1__sum','q12_1.2__sum',
                         'q12_2.0__sum','q12_3.0__sum','q12_4.0__sum','q12_5.0__sum','q12_6y7.0__sum',
                         'q12_8.0__sum','q12_9.0__sum','q12_10.0__sum','q12__.__sum','q12__.__a_sum',
                         'q12__.__b_sum','q12__.__c_sum','q12__.__d_sum','q12__.__e_sum','q12__.__f_sum',
                         'q12__.__g_sum','q12__.__h_sum','q12__.__i_sum','q12__.__j_sum'),
                   varName=c('week ending','responses','requests for advice',
                             'requests responded within agreed time','complaints received','complaints weekly',
                             'checks by week end','checks due to complaint by week end',
                             'non-compliant businesses found by checks','full-time equivalent employees','fte',
                             'businesses failing to remain closed',
                             'businesses failing to remain closed (all tiers)',
                             'businesses failing to remain closed (tier specific)',
                             'self-isolating workers working outside home',
                             'failing to comply with a Local Authority direction',
                             'not collecting test and trace information',
                             'not protecting staff and customers','other breaches',
                             'exceeding opening hours rules','not providing table service only',
                             'breaches of the rule of 6','all breach types','verbal advice',
                             'letter/email','direction notice','fixed penalty','prohibition notice',
                             'prosecutions','hsawa improvement notice','coronavirus improvement notice',
                             'coronavirus immediate restriction','coronavirus restriction'))

# This isn't strictly necessary since it's the function default:
group_cols='q24_'

# only include the data LAs have allowed us to share
rptDataByWeek = filter(pubDataByWeek, q24_>= "2020-12-13")

# We want some cumulative sums for charts...
# select w/e, total checks/wk, total FTE/wk, total actions/wk
standard_charts=select(rptDataByWeek, charts$var) %>% 
  # calculate the cumulative sums for these
  mutate(across(-q24_, list(csum=cumsum))) 

# use the vector of variable names $var and regular expression-replace with the English variable name
# Do in 2 operations because we will still have '_csum' suffixes
# names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts), charts$var, charts$varName, vectorize_all = FALSE)
# names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts),'_csum',' cum. sum', vectorize_all = FALSE)

# Standard charts ====
# ***************
# Let's use plotly
# survey responses each week
responsesChart = plot_ly(data = standard_charts, x = ~`q24_`, y = ~`q2__cnt`, type="scatter", mode="lines+markers", line=list(width = 1, color="#005674")) %>% 
  layout(title = "Survey responses",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of responses",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
responsesChart

# Simple line chart of cumulative requests for advice
# NB given that previous weeks' requests may be responded to within a subsequent week and count as "win agreed time"
# the "win agreed time" series can be greater than the requests
requestsChart = plot_ly(data=standard_charts, x = ~`q24_`, y=~`q4__sum_csum`, name='all requests', type='scatter',mode='lines+markers') %>% 
  #add_trace(y = ~	q5__sum_csum, name = 'response within agreed time', mode = 'lines+markers') %>% 
  layout(title = "Cumulative requests for advice",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of requests",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = TRUE)
requestsChart
  
complaintsChart = plot_ly(data=standard_charts, x = ~`q24_`, y=~`q6__sum_csum`, name='complaints', type='scatter',mode='lines+markers') %>% 
  layout(title = "Cumulative complaints",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of compaints",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
complaintsChart

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

# Add a vector of total actions for each breach. NB exclude 'q12_1.0__sum' if going from mid-Dec as this option
# not available
breachVars=c('q12_1.1__sum','q12_1.2__sum','q12_2.0__sum','q12_3.0__sum','q12_4.0__sum','q12_5.0__sum','q12_6y7.0__sum','q12_8.0__sum','q12_9.0__sum','q12_10.0__sum')
breaches=filter(charts,var %in% breachVars)

udf_plotf=function(var){
  plot_ly(standard_charts, x = ~q24_, y = as.formula(paste0("~", var[1]))) %>%
    add_lines(name = var[2])
}

plots=apply(breaches,1, udf_plotf)

# plots <- lapply(breachVars, function(i) {
#   plot_ly(standard_charts, x = ~q24_, y = as.formula(paste0("~", var))) %>%
#     add_lines(name = var)
# })
breachesChart=subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# Create a vector of total actions for each action type. 
actionVars=c('q12__.__a_sum','q12__.__b_sum','q12__.__c_sum','q12__.__d_sum',
             'q12__.__e_sum','q12__.__f_sum','q12__.__g_sum','q12__.__h_sum','q12__.__i_sum','q12__.__j_sum')

# Note that the magnitude of the first 2 actions, verbal advice and emails/letters, is much greater than
# any other. So makes sense to do 2 charts: one with verbal + emails + others [grouped] and another
# showing the lesser categories of actions individually.
# First, create an appropriate pair of cols for "others [grouped]" by week and cumulatively

otherActionVars=actionVars[-c(1:2)] # create a vector of "others"

# add the cols for "others grouped"
standard_charts=standard_charts %>% 
  mutate(q12__.__others_sum=rowSums(.[,c(otherActionVars)], na.rm=TRUE),
         q12__.__others_sum_csum=cumsum(q12__.__others_sum))

# add entries to the charts DF
charts=charts %>% 
  add_row(var='q12__.__others_sum',varName='all other actions')

# get the verbal/email bits of actionVars + other
mainActionVars=c(actionVars[1:2],'q12__.__others_sum')

mainActions=filter(charts,var %in% mainActionVars)
mainActions$var=paste0(mainActions$var,"_csum")

# **Create the plot of main actions [verbal + email + others grouped] **
# create a blank plot
blankPlot = plot_ly()%>%
  layout(title = "Actions undertaken",
         xaxis = list(title = "week ending"),
         yaxis = list (title = "number of actions"),
         type = 'scatter',
         mode = 'line+markers')

mainActionsChart=blankPlot
# run along variable list in actions DF and add these to plot
# match the variable name to get the English entry
for(i in mainActions$var){
  mainActionsChart = mainActionsChart %>% add_trace(x = standard_charts[["q24_"]], y = standard_charts[[i]], name = mainActions$varName[which(mainActions$var==i)],
                       type = 'scatter',
                       mode = 'line+markers',
                       line = list(width = 0.5))
}
mainActionsChart

otherActions=filter(charts,var %in% otherActionVars)
otherActions$var=paste0(otherActions$var,"_csum")
# **Create the plot of other actions  **
# create a blank plot
otherActionsChart=blankPlot

for(i in otherActions$var){
  otherActionsChart = otherActionsChart %>% add_trace(x = standard_charts[["q24_"]], y = standard_charts[[i]], name = otherActions$varName[which(otherActions$var==i)],
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
