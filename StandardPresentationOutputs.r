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

# list of standard effort measures [checks, FTE etc]: data variable and "English" translation
charts=data.frame(var=c('q24_','q8_1.0_sum','q9__.__sum','q12__.__sum'),
                  varName=c('week ending','checks by week end','FTE by week end','actions by week end'))

# list of action types: data variable and "English" translation
actions=data.frame(var=c('q24_','q12__.__a_sum','q12__.__b_sum','q12__.__c_sum',
                         'q12__.__d_sum','q12__.__e_sum','q12__.__f_sum',
                         'q12__.__g_sum','q12__.__h_sum','q12__.__i_sum','q12__.__j_sum'),
                   varName=c('week ending','verbal advice','letter/email','direction notice',
                             'fixed penalty','prohibition notice','prosecution',
                             'HSAWA improvement notice','Coronavirus improvement notice',
                             'Coronavirus immediate restriction','Coronavirus restriction'))

# [Title] font style for XL. This has to be here because the library needed is loaded in the source statement above
st1 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 14) # black, bold, larger
st2 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "red",fontSize = 10) # red, bold
st3 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 10) # black, bold

# This isn't strictly necessary since it's the function default:
group_cols='q24_'

# We want some cumulative sums for charts...
# select w/e, total checks/wk, total FTE/wk, total actions/wk
standard_charts=select(pubDataByWeek, charts$var) %>% 
  # calculate the cumulative sums for these
  mutate(across(-q24_, list(csum=cumsum))) 
# use the vector of variable names $var and regular expression-replace with the English variable name
# Do in 2 operations because we will still have '_csum' suffixes
names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts), charts$var, charts$varName, vectorize_all = FALSE)
names(standard_charts) = stringi::stri_replace_all_regex(names(standard_charts),'_csum',' cum. sum', vectorize_all = FALSE)

# Standard charts ====
# ***************
# Let's use plotly
# Simple line chart of cumulative checks with time
checks_chrt= plot_ly(data = standard_charts, x = ~`week ending`, y = ~`checks by week end cum. sum`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Covid compliance checks",
        xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
        yaxis = list(title = "Number of checks",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
checks_chrt

# Simple line chart of cumulative actions with time
actions_chrt= plot_ly(data = standard_charts, x = ~`week ending`, y = ~`actions by week end cum. sum`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Covid enforcement actions",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of actions",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
actions_chrt

# Simple line chart of cumulative FTE with time
fte_chrt= plot_ly(data = standard_charts, x = ~`week ending`, y = ~`FTE by week end cum. sum`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Full-Time Equivalent (FTE) resource for Covid activities",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "FTE",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
fte_chrt

# Types of action ====
# ***************
action_charts=select(pubDataByWeek, actions$var) %>% 
  # calculate the cumulative sums for these
  mutate(across(-q24_, list(csum=cumsum)))
# Regex-replace variable with English names [see above]
names(action_charts) = stringi::stri_replace_all_regex(names(action_charts), actions$var, actions$varName, vectorize_all = FALSE)
names(action_charts) = stringi::stri_replace_all_regex(names(action_charts),'_csum',' cum. sum', vectorize_all = FALSE)

# Output to XL ====
# ************
time=Sys.time()
## Create Workbook  ----
# ^^^^^^^^^^^^^^^
wb=createWorkbook()

# Set the base font for the XLS
modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

## Add sheet for the data and populate ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
sheet = addWorksheet(wb, "charts", gridLines = FALSE)

# Standard charts ----
# ^^^^^^^^^^^^^^^^
# Put in a title
title="Covid Business Restriction Survey: effort in England"
writeData(wb, sheet = "charts", x = title, colNames = FALSE)
# embolden the title
addStyle(wb, 'charts', st1, rows = 1, cols = 1,stack = TRUE)

title="Internal use only: please do not release. We can share data from mid-December 2020 onwards (only) with other Departments"
writeData(wb, sheet = "charts", x = title, startCol=1, startRow=2,colNames = FALSE)
# Make warning red, bold
addStyle(wb, 'charts', st2, rows = 2, cols = 1,stack = TRUE)

# output current date/time
writeData(wb, sheet = "charts", x = paste0("As at: ",time), startCol=1, startRow=3,colNames = FALSE)

# put in 'major major' heading
writeData(wb, sheet = "charts", x = 'Overall effort', startCol=1, startRow=5,colNames = FALSE)
# Make bold
addStyle(wb, 'charts', st3, rows = 5, cols = 1,stack = TRUE)

# put in 'major' headings
headers=data.frame(h=c('','for week ending',rep('',nrow(charts)-2),'Cumulative figures'))
writeData(wb, sheet = "charts", x = t(headers), startCol=1, startRow=7,colNames = FALSE)
# Make bold
addStyle(wb, 'charts', st3, rows = 7, cols = 1:nrow(headers),stack = TRUE)

# write the main DF including col headings
writeData(wb, sheet = "charts", x = standard_charts, startCol=1, startRow=8,colNames = TRUE)
# Make bold headers
addStyle(wb, 'charts', st3, rows = 8, cols = 1:ncol(standard_charts),stack = TRUE)

# Types of Action ----
# ^^^^^^^^^^^^^^^^
# put in 'major major' heading
writeData(wb, sheet = "charts", x = 'Action types', startCol=1, startRow=8+2+nrow(standard_charts),colNames = FALSE)
# Make bold
addStyle(wb, 'charts', st3, rows = 8+2+nrow(standard_charts), cols = 1,stack = TRUE)

# put in 'major' headings
headers=data.frame(h=c('','for week ending',rep('',nrow(actions)-2),'Cumulative figures'))
writeData(wb, sheet = "charts", x = t(headers), startCol=1, startRow=8+2+nrow(standard_charts)+2,colNames = FALSE)
# Make bold
addStyle(wb, 'charts', st3, rows = 8+2+nrow(standard_charts)+2, cols = 1:nrow(headers),stack = TRUE)

# add the actions data
writeData(wb, sheet = "charts", x = action_charts, startCol=1, startRow=8+2+nrow(standard_charts)+2+1,colNames = TRUE)
# Make bold headers
addStyle(wb, 'charts', st3, rows = 8+2+nrow(standard_charts)+2+1, cols = 1:ncol(action_charts),stack = TRUE)

# set the column widths. The datasets have different nos of cols
# First find how many letters each heading has in each dataset
actionsCols=nchar(names(action_charts)) # find the number of letters in each col heading [=gives vector of numbers]
chartsCols=nchar(names(standard_charts)) # as above
#write.table(chartsCols,'clipboard',sep='\t')
# for each col heading, we want the max. no of letters [i.e. the max of two sets of letters]
# we can only compare no. of letters for cols where there is a heading in both datasets so find the smaller dataset
minCol=min(length(actionsCols),length(chartsCols)) # = no of cols in smaller dataset
colWidths=pmax(actionsCols[1:minCol],chartsCols[1:minCol],na.rm=TRUE) # get the pairwise max letter count for these cols

# append the remaining letter counts from the larger dataset to these. The result is
# a vector of letter counts which is the maximum for each column across the two datasets
if(length(actionsCols)>length(chartsCols)){
  colWidths=c(colWidths,actionsCols[minCol+1:length(actionsCols)])
}else{
  colWidths=c(colWidths,chartsCols[minCol+1:length(chartsCols)])
}
colWidths=colWidths+1 # add a little to each col width
setColWidths(wb,"charts",col=1:length(colWidths), widths=colWidths) # set the col widths

# save workbook
saveWorkbook(wb, paste0(resultsDir,'StandardCharts.xlsx'),overwrite = TRUE)