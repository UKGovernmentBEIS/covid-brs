# *** Covid: Time series of key metrics by week [for Hampshire +  I of Wight respondents] *** ====
# ***************************************************************************************
#
# Amalgamate data in DF "data", filter for just Hampshire respondents and then create weekly summaries [totals].
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# * textResps [text]: this is unpivoted with weeks in their own col.

# *** Covid: Time series of key metrics by week [all England] *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# * textResps [text]: this is unpivoted with weeks in their own col.

# *** Optional:
rm(list=ls()) # remove anything in memory

# Libraries
library('plotly')

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# This isn't strictly necessary since it's the function default:
group_cols='q24_'

# aggregate data to week across regions [i.e. for England]
source('CovidByWeek.r')

# We want some cumulative sums for charts...
# select w/e, total checks/wk, total FTE/wk, total actions/wk
standard_data=select(pubDataByWeek, q24_, q8_1.0_sum,q9__.__sum,q12__.__sum) %>% 
  # calculate the cumulative sums for these
  mutate(across(c('q8_1.0_sum','q9__.__sum','q12__.__sum'), list(csum=cumsum))) %>% 
  rename(`cumulative checks`=q8_1.0_sum_csum,`cumulative actions`=q9__.__sum_csum,`cumulative fte`=q12__.__sum_csum)


# Let's use plotly
# Simple line chart of cumulative checks with time
checks_chrt= plot_ly(data = standard_data, x = ~q24_, y = ~`cumulative checks`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Covid compliance checks",
        xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
        yaxis = list(title = "Number of checks",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
checks_chrt

# Simple line chart of cumulative actions with time
actions_chrt= plot_ly(data = standard_data, x = ~q24_, y = ~`cumulative actions`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Covid enforcement actions",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "Number of actions",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
actions_chrt

# Simple line chart of cumulative FTE with time
fte_chrt= plot_ly(data = standard_data, x = ~q24_, y = ~`cumulative fte`, type="scatter", mode="lines+markers", line=list(width =4, color="#005674")) %>% 
  layout(title = "Cumulative Full-Time Equivalent (FTE) resource for Covid activities",
         xaxis = list(title = "Week ending",showticklabels = TRUE,showline=TRUE,showgrid=FALSE,rangemode='tozero', ticks='outside'),
         yaxis = list(title = "FTE",showticklabels = TRUE,showline=TRUE,tickformat= 's',showgrid=FALSE, rangemode='tozero',ticks='outside'), showlegend = FALSE)
fte_chrt