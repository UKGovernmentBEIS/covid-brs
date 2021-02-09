# *** Covid: Time series of key metrics by week [all England] *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# * textResps [text]: this is unpivoted with weeks in their own col.

# *** Optional:
rm(list=ls()) # remove anything in memory

# *** Change this to something appropriate:
setwd("C:/Users/fsymons/Downloads/Annexes")

# This isn't strictly necessary since it's the function default:
group_cols='q24_'

# Load all "external sharing" public datasets to concatenated DF.
source('CovidByWeek.r') # NB calls other scripts

# ***Optional save as csvs ***
# write.csv(pubDataByWeek,"C:/Users/fsymons/Downloads/Annexes/AllEnglandData_Nos.csv")

# write.csv(textResps,"C:/Users/fsymons/Downloads/Annexes/AllEnglandData_Txt.csv")

