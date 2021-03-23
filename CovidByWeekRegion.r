# *** Covid: Time series of key metrics by week by region *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with grouping variables [week + region] down the side.
# * textResps [text]: this is unpivoted with grouping variables [week + region] in their own cols.

# *** Optional:
rm(list=ls()) # remove anything in memory

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# Group by region [q1] and week [q24]:
group_cols=c('q1_','q24_')

# Load all "external sharing" public datasets to concatenated DF.
source('CovidByWeek.r') # NB calls other scripts

# ***Optional save as csvs ***
# write.csv(pubDataByWeek,"C:/Users/fsymons/Downloads/Annexes/RegionData_Nos.csv")

# write.csv(textResps,"C:/Users/fsymons/Downloads/Annexes/REgionData_Txt.csv")