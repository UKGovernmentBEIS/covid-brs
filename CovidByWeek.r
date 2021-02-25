# *** Covid: Time series of key metrics by week [all England] *** ====
# ***********************************************************
#
# Amalgamate data in DF "data", aggregating to survey weeks across England.

# *** Optional:
rm(list=ls()) # remove anything in memory

# Constants ====
# *********
# Change this to something appropriate:
setwd("C:/Users/fsymons/Downloads/Annexes/covid")

group_cols='q24_' # this is the default anyway

# Load all "external sharing" public datasets to concatenated DF.
source('CovidLoadPublicData.r') # NB calls other scripts

# This function is in CovidLoadDataFunctions. If a file fails to load, the R console should "hang" until you dismiss it
udf_waitifnot(is.list(data),"problem loading one or more files: are they open?")

# *** Optional: run this with a separate input file [xlsx] which has a list of working files to add in:
# source('CovidLoadWorkingData.r')

# perform "aggregation" calculations ahead of grouping at week level.
# create the aggregations list of lists
# [put the lists of filters into global env. so we have them in case we want to use them for other purposes]
aggs=udf_aggs()

# run the aggregations, remove any entries without an end date. There should be none on the public datasets; there might be
# some on the working files. This function generates totals for multi-part qs etc.
pubData=udf_agg_data(data) %>% 
  drop_na("q24_") %>% 
  # Split the text field giving the things which would most help enforcement
  udf_split_txt(., 'q10_1.0')

# Summarise data by week. [q24_ is the end date variable]. This sums / counts up nos
pubDataByWeek=udf_agg_nos(pubData,group_cols)

# Perform the text aggregations, calculating % by week and entry type across text fields.
textResps=udf_agg_text(pubData,group_cols)



