# *** Covid: Time series of key metrics by week [for Hampshire +  I of Wight respondents] *** ====
# ***************************************************************************************
#
# Amalgamate data in DF "data", filter for just Hampshire respondents and then create weekly summaries [totals].
# Creates 2 DFs in the global environment: 
# * pubDataByWeek [numbers]: this is pivoted with week down the side.
# * textResps [text]: this is unpivoted with weeks in their own col.

# *** Optional:
rm(list=ls()) # remove anything in memory

# Constants ====
# *********
# Change this to something appropriate:
setwd("C:/Users/fsymons/Downloads/Annexes/covid")
# NB if not creating a separate R project, you'll need to put all the scripts [like the one below and its precedents] into the working 
# directory
resultsDir="C:/Users/fsymons/Downloads/Annexes/"

# Names of Hampshire + IoWight respondents as they appear in the survey
hamps=c(
  'Basingstoke and Deane',
  'East Hampshire',
  'Eastleigh',
  'Fareham & Gosport',
  'Hampshire',
  'Hart',
  'Havant',
  'Isle of Wight',
  'New Forest',
  'Portsmouth',
  'Rushmoor',
  'Southampton',
  'Test Valley',
  'Winchester'
)
# [Title] font style for XL
st1 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 14) # black, bold, larger

source("CovidLoadPublicData.R") # Main loading code which concatenates all public data together. This calls other scripts...

# This function is in CovidLoadDataFunctions. If a file fails to load, the R console should "hang" until you dismiss it
udf_waitifnot(is.list(data),"problem loading one or more files: are they open?")

# *** Optional: run this with a separate input file [xlsx] which has a list of working files to add in:
# source('CovidLoadWorkingData.r')

hampsData=filter(data, q2_ %in% hamps)

# perform "aggregation" calculations ahead of grouping at week level.
# create the aggregations list of lists
# [put the lists of filters into global env. so we have them in case we want to use them for other purposes]
aggs=udf_aggs()

# run the aggregations, remove any entries without an end date. There should be none on the public datasets; there might be
# some on the working files. This function generates totals for multi-part qs etc.
# first split the business types causing trouble [ranks 1-3] into 3 separate cols for each parent col
pubData=udf_split_ranks(hampsData) %>% 
  # run the general aggregation script, to get sums of breach types or sums of actions [letters etc]
  udf_agg_data(.) %>% 
  # make sure there are no blank ending weeks
  drop_na("q24_") %>% 
  # Split the text field giving the things which would most help enforcement into separate fields
  udf_split_txt(., 'q10_1.0')

# Specify the columns to aggregate by. This isn't strictly necessary [q24_] since it's the function default:
group_cols='q24_'

# Summarise data by week. [q24_ is the end date variable]. This sums / counts up nos
pubDataByWeek=udf_agg_nos(pubData,group_cols)

# aggregate the text question responses. This calculates %s giving each answer type.
textResps=udf_agg_text(pubData,group_cols)

# Output to XL ====
# ************
# 
time=Sys.time()
# Create Workbook and get intro sheet from README ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#
# Take the "Intro" sheet from the README file in the "external sharing directory" in its entirety.
# Do this to ensure the information is always up to date. Load rather than read the file to preserve formatting
wb=loadWorkbook(paste0(filesDir,"README_BusinessRestrictionsSurvey.xlsx"))
# delete all except the "Intro" sheet [if there are any others: have to do like this: have to delete sheets one at a time]
lapply(names(wb)[which(!(names(wb) %in% c('Intro')))],function(i){removeWorksheet(wb,i)})

# Set the base font for the XLS
modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

# Add sheet for the numeric data and populate ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

sheet = addWorksheet(wb, "numeric", gridLines = FALSE)

# Put in a title
title="Covid Business Restriction Survey: aggregate responses from Hampshire respondents"

# Put in an explanation of the data
msg=data.frame(msg=c('This sheet gives data for numerical questions in the survey.',
                     'Dataset IDs in this dataset are as overleaf (sheet "intro"). However, because this file contains weekly summaries the columns differ slightly: suffixes _cnt = count of entries, _sum = sum of entries for the week across respondents.',
                     '"Placeholders", such as in q12__.__a_sum, mean the sum/count across categories indicated by the blanks. So here, the column gives the sum of "verbal advice" action taken across breach types.',
                     'Variables ending "_comp" are "composite" variables for q12_1.0_a etc. They include a version of the variable which is broadly consistent with the earlier entries [see "intro" sheet].',
                     'We have broken out dataset ID q25, business types causing most concern, into 3 separate columns for each business type. These hold counts of respondents where respondents ranked that business type ',
                     'most troublesome [q25_1.1 etc], less [q25_1.2 etc] or least troubling [q25_1.3 etc] when ranking the top 3 most troublesome business types. The parent variable [q25_1.0 etc] contains counts for respondents ranking that business type as any rank [1-3] incl.' 
                     ))

writeData(wb, sheet = "numeric", x = title, colNames = FALSE)
# output current date/time
writeData(wb, sheet = "numeric", x = paste0("As at: ",time), startCol=1, startRow=2,colNames = FALSE)
# write the numeric sheet notes
writeData(wb, sheet = "numeric", x = msg, startCol=1, startRow=4,colNames = FALSE)
# write the main DF
writeData(wb, sheet = "numeric", x = pubDataByWeek, startCol=1, startRow=4+nrow(msg)+2,colNames = TRUE)
# embolden the title
addStyle(wb, 'numeric', st1, rows = 1, cols = 1,stack = TRUE)

# Add sheet for the text data and populate ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
sheet = addWorksheet(wb, "txt", gridLines = FALSE)

# Put in an explanation of the data
txtmsg=data.frame(msg=c('On this sheet, we give results for text-based questions: dataset IDs q7 [top 3 complaints], q10 [greatest impact on improving enforcement] & q11 [top 3 compliance concerns].',
                        'To calculate percentages etc for q10 we split the field from the original survey and count up all the responses [each respondent may make up to 3, separated by commas].',
                        "We only keep the distinct responses by each respondent. So we only keep \"don't know\" once even if a respondent entered it multiple times.",'Fields cnt=count of respondents giving that answer, tot=total respondents, pct=percentage this represents'))
# repeat the title
writeData(wb, sheet = "txt", x = title, colNames = FALSE)
# output current date/time
writeData(wb, sheet = "txt", x = paste0("As at: ",time), startCol=1, startRow=2,colNames = FALSE)
# write the numeric sheet notes
writeData(wb, sheet = "txt", x = txtmsg, startCol=1, startRow=4,colNames = FALSE)
# write the main DF
writeData(wb, sheet = "txt", x = textResps, startCol=1, startRow=4+nrow(txtmsg)+2,colNames = TRUE)
# embolden the title
addStyle(wb, 'txt', st1, rows = 1, cols = 1,stack = TRUE)

# save
saveWorkbook(wb, paste0(resultsDir,'Hampshire.xlsx'),overwrite = TRUE)

