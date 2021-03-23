# *** Covid: Matrix of respondents reporting no FTEs by survey weeks *** ====
# ******************************************************************
#
# Across the data in DF "data", find the matrix of respondents reporting no FTEs by survey date with counts of nos of surveys where they recorded none

# *** Optional
rm(list=ls()) # remove anything in memory [optional]

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# Load all "external sharing" public datasets to concatenated DF.
source('CovidLoadPublicData.r') # NB calls other scripts

# Optional: run this with a separate input file [xlsx] which has a list of working files [edit the script]:
# source('CovidLoadWorkingData.r')

# *** constants *** ====
# *****************
# NB the direct link to the SP file doesn't always work: sometimes returns file doesn't exist errors
#resultsDir="\\\\beisgov.sharepoint.com@SSL/DavWWWRoot/sites/beis2/169/Shared Documents/Analytical Team/COVID-19/Business Restrictions/Survey results/"
# in that case, can use mapped OneDrive folder [but that has disadvantage that makes local copy of data AFAIK]:
resultsDir="C:/Users/fsymons/OneDrive - Department for Business Energy and Industrial Strategy/Business Restrictions/Survey results/"

# XL title style object
st1 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 14) # black, bold, larger

# *** Main code *** ====
# *****************
# perform "aggregation" calculations ahead of grouping at week level.
# create the aggregations list of lists
# [put the lists of filters into global env. so we have them in case we want to use them for other purposes]
aggs=udf_aggs()
# add in some summary counts for FTEs etc
agg_data=data %>% udf_agg_data(.) 

# get just the respondent name and the end date for those respondents recording no FTEs
no_fte=filter(agg_data, q9__._==0) %>% 
  select(., q2_,q24_)

# Now create a crosstab "presence-absence" table of respondent by survey date.
# Inc. marginal totals [counts here] using "addmargins"
addmargins(table(no_fte[c('q2_','q24_')]))

no_fte=addmargins(table(no_fte[c('q2_','q24_')]))

# Results in an object of type "table". Better to have this as a DF. First convert to matrix:--
# simply change the object attribute...
attributes(no_fte)$class = "matrix"
# convert matrix to DF.
no_fte=as.data.frame(no_fte) %>% 
  # It has row names, but we want explicit entries their own column  
  # [dplyr equivalent didn't work first time here]  
  mutate(q2_=rownames(.)) %>% 
  # reorder the cols so name of respondent col is first  
  select(q2_,everything()) %>% 
  # rename the last col [=total no of surveys responded to]  
  rename('All surveys'=Sum) %>% 
  # Rename the last row [=count of respondents / survey week]  
  mutate(q2_=ifelse(q2_=='Sum','All',q2_)) %>% 
  # We're going to replace the 0/1s in the main table with text. To do this,
  # unpivot, cast the numbers to char, then do the replacement and repivot
  pivot_longer(.,-q2_) %>%
  # cast to char
  mutate(value=as.character(value)) %>% 
  # keep the row/col totals as [text] nos, change all the other entries
  mutate(value=case_when(
    (q2_=='All' | name=='All surveys')~value,
    value=='0'~'-',
    value=='1'~"no FTEs"
  )) %>% 
  pivot_wider(.,)

#View(no_fte)

# Send this table to XL: ====
# *********************
# create XL
wb=createWorkbook()
# Add sheet
sheet = addWorksheet(wb, "noFTEs", gridLines = FALSE)
# Set the base font for the XLS
modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

# Put in a title
title="Covid Business Restrictions Survey: respondents reporting no FTEs by week"
writeData(wb, sheet = "noFTEs", x = title, colNames = FALSE)
# output current date/time
writeData(wb, sheet = "noFTEs", x = paste0("As at: ",Sys.time()), startCol=1, startRow=2,colNames = FALSE)

writeData(wb, sheet = "noFTEs", x = 'Matrix of responses by weeks. Numbers are counts of "no FTE" responses. We generate this from the "public" files in the external sharing folder', startCol=1, startRow=4,colNames = TRUE)
# write the main DF
writeData(wb, sheet = "noFTEs", x = no_fte, startCol=1, startRow=6,colNames = TRUE)
# embolden the title
addStyle(wb, 'noFTEs', st1, rows = 1, cols = 1,stack = TRUE)
# Save
saveWorkbook(wb, paste0(resultsDir,'noFTEs.xlsx'),overwrite = TRUE)

