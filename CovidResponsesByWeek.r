# *** Covid: Matrix of respondents by survey weeks *** ====
# *********************************
#
# Across the data in DF "data", find the matrix of respondent by survey date with counts of nos of surveys responded to
# As part of the "data" DF creation [CovidLoadData.r], we join "data" DF to the complete list of [possible] "surveyRespondents". 
# In the result, entries in "data" col q23_/q24_ will be missing for those respondents who have never responded.
# NAs in the numeric cols of the dataframe get set to 0 so be careful in case this is important.

# *** Optional
rm(list=ls()) # remove anything in memory

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# Load all "external sharing" public datasets to concatenated DF.
source('CovidLoadPublicData.r') # NB calls other scripts

# *** Optional: run this with a separate input file [xlsx] which has a list of working files:
# source('CovidLoadWorkingData.r')

# *** constants *** ====
# *****************
# NB the direct link to the SP file doesn't always work: sometimes returns file doesn't exist errors
#resultsDir="\\\\beisgov.sharepoint.com@SSL/DavWWWRoot/sites/beis2/169/Shared Documents/Analytical Team/COVID-19/Business Restrictions/Survey results/"
# in that case, can use mapped OneDrive folder [but that has disadvantage that makes local copy of data AFAIK]:
resultsDir="C:/Users/fsymons/Downloads/Annexes/covid/"

# XL title style object
st1 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 14) # black, bold, larger

# *** Main code *** ====
# *****************
udf_geog_lookups() # load the geography lookups [from CovidLoadDataFunctions.r]

respsOverSurveys=udf_join_geog(data) %>% # join the geography on
  distinct(q2_,q23_) # NB there are some duplications of LA/week. Unclear what these are. This removes them

# Now create a crosstab "presence-absence" table of respondent by survey date.
# Inc. marginal totals [counts here] using "addmargins"
respsOverSurveys=addmargins(table(respsOverSurveys[c('q2_','q23_')]))
# Results in an object of type "table". Better to have this as a DF. First convert to matrix:--
# simply change the object attribute...
attributes(respsOverSurveys)$class = "matrix"
# convert matrix to DF.
respsOverSurveys=as.data.frame(respsOverSurveys) %>% 
  # It has row names, but we want explicit entries their own column  
  # [dplyr equivalent didn't work first time here]  
  mutate(q2_=rownames(.)) %>% 
  # reorder the cols so name of respondent col is first  
  select(q2_,everything()) %>% 
  # rename the last col [=total no of surveys responded to]  
  rename('All surveys'=Sum) %>% 
  # Rename the last row [=count of respondents / survey week]  
  mutate(q2_=ifelse(q2_=='Sum','All',q2_))

# Send this table to XL: ====
# *********************
# create XL
wb=createWorkbook()
# Add sheet
sheet = addWorksheet(wb, "Responses", gridLines = FALSE)
# Set the base font for the XLS
modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

# Put in a title
title="Summary of Covid Business Restrictions Survey responses"
writeData(wb, sheet = "Responses", x = title, colNames = FALSE)
# output current date/time
writeData(wb, sheet = "Responses", x = paste0("As at: ",Sys.time()), startCol=1, startRow=2,colNames = FALSE)

writeData(wb, sheet = "Responses", x = "Matrix of responses by weeks. 1 = response received, 0 = none. We generate this from the 'public' files in the external sharing folder", startCol=1, startRow=4,colNames = TRUE)
# write the main DF
writeData(wb, sheet = "Responses", x = respsOverSurveys, startCol=1, startRow=6,colNames = TRUE)
# embolden the title
addStyle(wb, 'Responses', st1, rows = 1, cols = 1,stack = TRUE)
# Save
saveWorkbook(wb, paste0(resultsDir,'SurveyResponses.xlsx'),overwrite = TRUE)

