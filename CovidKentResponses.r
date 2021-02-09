# ** Kent Business Restrictions Survey responses **
# *************************************************
#
# Just responses for Kent area in a single dataset.
# NB "sensitive" data removed. These are fields with contact details or with the name of any business specifically identified 
# [e.g. mentioned in a case study]
#
# Procedure:
# **********
# 1) Change the parameters below.
# 2) run the script.
# 3) Check the file produced in XL for disclosive/embarassing info like names of businesses/individuals which/who were sanctioned.
#    Make sure all is removed. The script will remove some but perhaps not all
# 4) Upload the file to SharePoint, overwriting the version which is there. "Publish" the file to the next major version and add any
#    version-specific comments.
# *** Optional:
rm(list=ls()) # remove anything in memory

# Change this to something appropriate:
setwd("C:/Users/fsymons/Downloads/Annexes") 
# NB if not creating a separate R project, you'll need to put all the scripts [like the one below and its precedents] into the working 
# directory

source("CovidLoadPublicData.R") # Main loading code which concatenates all public data together. This calls other scripts...

# Constants ====
# *********
resultsDir="C:/Users/fsymons/Downloads/Annexes/"

# Names of Kent respondents as they appear in the survey
kent=c(
  'Kent',
  'Ashford',
  'Canterbury',
  'Dartford & Sevenoaks',
  'Dover',
  'Folkestone and Hythe',
  'Gravesham',
  'Mid Kent',
  'Thanet',
  'Tonbridge and Malling'
  )

# [Title] font style for XL
st1 = createStyle(fontName = 'Arial',textDecoration = "Bold",fontColour = "black",fontSize = 14) # black, bold, larger

# Main processing inc. email address removal ====
# ******************************************
# Here we select just those columns present in the original public dataset and not added ones for [e.g.] geography lookups
# This list is taken from the cols DF [=from load script]; we do the column filtering as part of the loading process
# [geography lookups and aggregations added by function udf_agg_data in CovidLoadDataFunctions which we don't call in the above script]
kentData=filter(data, q2_ %in% kent)

# "clean" the data of any sensitive info like emails, tels
# [NB this method is "gross": will remove emails of form somebody@somewhere but not more free-form embedded sensitive data.
# Manually check output data carefully before publishing.]

# First separate data into numbers and text questions: We will search and replace sensitive data in text portion, then add back to numbers
# NB use !(is.character) here to capture date [or any other exotic non-string] fields too. This covers the unique key of LA name and date
# we use for matching the text back in.
kentNos=kentData %>% 
  select(., c(q2_, !(where(is.character))))

# we need to include the date here to make a unique id of LA + date
kentText=kentData %>% 
  select(., c(q24_,where(is.character))) %>% 
  pivot_longer(., -c(q2_,q24_),)

# extract email addresses:
# regex = any "@" bordered by seq of any characters except spaces
mails=regmatches(kentText$value,regexpr('[^ ]*@[^ ]*',kentText$value))

kentData=kentText %>% 
  # do the replacement [replace email address with '']
  mutate(value=gsub(mails,'',.$value)) %>% 
  # repivot
  pivot_wider(.,) %>% 
  # join back to nos  
  left_join(., kentNos, by=c('q2_'='q2_' , 'q24_'='q24_')) %>% 
  # reorder things back to how they were [see above]. DF "cols" comes from the loading process: see
  # udf_survey_fields
  select(., as.vector(cols$ID.in.dataset))

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

# Add sheet for the kent data and populate ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
sheet = addWorksheet(wb, "kent", gridLines = FALSE)

# Put in a title
title="Covid Business Restriction Survey responses from Kent respondents"
writeData(wb, sheet = "kent", x = title, colNames = FALSE)
# output current date/time
writeData(wb, sheet = "kent", x = paste0("As at: ",time), startCol=1, startRow=2,colNames = FALSE)
# write the main DF
writeData(wb, sheet = "kent", x = kentData, startCol=1, startRow=4,colNames = TRUE)
# embolden the title
addStyle(wb, 'kent', st1, rows = 1, cols = 1,stack = TRUE)
# save
saveWorkbook(wb, paste0(resultsDir,'Kent.xlsx'),overwrite = TRUE)

# *** Now carefully check the file for disclosive info before posting to SharePoint..!

