# *** Covid: Load public ["External Sharing"] data *** ====
#
# This code loads all and concatenates external sharing folder datasets [files = "br*.xlsx"]
# listed in the external sharing README file. The result is a DF "data" with
# standardised "dataset IDs": see the README in the "External Sharing" folder.
# requires functions from CovidLoadDataFunctions.r
# See CovidLoadWorkingData.r to additionally/separately load working file data
#
# Fernley Symons 8/1/2020

# *** libraries *** ====
# *************
# check which libraries of [openxlsx,tidyverse] are not loaded from the current loaded list:
# creates a char vector of missing libraries
missingLibs=setdiff(c("openxlsx","tidyverse"), (.packages()))
# now load them
lapply(missingLibs,function(x){eval(parse(text=paste0('library("',x,'")')))})

# ***constants***
# change this...
# Base of the path to the working files you want to process. The actual files may be in subfolders off this
#
# This is the way to access a SharePoint directory. Note the direction of slashes
# NB can be problems with this: can get "File does not exist" messages. This might be due access issues, perhaps someone else
# has file open [seems to fail in this case]. Alternative approach is to map to onedrive. But this takes up space on your computer [AFAIK] as it duplicates 
# contents of folder, and also seems to fail if file is open in XL.
#stemDir="\\\\beisgov.sharepoint.com@SSL/DavWWWRoot/sites/beis2/169/Shared Documents/Analytical Team/COVID-19/Business Restrictions/"
stemDir="C:/Users/fsymons/OneDrive - Department for Business Energy and Industrial Strategy/Business Restrictions/"
filesDir=paste0(stemDir,"External Sharing/")
#list.files(filesDir)

# *** main processing *** ====
# *******************
# check if Covid functions have already been loaded. if not, load them [run script]
# and run some to create lookup tables etc
if(!(exists("udf_load_data") && is.function(udf_load_data))){
  # load loading functions
  source('CovidLoadDataFunctions.r')
}

# get survey fields lookup
udf_survey_fields(filesDir, "README_BusinessRestrictionsSurvey.xlsx")
# get the list of files to process [from the README]
udf_public_data(filesDir, "README_BusinessRestrictionsSurvey.xlsx")

# ***Loading/processing data**noCols=colnames(data)[(colnames(data) %in% noCols$"ID.in.dataset")]* ----
# apply the udf load function to each XL in the list [surveyFiles DF]. Wrap the call in an anonymous function
# so we can pass the slice [row] of the surveyFiles DF we're processing [there may be a neater way
# of doing this]. Result is concatenated DF of all data in DF "data". The function returns a list which has
# [for each file loaded]: filename, no. of q24_=NAs removed, vector of field names [dataset IDs]
publicDataFiles=apply(surveyFiles,1,function(x){udf_load_data(x)})


