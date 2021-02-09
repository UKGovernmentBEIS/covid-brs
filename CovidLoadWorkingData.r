# *** Covid: Load working ["survey results"] data *** ====
#
# This code loads and concatenates named files listed in an input file. These will mostly be in the "survey results" folder,
# but you can use the code to load only some of the "br*.xlsx" ["external sharing" folder] files
# The result is a DF "data" with standardised "dataset IDs": see the README in the "External Sharing" folder.
# requires functions from CovidLoadDataFunctions.r
# See CovidLoadPublicData.r to additionally/separately load all "public" [=external sharing] files.
#
# Fernley Symons 8/1/2020

# *** libraries *** ====
# *************
# check which libraries of [openxlsx,tidyverse] are not loaded from the current loaded list:
# creates a char vector of missing libraries
missingLibs=setdiff(c("openxlsx","tidyverse"), (.packages()))
# now load them
lapply(missingLibs,function(x){eval(parse(text=paste0('library("',x,'")')))})

# *** constants *** ====
# *************
# change this...
# Base of the path to the working files you want to process. The actual files may be in subfolders off this
#
# This is the way to access a SharePoint directory. Note the direction of slashes
# NB can be problems with this: can get "File does not exist" messages. This might be due access issues, perhaps someone else
# has file open [seems to fail in this case]. Alternative approach is to map to onedrive. But this takes up space on your computer [AFAIK] as it duplicates 
# contents of folder, and also seems to fail if file is open in XL.
#stemDir="\\\\beisgov.sharepoint.com@SSL/DavWWWRoot/sites/beis2/169/Shared Documents/Analytical Team/COVID-19/Business Restrictions/Survey results/"
stemDir="C:/Users/fsymons/OneDrive - Department for Business Energy and Industrial Strategy/Business Restrictions/"

# The list of files you want to process tgr with any subfolder part of the path; change this but record [keep] this file list with the analysis
# as demonstration of which files you processed
filesList="C:/Users/fsymons/Downloads/Annexes/files.xlsx"

# *** main processing *** ====
# *******************
workingFiles=readWorkbook(filesList,colNames=FALSE) # FALSE assumes there's no column heading

# check if Covid functions have already been loaded. if not, load them [run script]
# and run some to create lookup tables etc
if(!(exists("udf_load_data") && is.function(udf_load_data))){
  source('CovidLoadDataFunctions.r')
}
  
filesDir=paste0(stemDir,"External Sharing/")

# get survey fields lookup
udf_survey_fields(filesDir, "README_BusinessRestrictionsSurvey.xlsx")

# change the base directory
filesDir=paste0(stemDir,'Survey results/')

# load these file[s]: result is "data" DF
names=apply(workingFiles,1,function(x){udf_load_data(x)})
