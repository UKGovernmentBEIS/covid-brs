# *** Covid: Load working ["survey results"] data *** ====
#
# This code loads and concatenates named files listed in an input file. These will mostly be in the "survey results" folder,
# but you can use the code to load only some of the "br*.xlsx" ["external sharing" folder] files
# The result is a DF "data" with standardised "dataset IDs": see the README in the "External Sharing" folder.
# requires functions from CovidLoadDataFunctions.r
# See CovidLoadPublicData.r to additionally/separately load all "public" [=external sharing] files.
#
# Fernley Symons 8/1/2020

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

# *** constants for just this script *** ====
# *************
# Others in CovidConstants.r
# The list of files you want to process tgr with any subfolder part of the path; change this but record [keep] this file list with the analysis
# as demonstration of which files you processed
# assume the file's an XL in the same directory as the scripts:
# NB files listed in this XL should have
# file / path from stemDir onwards. E.g. like this: "/March 2021 Output/Output_8th-14th_March_2021.xlsx"

filesList=paste0(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)),"/files.xlsx")
# or uncomment / change this:
#filesList="C:/Users/fsymons/Downloads/Annexes/files.xlsx"

# *** main processing *** ====
# *******************
source('CovidLibraries.r') # Load libraries if not already
source('CovidConstants.r') # set constants like output directory

workingFiles=readWorkbook(filesList,colNames=FALSE) # FALSE assumes there's no column heading

# check if Covid functions have already been loaded. if not, load them [run script]
# and run some to create lookup tables etc
if(!(exists("udf_load_data") && is.function(udf_load_data))){
  source('CovidLoadDataFunctions.r')
}
  
# get survey fields lookup [dataset IDs, field type]
udf_survey_fields(filesDir, readmeFile) # rest of path is in CovidConstants.r

# change the base directory
resultsFilesDir=paste0(stemDir,'Survey results/')

# load these file[s]: result is "data" DF [appends to "data" if already exists]
names=apply(workingFiles,1,function(x){udf_load_data(x,resultsFilesDir)})
