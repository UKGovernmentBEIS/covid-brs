# *** Covid: Load public ["External Sharing"] data *** ====
#
# This code loads all and concatenates external sharing folder datasets [files = "br*.xlsx"]
# listed in the external sharing README file. The result is a DF "data" with
# standardised "dataset IDs": see the README in the "External Sharing" folder.
# requires functions from CovidLoadDataFunctions.r
# See CovidLoadWorkingData.r to additionally/separately load working file data
#
# Fernley Symons 8/1/2020

# ***Working directory*** ====
# *************
# Assumes we're using R Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use the following if not:
# setwd(getSrcDirectory()[1])

source('CovidLibraries.r') # Load libraries if not already
source('CovidConstants.r') # set constants like output directory

# *** main processing *** ====
# *******************
# check if Covid functions have already been loaded. if not, load them [run script]
# and run some to create lookup tables etc
if(!(exists("udf_load_data") && is.function(udf_load_data))){
  # load loading functions
  source('CovidLoadDataFunctions.r')
}

udf_survey_fields(filesDir, readmeFile) # get survey fields lookup
udf_public_data(filesDir, readmeFile) # get the list of files to process [from the README]

# ***Loading/processing data**noCols=colnames(data)[(colnames(data) %in% noCols$"ID.in.dataset")]* ----
# apply the udf load function to each XL in the list [surveyFiles DF]. Wrap the call in an anonymous function
# so we can pass the slice [row] of the surveyFiles DF we're processing [there may be a neater way
# of doing this]. Result is concatenated DF of all data in DF "data". The function returns a list which has
# [for each file loaded]: filename, no. of q24_=NAs removed, vector of field names [dataset IDs]
publicDataFiles=apply(surveyFiles,1,function(x){udf_load_data(x)})


