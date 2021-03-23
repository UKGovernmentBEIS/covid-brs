# *** Covid: Constants *** ====
#
# ***constants***
# change this...
# Base of the path to the working files you want to process. The actual files may be in subfolders off this
#
# This is the way to access a SharePoint directory. Note the direction of slashes
# NB can be problems with this: can get "File does not exist" messages. This might be due access issues, perhaps someone else
# has file open [seems to fail in this case]. Alternative approach is to map to onedrive. But this takes up space on your computer [AFAIK] as it duplicates 
# contents of folder, and also seems to fail if file is open in XL. Seems a little more stable.

#stemDir="\\\\beisgov.sharepoint.com@SSL/DavWWWRoot/sites/beis2/169/Shared Documents/Analytical Team/COVID-19/Business Restrictions/"
stemDir="C:/Users/fsymons/OneDrive - Department for Business Energy and Industrial Strategy/Business Restrictions/"
# 
filesDir=paste0(stemDir,"External Sharing/")
#list.files(filesDir)

readmeFile="README_BusinessRestrictionsSurvey.xlsx"