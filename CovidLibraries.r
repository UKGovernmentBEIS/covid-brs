# *** libraries *** ====
# *************
# check which libraries of [openxlsx,tidyverse] are not loaded from the current loaded list:
# creates a char vector of missing libraries
missingLibs=setdiff(c("openxlsx","tidyverse","formattable","english"), (.packages()))
# now load them
lapply(missingLibs,function(x){eval(parse(text=paste0('library("',x,'")')))})