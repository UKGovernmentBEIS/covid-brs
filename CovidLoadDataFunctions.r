# *** Covid Load Data Functions *** ====
# *********************************
# Initial code to analyse results of Covid Business Restrictions Survey [XL] files.
# Includes steps for loading and concatenating the datasets. The references to column headings are the 
# standardised "dataset IDs": see the README in the "External Sharing" folder.
# this includes cross-survey weeks matrix of responses by respondents with output to XL.
# At foot are some quick replications of the figs in the XL CO template return. [Vals quoted 
# there are for br20201213.xlsx]
# Fernley Symons 8/1/2020

# *** Function definitions ***
# **************************

# *** error handler *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^
# Aims to stop further execution if R can't read XL. In interactive mode [default for r studio], then may continue after
# reaching a read error. Here, we aim to make R wait [=stop further execution] in this case.
# NB when running from R studio, this not work as expected. If invoked as part of a script, only that script stops and then
# next statement executed. By editing as shown, execution will stop either way. [have retained original code in case useful]
udf_waitifnot = function(cond,msg) {
  if (!cond) {
#    if (interactive()) {
      message(msg)
      while (TRUE) {}
#    } else {
#      stop(msg)
#    }
  }
}
# *** Get survey fields *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^
# Get the fields in the survey and their types [first col has "dataset ID", last has type=char/no.]
# NB The reading process only includes XL populated lines. Blank lines separating content in the XL are ignored
udf_survey_fields=function(filesDir,filename){
  surveyFields=readWorkbook(paste0(filesDir,filename),namedRegion='fieldDef')
  udf_waitifnot(is.list(surveyFields),paste0("Can't read: ",filesDir,filename,": is it open?"))
  assign('surveyFields',surveyFields,envir=.GlobalEnv)
  #View(surveyFields)

  # separate these 2 cols into their own DF. Remove any rows which are NA [should be none...]
  # This gives us a DF with potentially present cols which are of type number. We don't know which of these _are_ actually present
  # in this particular dataset, since we might only be processing the survey results for one/a few week[s] [see later]
  cols=surveyFields[c("ID.in.dataset","Response.type.[number/text]")]
  assign('cols',cols,envir=.GlobalEnv)
  # View(noCols)

  # now get any fields which are numeric and put into their own DF
  noCols=cols %>% filter(., `Response.type.[number/text]`=='n') %>% 
    na.omit() %>% 
    select(ID.in.dataset)
  # convert to vector
  noCols=noCols[['ID.in.dataset']]
  
  assign('noCols',noCols,envir=.GlobalEnv)
}

# *** Geography lookups *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^
# This table [named range] has the full look up from constituent LADs to respondents to non-overlapping geography. 
# I.e. lut goes from >311 => 311 => 143 areas
# lut has GOR names in it [be aware that 2 survey respondents overlap GORs: assigned to one GOR only here.]
udf_geog_lookups=function(){
  surveyLADs=readWorkbook(paste0(filesDir,"CovidSurveyGeography.xlsx"),namedRegion='lad_to_area')
  udf_waitifnot(is.list(surveyLADs),paste0("Can't read: ",filesDir,"CovidSurveyGeography.xlsx",": is it open?"))
  assign('surveyLADs',surveyLADs,envir=.GlobalEnv)
  # create table at respondent level [=311; each respondent name appears once]. Add resp. ID and non-overlapping area ID [=143 areas],
  # GOR [9 areas]. Do this by getting unique combinations of required fields.
  surveyRespondents=unique(surveyLADs[c('Name.of.reporting.entity','ID.on.map','newID:.non-overlapping.area.ID','GOR')])
  assign('surveyRespondents',surveyRespondents,envir=.GlobalEnv)
}

# *** List of public data files *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# This XL named range contains the list of data files. We can use it to extract names of files to process 
# which are in the external sharing directory [see constant at head]
# [NB we don't treat first line as field header, the default behaviour:]
udf_public_data=function(filesDir,filename){
  surveyFiles=readWorkbook(paste0(filesDir,filename),namedRegion='survey_files',colNames=FALSE)
  udf_waitifnot(is.list(surveyFiles),paste0("Can't read: ",filesDir,filename,": is it open?"))

  # For transparency, change the default column/field name to something sensible:
  names(surveyFiles)="filename"
  assign('surveyFiles',surveyFiles,envir=.GlobalEnv)
}

# *** Load and concatenate data *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Function to load an XL workbook  
# NB uses filesDir from outside function call; takes argument "filename" [string] from call to function
# We perform some conversion operations like converting to dates, numbers here instead of in a single
# global operation later to have a general purpose function
# returns a list with, for ea. file processed: filename, no. of q24_=NA removed, vector of fields
# It separately assigns DF "data" [concatenated DF] to the global enviro.
udf_load_data = function(filename){

  # read the whole workbook, sheet "data" if it's one of the external sharing datasets
  if(grepl('^(BR)|(br)', filename)==TRUE){
    inData=readWorkbook(paste0(filesDir,filename), sheet='data')
    udf_waitifnot(is.list(inData),paste0("Can't read: ",filesDir,filename,": is it open?"))

    # remove the second row of XL [first row of DF] as this has the text which
    # goes with each question
      inData=inData[-1,]
  }else{
    # assume it's a working file, sheet "Input data"
    inData=readWorkbook(paste0(filesDir,filename), sheet='Input data')
    udf_waitifnot(is.list(inData),paste0("Can't read: ",filesDir,filename,": is it open?"))
    
    # assume that the first 2 rows below the headings are qualtrics ID and question text respectively.
    # Note that loading things this way will include lots of extraneous columns called "col<some no>".
    # we remove those later
    inData=inData[-c(1:2),]
  }
  
  # Get the count of any rows which have a missing end date. We return this as part of the list
  naCount=length(which(is.na(inData$q24_)))
  
  inData=inData %>% 
    # The survey start/end dates are XL date numbers as char [like "44158"]. let's convert them to R dates. 
    # Do this here to ensure fields are of same type when combining different weeks' data
    mutate(q23_=as.Date(as.numeric(q23_), origin = "1899-12-30"),
           q24_=as.Date(as.numeric(q24_), origin = "1899-12-30")) %>% 
    # set number cols to type numeric; as above, do this here to ensure entries in col are all of the same
    # type when we concatenate them together. Intersect required because any given "inData" may not have
    # all the possible numeric columns listed in noCols.
    mutate(across(intersect(names(.), noCols), as.numeric)) %>% 
    # remove any extraneous cols with names like "col1"
    select(intersect(names(.), as.vector(cols$ID.in.dataset))) %>% 
    # remove any entries with missing end dates
    drop_na("q24_")
  
  # check if the DF data exists. If it does we'll add rows to it, if not create it  
  if(exists("data") && is.data.frame(data)){
  # bind the result to the existing contents of data if already exists
  # bind_rows here doesn't require the fields to be the same in the things we're concatenating [v useful!]
  # if col is missing in data it adds it and fills previous rows with NA. Likewise if not present in inData it
  # add NA-filled col.
    data=bind_rows(data,inData)
  }else{
  # assign if not
    data=inData
  }
  # update the version of the object in the Global environment [i.e. outside this function]  
  assign('data',data,envir=.GlobalEnv)
  # This returns a vector of the column names, a count of any rows with missing end dates and the filename
  # When called with apply [below]; you get a list with one entry per original row [file]
  return(list("file"=filename,"nas"=naCount,"variables"=names(data)))
}

# *** Join on the geography lookups function *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
udf_join_geog = function(data){
  # join on key geography lookups
  data=data %>% 
    right_join(.,surveyRespondents, by=c("q2_"="Name.of.reporting.entity"))
  return(data)
}

# *** Fields to aggregate constants [function] *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
udf_aggs = function(){
  # Define constants. These lists are the items to add together to get totals by breach or by action etc
  # Put each set into a list, and combine to a list of lists. This is safer than using wildcards/regex 
  # where you might pick up unintended items. Easier to amend too.

  aggs=list(   
  
    # a) resource counts
    fte=list(c('q9_1.0','q9_2.0','q9_3.0','q9_4.0','q9_5.0','q9_6.0','q9_7.0','q9_9.0')),
  
    # b) different types of action
    verbal=list(c('q12_1.0_a','q12_1.1_a','q12_1.2_a','q12_2.0_a','q12_3.0_a','q12_4.0_a',
             'q12_5.0_a','q12_6.0_a','q12_7.0_a','q12_8.0_a','q12_9.0_a','q12_10.0_a')),
    letter=list(c('q12_1.0_b','q12_1.1_b','q12_1.2_b','q12_2.0_b','q12_3.0_b','q12_4.0_b',
             'q12_5.0_b','q12_6.0_b','q12_7.0_b','q12_8.0_b','q12_9.0_b','q12_10.0_b')),
    direction=list(c('q12_1.0_c','q12_1.1_c','q12_1.2_c','q12_2.0_c','q12_3.0_c','q12_4.0_c',
                'q12_5.0_c','q12_6.0_c','q12_7.0_c','q12_8.0_c','q12_9.0_c','q12_10.0_c')),
    fixed=list(c('q12_1.0_d','q12_1.1_d','q12_1.2_d','q12_2.0_d','q12_3.0_d','q12_4.0_d',
            'q12_5.0_d','q12_6.0_d','q12_7.0_d','q12_8.0_d','q12_9.0_d','q12_10.0_d')),
    prohibition=list(c('q12_1.0_e','q12_1.1_e','q12_1.2_e','q12_2.0_e','q12_3.0_e','q12_4.0_e',
                  'q12_5.0_e','q12_6.0_e','q12_7.0_e','q12_8.0_e','q12_9.0_e','q12_10.0_e')),
    prosecution=list(c('q12_1.0_f','q12_1.1_f','q12_1.2_f','q12_2.0_f','q12_3.0_f','q12_4.0_f',
                  'q12_5.0_f','q12_6.0_f','q12_7.0_f','q12_8.0_f','q12_9.0_f','q12_10.0_f')),
    hsawa=list(c('q12_1.0_g','q12_1.1_g','q12_1.2_g','q12_2.0_g','q12_3.0_g','q12_4.0_g','q12_5.0_g',
            'q12_6.0_g','q12_7.0_g','q12_8.0_g','q12_9.0_g','q12_10.0_g')),
    coron_imp=list(c('q12_1.1_h','q12_1.2_h','q12_2.0_h','q12_4.0_h','q12_5.0_h','q12_6.0_h',
                'q12_7.0_h','q12_8.0_h','q12_9.0_h','q12_10.0_h')),
    coron_imm=list(c('q12_1.1_i','q12_1.2_i','q12_2.0_i','q12_4.0_i','q12_5.0_i','q12_6.0_i',
                'q12_7.0_i','q12_8.0_i','q12_9.0_i','q12_10.0_i')),
    coron_res=list(c('q12_1.1_j','q12_1.2_j','q12_2.0_j','q12_4.0_j','q12_5.0_j','q12_6.0_j',
                'q12_7.0_j','q12_8.0_j','q12_9.0_j','q12_10.0_j')),
  
    # c) breach types
    # i) Actual survey types
    closed_law=list(c('q12_1.0_a','q12_1.0_b','q12_1.0_c','q12_1.0_d','q12_1.0_e','q12_1.0_f','q12_1.0_g')),
    closed_all=list(c('q12_1.1_a','q12_1.1_b','q12_1.1_c','q12_1.1_d','q12_1.1_e','q12_1.1_f',
                 'q12_1.1_g','q12_1.1_h','q12_1.1_i','q12_1.1_j')),
    closed_tier=list(c('q12_1.2_a','q12_1.2_b','q12_1.2_c','q12_1.2_d','q12_1.2_e','q12_1.2_f','q12_1.2_g',
                  'q12_1.2_h','q12_1.2_i','q12_1.2_j')),
    home=list(c('q12_2.0_a','q12_2.0_b','q12_2.0_c','q12_2.0_d','q12_2.0_e','q12_2.0_f','q12_2.0_g','q12_2.0_h',
           'q12_2.0_i','q12_2.0_j')),
    la_order=list(c('q12_3.0_a','q12_3.0_b','q12_3.0_c','q12_3.0_d','q12_3.0_e','q12_3.0_f','q12_3.0_g')),
    test_and_trace=list(c('q12_4.0_a','q12_4.0_b','q12_4.0_c','q12_4.0_d','q12_4.0_e','q12_4.0_f','q12_4.0_g',
                     'q12_4.0_h','q12_4.0_i','q12_4.0_j')),
    no_protect=list(c('q12_5.0_a','q12_5.0_b','q12_5.0_c','q12_5.0_d','q12_5.0_e','q12_5.0_f','q12_5.0_g',
                 'q12_5.0_h','q12_5.0_i','q12_5.0_j')),
    opening_hrs=list(c('q12_8.0_a','q12_8.0_b','q12_8.0_c','q12_8.0_d','q12_8.0_e','q12_8.0_f','q12_8.0_g',
                  'q12_8.0_h','q12_8.0_i','q12_8.0_j')),
    table=list(c('q12_9.0_a','q12_9.0_b','q12_9.0_c','q12_9.0_d','q12_9.0_e','q12_9.0_f','q12_9.0_g',
            'q12_9.0_h','q12_9.0_i','q12_9.0_j')),
    six=list(c('q12_10.0_a','q12_10.0_b','q12_10.0_c','q12_10.0_d','q12_10.0_e','q12_10.0_f','q12_10.0_g',
          'q12_10.0_h','q12_10.0_i','q12_10.0_j')),
    other=list(c('q12_6.0_a','q12_6.0_b','q12_6.0_c','q12_6.0_d','q12_6.0_e','q12_6.0_f','q12_6.0_g',
            'q12_6.0_h','q12_6.0_i','q12_6.0_j')),
    other2=list(c('q12_7.0_a','q12_7.0_b','q12_7.0_c','q12_7.0_d','q12_7.0_e','q12_7.0_f','q12_7.0_g',
             'q12_7.0_h','q12_7.0_i','q12_7.0_j')),
    
    # ii) consistent closed by law series
    consistent_closed=list(c('q12_1.0_a_comp','q12_1.0_b_comp','q12_1.0_c_comp','q12_1.0_d_comp',
                        'q12_1.0_e_comp','q12_1.0_f_comp','q12_1.0_g_comp','q12_1.0_h_comp','q12_1.0_i_comp','q12_1.0_j_comp')),
    
    # d) text variables [things we want counts/%s for]
    text_vars=list(c('q7_1.0','q7_2.0','q7_3.0','q10_1.0_a','q10_1.0_b','q10_1.0_c','q11_1.0','q11_2.0','q11_3.0')),
    
    # e) names of new "impact" text variables [split from one field in original data]
    impact_vars=list(c('q10_1.0_a','q10_1.0_b','q10_1.0_c'))
  )
  
  return(aggs)
}

# *** Totals function for numbers *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# This function performs optional aggregation-type manipulations on an input DF
# Designed to be called after individ. weeks' datasets assembled into concatenated DF. Won't be needed for all scripts
udf_agg_data = function(data){

  data=data %>%
    # create col which is sum of all FTE of any type [q9_1-q9_9]
    # We use this to get counts later
    mutate(q9__._=rowSums(.[unlist(aggs$fte)], na.rm=TRUE),
  
           # NB need rowSums approach because there will be NAs.
           # a) By sanction type [aggs$across breach types]
           # verbal advice
           q12__.__a=rowSums(.[unlist(aggs$verbal)],na.rm=TRUE),
           # letter/email
           q12__.__b=rowSums(.[unlist(aggs$letter)],na.rm=TRUE),
           # direction notice
           q12__.__c=rowSums(.[unlist(aggs$direction)],na.rm=TRUE),
           # fixed penalty
           q12__.__d=rowSums(.[unlist(aggs$fixed)],na.rm=TRUE),
           # prohibition notice
           q12__.__e=rowSums(.[unlist(aggs$prohibition)],na.rm=TRUE),
           # prosecutions
           q12__.__f=rowSums(.[unlist(aggs$prosecution)],na.rm=TRUE),
           # hsawa improvement notice
           q12__.__g=rowSums(.[unlist(aggs$hsawa)],na.rm=TRUE),
           # coronavirus improvement notice
           q12__.__h=rowSums(.[unlist(aggs$coron_imp)],na.rm=TRUE),
           # coronavirus immediate restriction
           q12__.__i=rowSums(.[unlist(aggs$coron_imm)],na.rm=TRUE),
           # coronavirus restriction
           q12__.__j=rowSums(.[unlist(aggs$coron_res)],na.rm=TRUE),
  
           # The following are for items which were split into several categories so
           # the o/a totals aren't on the later datasets and we have to create them.
           # [aggs$Totals created this way may not be 100% compatible with the definitions used in earlier
           # surveys but they're close]. Do this transformation in this function [udf_agg_data] so can be
           # fairly sure all the cols/fields mentioned are present. Otherwise have to do extensive testing
  
           # b) Summaries for breach types
           # i) Totals for all actions in a breach type
           # Now we calculate higher level totals for things which have sub-categories [broad compliance issues]
           # closed by law
           q12_1.0_=rowSums(.[unlist(aggs$closed_law)], na.rm = TRUE),
           # remain closed, all tiers
           q12_1.1_=rowSums(.[unlist(aggs$closed_all)], na.rm = TRUE),
           # remain closed, tier specific
           q12_1.2_=rowSums(.[unlist(aggs$closed_tier)], na.rm = TRUE),
           # stay at home if isolating
           q12_2.0_=rowSums(.[unlist(aggs$home)], na.rm = TRUE),
           # not do what LA says [earlier weeks]
           q12_3.0_=rowSums(.[unlist(aggs$la_order)], na.rm = TRUE),
           # no test and trace
           q12_4.0_=rowSums(.[unlist(aggs$test_and_trace)], na.rm = TRUE),
           # not protect workers, customers
           q12_5.0_=rowSums(.[unlist(aggs$no_protect)], na.rm = TRUE),
           # other
           q12_6.0_=rowSums(.[unlist(aggs$other)], na.rm = TRUE),
           # other 2
           q12_7.0_=rowSums(.[unlist(aggs$other2)], na.rm = TRUE),
           # opening hours
           q12_8.0_=rowSums(.[unlist(aggs$opening_hrs)], na.rm = TRUE),
           # table service only
           q12_9.0_= rowSums(.[unlist(aggs$table)], na.rm = TRUE),
           # bookings over 6
           q12_10.0_= rowSums(.[unlist(aggs$six)], na.rm = TRUE),

          # ii) Closed by law consistent series
          # NB in q12...a-g, reconstruct figs from later weeks which are consistent with earlier by adding together later
          # weeks' sub categories. [in later weeks this was divided into "national" and "tier-specific" closures]
          # verbal advice
          q12_1.0_a_comp=ifelse(is.na(q12_1.0_a) | q12_1.0_a==0,rowSums(.[,c("q12_1.1_a","q12_1.2_a")], na.rm=TRUE),q12_1.0_a),
          # letter
          q12_1.0_b_comp=ifelse(is.na(q12_1.0_b) | q12_1.0_b==0,rowSums(.[,c("q12_1.1_b","q12_1.2_b")], na.rm=TRUE),q12_1.0_b),
          # direction notice
          q12_1.0_c_comp=ifelse(is.na(q12_1.0_c) | q12_1.0_c==0,rowSums(.[,c("q12_1.1_c","q12_1.2_c")], na.rm=TRUE),q12_1.0_c),
          # fixed penalty
          q12_1.0_d_comp=ifelse(is.na(q12_1.0_d) | q12_1.0_d==0,rowSums(.[,c("q12_1.1_d","q12_1.2_d")], na.rm=TRUE),q12_1.0_d),
          # prohibition notice
          q12_1.0_e_comp=ifelse(is.na(q12_1.0_e) | q12_1.0_e==0,rowSums(.[,c("q12_1.1_e","q12_1.2_e")], na.rm=TRUE),q12_1.0_e),
          # prosecutions
          q12_1.0_f_comp=ifelse(is.na(q12_1.0_f) | q12_1.0_f==0,rowSums(.[,c("q12_1.1_f","q12_1.2_f")], na.rm=TRUE),q12_1.0_f),
          # improvement notice
          q12_1.0_g_comp=ifelse(is.na(q12_1.0_g) | q12_1.0_g==0,rowSums(.[,c("q12_1.1_g","q12_1.2_g")], na.rm=TRUE),q12_1.0_g),
          # 'synthetic' option not present in early weeks [Coronavirus improvement notice]
          q12_1.0_h_comp=rowSums(.[,c("q12_1.1_h","q12_1.2_h")], na.rm=TRUE),
          # 'synthetic' option not present in early weeks [Coronavirus immediate restriction notice]
          q12_1.0_i_comp=rowSums(.[,c("q12_1.1_i","q12_1.2_i")], na.rm=TRUE),
          # 'synthetic' option not present in early weeks [Coronavirus restriction notice]
          q12_1.0_j_comp=rowSums(.[,c("q12_1.1_j","q12_1.2_j")], na.rm=TRUE)
          )
  data=data %>% 
    # any action against businesses which should be closed by law, consistent series
    mutate(q12_1.0___comp= rowSums(.[unlist(aggs$consistent_closed)], na.rm = TRUE))

    return(data)
}

# *** Separate concatenated text fields *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
udf_split_txt = function(data, txt){
  # split out the composite comma-limited fields like "what would help enforcement most" out into sep. cols
  txt='q10_1.0'
  
  # select the focal column and turn it into a vector. Use unlist here as tidyverse returns a DF [=list]
  ncols=unlist(select(data,!!txt))
  # the regmatches part of this gives a list of the matches [character positions in the string] to the match 
  # character [=','] for each string 
  # lengths() then converts this into a list of lengths [=no. of matches]. Since we want a string containing 
  # 2 x ',' to convert into 3 cols, we take the max number of no. of matches + 1. This gives the no. of cols
  # in the split entity. We can use this to construct col. names for the split
  ncols=1+max(lengths(regmatches(ncols,gregexpr(",",ncols))))

  # create a [char] vector of the names we want for the col we want to split. These will be (parent col. name + "_a"),
  # (parent col. name + "_b") etc.
  ncols=paste0(txt,"_",letters[1:ncols])

  # split the focal column into n cols [retain original]. Note "!!" syntax allows us to use a variable here
  data = separate(data,!!txt,into=ncols, sep = ",", extra = "drop", remove=FALSE) %>% 
    # remove any trailing or leading whitespace from any entries
    mutate(across(where(is.character), ~trimws(.x,which="both")))
  
  return(data)
}

# *** Aggregation function for text fields *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
udf_agg_text=function(pubData, group_cols='q24_'){
  # takes pubData=DF, optional group_cols = char vector of variables to split by. E.g. group_cols="q1_" splits things by 
  # region. Potentially can be multi-length---e.g. group_cols=c("q1_","q2") [but splitting by respondent gives 1s for counts etc]
  # default = end-date [q24_].
  # In udf_agg_data() we split the concatenated list of things which would make enforcement etc more effective
  # into 3 variables corresponding to mentioned first, second and third [delimited by commas]. For reporting in CO template, 
  # we treat these as of equal weight [first mentioned not more important than 3rd mentioned]. We therefore need to put
  # the contents of these 3 variables into one [where each respondent will be represented up to 3 x]
  #
  # NB The current XL counts a particular entry by a given LA once regardless of how many times it appears. For example,
  # if response is "Don't know,Don't know,Don't know", then "Don't know" is recorded once for that LA. This might make sense
  # if we don't care whether a response is ranked most, second-most or third-most important. We reproduce this here
  # [if analysing by 1st, 2nd, 3rd then don't do this]
  #
  # add LA [q2_] to the list of variables:
  allImpacts=select(pubData,c('q2_',unlist(aggs$impact_vars),group_cols)) %>% 
    # unpivot to get variable names in one column
    # [keep end data/other grouping cols in separate cols]
    pivot_longer(., -c('q2_',group_cols)) %>% 
    # set name to our new composite variable name instead of q10_1.0_a, q10_1.0_b or q10_1.0_c
    mutate(name='q10_1.0__') %>% 
    # quite a few rows are missing data. Remove them
    drop_na(value) %>% 
    # only keep distinct entries for each respondent
    distinct(.) %>% 
    # only keep the variables going forward to the final calculation [might not include q2_]
    select(., c(name,group_cols,value))

  # We want to calculate counts and % for text variables [how many responses of type x were recorded]
  # put text responses in their own DF
  textResps=select(pubData,c(unlist(aggs$text_vars),group_cols)) %>% 
    # unpivot it  
    pivot_longer(., -group_cols) %>% 
    # quite a few rows are missing data. Remove them
    drop_na(value) %>% 
    # add the "what would make things better" variable
    bind_rows(allImpacts)
  # Now create 2 sets of counts: one by week,variable & value [=text entry] and one by week and variable
  # After calculating, add the latter as a "total" for that variable so we can get %s
  weekVarTots=textResps
  # counts by week, variable and entry [value]
  textResps=textResps %>% 
    group_by(across(all_of(c('name','value',group_cols)))) %>% 
    summarise(., cnt=n()) %>% 
    ungroup()
  # counts by week and variable [=total across entries]
  weekVarTots=weekVarTots %>% 
    group_by(across(all_of(c('name',group_cols)))) %>% 
    # group_by(q24_,name) %>% 
    summarise(., tot=n()) %>% 
    ungroup()
  # Join the 2 counts together & calc %
  textResps=left_join(textResps,weekVarTots,by=c('name',group_cols)) %>% 
    # add % calculation
    mutate(pct=cnt/tot*100)
  
  return(textResps)
}

# *** Aggregation function for numbers *** ----
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
udf_agg_nos=function(data,group_cols='q24_'){
  # summarise numeric data by [e.g.] week
  # takes pubData=DF, optional group_cols = char vector of variables to split by. E.g. group_cols="q1_" splits things by 
  # region. Potentially can be multi-length---group_cols=c("q1_","q2") [but splitting by respondent gives 1s for counts etc]
  # default = end-date [q24_].
  
  pubDataByGrp=pubData %>% 
    # i.e. by week end date
    group_by(across(all_of(group_cols))) %>% 
    # get sums and counts of entries > 0. Need to remove the NAs here...
    summarise(across(where(is.numeric),
                     list(sum=~sum(.x, na.rm=TRUE),cnt=~sum(.x>0,
                                                            na.rm = TRUE))),
              q2__cnt=n()) %>%
    ungroup()
  
  return(pubDataByGrp)
}

