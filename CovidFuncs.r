#** CovidFuncs **
#============================

# * user-defined functions

# udf_round2 Rounding function ====
# ****************************
# Functions need to be defined before they are called
# This rounds to "scientific rounding": 0.5 rounds to 1. Base R uses round to nearest even normally
#RC amended 7/7/20 so that it uses dp0 rather than 0
#This helps for regression testing: we can amend dp0 to 9 in constants, and it'll feed here too
# udf_round2 = function(n,no_digits=0){ # note that default is zero
udf_round2 = function(n,no_digits = dp0){ # note that default is zero
  posneg = sign(n)
  z = abs(n)*10^no_digits
  z = z+0.5
  z = trunc(z)
  z = z/10^no_digits
  z * posneg
}

# udf_function_elevator Elevator function ====
# ***************************************
# Returns an "elevator language" text string
udf_function_elevator = function(change_no,tolerance){
  el = case_when(
    change_no<(-tolerance)~paste0(as.character(abs(change_no)),"% lower than"),
    change_no>tolerance~paste0(as.character(abs(change_no)),"% higher than"),
    TRUE~"the same as"
  )
  return(el)
}

# udf_lessThan Format rounded values function ====
# *******************************************
# Applies a test to all values it's fed (e.g. from the body of a table)
# and sets them to "<x" where x is the level of rounding applied
# E.g. a value of 0.4 with rounding = 0 will => "<1" and 1 with rounding = -1 => "<10"
# NB seems have to round those numbers which fall above threshold too or else get full 
# unrounded many dec. place vals in output
udf_lessThan = function(x, na.rm = FALSE) ifelse(udf_round2(x,dp)==0,paste0("<",as.numeric(10^-dp)/2),udf_round2(x,dp))

# udf_getVal Return value from call to evaluate ====
# *********************************************
# Returns value from an eval call. Convenience to reduce the amount of typing
# This uses base R. There is a tidyverse alternative
udf_getVal = function(filter_arg){
  valFromTab =eval(parse(text=filter_arg))  # extract single value
  return(valFromTab)
}
