#MICROSTRATEGY_BEGIN
#
#RVAR Cohort -input -numeric -vector
#RVAR LINIC -input -numeric -vector
#RVAR SLICR -input -numeric -vector
#RVAR RCR -input -numeric -vector
#RVAR ESLI -input -numeric -vector
#
#RVAR APLCR -output -numeric -vector  #Metric Expression: RScript<_RScriptFile="Gates.r", _InputNames="Cohort, LINIC, SLICR, RCR, ESLI", SortBy=(Cohort,Age)>(Cohort, LINIC, SLICR, RCR, ESLI)
if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)  #Working Directory if executed by MicroStrategy
#
#MICROSTRATEGY_END

mstr.ErrMsg <- tryCatch({                                      #tryCatch for Exception Handling
  if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)         #Working Directory if executed by MicroStrategy
  if(!exists("mstr.ExFlag")) {                                 #If MicroStrategy is not executing
     df <- read.csv("Gates.csv",header=TRUE)                   #  Read in CSV
	 df <- df[1:21,1:11]                                       #  Formatting
	 attach(df)                                                #  Attach to df
  }	 
  
  APLCR <- rep(0 , length(Cohort))                             #Create APLCR vector
  for(i in 1 : length(Cohort)) {                               #Loop over all values
    first_index <- min(which(Cohort[] == Cohort[i]))           #  Find first row index for this cohort
    if(first_index == i) {                                     #  If this is the first row of the cohort
       APLCR[i] = LINIC[i] * SLICR[i] *RCR[i] *ESLI[i]         #    Set ALPCR to Product
    } else {                                                   #  If this is not the first row
       APLCR[i] = (LINIC[i] *SLICR[i] * RCR[i]                 #    Set ALPCR to Product - Running Sum
	              - sum(APLCR[first_index:(i-1)])) * ESLI[i]
    }
  }
  mstr.ErrMsg <- ""                                            #If we made it here, no errors were caught
}, error = function(err) {                                     #Catch block to report an error
  try(print(err))                                              #  Print error message to console (using try to continue on any print error)
  return(err$message)                                          #  Return error Message
})
  
