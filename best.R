best <-function(state, outcome){
   ##read outcome data
   outcomes <- read.csv("data/outcome-of-care-measures.csv")
   ##check that state and outcome are valid
   if (!(state %in% outcomes[['State']])){
      stop("invalid state")
   }
   
   valid_outcomes <-  c("heart attack", "heart failure", "pneumonia")
   
   if (!(outcome %in% valid_outcomes)){
      stop("invalid outcome")
   }
   ##return hospital name with lowest 30-day rate for given outcome in given state
   state_hospitals <- outcomes[outcomes[['State']] == state,]
   state_hospitals[['Hospital.Name']]

   if (outcome == "heart attack") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
   else if (outcome == "heart failure") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
   else if (outcome == "pneumonia") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
   
   state_hospitals[[column]] <- suppressWarnings(as.numeric(state_hospitals[[column]]))
   state_hospitals <- state_hospitals[!is.na(state_hospitals[[column]]), ]
   minval <- min(state_hospitals[[column]])
   best_hospitals <- state_hospitals[state_hospitals[[column]]==minval,'Hospital.Name']
   best_hospitals <- sort(best_hospitals)
   best_hospitals[1]
}