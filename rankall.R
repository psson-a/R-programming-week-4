rankall <- function(outcome, num = "best") {
   ## Read outcome data
   outcomes <- read.csv("data/outcome-of-care-measures.csv")
   
   ## Check that state and outcome are valid
   valid_outcomes <-  c("heart attack", "heart failure", "pneumonia")
   
   if (!(outcome %in% valid_outcomes)){
      stop("invalid outcome")
   }
   
   ## For each state, find the hospital of the given rank
   hospitals <- split(outcomes, outcomes[['State']])
   
   states <- list(NULL)
   hosp_rank <- list(NULL)
   
   for (hospital in hospitals){
      states <- c(states, hospital[1, 'State'])
   }
   return(states)
   state_hospitals <- outcomes[outcomes[['State']] == state,]
   #state_hospitals[['Hospital.Name']]
   
   if (outcome == "heart attack") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
   else if (outcome == "heart failure") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
   else if (outcome == "pneumonia") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
   
   state_hospitals[[column]] <- suppressWarnings(as.numeric(state_hospitals[[column]]))
   state_hospitals <- state_hospitals[!is.na(state_hospitals[[column]]), ]
   state_hospitals <- state_hospitals[order(state_hospitals[[column]], state_hospitals[['Hospital.Name']]),]
   hospitals <- split(outcomes, outcomes[['State']])

   
   if (num == "best") return (state_hospitals[1,'Hospital.Name'])
   else if (num =="worst") return (state_hospitals[nrow(state_hospitals),'Hospital.Name'])
   else return (state_hospitals[num,'Hospital.Name'])
   ## Return a data frame with the hospital names and the
   ## (abbreviated) state name