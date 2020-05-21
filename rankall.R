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
   
   states <- character()
   hosp_rank <- character()
   
   if (outcome == "heart attack") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
   else if (outcome == "heart failure") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
   else if (outcome == "pneumonia") column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
   
   
   for (hospital in hospitals){
      states <- c(states, hospital[1, 'State'])
      
      hospital[[column]] <- suppressWarnings(as.numeric(hospital[[column]]))
      hospital <- hospital[!is.na(hospital[[column]]), ]
      hospital <- hospital[order(hospital[[column]], hospital[['Hospital.Name']]),]
      if (num == "best") hosp_rank <- c(hosp_rank, hospital[1,'Hospital.Name'])
      else if (num =="worst") hosp_rank <- c(hosp_rank, hospital[nrow(hospital),'Hospital.Name'])
      else hosp_rank <- c(hosp_rank, hospital[num,'Hospital.Name'])
   }
   answer=data.frame(hospital=hosp_rank, state=states)
   #answer <- hosp_rank
   answer
}