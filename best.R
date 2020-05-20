best <-function(state, outcome){
   ##read outcome data
   outcomes <- read.csv("data/outcome-of-care-measures.csv")
   print(head(outcomes[['State']]))
   ##check that state and outcome are valid
   if (!(state %in% outcomes[['State']])){
      stop("invalid state")
   }
      
   ##return hospital name with lowest 30-day rate for given outcome in given state
}