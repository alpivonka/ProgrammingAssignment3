library(plyr)

rankhospital<- function (state,type,num="best"){
  
  selectedType<-testArguments(state,type,num)
  
  #REad in the data
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Narrow down the data set to just the state of interest
  stateoutcome<-subset(outcome, outcome[[7]]==state, select=c(2,selectedType))
  
  #Replace "Not Available" with NA
  stateoutcome[stateoutcome =="Not Available"]<-NA
  
  #rename the columns to easier use
  names(stateoutcome)<-c("name","x")
  
  #We hae naroowed down the selection criterian and no only with complete cases
  bestComplete<-stateoutcome[complete.cases(stateoutcome),]
  
  #add ranking
  #Assign data type to x 
  bestComplete<-transform(bestComplete,x =as.numeric(x))
  #Arrange things by x and name
  RankedbestComplete<-arrange(bestComplete,x,name, desc(x))
  #Assign a ranking to x based on the order from above
  RankedbestComplete$rank<-with(RankedbestComplete,rank(x,ties.method = "first"))
  #print(RankedbestComplete)
  #Lets get the results....
  result<-if(num=="best"){
    RankedbestComplete[which.min(RankedbestComplete$rank),]$name
  }
  else if(num=="worst"){
    RankedbestComplete[which.max(RankedbestComplete$rank),]$name
  }
  else{
    if(nrow(RankedbestComplete)<num){
      NA
    }
    else{
      RankedbestComplete[RankedbestComplete$rank==num,]  
    }
    
  }
  
  #Get only the min value of the selected column
  result
}

testArguments<-function(state,type,num){
  #Check the length of the state argument
  if(nchar(state)!=2){
    stop("invalid state")
  }
  
  #Convert the type to all lower case
  typeLowercase<-tolower(type)
  
  #check the type argument
  if(!(typeLowercase=="heart attack" || typeLowercase =="heart failure" || typeLowercase == "pneumonia")){
    stop("invalid outcome")
  }else{
    selectedType<-if(typeLowercase=="heart attack"){
      11
    }else if(typeLowercase =="heart failure"){
      17
    }else if(typeLowercase == "pneumonia"){
      23
    }
  }
  if(!(num=="best" || num=="worst" || is.numeric(num))){
    stop ("invalid num")
  }
  selectedType
}