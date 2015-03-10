library(plyr)

rankall<- function (type,num="best"){
  
  selectedType<-testArguments(type,num)
  
  #REad in the data
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Narrow down the data set to just the state of interest
  #stateoutcome<-subset(outcome, outcome[[7]]==state, select=c(2,selectedType))
  ocSubset<-subset(outcome,select=c(7,2,selectedType))
  #rename the columns to easier use
  names(ocSubset)<-c("state","name","x")
  outcomeByState<-sqldf("select state,name,x from ocSubset order by state")
  
  #Replace "Not Available" with NA
  #stateoutcome[stateoutcome =="Not Available"]<-NA
  outcomeByState[outcomeByState=="Not Available"]<-NA
  
  #We hae naroowed down the selection criterian and no only with complete cases
  allComplete<-outcomeByState[complete.cases(outcomeByState),]
  
  #add ranking
  #Assign data type to x 
  allComplete<-transform(allComplete,x =as.numeric(x))
  
  byStateList<-split(allComplete,allComplete$state)
  
  states<-sqldf("select distinct state from x order by state")
  #print(summary(byStateList))
  #print(attributes(states))
  allResults<-data.frame(hospital=character(),state=character())
  for(statedf in byStateList){
    
    RankedbestComplete<-arrange(statedf,x,name, desc(x))
    
    #Assign a ranking to x based on the order from above
    RankedbestComplete$rank<-with(RankedbestComplete,rank(x,ties.method = "first"))
    #print(RankedbestComplete)
    #Lets get the results....
    result<-if(num=="best"){
      RankedbestComplete[which.min(RankedbestComplete$rank),]#$name$state
    }
    else if(num=="worst"){
      RankedbestComplete[which.max(RankedbestComplete$rank),]#$name
    }
    else{
      if(nrow(RankedbestComplete)<num){
        NA
      }
      else{
        RankedbestComplete[RankedbestComplete$rank==num,]  
      }
    }
    suppressWarnings(
      if(is.na(result)){
     hospital<-NA
     state<-sqldf("select distinct state from RankedbestComplete")
     result<-data.frame(state,hospital)
    }
    )
  
   xx<-result[,c(2,1)]
   names(xx)<-c("hospital","state")
  
   allResults<-rbind(allResults,xx)
   
  }
  allResults
 
}

testArguments<-function(type,num){
  
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