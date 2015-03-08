best<- function (state,type){
  
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
  
  #order the data.frame by the column names value, name
  bestComplete<-bestComplete[with(bestComplete, order(x,name)),]
  
  #Get only the min value of the selected column
  best<-bestComplete[which.min(bestComplete$x),]$name
 

  best
}