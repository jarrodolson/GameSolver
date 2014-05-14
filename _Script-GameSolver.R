#####################################################
## This script solves a game based on a user-defined
## payoff matrix
##
## Jarrod Olson, jarrod.olson@outlook.com
#####################################################
EnsurePackage <- function(x){
  x <- as.character(x)
  if(!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

##TODO: Need function to visualize payoff matrix
payoffMatrix.print <- function(payoffs,option="standard"){
  EnsurePackage('xtable')
  for(i in 1:nrow(payoffs)){
    data <- payoffs[i,]
    coop_coop <- paste(data$a_coop,data$b_coop,sep=":")
    coopa_defectb <- paste(data$a_coop,data$b_defect,sep=":")
    defecta_coopb <- paste(data$a_defect,data$b_coop,sep=":")
    defect_defect <- paste(data$a_defect,data$b_defect,sep=":")
    col1 <- data.frame("X"=c("","","Player A","Player A"))
    col2 <- data.frame("Y"=c("","","Cooperate","Defect"))
    col3 <- data.frame("Z"=c("Player B","Cooperate",coop_coop,defecta_coopb))
    col4 <- data.frame("W"=c("Player B","Defect",coopa_defectb,defect_defect))
    out <- cbind(col1,col2,col3,col4)
    if(option=="standard"){
      print(out)
    }
    if(option=="latex"){
      print(xtable(out,caption=paste("Payoff Table",i)),type="latex")
    }
    if(option=="html"){
      print(xtable(out,caption=paste("Payoff Table",i)),type='html')
    }
  }
}

##TODO: Need function to solve game
game.solve <- function(payoffs){
  choiceAList <- c()
  payoffAList <- c()
  choiceBList <- c()
  payoffBList <- c()
  outcomeList <- c()
  for(i in 1:nrow(payoffs)){
    data <- payoffs[i,]
    new <- data.frame(choice=c("coop","defect"),a=c(data$a_coop,data$a_defect),b=c(data$b_coop,data$b_defect),stringsAsFactors=FALSE)
    payoffA <- max(new$a)
    payoffAList <- c(payoffAList,payoffA)
    payoffB <- max(new$b)
    payoffBList <- c(payoffBList,payoffB)
    error <- FALSE
    if(new[1,]$a==new[2,]$a){
      ##print("Error - A unresolved")
      error <- TRUE
    }
    if(new[1,]$b==new[2,]$b){
      ##print("Error - B unresolved")
      error <- TRUE
    }
    if(error==FALSE){
      choiceA <- new[which(new$a==payoffA),]$choice
      choiceB <- new[which(new$b==payoffB),]$choice
    }
    if(error==TRUE){
      choiceA <- "ERROR"
      choiceB <- "ERROR"
    }
    outcome <- "undefined"
    if(choiceA=="defect" & choiceB=="defect"){
      outcome <- "Prisoners Dilemna"
    }
    if(choiceA=="coop" & choiceB =="coop"){
      outcome <- "Cooperative"
    }
    if(choiceA!=choiceB){
      outcome <- "Uncooperative"
    }
    if(choiceA=="ERROR" | choiceB=="ERROR"){
      outcome <- "ERROR"
    }
    choiceAList <- c(choiceAList,choiceA)
    choiceBList <- c(choiceBList,choiceB)
    outcomeList <- c(outcomeList,outcome)
  }
  dataNew <- data.frame(choiceA=choiceAList,choiceB=choiceBList,outcome=outcomeList,payoffA=payoffAList,payoffB=payoffBList)
#   payoffs$choiceA <- choiceAList
#   payoffs$choiceB <- choiceBList
#   payoffs$outcome <- outcomeList
  return(dataNew)
}

a_coop <- c(1,1)
a_defect <- c(2,3)
b_coop <- c(1,3)
b_defect <- c(2,2)

payoffs <- data.frame(a_coop=a_coop,a_defect=a_defect,b_coop=b_coop,b_defect=b_defect)
payoffMatrix.print(payoffs)
payoffOutcome <- game.solve(payoffs)
print(payoffOutcome)
