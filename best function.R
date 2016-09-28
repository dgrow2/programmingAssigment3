best<-function(state,outcome){
     setwd("C:/Users/Doug/Dropbox/R")
     data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors = FALSE)
     names<-c("hospital","state","outcome")
     outputData<-data.frame()
     stateIndex<-data.frame()
     checkState<-unique(data[,7])
     checkOutcome<-c("heart attack","heart failure","pneumonia")
     if (!(state %in% checkState)){
          stop("invalid state")
     }
     else if (!(outcome %in% checkOutcome)){
          stop("invalid outcome")
     }
     names<-c("hospital","state","outcome")
     states<-as.vector(na.omit(data[,7]))
     stateIndex<-na.omit(which(states==state))
     if (outcome == checkOutcome[1]){
          outputColumn<- 11
     }
     else if (outcome == checkOutcome[2]){
          outputColumn<-17
     }
     else if (outcome == checkOutcome[3]){
          outputColumn<-23
     }
     outputData<-na.omit(data.frame(data[stateIndex,c(2,7,outputColumn)],stringsAsFactors = FALSE))
     colnames(outputData)<-names
     orderedData<-outputData[order(outputData[,3],outputData[,1]),]
     print(orderedData[1,1])
      }