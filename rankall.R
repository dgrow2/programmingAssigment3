rankall<-function(outcome,num=1){
     setwd("C:/Users/Doug/Dropbox/R")
     data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available", stringsAsFactors = FALSE)
     names<-c("hospital","state","outcome")
     outputData<-data.frame()
     bestList<-data.frame()
     checkOutcome<-c("heart attack","heart failure","pneumonia")
     if (!(outcome %in% checkOutcome)){
          stop("invalid outcome")
     }
     columnNames<-c("hospital","state","outcome")
     if (outcome == checkOutcome[1]){
          outputColumn<-11
     }
     else if (outcome == checkOutcome[2]){
          outputColumn<-17
     }
     else if (outcome == checkOutcome[3]){
          outputColumn<-23
     }
     outputData<-na.omit(data.frame(data[,c(2,7,outputColumn)],stringsAsFactors = FALSE))
     colnames(outputData)<-columnNames
     orderedData<-outputData[order(outputData[,2],outputData[,3],outputData[,1]),]
     splitData<-invisible(split(orderedData, orderedData$state))
          hospitalName<-function(x, num){
               if (num=="worst"){
                     invisible(num<-length(x[,3]))
               }
               else if (num=="best"){
                    invisible(num<-1)
               }
          x<-invisible(x[num,1])
          }
     bestList<-invisible(lapply(splitData,hospitalName,num))
     hospital<-invisible(unlist(bestList))
     state<-invisible(names(bestList))
     finalData<-invisible(cbind(hospital,state))
     print(finalData)
     }