First_Sample=read.csv("ListofStocks.csv")
Ticker<-as.vector(First_Sample[,"Tickersymbol"])
Series<-as.vector(First_Sample[,"Company"])

StartDate="1920/01/01"
EndDate=Sys.Date()

library(tseries)

for(i in 1:length(Ticker)){
  assign(noquote(Ticker[i]), get.hist.quote(instrument=Ticker[i], start = as.Date(StartDate), end = as.Date(EndDate), quote=c("Open", "High", "Low", "Close","AdjClose", "Volume"), provider="yahoo", retclass="zoo"))    
  save(list=Ticker[i], file=paste("Data\\",noquote(Ticker[i]), ".rda", sep=""))
}

#Example
for(i in 1:2){
  assign(noquote(Ticker[i]), get.hist.quote(instrument=Ticker[i], start = as.Date(StartDate), end = as.Date(EndDate), quote=c("Open", "High", "Low", "Close","AdjClose", "Volume"), provider="yahoo", retclass="zoo"))    
  save(list=Ticker[i], file=paste("Data\\",noquote(Ticker[i]), ".rda", sep=""))
}


