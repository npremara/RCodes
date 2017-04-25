BootACFExtend=function(Series, NoLags=20, NoBoots=50000){
  #Series=(arima.sim(model=list(ar=c(0.6)),n=1000)+50)
  #NoLags=20
  #NoBoots=500
  NoObs=length(Series)
  BootMat=matrix(sample(Series, NoBoots*NoObs, replace=T), ncol=NoObs, nrow=NoBoots, byrow=T)
  
  
  PoorACFMat=apply(BootMat,1,acf,lag.max=NoLags, plot=F,na.action = na.pass)
  ACFMat=matrix(NA, nrow=NoBoots, ncol=NoLags)
  for(i in 1:NoBoots){
    ACFMat[i,] = PoorACFMat[[i]]$acf[(1:NoLags)+1]
  }
  
  #ACFresults=list(ActualACF<SigACFLower| ActualACF>SigACFUpper,OddACF<SigACFLowerOE| OddACF>SigACFUpperOE,EvenACF<SigACFLowerOE| EvenACF>SigACFUpperOE)
  
  # Estended section
  BoxLjLag=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  BoxLjungtest= matrix(NA, nrow=NoBoots, ncol=length(BoxLjLag))
  for (j in 1:length(BoxLjLag)){
    for(i in 1:NoBoots){
      BoxLjungtest[i,j] = NoObs * (NoObs + 2) * sum(1/seq.int(NoObs - 1, NoObs - BoxLjLag[j]) *ACFMat[i,1:BoxLjLag[j]]^2)
    }
  }
  
  
  SigACFLowerBoxLjungtest= apply(BoxLjungtest, 2, quantile, 0.025)
  SigACFUpperBoxLjungtest= apply(BoxLjungtest, 2, quantile, 0.975)
  
  
  ActualBoxLjungtest=matrix(NA, nrow=1, ncol=length(BoxLjLag))
  for(i in 1:length(BoxLjLag)){
    ActualBoxLjungtest[1,i]=Box.test (Series, lag = BoxLjLag[i], type = "Ljung-Box")$statistic
  }
  
  
  ACFAll=list(ActualBoxLjungtest, ActualBoxLjungtest<SigACFLowerBoxLjungtest| ActualBoxLjungtest>SigACFUpperBoxLjungtest)
  
  return(ACFAll)
  #ACFresults=list(ActualACF=ActualACF,ActualACF<SigACFLower| ActualACF>SigACFUpper)
  #return(ACFresults)
  
}
