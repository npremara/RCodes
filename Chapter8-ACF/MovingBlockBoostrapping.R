library(quantspec)


BootACFBBM=function(Series, NoLags=26, NoBoots=2000, MovingBlockLength=25){
  
  NoObs=length(Series)
  k=movingBlocks(MovingBlockLength, NoObs)
  PoorACFMatIndex=getPositions(k, B = NoBoots)
  Mat=matrix(rep(Series,NoBoots), ncol = NoBoots)
  
  BootMat=matrix(Mat[PoorACFMatIndex],ncol=NoObs, nrow=NoBoots, byrow=T)
  BootMat=t(BootMat)
  
  PoorACFMat=apply(BootMat,1,acf,lag.max=NoLags, plot=F,na.action = na.pass)
  ACFMat=matrix(NA, nrow=NoBoots, ncol=NoLags)
  for(i in 1:NoBoots){
    ACFMat[i,] = PoorACFMat[[i]]$acf[(1:NoLags)+1]
  }
  
  SigACFLower= apply(ACFMat, 2, quantile, 0.025)
  SigACFUpper= apply(ACFMat, 2, quantile, 0.975)
  
  SigACFLowerOE=SigACFLower[seq(2, length(SigACFLower), 2)]
  SigACFUpperOE=SigACFUpper[seq(2, length(SigACFUpper), 2)]
  
  ActualACF=acf(Series, plot = FALSE, lag.max = NoLags,na.action = na.pass)$acf[(1:NoLags)+1]
  
  SplitData=matrix(NA, nrow=length(Series)/2, ncol=2)
  SplitData[,1]=Series[seq(1, length(Series), 2)]
  SplitData[,2]=Series[seq(2, length(Series), 2)]
  
  OddACF=acf(SplitData[,1], plot = FALSE, lag.max = NoLags/2,na.action = na.pass)$acf[(1:(NoLags/2))+1]
  EvenACF=acf(SplitData[,2], plot = FALSE, lag.max = NoLags/2,na.action = na.pass)$acf[(1:(NoLags/2))+1]
  
  ACFresults=list(ActualACF<SigACFLower| ActualACF>SigACFUpper,OddACF<SigACFLowerOE| OddACF>SigACFUpperOE,EvenACF<SigACFLowerOE| EvenACF>SigACFUpperOE)
  
  return(ACFresults)
  
}
