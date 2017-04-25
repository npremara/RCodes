
CoMomentsNonMP<-function(x,y){
  
  xv=1:length(x)
  yv=1:length(y)
  xGrid=combn(xv, 2)
  yGrid=combn(yv, 2)
  
  Length=(length(yGrid))/2
  
  xyGrid=expand.grid(1:Length, 1:Length)
  
  GridIndex=data.frame(Var1=xGrid[,xyGrid[,1]][1,],Var2=xGrid[,xyGrid[,1]][2,],Var3=yGrid[,xyGrid[,2]][1,],Var4=yGrid[,xyGrid[,2]][2,])
  
  NonMatchGridIndex=GridIndex[!((GridIndex$Var1==GridIndex$Var2)|(GridIndex$Var1==GridIndex$Var3) |(GridIndex$Var1==GridIndex$Var4)| (GridIndex$Var2==GridIndex$Var3)|(GridIndex$Var2==GridIndex$Var4)| (GridIndex$Var3==GridIndex$Var4)),c(1,2,3,4)]
  
  xyproducts=(x[NonMatchGridIndex[,1]]-x[NonMatchGridIndex[,2]])*(y[NonMatchGridIndex[,3]]-y[NonMatchGridIndex[,4]])
  length(xyproducts)
  CovNMP=sum(xyproducts)/length(xyproducts)
  xy2products=(x[NonMatchGridIndex[,1]]-x[NonMatchGridIndex[,2]])*(y[NonMatchGridIndex[,3]]-y[NonMatchGridIndex[,4]])^2
  CoSkewNMP=sum(xy2products)/length(xyproducts)
  xy3products=(x[NonMatchGridIndex[,1]]-x[NonMatchGridIndex[,2]])*(y[NonMatchGridIndex[,3]]-y[NonMatchGridIndex[,4]])^3
  CoKurtNMP=sum(xy3products)/length(xyproducts)
  return(list(CovNMP=CovNMP,CoSkewNMP=CoSkewNMP,CoKurtNMP=CoKurtNMP))
}




CoMoments<-function(x,y){
  NCov = mean(na.omit((x[, drop = FALSE] - mean(x[, drop = FALSE]))* (y[, drop = FALSE] - mean(y[, drop = FALSE]))))
  NCoSkew = mean(na.omit((x[, drop = FALSE] - mean(x[, drop = FALSE])) * (y[, drop = FALSE] - mean(y[, drop = FALSE]))^2))
  NCoKurt = mean(na.omit((x[, drop = FALSE] - mean(x[, drop = FALSE]))* (y[, drop = FALSE] - mean(y[, drop = FALSE]))^3))
  return(list(Cov=NCov,CoSkew=NCoSkew,CoKurt=NCoKurt))
}

