AVRLC<-function(LCL,UCL,type,sg,mx,my,sdx,sdy,rho){
  RL=0
  while(TRUE)
  {
    DataSample <- rbivariateMInc(sg,mx,my,sdx,sdy,rho)
    if (type=="dbar"){
      DataSat=mean(DataSample[,1]-DataSample[,2])
    }
    else if (type=="sdd") {
      DataSat=sd(DataSample[,1]-DataSample[,2])
    }
    else if (type=="sdx") {
      DataSat=sd(DataSample[,1])
    }
    else if (type=="sdy") {
      DataSat=sd(DataSample[,2])
    }
    else if (type=="detS") {
      DataSat=det(cov(data.frame(DataSample[,1],DataSample[,2])))
    }
    else if (type=="W") {
      si=cov(data.frame(DataSample[,1],DataSample[,2]))
      s0=matrix(c(1, 0.75,0.75,1), 2)
      DataSat=-2*sg+2*sg*log(sg)-sg*log(det((sg-1)*si)/det(s0))+matrix.trace(solve(s0)*(sg-1)*si)
    }
    
    
    RL=RL+1
    if (DataSat<LCL|DataSat>UCL)
      break
  }
  return(RL-1)
}

