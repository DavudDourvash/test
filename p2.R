
incforecast<-function(n=5)  {


  ##Load objects from main WorkSpace
  load(file = "H:\\since95\\Dayche-1\\p1.RData")
  n<-as.integer(n)
  ##return(n)



  ##make forecast vectors
  ##n last regressors data
  regdata<-data.frame()
  regdata<-Datrain[(length(Datrain[,1])-(n-1)):length(Datrain[,1]), ]
  regdata<-regdata[,-c(1,4,7,14,15,20)]

  ##n last empty regressors forecast data
  fcreg<-vector(length = n)
  fcreg[1:length(fcreg)]<-0




  ##n last residuals empty data
  k<-length(Datrain[,1])-length(nipercent[,1])
  fcres<-vector(length = n)
  fcres[]<-0

  ##n last total forecast data
  fcfinal<-vector(length = n)
  fcfinal[]<-0
  regr2<-0
  regresr2<-0


    ##regressors model forecast
    for (i in 1:length(regdata[,1])){
      s<-0;
      for(j in 1:length(regcoeff)){
        s <-sum((regcoeff[j])*(regdata[i,j]))
        fcreg[i]<-s+fcreg[i]
      }
    }


    ##residuals model forecast
    fcres[1:n]<-respredtest[(length(respredtest)-(n-1)):length(respredtest)]

    ##regressors + residuals model forecast
    fcfinal<-fcreg[1:n]+fcres[1:n]

    ##R2 and Mse Calculation

    regr2<-r2calc(fcreg,regdata)
    regresr2<-r2calc(fcfinal,regdata)

    regmse<-Metrics::mse(regdata,fcreg)
    regresmse<-Metrics::mse(regdata,fcfinal)


  c(regressorsforecast=fcreg,totallyforecast=fcfinal)
  c(regr2=regr2,regmse=regmse,regresr2=regresr2)
  par(mfrow=c(1,2))
  plot(fcreg,type='l',xlab="time" ,ylab="fcreg")
  plot(fcfinal,type='l',xlab="time" ,ylab="fcfinal")
  forecastreport<-data.frame(fcreg,fcfinal,row.names = dt[1:n])
}

