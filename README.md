#bstall2varLGD[c('Автокредиты1','Автокредиты2','Автокредиты3','Автокредиты4')]
#Extr(as.Date('2015-10-01'),100,trainOILLGD[c('L6MCOILBRENTEU',"Lm1RTS_M",auLGD,'Date',"MCOILBRENTEU")],'Date','2008-01-01',40,TRUE,"MCOILBRENTEU",1.5)->train40
#plot.forecast(auPDmodels,train40[train60['Date']>='2013-10-01',],c('Автокредиты1','Автокредиты2','Автокредиты3','Автокредиты4'),'Date')->plotsautoPD40
#multiplot(plotsautoPD40$Автокредиты1,plotsautoPD40$Автокредиты2,plotsautoPD40$Автокредиты3,plotsautoPD40$Автокредиты4,cols=2)


#Extr(as.Date('2015-10-01'),100,trainOILLGD[c('L6MCOILBRENTEU',"Lm1RTS_M",auLGD,'Date',"MCOILBRENTEU")],'Date','2008-01-01',40,FALSE,"MCOILBRENTEU",1.5)->train0
Extr<-function(endDate,N,data,name_period='Date',start_date,Scenario.param,intervalfrcst,isitstress,names_stress,kmx,p)
{
  # подумать о прогнозе лагов
  data<-data[data[name_period]>=start_date,]
  
  names_macro<-setdiff(colnames(data),name_period)
  lnames_mustbe<-colnames(data)[grep(names_stress,colnames(data))]
  stat<-data[names_macro]
  # смотрим на порядок интеграции для ряда
  Iorder(stat,0.01)->io# стандартно p=0.01
  io[[2]]->nonStnames
  # при построении мы не рассматриваем ряды порядка интеграции >1, не смотрим на большие порядки
  
  
  #if (!is.null(io[[2]])) {L(stat[io[[2]]])->stat[io[[2]]]}
  #stat[name_period]<-data[name_period]
  stat<-data
  
  
  res<-c()
  for (i in lnames_mustbe)
  {
    
    # если ряд не стационарный, то ищем темпы роста, которые будут средними для процесса полученного после взятия разности
"    if (i %in% nonStnames)
    {
      ndata<-na.omit(data[i])
      ndata[dim(ndata)[[1]],i]->cur_val # текущее значение стрессововй переменной
      (Scenario.param-cur_val)/intervalfrcst->statAVG # помеячное среднее  для стационарного, со знаком; если ориентироваться на среднюю цену на нефть то надо этот темп умножить на 2
      
      Simulation(stat[c(i,name_period)],endDate,N,statAVG,isitstress,kmx)->sim
      na.omit(data[c(i,name_period)])->temp
      temp[dim(temp)[1],c(i,name_period)]->shot

      rbind(shot,sim)->d1
     
      
      d1[i]<-cumsum(d1[,i]) # восстанавливаем процесс
    }
    else"
    {
      statAVG <-Scenario.param    
      
      Simulation(stat[c(i,name_period)],endDate,N,statAVG,isitstress,kmx)->sim
      na.omit(data[c(i,name_period)])->temp
      temp[dim(temp)[1],c(i,name_period)]->shot
      rbind(shot,sim)->d1
      
    }
    
    if (is.null(res)) {res<-d1}else {res<-merge(res,d1,by=name_period,all=TRUE)}
  
  }
  setdiff(colnames(data),c(lnames_mustbe,'Date'))->ec.var

  if (TRUE==isitstress)
  {
    W_other(names_stress,data,'Date',p)->list.lm # модели для прогнозирования  долгосрочных средних

    for (j in ec.var)
    {
 # print(j)
   "   if (j %in% nonStnames)
      {
        
        data[dim(data)[[1]],j]->cur_val # текущее значение стрессововй переменной
        (Scenario.param-cur_val)/intervalfrcst->statAVG # помеячное среднее  для стационарного, со знаком
        statAVG<-as.data.frame(statAVG)
        colnames(statAVG)<-setdiff(rownames(coefficients(summary(list.lm[[j]]))),'(Intercept)')

        if(!is.null(list.lm[[j]])){predict(list.lm[[j]],statAVG)->mu;isitstress=FALSE}
        Simulation(stat[c(j,name_period)],endDate,N,mu,isitstress,kmx)->sim
        na.omit(data[c(j,name_period)])->temp
        temp[dim(temp)[1],c(j,name_period)]->shot
    
        rbind(shot,sim)->d1
        d1[j]<-cumsum(d1[,j])
      
      }
      else"
      {
        statAVG <-Scenario.param 
        statAVG<-as.data.frame(statAVG)
        colnames(statAVG)<-setdiff(rownames(coefficients(summary(list.lm[[j]]))),'(Intercept)')
        predict(list.lm[[j]],statAVG)->mu
        Simulation(stat[c(j,name_period)],endDate,N,mu,isitstress,kmx)->sim
        na.omit(data[c(j,name_period)])->temp
        temp[dim(temp)[1],c(j,name_period)]->shot
        
        rbind(shot,sim)->d1
      }
      
      
      if (is.null(res)) {res<-sim}else {res<-merge(res,d1,by=name_period,all=TRUE)}
    }
  }else
  {
    for (j in ec.var)
    { 
      print(j)
   "   if (j %in% nonStnames)
      {       
        na.omit(data[c(j,name_period)])->temp
        temp[dim(temp)[1],c(j,name_period)]->shot
        
        rbind(shot,sim)->d1
        
        d1[j]<-cumsum(c(d1[dim(d1)[[1]],j],sim[,j])) # восстанавливаем процесс

      }else"
      {
        Simulation(stat[c(j,name_period)],endDate,N,mu,isitstress,kmx)->sim
        na.omit(data[c(j,name_period)])->temp
        temp[dim(temp)[1],c(j,name_period)]->shot
        
        rbind(shot,sim)->d1
      }
      
      
      if (is.null(res)) {res<-d1}else {res<-merge(res,d1,by=name_period,all=TRUE)}
    }
  }

  # соединяем данные и прогноз 
  datares<-c()
  
  for (i in setdiff(colnames(data),name_period))
  {

  na.omit(as.data.frame(res[i]))->rtemp
    
   as.data.frame( rtemp[2:dim(rtemp)[1],])->restemp
    
    colnames(restemp)<-i
 

    if (!is.null(datares))
    {     
      datares<-cbind(rbind(na.trim(data[i],'right'),restemp),datares)}
    else{
      datares<-rbind(na.trim(data[i],'right'),restemp)

    }
   
  }
  

  Date<-as.Date(start_date)
  month(Date)<-month(Date)+0:(dim(datares)[1]-1)
  datares$Date<-Date
  datares
}




#delta : step
#MLE-OLS estimation
estOU<-function(delta,S)
{
  n <- length(S)-1
  delta <- 0.1
  
  Sx <- sum(S[1:length(S)-1])
  Sy <- sum(S[2:length(S)])
  Sxx <- crossprod(S[1:length(S)-1], S[1:length(S)-1])
  Sxy <- crossprod(S[1:length(S)-1], S[2:length(S)])
  Syy <- crossprod(S[2:length(S)], S[2:length(S)])
  
  
  a  = ( n*Sxy - Sx*Sy ) / ( n*Sxx -Sx^2 );
  b  = ( Sy - a*Sx ) / n;
  sd = sqrt( (n*Syy - Sy^2 - a*(n*Sxy - Sx*Sy) )/n/(n-2) );
  
  if(a<=0){#print('my_warnings: SIGMA is NAN');
           a<-abs(a)}
  if(log(a)>=0){ #'my_warnings: LAMBDA is NEGATIVE');
lambda = log(a)/delta;}else{lambda = -log(a)/delta;}
  
  mu     = b/(1-a);
  sigma  =  sd * sqrt( -2*log(a)/delta/(1-a^2) );
  
  list(mu,lambda,sigma)
}

# мой вариант симуляции
ornstein_uhlenbeck <- function(T,n,mu,lambda,sigma,x0,t0,kmx)
{
  
  n<-round(n) # as.yearmon выдает странный объект
  T<-round(T)
  
  par=10 # во сколько раз уменьшить шаг 
  
  #class(n)<-"integer"
  #class(T)<-"integer"
  dw<-rnorm(n,0,sqrt((T-t0)/(n*par))) 
  
  dt  <- (T-t0)/(n*par) # сделаем шаг 0.1
  x <- c(x0)
  for (i in 2:(n+1)){
    
    x[i]  <-  x[i-1] + lambda*(mu-x[i-1])*dt + sigma*dw[i-1]
    #добавка коридора для процесса
    delt=abs(mu-x0)
    if (abs(x[i])<abs(mu)+delt*kmx){x[i]<-(abs(mu)+delt*kmx)*sign(x[i])}
  }
  t<-seq(t0,T,by=dt)
  as.data.frame(cbind(x,t))->sim
  
  colnames(sim)<-c('S','t')
  return(sim);
}

# у нас прогноз на 12 месяцев 
#Process - data.frame(c('Date','Value'))
#kmx - задается экспертно, во сколько раз может меняться значение параметра

Simulation<-function(Process,endDate,N,Scenario.mean,exst,kmx)
{
  require(lubridate)
  require(zoo)
  
  
  
  Process<-na.omit(Process)

  # шаг 1 месяц
  Process[,!(colnames(Process) %in% 'Date')]->S
  Process[,(colnames(Process) %in% 'Date')]->DateS
  min(Process[colnames(Process[!(colnames(Process) %in% 'Date')])])->minP
  estOU(1,S)->parametrers

  
  if(exst==FALSE)
  {
    Scenario.mean=parametrers[[1]]
    
  }
  
  S[length(S)]->V.last
  DateS[length(DateS)]->D.last
  round((as.yearmon(endDate)-as.yearmon(D.last))*12)-1->h  
  allforplot<-c()
  allSim<-c()
  
  i<-1
  repeat
  {
    
    #HWV(N = 100, M = 1, x0 = 2, t0 = 0, T = 1, Dt, mu = 4, theta = 1,sigma = 0.1, ...)
    library(Sim.DiffProc)
    #delta <- 0.25 in estOU
    HWV(h,1,V.last,0,h,0.1,parametrers[[2]],theta=Scenario.mean,sigma=parametrers[[3]])->res

    res<-as.data.frame(res)
    colnames(res)<-c('S')
    res$t<-1:dim(res)[1]
    
    #ornstein_uhlenbeck(h,h,Scenario.mean,parametrers[[2]],parametrers[[3]],V.last,0,kmx)->res
    
    if(prod(res$S>0)==TRUE | minP<0)
    { 
      
      
      if (1==i) {
        
        resplot<-res
        colnames(res)<-sub('S',paste0('S',i),colnames(res))
        allSim<-res;
        resplot$n<-i
        allforplot<-resplot;
      } else { 
        resplot<-res
        colnames(res)<-sub('S',paste0('S',i),colnames(res))
        resplot$n<-i
        allforplot<-rbind(resplot,allforplot);
        allSim<-merge(res,allSim,by='t');}
      i<-i+1
    }
    if (i>=N ) {break}
    
  }
  
  D.last->D.lastSim
  
  month(D.last)<-month(D.last)+allforplot$t #интересное поведение, правильное
  
  allforplot$t<-D.last
  
  month(D.lastSim)<-month(D.lastSim)+allSim$t
 
  allSim$t<-D.lastSim
  
  allforplot<-as.data.frame(allforplot)
  colnames(allforplot)<-c(colnames(Process[!(colnames(Process) %in% 'Date')]),'Date','N.Sim')
  
  #allforplot
  #allSim  
  allSim$avg<-apply(allSim[!(colnames(allSim) %in% 't')],1,mean)
  
  allSim<-allSim[c("avg",'t')]
  
  
  sub('avg',colnames(Process[!(colnames(Process) %in% 'Date')]),colnames(allSim))->colnames(allSim)
  sub('t','Date',colnames(allSim))->colnames(allSim)
  
  allSim
}






