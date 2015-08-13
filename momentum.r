#Given data with the following layout
#      ac1 ac2 ac3 ... acN
# row1 r11 r12 r13 ... r1N
# ...
# rowM rM1 rM2 rM3 ... rMN

#mom_ac_returns takes any row (i) and calculates the returns for each ac from m.from to m.to periods ago.


mom_ac_backward_returns<-function(data,m.from=6,m.to=1){
    # Past returns to calculate momentum starting m.from months (records ago) to m.to records ago inclusive 
    # where 0 is current record. 
    # data is a matrix (xts, data frame) of returns in decimal format.
    n.rec<-length(data[,1])
    returns<-data
    returns<-returns[-(1:(m.from)),] #Remove first rows
    for (i in (m.from+1):n.rec){
        returns[(i-m.from),]<-apply(1+data[(i-m.from):(i-m.to),],2,prod)-1    
    }
    return(returns)
}

mom_ac_return_rank<-function(data,m.from=6,m.to=1){
    #returns ranking of each asset class based on mom_ac_backward_return
    #columns are assset classes, values are ranks
    returns<-mom_ac_backward_returns(data,m.from,m.to)
    return(t(apply(-returns,1,rank,ties.method="random")))
}

mom_ranks<-function(data,m.from=6,m.to=1){
    #Asset classes according to momentum rank. columns are ranks, values are asset class (by column number according to data)
    ranks<-mom_ac_return_rank(data,m.from,m.to)
    result<-t(apply(ranks, 1, function(x) match(seq(1,ncol(ranks)),x) ))
    colnames(result)<-paste("Rank" , 1:ncol(ranks), sep="")
    return(result)
}
    
mom_rank_return<-function(data,m.from=6,m.to=1,m.window=1){
    #returns for the rank looking ahead for a window of m.window periods
    rank.ac<-mom_ranks(data,m.from,m.to) #which asset classes have rank==1 , rank==2 etc
    returns<-data
    returns<-returns[-(1:(m.from)),] #Remove first rowsto account for m.from
    returns<-returns[-((nrow(returns)-m.window+2):nrow(returns)),] #remove rows to account for m.window
    colnames(returns)<-colnames(rank.ac)
    for (i in 1:nrow(returns)){
        ifelse(m.window==1,
               returns[i,]<-data[(m.from+i),rank.ac[i,]],
               returns[i,]<-apply(1+data[(m.from+i):(m.from+i+m.window-1),rank.ac[i,]],2,prod)-1)
    }
    return(returns)
}

mom_rank_rank<-function(data,m.from=6,m.to=1,m.window=1){
    returns<-mom_rank_return(data,m.from,m.to,m.window)
    return(t(apply(-returns,1,rank,ties.method="random")))
    #colnames(result)<-paste("Rank" , 1:ncol(ranks), sep="")
}

mom_ac_return<-function(data,m.from=6,m.to=1,m.window=1){
    # forward looking returns of asset classes over m.window periods
    # comparable to mom_rank_return
    returns<-data
    returns<-returns[-(1:(m.from)),] #Remove first rowsto account for m.from
    returns<-returns[-((nrow(returns)-m.window+2):nrow(returns)),] #remove rows to account for m.window
    for (i in 1:nrow(returns)){
        #returns[i,]<-data[m.from+i,rank.ac[i,]]
        returns[i,]<-         apply(1+data[(m.from+i):(m.from+i+m.window-1),],2,prod)-1
    }
    return(returns)
}

mom_study<-function(data,m.from=6,m.to=1,m.window=1){
    nper_per_yr<-12 #assumes monthly data
    result<-list()
    result$ranks<-mom_ranks(data,m.from,m.to)
    result$rank.return<-mom_rank_return(data,m.from,m.to,m.window)
    result$ac.return<-mom_ac_return(data,m.from,m.to,m.window)
    result$m.from<-m.from
    result$m.to<-m.to
    result$m.window<-m.window
    result$ac.return.mean<-colMeans(result$ac.return)
    result$rank.return.mean<-colMeans(result$rank.return)
    result$n.classes<-ncol(data)
    summary<-matrix(NA,1,4,dimnames=list(c("Return"),c("BestRank","WorstRank","Best-Worst","AvgAC")))
    summary[1,"BestRank"]<-result$rank.return.mean[1]
    summary[1,"WorstRank"]<-result$rank.return.mean[result$n.classes]
    summary[1,"Best-Worst"]<-summary[1,"BestRank"]-summary[1,"WorstRank"]
    summary[1,"AvgAC"]<-mean(result$ac.return.mean)
    result$Summary<-summary
    result$Start.Date<-row.names(data)[1]
    result$End.Date<-row.names(data)[nrow(data)]
    return(result)
}

# Functions below are used for Flexible Asset Allocation.
# data is xts with daily prices if data.type="prices" otherwise it is returns
# date is through date so: a 10/31/YYYY return is the return through that date. 
mom_ac_hist_ret<-function(data,lookback,data.type="prices"){ #similar to mom_ac_backward_returns but data is xts with daily prices
    if (data.type=="prices"){
        returns.daily<-ROC(data,type="discrete",na.pad=F)
    } else {
        returns.daily<-data
    }
    monthendpts<-endpoints(returns.daily)
    result<-returns.daily[monthendpts[(lookback+1):length(monthendpts)],] #container for results
    for (i in (lookback+1):length(monthendpts)){
        temp<-returns.daily[(monthendpts[i-lookback]+1):monthendpts[i],]+1
        result[i-lookback,]<-apply(temp,2,prod)-1
    }
    return(result)
}
mom_ac_ret_rank<-function(data,lookback,data.type="prices"){
    #columns are assset classes, values are ranks
    score<-mom_ac_hist_ret(data,lookback,data.type)
    return(t(apply(-score,1,rank,ties.method="random")))
}
# Volatilty functions...data is an xts object with daily prices
vol_ac_hist_sd<-function(data,lookback,data.type="prices"){
    if (data.type=="prices"){
        returns.daily<-ROC(data,type="discrete",na.pad=F)
    } else {
        returns.daily<-data
    }
    monthendpts<-endpoints(returns.daily)
    result<-returns.daily[monthendpts[(lookback+1):length(monthendpts)],] #container for results
    for (i in (lookback+1):length(monthendpts)){
        temp<-returns.daily[(monthendpts[i-lookback]+1):monthendpts[i],]
        result[i-lookback,]<-apply(temp,2,sd)
    }
    return(result)
}
vol_ac_sd_rank<-function(data,lookback,data.type="prices"){
    #columns are assset classes, values are ranks
    ac.sd<-vol_ac_hist_sd(data,lookback,data.type="prices")
    return(t(apply(ac.sd,1,rank,ties.method="random")))
}

cor_ac_hist_mean<-function(data,lookback,data.type="prices"){ #columns are asset classes, values are avg correlations to other asset classes
    if (data.type=="prices"){
        returns.daily<-ROC(data,type="discrete",na.pad=F)
    } else {
        returns.daily<-data
    }
    monthendpts<-endpoints(returns.daily)
    result<-returns.daily[monthendpts[(lookback+1):length(monthendpts)],] #container for results
    for (i in (lookback+1):length(monthendpts)){
        temp<-cor(returns.daily[(monthendpts[i-lookback]+1):monthendpts[i],])
        result[i-lookback,]<-colMeans(temp)
    }
    return(result)
}

cor_ac_cor_rank<-function(data,lookback,data.type="prices"){
    #columns are assset classes, values are ranks
    ac.cor<-cor_ac_hist_mean(data,lookback,data.type="prices")
    return(t(apply(ac.cor,1,rank,ties.method="random")))    
}

cov_hist<-function(data,lookback,data.type="prices"){ #columns are asset classes, returns list of cov matrices
    if (data.type=="prices"){
        returns.daily<-ROC(data,type="discrete",na.pad=F)
    } else {
        returns.daily<-data
    }
    monthendpts<-endpoints(returns.daily)
    result<-list()
    for (i in (lookback+1):length(monthendpts)){
        item.name<-as.character(index(data)[monthendpts[i]])
        result[[item.name]]<-cov(returns.daily[(monthendpts[i-lookback]+1):monthendpts[i],])
    }
    return(result)
}
shp_ac_hist_shp<-function(data,lookback,data.type="prices"){
  ret<-mom_ac_hist_ret(data,lookback,data.type)
  sd<-vol_ac_hist_sd(data,lookback,data.type)
  return(ret/sd)
}


shp_ac_shp_rank<-function(data,lookback,data.type="prices"){
  #columns are assset classes, values are ranks
  ac.shp<-shp_ac_hist_shp(data,lookback,data.type)
  return(t(apply(-ac.shp,1,rank,ties.method="random")))
}

shp_ranks<-function(data,m.from=6,data.type="prices"){
  #Asset classes according to sharpe rank. columns are ranks, values are asset class (by column number according to data)
  ranks<-shp_ac_shp_rank(data,m.from,data.type)
  result<-t(apply(ranks, 1, function(x) match(seq(1,ncol(ranks)),x) ))
  colnames(result)<-paste("Rank" , 1:ncol(ranks), sep="")
  return(result)
}

shp_rank_return<-function(data,m.from=6,m.window=1,data.type="prices"){
  #returns for the rank looking ahead for a window of m.window periods
  rank.ac<-shp_ranks(data,m.from,data.type) #which asset classes have rank==1 , rank==2 etc
  returns<-data
  returns<-returns[-(1:(m.from)),] #Remove first rowsto account for m.from
  returns<-returns[-((nrow(returns)-m.window+2):nrow(returns)),] #remove rows to account for m.window
  colnames(returns)<-colnames(rank.ac)
  for (i in 1:nrow(returns)){
    ifelse(m.window==1,
           returns[i,]<-data[(m.from+i),rank.ac[i,]],
           returns[i,]<-apply(1+data[(m.from+i):(m.from+i+m.window-1),rank.ac[i,]],2,prod)-1)
  }
  return(returns)
}

shp_study<-function(data,m.from=6,m.window=1,data.type="prices",n.top=1){
    nper_per_yr<-12 #assumes monthly data
    result<-list()
    result$ranks<-shp_ac_shp_rank(data,m.from,data.type)
    result$rank.return<-shp_rank_return(data,m.from,m.window,data.type)
    result$m.from<-m.from
    result$m.window<-m.window
    result$rank.return.mean<-colMeans(result$rank.return)
    result$n.classes<-ncol(data)
    summary<-matrix(NA,1,3,dimnames=list(c("Return"),c("BestAvg","AllAvg","Best-Avg")))
    summary[1,"BestAvg"]<-mean(result$rank.return.mean[1:n.top])
    summary[1,"AllAvg"]<-mean(result$rank.return.mean)
    summary[1,"Best-Avg"]<-summary[1,"BestAvg"]-summary[1,"AllAvg"]
    result$Summary<-summary
    result$Start.Date<-row.names(data)[1]
    result$End.Date<-row.names(data)[nrow(data)]
    return(result)
}
