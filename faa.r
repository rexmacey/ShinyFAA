#Flexible AA Study
faa<-function(data,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=.5,cash.col=1,data.type="prices",tc=0){
    return(faa.min.corr(data,lookback,wt,n.top,cash.col,data.type,tc))
}

faa.signal<-function(data,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=.5,cash.col=1,data.type="prices",tc=0){
    # faa w transaction costs. tc is the 1 way transaction costs.  1=100%
    # data xts of daily prices
    # cash.col columns which is the cash substitute when abs mom is negative
    lookback.mom<-lookback[1] ; lookback.vol<-lookback[2] ; lookback.cor<-lookback[3] # months to lookback to calc values
    wt.mom<-wt[1] ;    wt.vol<-wt[2] ;    wt.cor<-wt[3] #wts of ranks in scoreing
    if (n.top==.5) n.top<- trunc(ncol(data)/2) #if set to .5, use top half of asset classes
    sec.names<-names(data)
    source("momentum.r")
    library(timeSeries)
    library(quantmod)
    library(xts)
    library(TTR)
    ac.ret<-mom_ac_hist_ret(data,lookback.mom,data.type)
    ac.ret.rank<-mom_ac_ret_rank(data,lookback.mom,data.type)
    ac.sd<-vol_ac_hist_sd(data,lookback.vol,data.type)
    ac.sd.rank<-vol_ac_sd_rank(data,lookback.vol,data.type) #ranks of each asset class
    ac.cor.mean<-cor_ac_hist_mean(data,lookback.cor,data.type)
    ac.cor.rank<-cor_ac_cor_rank(data,lookback.cor,data.type)
    #align them all if the lookbacks are different
    ac.ret<-ac.ret[(1+max(lookback)-lookback.mom):nrow(ac.ret),]
    ac.ret.rank<-ac.ret.rank[(1+max(lookback)-lookback.mom):nrow(ac.ret.rank),]
    ac.sd<-ac.sd[(1+max(lookback)-lookback.vol):nrow(ac.sd),]
    ac.sd.rank<-ac.sd.rank[(1+max(lookback)-lookback.vol):nrow(ac.sd.rank),]
    ac.cor.mean<-ac.cor.mean[(1+max(lookback)-lookback.cor):nrow(ac.cor.mean),]
    ac.cor.rank<-ac.cor.rank[(1+max(lookback)-lookback.cor):nrow(ac.cor.rank),]
    #score the ac for each month, lowest is best
    #ac.score<-ac.ret.rank*wt.mom+ac.sd.rank*wt.vol+ac.cor.rank*wt.cor
    #ac.score<- -ac.ret/ac.sd*wt.mom + ac.cor.mean*wt.cor #negate to make lower better.
    #2015-02-02 adjust sd to same period as year
    ac.score<- -ac.ret/(ac.sd*sqrt(252*lookback.mom/12))*wt.mom + ac.cor.mean*wt.cor #negate to make lower better.
    ac.score.rank<-t(apply(ac.score,1,rank,ties.method="random"))
    ac.topn<-t(apply(ac.score.rank, 1, function(x) match(seq(1,n.top),x) ))
    colnames(ac.topn)<-paste("Rank" , 1:n.top, sep="")
    head(ac.topn)
    # implement abs ret. Repace those with negative momentum with cash 
    ac.topn.abs<-ac.topn
    ac.topn.ret<-t(sapply(seq(1,length(ac.topn[,1])),function(i) ac.ret[i,ac.topn[i,]])) # returns corresponding to top ranks
    neg.idx<-ac.topn.ret<0
    ac.topn.abs[neg.idx]<-cash.col  #invest in these asset classes for the subsequent month
    #calc returns for strategy
#     ac.returns.monthly<-mom_ac_hist_ret(data,1,data.type)
#     ac.returns.monthly<-ac.returns.monthly[row.names(ac.topn.abs)]
#     # to deal with lag, remove first monthly return and last obs of asset class rank
#     ac.topn.abs<-ac.topn.abs[-nrow(ac.topn.abs),] #remove last row. No data to calc the next month's return
#     ac.returns.monthly<-ac.returns.monthly[-1,]
#     benchmark.returns<-rowMeans(ac.returns.monthly) #equal weighted
#     strategy.returns<-t(sapply(seq(1,length(ac.topn.abs[,1])),function(i) ac.returns.monthly[i,ac.topn.abs[i,]]))
#     strategy.returns<-rowMeans(strategy.returns)
#     study.returns<-merge.xts(strategy.returns,benchmark.returns,ac.returns.monthly)
    allocations<-matrix(0,nrow=nrow(ac.topn),ncol=length(sec.names),dimnames=list(row.names(ac.topn),sec.names)) #container
    row.names(allocations)<-row.names(ac.topn)
    for (i in 1:length(allocations[,1])){
        for (j in 1:n.top){
            allocations[i,ac.topn.abs[i,j]]<-allocations[i,ac.topn.abs[i,j]]+1/n.top
        }
    }
     transactions<-diff(as.matrix(allocations))
     turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
     tc.monthly<-turnover.monthly*tc*2
#     strategy.returns.net<-strategy.returns #container
#     strategy.returns.net[2:length(strategy.returns.net)]<-strategy.returns.net[2:length(strategy.returns.net)]-
#         tc.monthly
#     study.returns[,1]<-strategy.returns.net
    turnover.mean<-mean(turnover.monthly)
    result<-list()
    result$allocations<-allocations
    result$topn<-ac.topn.abs
    result$turnover.monthly<-turnover.monthly
    result$turnover.mean<-turnover.mean
    result$safe.ac<-cash.col
    result$return.value<-ac.ret
    result$return.rank<-ac.ret.rank
    result$sd.value<-ac.sd
    result$sd.rank<-ac.sd.rank
    result$cor.value<-ac.cor.mean
    result$cor.rank<-ac.cor.rank
    result$score.value<-ac.score
    result$score.rank<-ac.score.rank
    return(result)
}

faa.min.corr<-function(data,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=.5,cash.col=1,data.type="prices",tc=0){
    # faa w transaction costs. tc is the 1 way transaction costs.  1=100%
    # data xts of daily prices
    # cash.col columns which is the cash substitute when abs mom is negative
    lookback.mom<-lookback[1] ; lookback.vol<-lookback[2] ; lookback.cor<-lookback[3] # months to lookback to calc values
    wt.mom<-wt[1] ;    wt.vol<-wt[2] ;    wt.cor<-wt[3] #wts of ranks in scoreing
    if (n.top==.5) n.top<- trunc(ncol(data)/2) #if set to .5, use top half of asset classes
    source("momentum.r")
    library(quantmod)
    library(xts)
    library(TTR)
    ac.ret<-mom_ac_hist_ret(data,lookback.mom,data.type)
    ac.ret.rank<-mom_ac_ret_rank(data,lookback.mom,data.type)
    ac.sd<-vol_ac_hist_sd(data,lookback.vol,data.type)
    ac.sd.rank<-vol_ac_sd_rank(data,lookback.vol,data.type) #ranks of each asset class
    ac.cor.mean<-cor_ac_hist_mean(data,lookback.cor,data.type)
    ac.cor.rank<-cor_ac_cor_rank(data,lookback.cor,data.type)
    ac.cov.hist<-cov_hist(data,lookback.vol,data.type)
    #align them all if the lookbacks are different
    ac.ret<-ac.ret[(1+max(lookback)-lookback.mom):nrow(ac.ret),]
    ac.ret.rank<-ac.ret.rank[(1+max(lookback)-lookback.mom):nrow(ac.ret.rank),]
    ac.sd<-ac.sd[(1+max(lookback)-lookback.vol):nrow(ac.sd),]
    ac.sd.rank<-ac.sd.rank[(1+max(lookback)-lookback.vol):nrow(ac.sd.rank),]
    ac.cor.mean<-ac.cor.mean[(1+max(lookback)-lookback.cor):nrow(ac.cor.mean),]
    ac.cor.rank<-ac.cor.rank[(1+max(lookback)-lookback.cor):nrow(ac.cor.rank),]
    ac.cov.hist<-ac.cov.hist[(1+max(lookback)-lookback.vol):length(ac.cov.hist)]
    #score the ac for each month, lowest is best
    #ac.score<- -ac.ret/ac.sd*wt.mom + ac.cor.mean*wt.cor #negate to make lower better.
    #2015-02-02 adjust sd to same period as year
    ac.score<- -ac.ret/(ac.sd*sqrt(252*lookback.mom/12))*wt.mom + ac.cor.mean*wt.cor #negate to make lower better.
    ac.score.rank<-t(apply(ac.score,1,rank,ties.method="random"))
    ac.topn<-t(apply(ac.score.rank, 1, function(x) match(seq(1,n.top),x) ))
    colnames(ac.topn)<-paste("Rank" , 1:n.top, sep="")
    # implement abs ret. Repace those with negative momentum with cash 
    ac.topn.abs<-ac.topn
    ac.topn.ret<-t(sapply(seq(1,length(ac.topn[,1])),function(i) ac.ret[i,ac.topn[i,]])) # returns corresponding to top ranks
    neg.idx<-ac.topn.ret<0
    ac.topn.abs[neg.idx]<-cash.col  #invest in these asset classes for the subsequent month
    #calc returns for strategy
    ac.returns.monthly<-mom_ac_hist_ret(data,1,data.type)
    ac.returns.monthly<-ac.returns.monthly[row.names(ac.topn.abs)]
    # to deal with lag, remove first monthly return and last obs of asset class rank
    ac.topn.abs<-ac.topn.abs[-nrow(ac.topn.abs),] #remove last row. No data to calc the next month's return
    ac.returns.monthly<-ac.returns.monthly[-1,]
    ac.cov.hist<-ac.cov.hist[-length(ac.cov.hist)] 
    benchmark.returns<-rowMeans(ac.returns.monthly) #equal weighted
    strategy.wts<-min.corr.wts(ac.cov.hist,ac.topn.abs,cash.col)
    strategy.returns<-t(sapply(seq(1,length(ac.topn.abs[,1])),function(i) ac.returns.monthly[i,ac.topn.abs[i,]]))
    strategy.returns<-rowSums(strategy.returns*strategy.wts)
    study.returns<-merge.xts(strategy.returns,benchmark.returns,ac.returns.monthly)
    allocations<-ac.returns.monthly #container
    allocations[,]<-0 #set all values to zero
    for (i in 1:length(allocations[,1])){
        for (j in 1:n.top){
            allocations[i,ac.topn.abs[i,j]]<-allocations[i,ac.topn.abs[i,j]]+strategy.wts[i,j]
        }
    }
    transactions<-diff(as.matrix(allocations))
    turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
    tc.monthly<-turnover.monthly*tc*2
    strategy.returns.net<-strategy.returns #container
    strategy.returns.net[2:length(strategy.returns.net)]<-strategy.returns.net[2:length(strategy.returns.net)]-
        tc.monthly
    study.returns[,1]<-strategy.returns.net
    turnover.mean<-mean(turnover.monthly)
    result<-list()
    result$returns<-study.returns
    result$allocations<-as.timeSeries(allocations)
    result$topn<-ac.topn.abs
    result$turnover.monthly<-turnover.monthly
    result$turnover.mean<-turnover.mean
    result$returns.gross<-strategy.returns
    result$safe.ac<-cash.col
    return(result)
}

min.corr.wts<-function(ac.cov.hist,ac.topn.abs,cash.col=1){
    #for each month build a cov matrix of just the top n classes
    n.classes<-ncol(ac.topn.abs)
    out<-matrix(NA,nr=nrow(ac.topn.abs),nc=n.classes)
    for (i in 1:nrow(ac.topn.abs)){
        ac.cov<-ac.cov.hist[[i]]
        ac.top<-ac.topn.abs[i,]
        cash.col.idx<-ac.top==cash.col  #these are cash columns and get 1/n.classes wt
        n.cash.cols<-sum(cash.col.idx) #how many such columns are there
        n.noncash.classes<-n.classes-n.cash.cols
        if (n.noncash.classes<2){ #special case, all cash or only 1 non-cash class
            out[i,]<-1/n.classes
        } else { #at least 2 non cash classes
            out[i,cash.col.idx]<-1/n.classes #cash columns get 1/n.assets wt other assets get the rest
            ac.noncash.top<-ac.top[!cash.col.idx]
            #build cov cor matrices of non cash columns
            cov.mat<-matrix(NA,nr=n.noncash.classes,nc=n.noncash.classes)
            cor.mat<-matrix(NA,nr=n.noncash.classes,nc=n.noncash.classes)
            for (j in 1:n.noncash.classes){
                for (k in 1:n.noncash.classes){
                    cov.mat[j,k]<-ac.cov[ac.noncash.top[j],ac.noncash.top[k]]
                }
            }
            std.dev<-sqrt(diag(cov.mat))
            for (j in 1:n.noncash.classes){
                for (k in 1:n.noncash.classes){
                    cor.mat[j,k]<-cov.mat[j,k]/(std.dev[j]*std.dev[k])
                }
            }
            cov.cor<-list()
            cov.cor$correlation<-cor.mat
            cov.cor$cov<-cov.mat
            noncash.wts<-min.corr.portfolio(cov.cor)*n.noncash.classes/n.classes
            out[i,!cash.col.idx]<-noncash.wts
        }
    }
    idx<-is.nan(rowSums(out))
    out[idx,]<-1/n.classes
    return(out)
}

