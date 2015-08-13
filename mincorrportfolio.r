min.corr.portfolio <- function
(
    ia				# input assumptions $correlation, $cov
)
    #modified rpm for 1 and 2 asset classes
{
    if (nrow(ia$correlation)==1){
        return(1) # one asset class gets 100% wt
    }
    if (nrow(ia$correlation)==2){
        x = 1 / sqrt( diag(ia$cov) )
        return(x/sum(x)) # one asset class gets 100% wt
    }
    upper.index = upper.tri(ia$correlation)
    cor.m = ia$correlation[upper.index]
    cor.mu = mean(cor.m)
    cor.sd = sd(cor.m)
    
    norm.dist.m = 0 * ia$correlation	
    diag(norm.dist.m) = NA
    norm.dist.m[upper.index] = 1-pnorm(cor.m, cor.mu, cor.sd)
    norm.dist.m = (norm.dist.m + t(norm.dist.m))
    
    norm.dist.avg = rowMeans(norm.dist.m, na.rm=T)
    
    norm.dist.rank = rank(-norm.dist.avg)
    
    norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
    
    diag(norm.dist.m) = 0
    weighted.norm.dist.average = norm.dist.weight %*% norm.dist.m
    
    final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
    
    # re-scale weights to penalize for risk
    x = final.weight
    x = x / sqrt( diag(ia$cov) )
    
    # normalize weights to sum up to 1
    return( x / sum(x) )
}		