x <- (retorno[2:nrow(retorno),ac_util])
x2 = as.matrix(x)
media_retorno = meanEstimation(x2)

covlw = covEstimation(x2, control = list(type = 'lw')) 

ws <- optimalPortfolio(Sigma = Sigma, mu=media_retorno, control = list(type='minvol'))

ws2 <- optimalPortfolio(Sigma = S.hat_shrink2, mu=media_retorno, control = list(type='minvol')) 

length(2:length(retorno))
res1 <- portfolio.optim(x, pm=media_retorno)

res2 <- portfolio.optim(x ,cov=covlw)

library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
data(edhec)
R <- edhec[, 1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQMN", "ED")
funds <- colnames(x)
# Create an initial portfolio object with leverage and box constraints
init <- portfolio.spec(assets=funds)
init <- add.constraint(portfolio=init, type="full_investiment")
init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.95)
rportfolios <- random_portfolios(init, permutations = 500, rp_method = "sample")
maxret <- add.objective(portfolio=init, type="return", name="mean")
minvar <- add.objective(portfolio=init, type="risk", name="var")
#Run the optimization.
opt_maxret<- optimize.portfolio(x, portfolio=maxret, optimize_method="GenSA",trace=TRUE)
print(opt_maxret)
plot(opt_maxret, risk.col="StdDev", return.col="mean",main="Maximum Return Optimization", chart.assets=TRUE,xlim=c(0, 0.05), ylim=c(0,0.0085))



sigma.robust <- function(R){
      require(RiskPortfolios)
      out <- list()
      set.seed(1234)  
      out$sigma <- covEstimation(as.matrix(x), control = list(type = 'lw'))
      return(out)
}  



opt_maxret2 <- optimize.portfolio(x, portfolio=maxret, optimize_method="GenSA",trace=TRUE, momentFUN = "sigma.robust")
print(opt_maxret2)
plot(opt_maxret2, risk.col="StdDev", return.col="mean",main="Maximum Return Optimization", chart.assets=TRUE,xlim=c(0, 0.05), ylim=c(0,0.0085))



t = round(corr,2)
ver = numeric(nrow(t))
for(i in 1:nrow(t)){
  for(j in 1:ncol(t)){
    if(t[i,j] < -0.6){
      ver[i]=ver[i] + 1
    } 
  }
}
ind = which(ver!=0)
value = rownames(t)
ac_util = value[ind]