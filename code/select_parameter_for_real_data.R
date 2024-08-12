
#---给real data 选最优lambda1---#

Q=25#real data的等级设置大一点，这样画lorenz curve会更平滑
n=51
T=5
loglikelihood=array(0,c(n,T,300))
loglikelihood_sum=c()
deviance_bar=c()
loglikelihood_dahlans=matrix(0,n,T)
deviance_bar_Dahl=c()
#---DIC---#
for (lambda1 in seq(0,3,0.1)) {#31个模型参数
  result_real=
    CDMFM_new1(data= multinomial_data_1 , niterations=500, lambda1=lambda1 , neighbour=1 , distance=distance , alpha=rep(1:Q) , GAMMA = 1 , LAMBDA = 1 , initNClusters=5 , VN=VN)
  #写loglikelihood function，将每次迭代的参数估计值代入
  for (q in 201:500) { #从第201次迭代开始计算,舍去前200次
    for (j in 1:T) {
      for (i in 1:n) {
        loglikelihood[i,j,q-200]=dmultinom(x=multinomial_data_1[i,,j],
                                            size = NULL, 
                                            prob=result_real[["Iterates"]][[q]][["phiout"]][result_real[["Iterates"]][[q]][["zout"]][i],],
                                            log = TRUE)#计算每次迭代时，洲数据对应概率估计的对数似然值
      }
    }
  }
  for (q in 201:500) {
    loglikelihood_sum[q-200]=sum(loglikelihood[,,q-200])
  }
  #Deviance=-2*loglikelihood
  deviance=-2*loglikelihood_sum#迭代300次的deviance被记录下来
  #D(theta)_bar
  deviance_bar[10*lambda1+1]=sum(deviance)/300#求迭代的deviance均值
  #D(theta_bar)#求theta_bar的deviance
  DahlAns<-getDahl(result_real,burn=200)
  for (j in 1:T) {
    for (i in 1:n) {
      loglikelihood_dahlans[i,j]=dmultinom(x=multinomial_data_1[i,,j],
                                             size = NULL, 
                                             prob= DahlAns[["phiout"]][DahlAns[["zout"]][i],],
                                             log = TRUE)#计算在最优迭代下，每个洲数据对应概率估计的对数似然值
     }
  }
  for (j in 1:T) {
    for (i in 1:n) {
      loglikelihood_sum_dahlans=sum(loglikelihood_dahlans)
    }
  }
  deviance_bar_Dahl[10*lambda1+1]=-2*loglikelihood_sum_dahlans
}

#pD——参数复杂度
pD=deviance_bar-deviance_bar_Dahl
#DIC
DIC=deviance_bar+pD
#根据最小的DIC选出最优的lambda1
lambda1_real_data=seq(0,3,0.1)[which.min(DIC)]

#---调用本文算法(CDMFM_new1)---#

result=
  CDMFM_new1(data=multinomial_data_1 , niterations=500, lambda1=lambda1_real_data , neighbour=1 , distance=distance , alpha=rep(1:Q) , GAMMA = 1 , LAMBDA = 1 , initNClusters=5 , VN=VN)
DahlAns<-getDahl(result,burn=200)

result_0=
  CDMFM_new1(data=multinomial_data_1 , niterations=500, lambda1=0 , neighbour=1 , distance=distance , alpha=rep(1:Q) , GAMMA = 1 , LAMBDA = 1 , initNClusters=5 , VN=VN)
DahlAns_0<-getDahl(result_0,burn=200)






