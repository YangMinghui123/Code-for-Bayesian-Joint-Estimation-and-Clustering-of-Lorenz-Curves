#---处理income data(转化成矩阵，51行20列)---#
#51代表美国洲的数量，20代表将收入等级划分为10类#
library(DirichletReg)
library(usmap)
library(ggplot2)
library(DescTools)
library(magrittr)
library(dplyr)
library(ggpubr)

#----将真实收入数据转换成收入等级矩阵---#

load("D:/Edge Download/income_data.Rdata")
income_data<-data
state_fips_name <- readRDS("D:/Edge Download/state_fips_name.rds")#载入洲名称数据
state_fips<-as.numeric(state_fips_name$fips)#将洲数据转化成数值型
Q<-25#收入等级超过30则会报错：too few positive probabilities
multinomial_data<-matrix(0,51,Q)#将收入等级设置为Q
for(i in 1:51){
  income_data_new<-subset(income_data,data.ST==state_fips[i],select = data.HINCP)%>%#选第i个洲的收入数据
    arrange(data.HINCP)%>%#给收入数据从小到大排序
    mutate(accumulate_population = rank(data.HINCP)/n())%>%#计算人口累计百分比
    mutate(accumulate_Income = cumsum(as.numeric(data.HINCP))/sum(data.HINCP)) #计算收入累计百分比
  for (j in 1:Q) {#求各洲的Q个收入等级的人口数
    multinomial_data[i,j]<-sum(income_data_new$accumulate_Income<j/Q&income_data_new$accumulate_Income>=(j-1)/Q)
  }
}

#---按照算法得到聚类结果---#

multinomial_data_1<-array(0,c(51,Q,5))#将真实收入数据设置为数组，T=5
multinomial_data_1[,,1]<-multinomial_data
distance=readRDS("D:/Edge Download/USgdist.rds")
#result=
#  CDMFM_new1(data=multinomial_data_1 , niterations=500, lambda1=1 , neighbour=1 , distance=distance , alpha=rep(1:Q) , GAMMA = 1 , LAMBDA = 1 , initNClusters=5 , VN=VN)
# lambda1 is spatial parameter, neighbour=1, distance is graph distance calculated by adjmatrix.
#lambda1越大，分的类别越少。lambda1=0,0.5,1,1.5,2


#---选出最优迭代，并得到其聚类结果---#

#DahlAns<-getDahl(result,burn=200)#从第200次迭代之后选最优迭代
