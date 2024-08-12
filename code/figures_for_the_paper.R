
#---画各种图---#

#install.packages("REAT")
#install.packages("patchwork")
library(REAT)
library(reshape2)
library(patchwork)


#--- 对real data计算Gini index、画lorenz curve ---#

#计算各洲的基尼系数
Gini=c()
for (i in 1:51) {
  income_data_new<-subset(income_data,data.ST==state_fips[i],select = data.HINCP)%>%#选第i个洲的收入数据
    arrange(data.HINCP)%>%#给收入数据从小到大排序
    mutate(accumulate_population = rank(data.HINCP)/n())%>%#计算人口累计百分比
    mutate(accumulate_Income = cumsum(as.numeric(data.HINCP))/sum(data.HINCP))#计算收入累计百分比
    Gini[i] = sum(2*(income_data_new$accumulate_population - income_data_new$accumulate_Income)/nrow(income_data_new))
}
#计算每个类别的平均基尼系数
gini=rep(0,length(unique(DahlAns$zout)))
t1=t2=t3=t4=t5=0
for (j in 1:51) {
  if (DahlAns$zout[j]==1) {
    gini[1]=gini[1]+Gini[j]
    t1=t1+1
  }
  if (DahlAns$zout[j]==2) {
    gini[2]=gini[2]+Gini[j]
    t2=t2+1
  }
  if (DahlAns$zout[j]==3) {
    gini[3]=gini[3]+Gini[j]
    t3=t3+1
  }
  if (DahlAns$zout[j]==4) {
    gini[4]=gini[4]+Gini[j]
    t4=t4+1
  }
  if (DahlAns$zout[j]==5) {
    gini[5]=gini[5]+Gini[j]
    t5=t5+1
  }
}
gini=c(gini[1]/t1,gini[2]/t2,gini[3]/t3,gini[4]/t4,gini[5]/t5)


#计算各洲的基尼系数
Gini_0=c()
for (i in 1:51) {
  income_data_new<-subset(income_data,data.ST==state_fips[i],select = data.HINCP)%>%#选第i个洲的收入数据
    arrange(data.HINCP)%>%#给收入数据从小到大排序
    mutate(accumulate_population = rank(data.HINCP)/n())%>%#计算人口累计百分比
    mutate(accumulate_Income = cumsum(as.numeric(data.HINCP))/sum(data.HINCP))#计算收入累计百分比
  Gini_0[i] = sum(2*(income_data_new$accumulate_population - income_data_new$accumulate_Income)/nrow(income_data_new))
}
#计算每个类别的平均基尼系数
gini_0=rep(0,length(unique(DahlAns_0$zout)))
t1=t2=t3=t4=t5=0
for (j in 1:51) {
  if (DahlAns_0$zout[j]==1) {
    gini_0[1]=gini_0[1]+Gini_0[j]
    t1=t1+1
  }
  if (DahlAns_0$zout[j]==2) {
    gini_0[2]=gini_0[2]+Gini_0[j]
    t2=t2+1
  }
  if (DahlAns_0$zout[j]==3) {
    gini_0[3]=gini_0[3]+Gini_0[j]
    t3=t3+1
  }
  if (DahlAns_0$zout[j]==4) {
    gini_0[4]=gini_0[4]+Gini_0[j]
    t4=t4+1
  }
  if (DahlAns_0$zout[j]==5) {
    gini_0[5]=gini_0[5]+Gini_0[j]
    t5=t5+1
  }
}
gini_0=c(gini_0[1]/t1,gini_0[2]/t2,gini_0[3]/t3,gini_0[4]/t4,gini_0[5]/t5)

#---根据类别画lorenz curve---#

lorenz_data=matrix(0,Q,length(unique(DahlAns$zout)))
for (i in 1:length(unique(DahlAns$zout))) {
  lorenz_data[,i]=cumsum(DahlAns$phiout[i,])
}#求累计概率
#为画图调整数据
lorenz_data=rbind(rep(0,length(unique(DahlAns$zout))),lorenz_data)
lorenz_data=cbind(lorenz_data,seq(0,1,0.04))
colnames(lorenz_data)=c("Cluster 1 (0.43)","Cluster 2 (0.46)","Cluster 3 (0.48)","Absolute average line")#调整将要画lorenz曲线的数据
lorenz_data=melt(lorenz_data)
colnames(lorenz_data)=c("y","Cluster","value")
lorenz_data$y=seq(0,1,0.04)
color=c("#f0e54b","#2b9f78", "#5cb6ea","#000000")
lorenz_curve=ggplot(lorenz_data)+
  geom_line(aes(x=value,y=y,color=Cluster,linetype=Cluster),size=0.6)+
  scale_color_manual(values = color)+
  scale_linetype_manual(values = c("dashed","dotdash","twodash","solid"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x = "% of Population", y = "% of Income",title = "(b)")


lorenz_data_0=matrix(0,Q,length(unique(DahlAns_0$zout)))
for (i in 1:length(unique(DahlAns_0$zout))) {
  lorenz_data_0[,i]=cumsum(DahlAns_0$phiout[i,])
}#求累计概率
#为画图调整数据
lorenz_data_0=rbind(rep(0,length(unique(DahlAns_0$zout))),lorenz_data_0)
lorenz_data_0=cbind(lorenz_data_0,seq(0,1,0.04))
colnames(lorenz_data_0)=c("Cluster 1 (0.43)","Cluster 2 (0.50)","Cluster 3 (0.46)","Cluster 4 (0.49)","Cluster 5 (0.48)","Absolute average line")#调整将要画lorenz曲线的数据
lorenz_data_0=melt(lorenz_data_0)
colnames(lorenz_data_0)=c("y","Cluster","value")
lorenz_data_0$y=seq(0,1,0.04)
color_0=c("#f0e54b","#e5a11c","#2b9f78", "#cc7daa","#5cb6ea","#000000")
lorenz_curve_0=ggplot(lorenz_data_0)+
  geom_line(aes(x=value,y=y,color=Cluster,linetype=Cluster),size=0.6)+
  scale_color_manual(values = color_0)+
  scale_linetype_manual(values = c("dashed","dotdash","twodash","dotted","longdash","solid"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x = "% of Population", y = "% of Income",title = "(d)") 



#---根据收入等级矩阵的聚类结果画图---#

stateinfo <- readRDS("D:/Edge Download/state_fips_name.rds")#导入洲名数据
stateinfo$Cluster <- as.factor(DahlAns[["zout"]])#根据聚类结果给各洲赋上类别
usmap=plot_usmap(data = stateinfo, values = "Cluster", labels = TRUE) +
  scale_fill_manual(values = color)+#根据聚类结果画美国地图
  labs(title="(a)")

color_0=c("#f0e54b","#e5a11c","#2b9f78","#cc7daa","#5cb6ea")
stateinfo_0 <- readRDS("D:/Edge Download/state_fips_name.rds")#导入洲名数据
stateinfo_0$Cluster <- as.factor(DahlAns_0[["zout"]])#根据聚类结果给各洲赋上类别
usmap0=plot_usmap(data = stateinfo_0, values = "Cluster", labels = TRUE) +
  scale_fill_manual(values = color_0)+#根据聚类结果画美国地图
  labs(title="(c)")


#把四张图画在一起（两张地图+两张洛伦兹曲线）
usmap+lorenz_curve+usmap0+lorenz_curve_0+plot_layout(heights = c(1, 1),widths=c(2,1))



#---画ARI大小比较的直方图---#

ARI_MFM_simu1_design1=rep(0,100)
ARI_APcluster_simu1_design1=rep(0,100)
ARI_DBSCAN_simu1_design1=rep(0,100)
ARI_Kmeans_simu1_design1=rep(0,100)
ARI_Model_based_simu1_design1=rep(0,100)
ARI_Optics_simu1_design1=rep(0,100)

ARI_MFM_simu2_design1=rep(0,100)
ARI_APcluster_simu2_design1=rep(0,100)
ARI_DBSCAN_simu2_design1=rep(0,100)
ARI_Kmeans_simu2_design1=rep(0,100)
ARI_Model_based_simu2_design1=rep(0,100)
ARI_Optics_simu2_design1=rep(0,100)

ARI_MFM_simu1_design2=rep(0,100)
ARI_APcluster_simu1_design2=rep(0,100)
ARI_DBSCAN_simu1_design2=rep(0,100)
ARI_Kmeans_simu1_design2=rep(0,100)
ARI_Model_based_simu1_design2=rep(0,100)
ARI_Optics_simu1_design2=rep(0,100)

ARI_MFM_simu2_design2=rep(0,100)
ARI_APcluster_simu2_design2=rep(0,100)
ARI_DBSCAN_simu2_design2=rep(0,100)
ARI_Kmeans_simu2_design2=rep(0,100)
ARI_Model_based_simu2_design2=rep(0,100)
ARI_Optics_simu2_design2=rep(0,100)

#---画ARI对比图---#

ARI1=cbind(ARI_MFM_simu1_design1,ARI_APcluster_simu1_design1,ARI_DBSCAN_simu1_design1,ARI_Kmeans_simu1_design1,ARI_Model_based_simu1_design1,ARI_Optics_simu1_design1)
ARI1=as.data.frame(ARI1)
colnames(ARI1)=c("MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
ARI1=melt(ARI1)
ARI1$Setting="Design1"
ARI1$index= "simulation1"
colnames(ARI1)=c("method","value","Setting","index")

ARI2=cbind(ARI_MFM_simu2_design1,ARI_APcluster_simu2_design1,ARI_DBSCAN_simu2_design1,ARI_Kmeans_simu2_design1,ARI_Model_based_simu2_design1,ARI_Optics_simu2_design1)
ARI2=as.data.frame(ARI2)
colnames(ARI2)=c("MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
ARI2=melt(ARI2)
ARI2$Setting="Design1"
ARI2$index= "simulation2"
colnames(ARI2)=c("method","value","Setting","index")

ARI3=cbind(ARI_MFM_simu1_design2,ARI_APcluster_simu1_design2,ARI_DBSCAN_simu1_design2,ARI_Kmeans_simu1_design2,ARI_Model_based_simu1_design2,ARI_Optics_simu1_design2)
ARI3=as.data.frame(ARI3)
colnames(ARI3)=c("MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
ARI3=melt(ARI3)
ARI3$Setting="Design2"
ARI3$index= "simulation1"
colnames(ARI3)=c("method","value","Setting","index")

ARI4=cbind(ARI_MFM_simu2_design2,ARI_APcluster_simu2_design2,ARI_DBSCAN_simu2_design2,ARI_Kmeans_simu2_design2,ARI_Model_based_simu2_design2,ARI_Optics_simu2_design2)
ARI4=as.data.frame(ARI4)
colnames(ARI4)=c("MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
ARI4=melt(ARI4)
ARI4$Setting="Design2"
ARI4$index= "simulation2"
colnames(ARI4)=c("method","value","Setting","index")

ARI=rbind(ARI1,ARI2,ARI3,ARI4)
ARI=ggplot(ARI,aes(x=value))+
  geom_bar(bins=100,stat="count",width = 0.5)+ 
  scale_x_discrete(limits = c("equal","excess","deficiency"))+
  theme_bw()+
  facet_grid(Setting+index~method)+
  labs(title="(b)",x ="Comparison of the size of ARI", y = "Frequency")



#---画类别个数分布图---#

#design1(simulation1)
cluster_simu1_design1=cbind(cluster_CDMFM1_design1,cluster_CDMFM1_non_design1,cluster_ap1_design1,cluster_dbscan1_design1,cluster_kmeans1_design1,cluster_Mclust1_design1,cluster_optics1_design1)
cluster_simu1_design1=as.data.frame(cluster_simu1_design1)
colnames(cluster_simu1_design1)=c("CDMFM","MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
cluster_simu1_design1=melt(cluster_simu1_design1)
cluster_simu1_design1$setting="Design1"
cluster_simu1_design1$index="Simulation1"
colnames(cluster_simu1_design1)=c("Method","value","setting","index")

#design1(simulation2)
cluster_simu2_design1=cbind(cluster_CDMFM2_design1,cluster_CDMFM2_non_design1,cluster_ap2_design1,cluster_dbscan2_design1,cluster_kmeans2_design1,cluster_Mclust2_design1,cluster_optics2_design1)
cluster_simu2_design1=as.data.frame(cluster_simu2_design1)
colnames(cluster_simu2_design1)=c("CDMFM","MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
cluster_simu2_design1=melt(cluster_simu2_design1)
cluster_simu2_design1$setting="Design1"
cluster_simu2_design1$index="Simulation2"
colnames(cluster_simu2_design1)=c("Method","value","setting","index")

#design2(simulation1)
cluster_simu1_design2=cbind(cluster_CDMFM1_design2,cluster_CDMFM1_non_design2,cluster_ap1_design2,cluster_dbscan1_design2,cluster_kmeans1_design2,cluster_Mclust1_design2,cluster_optics1_design2)
cluster_simu1_design2=as.data.frame(cluster_simu1_design2)
colnames(cluster_simu1_design2)=c("CDMFM","MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
cluster_simu1_design2=melt(cluster_simu1_design2)
cluster_simu1_design2$setting="Design2"
cluster_simu1_design2$index="Simulation1"
colnames(cluster_simu1_design2)=c("Method","value","setting","index")

#design2(simulation2)
cluster_simu2_design2=cbind(cluster_CDMFM2_design2,cluster_CDMFM2_non_design2,cluster_ap2_design2,cluster_dbscan2_design2,cluster_kmeans2_design2,cluster_Mclust2_design2,cluster_optics2_design2)
cluster_simu2_design2=as.data.frame(cluster_simu2_design2)
colnames(cluster_simu2_design2)=c("CDMFM","MFM","APcluster","DBSCAN","Kmeans","Model-based","Optics")
cluster_simu2_design2=melt(cluster_simu2_design2)
cluster_simu2_design2$setting="Design2"
cluster_simu2_design2$index="Simulation2"
colnames(cluster_simu2_design2)=c("Method","value","setting","index")

#拼接4张图
cluster=rbind(cluster_simu1_design1,cluster_simu2_design1,cluster_simu1_design2,cluster_simu2_design2)
cluster=ggplot(cluster,aes(x=value))+
  geom_histogram(bins=100,stat="count",colour = "#000000",fill=c("#666666"))+ 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9"))+
  theme_bw()+
  facet_grid(setting+index~Method)+
  labs(x ="K", y = "Frequency")

#将两幅拼接图放在一起
cluster/ARI+plot_layout(heights = c(1, 1))

#计算平均lambda1
lambda1_avg_disjoint_simulation_data1=mean(lambda1_disjoint_simulation_data1)
lambda1_avg_disjoint_simulation_data2=mean(lambda1_disjoint_simulation_data2)
lambda1_avg_joint_simulation_data1=mean(lambda1_joint_simulation_data1)
lambda1_avg_joint_simulation_data2=mean(lambda1_joint_simulation_data2)




#---洛伦兹曲线的比较---#
library(REAT)
par(mfrow = c(1,2))
lorenz(data$data.HINCP,  bg.col = "white", lctitle = "(a)")
## Minnesota
lorenz(data$data.HINCP[data$data.ST == 27], lctitle = "", bg.col = "white",
       lc.col =  "#E69F00", add.lc = TRUE, ltype = "dashed")
## New York
lorenz(data$data.HINCP[data$data.ST == 36], lctitle = "", bg.col = "white",
       lc.col = "#56B4E9", add.lc = TRUE, ltype = "dotdash")
legend("topleft", legend = c("National", "Minnesota", "New York"),
       col = c("black", "#E69F00", "#56B4E9"),
       lty = c("solid", "dashed", "longdash"))

lorenz(data$data.HINCP, lctitle = "(b)", bg.col = "white")
for (i in unique(data$data.ST)) {
  lorenz(data$data.HINCP[data$data.ST == i], lctitle = "", bg.col = "white",
         lc.col =  "grey", add.lc = TRUE, ltype = "dashed")
}
lorenz(data$data.HINCP, lctitle = "", bg.col = "white", add.lc = TRUE)

#---画gini系数图和平均收入图，用地图表示---#
library(purrr)
library(DescTools)
index <- unique(data$data.ST)
medincome <- map_dbl(index, ~median(data$data.HINCP[data$data.ST == .x]))
Gini_coefficient <- rep(0, length(index))
for (i in 1:51) {
  income_data_new<-subset(income_data,data.ST==state_fips[i],select = data.HINCP)%>%#选第i个洲的收入数据
    arrange(data.HINCP)%>%#给收入数据从小到大排序
    mutate(accumulate_population = rank(data.HINCP)/n())%>%#计算人口累计百分比
    mutate(accumulate_Income = cumsum(as.numeric(data.HINCP))/sum(data.HINCP))#计算收入累计百分比
  Gini_coefficient[i] = sum(2*(income_data_new$accumulate_population - income_data_new$accumulate_Income)/nrow(income_data_new))
}
stateinfo <- stateinfo %>% mutate(fips = as.numeric(fips)) %>%
  mutate(Gini = Gini_coefficient) %>%
  mutate(medincome = medincome)

gini <- plot_usmap(data = stateinfo, value = "Gini", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "#004D40", name = "Gini coefficient") +
  ggtitle("(a)")

income <- plot_usmap(data = stateinfo, value = "medincome", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "#004D40", name = "Median income") +
  ggtitle("(b)")
gridExtra::grid.arrange(gini, income, nrow = 1)


#---两种模拟设置---#
color=c("#2b9f78", "#5cb6ea", "#e5a11c")
plot_disjoint_map=plot_usmap(data = stateinfo_disjoint_init, values = "cluster", labels = TRUE) +
  scale_fill_manual(values = color)+ggtitle("(a)")
plot_joint_map=plot_usmap(data = stateinfo_joint_init, values = "cluster", labels = TRUE) +
  scale_fill_manual(values = color)+ggtitle("(b)")
gridExtra::grid.arrange(plot_disjoint_map, plot_joint_map, nrow = 1)



