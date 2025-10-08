install.packages("blockra", repos="http://R-Forge.R-project.org")
library(blockra)

#########################################################
setwd("data/CDF_P_return/")           ## The physical c.d.f of single-asset price has changed to the physical c.d.f of single-asset return.
SPY = read.csv("SPY_2019.csv", check.names = FALSE)
XLB = read.csv("XLB_2019.csv", check.names = FALSE)
XLE = read.csv("XLE_2019.csv", check.names = FALSE)
XLF = read.csv("XLF_2019.csv", check.names = FALSE)
XLI = read.csv("XLI_2019.csv", check.names = FALSE)
XLK = read.csv("XLK_2019.csv", check.names = FALSE)
XLP = read.csv("XLP_2019.csv", check.names = FALSE)
XLU = read.csv("XLU_2019.csv", check.names = FALSE)
XLV = read.csv("XLV_2019.csv", check.names = FALSE)
XLY = read.csv("XLY_2019.csv", check.names = FALSE)

a = c(colnames(SPY))
a_1 = as.Date(a,"%Y/%m/%d")
colnames(SPY) = a_1
colnames(XLB) = a_1
colnames(XLE) = a_1
colnames(XLF) = a_1
colnames(XLI) = a_1
colnames(XLK) = a_1
colnames(XLP) = a_1
colnames(XLU) = a_1
colnames(XLV) = a_1
colnames(XLY) = a_1

weight = read.csv("data/weight_finally.csv")
weight$Date <- as.Date(weight$Date, format = "%Y/%m/%d")
weight$Date <- format(weight$Date, "%Y-%m-%d")


#########################################################
day = ncol(SPY)
zhangliang=array(dim=c(1000,10,4))

#########################################################
for (mm in 1:4){
  date = colnames(SPY)[mm]
  SPY_r = as.numeric(unlist(SPY[colnames(SPY)==date]))
  XLB_r = as.numeric(unlist(XLB[colnames(XLB)==date]))
  XLE_r = as.numeric(unlist(XLE[colnames(XLE)==date]))
  XLF_r = as.numeric(unlist(XLF[colnames(XLF)==date]))
  XLI_r = as.numeric(unlist(XLI[colnames(XLI)==date]))
  XLK_r = as.numeric(unlist(XLK[colnames(XLK)==date]))
  XLP_r = as.numeric(unlist(XLP[colnames(XLP)==date]))
  XLU_r = as.numeric(unlist(XLU[colnames(XLU)==date]))
  XLV_r = as.numeric(unlist(XLV[colnames(XLV)==date]))
  XLY_r = as.numeric(unlist(XLY[colnames(XLY)==date]))
  
  subweight = subset(weight, Date == date)
  SPY_w = subweight[,2]/100
  XLB_w = subweight[,3]/100
  XLE_w = subweight[,4]/100
  XLF_w = subweight[,5]/100+subweight[,13]/100
  XLI_w = subweight[,6]/100
  XLK_w = subweight[,7]/100+subweight[,12]/100
  XLP_w = subweight[,8]/100
  XLU_w = subweight[,9]/100
  XLV_w = subweight[,10]/100
  XLY_w = subweight[,11]/100
  
  
  #######################################################
  df1 = matrix(,nrow=1000,ncol=10)
  df1[,1] = XLB_r*XLB_w
  df1[,2] = XLE_r*XLE_w
  df1[,3] = XLF_r*XLF_w
  df1[,4] = XLI_r*XLI_w
  df1[,5] = XLK_r*XLK_w
  df1[,6] = XLP_r*XLP_w
  df1[,7] = XLU_r*XLU_w
  df1[,8] = XLV_r*XLV_w
  df1[,9] = XLY_r*XLY_w
  df1[,10] = -SPY_r*SPY_w
  
  
  BRA = bra(df1, f=var, shuffle=T, maxiter=1e5, stalliter=1e5, abs.tol=0, rel.tol=0, f.target=-Inf) #the Block Rearrangement Algorithm
  
  new_matrix = BRA$X
  
  new_df = matrix(,nrow=1000,ncol=10)
  new_df[,1] = new_matrix[,1]/XLB_w
  new_df[,2] = new_matrix[,2]/XLE_w
  new_df[,3] = new_matrix[,3]/XLF_w
  new_df[,4] = new_matrix[,4]/XLI_w
  new_df[,5] = new_matrix[,5]/XLK_w
  new_df[,6] = new_matrix[,6]/XLP_w
  new_df[,7] = new_matrix[,7]/XLU_w
  new_df[,8] = new_matrix[,8]/XLV_w
  new_df[,9] = new_matrix[,9]/XLY_w
  new_df[,10] = -new_matrix[,10]/SPY_w
  
  
  zhangliang[,,mm]=new_df               ## the joint physical distribution matrix among multiple asset returns
  
  print(mm)
}



zhangliang_2019 = zhangliang[,,1:4]

