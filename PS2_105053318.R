#Quantitative Asset Allocation HW2
library(data.table)
library(readxl)
library(moments)
library(lubridate)
library(xts)
library(dplyr)

options(scipen=999)
#Q1
CRSP_Bonds<-data.table(read.csv("D:/UCLA Master/UCLA Sem3/Quant Asset/HW2/All_Bonds_Data_2.csv"))
PS2_Q1<-function(x)
{
CRSP_Bonds<-x
CRSP_Bonds[,MCALDT:=as.Date(MCALDT,format="%m/%d/%Y")]



#Delete unnecessary lines
CRSP_Bonds<-CRSP_Bonds[MCALDT>="1925-12-30"& MCALDT<="2018-01-01"]
Bond_Date<-unique(CRSP_Bonds[,MCALDT])
total_bond_cap<-double()
weight_bond_vw<-double()
for(i in 1:length(Bond_Date))
{
  total_bond_cap<-c(total_bond_cap,sum(na.omit(CRSP_Bonds[MCALDT==Bond_Date[i]]$TMTOTOUT)))

}

CRSP_Bonds<-CRSP_Bonds[order(CRSP_Bonds[,MCALDT])]
CRSP_Bonds<-CRSP_Bonds[!(TMRETNUA==-99&is.na(TMTOTOUT)),]
CRSP_Bonds[(TMRETNUA==-99&is.na(TMTOTOUT))]$TMRETNUA<-0
CRSP_Bonds[is.na(TMTOTOUT)]$TMTOTOUT<-0

##############

#Total Bond Cap For Each Date
Date_bond_cap<-data.table(Bond_Date[1:length(Bond_Date)],total_bond_cap)


#Bond Value-Weighted Return
vwretd_bond<-double()
for(i in 1:(length(Bond_Date)-1))

{ weight_bond<-CRSP_Bonds[MCALDT==Bond_Date[i]]$TMTOTOUT/sum(CRSP_Bonds[MCALDT==Bond_Date[i]]$TMTOTOUT)
  weight_KYCRSPID<-data.table(CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID,weight_bond)
  Common_KYCRSPID<-intersect(CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID)
  cm_weight_bond<-weight_bond[match(Common_KYCRSPID,weight_KYCRSPID$V1)]
  cm_return_bond<-CRSP_Bonds[MCALDT==Bond_Date[i+1]][match(Common_KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID)]$TMRETNUA
  vwretd_bond<-c(vwretd_bond,sum(cm_weight_bond*cm_return_bond))
}

#First Month
result_vw_bond<-data.table(Bond_Date[2:length(Bond_Date)],vwretd_bond)[1]

vwretd_bond<-double()
#Remove -99 return
CRSP_Bonds<-CRSP_Bonds[!TMRETNUA==-99]

for(i in 2:(length(Bond_Date)-1))

{ weight_bond<-CRSP_Bonds[MCALDT==Bond_Date[i]]$TMTOTOUT/sum(CRSP_Bonds[MCALDT==Bond_Date[i]]$TMTOTOUT)
weight_KYCRSPID<-data.table(CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID,weight_bond)
Common_KYCRSPID<-intersect(CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID)
cm_weight_bond<-weight_bond[match(Common_KYCRSPID,weight_KYCRSPID$V1)]
cm_return_bond<-CRSP_Bonds[MCALDT==Bond_Date[i+1]][match(Common_KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID)]$TMRETNUA
vwretd_bond<-c(vwretd_bond,sum(cm_weight_bond*cm_return_bond))
}

result_vw_bond<-rbind(result_vw_bond,data.table(Bond_Date[3:length(Bond_Date)],vwretd_bond))

#1925-12-31
ewretd_bond_first<-data.table(Bond_Date[2],0.5*(CRSP_Bonds[1,TMRETNUA]+CRSP_Bonds[2,TMRETNUA]))
names(ewretd_bond_first)<-c("V1","ewretd_bond")

#Bond Equal-Weighted Return
ewretd_bond<-double()
for(i in 2:(length(Bond_Date)-1))  #1925-12-31 not in CRSP here.

{
  weight_bond<-rep(1/nrow(CRSP_Bonds[MCALDT==Bond_Date[i]]),nrow(CRSP_Bonds[MCALDT==Bond_Date[i]]))
  weight_KYCRSPID<-data.table(CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID,weight_bond)
  Common_KYCRSPID<-intersect(CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i]]$KYCRSPID)
  cm_weight_bond<-weight_bond[match(Common_KYCRSPID,weight_KYCRSPID$V1)]
  cm_return_bond<-CRSP_Bonds[MCALDT==Bond_Date[i+1]][match(Common_KYCRSPID,CRSP_Bonds[MCALDT==Bond_Date[i+1]]$KYCRSPID)]$TMRETNUA
  ewretd_bond<-c(ewretd_bond,sum(cm_weight_bond*cm_return_bond))
}


result_ew_bond<-data.table(Bond_Date[3:length(Bond_Date)],ewretd_bond)
result_ew_bond<-rbind(ewretd_bond_first,result_ew_bond)



Year_vec_bond = year(Bond_Date[2:(length(Bond_Date))])
Month_vec_bond=month(Bond_Date[2:(length(Bond_Date))])
Value_Weighted_Return_bond<-data.table(Year_vec_bond[1:(length(Bond_Date)-1)],Month_vec_bond[1:(length(Bond_Date)-1)],total_bond_cap[1:(length(Bond_Date)-1)],result_ew_bond[,ewretd_bond],result_vw_bond[,vwretd_bond])

names(Value_Weighted_Return_bond)[names(Value_Weighted_Return_bond) == "V1"] = "Year"
names(Value_Weighted_Return_bond)[names(Value_Weighted_Return_bond) == "V2"] = "Month"
names(Value_Weighted_Return_bond)[names(Value_Weighted_Return_bond) == "V3"] = "Stock_lag_MV"
names(Value_Weighted_Return_bond)[names(Value_Weighted_Return_bond) == "V4"] = "Equal-Weighted Returns"
names(Value_Weighted_Return_bond)[names(Value_Weighted_Return_bond) == "V5"] = "Value-Weighted Returns"

Monthly_CRSP_Bonds<-Value_Weighted_Return_bond
return(Monthly_CRSP_Bonds)
}

Monthly_CRSP_Bonds<-PS2_Q1(CRSP_Bonds)

#Q2
Monthly_CRSP_Stocks <- data.table(read.csv("D:/UCLA Master/UCLA Sem3/Quant Asset/HW2/Monthly_CRSP_Stocks.csv"))
Monthly_CRSP_Riskless <- read.csv("D:/UCLA Master/UCLA Sem3/Quant Asset/HW2/Risk_Free.csv")
PS2_Q2<-function(x,y,z)
{
Monthly_CRSP_Stocks<-x
Monthly_CRSP_Bonds<-y
Monthly_CRSP_Riskless<-z
Monthly_CRSP_Bonds_Excess<-Monthly_CRSP_Bonds$`Value-Weighted Returns`-Monthly_CRSP_Riskless$t30ret
Monthly_CRSP_Stocks_Excess<-Monthly_CRSP_Stocks$Stock_Vw_Ret-Monthly_CRSP_Riskless$t30ret

Monthly_BS<-data.table(Monthly_CRSP_Bonds$Year,Monthly_CRSP_Bonds$Month,Monthly_CRSP_Stocks$Stock_lag_MV,Monthly_CRSP_Stocks_Excess,Monthly_CRSP_Bonds$Stock_lag_MV,Monthly_CRSP_Bonds_Excess)
names(Monthly_BS)<-c("Year","Month","Stock_lag_MV","Stock_Excess_Vw_Ret","Bond_lag_MV","Bond_Excess_Vw_Ret")
Monthly_CRSP_Universe<-Monthly_BS
return(Monthly_CRSP_Universe)
}

Monthly_CRSP_Universe<-PS2_Q2(Monthly_CRSP_Stocks,Monthly_CRSP_Bonds,Monthly_CRSP_Riskless)
  
#Q3
PS2_Q3<-function(x)
{
Monthly_CRSP_Universe<-x  
Port_Rets<-Monthly_CRSP_Universe
Port_Rets[,Excess_Vw_Ret:=Stock_lag_MV/(Stock_lag_MV+Bond_lag_MV)*Stock_Excess_Vw_Ret+Bond_lag_MV/(Stock_lag_MV+Bond_lag_MV)*Bond_Excess_Vw_Ret]
Port_Rets[,Excess_60_40_Ret:=0.6*Stock_Excess_Vw_Ret+0.4*Bond_Excess_Vw_Ret]

Stock_inverse_sigma_hat<-double()
Bond_inverse_sigma_hat<-double()

Target_Date<-c("1929-12-31")


for(i in 37:nrow(Port_Rets))
{
  Stock_inverse_sigma_hat<-c(Stock_inverse_sigma_hat,1/(sd(Port_Rets[(i-36):(i-1),Stock_Excess_Vw_Ret])))
  Bond_inverse_sigma_hat<-c(Bond_inverse_sigma_hat,1/(sd(Port_Rets[(i-36):(i-1),Bond_Excess_Vw_Ret])))
}

Stock_inverse_sigma_hat_result<-c(rep(NA,36),Stock_inverse_sigma_hat)
Bond_inverse_sigma_hat_result<-c(rep(NA,36),Bond_inverse_sigma_hat)

Port_Rets[,Stock_inverse_sigma_hat:=Stock_inverse_sigma_hat_result][,Bond_inverse_sigma_hat:=Bond_inverse_sigma_hat_result]
Port_Rets[,Unlevered_k:=1/(Stock_inverse_sigma_hat+Bond_inverse_sigma_hat)]
Port_Rets[,Excess_Unlevered_RP_Ret:=Unlevered_k*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+Unlevered_k*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret]
k_square<-var(Port_Rets$Excess_Vw_Ret)/(var(na.omit(Port_Rets$Stock_Excess_Vw_Ret*Port_Rets$Stock_inverse_sigma_hat))+var(na.omit(Port_Rets$Bond_Excess_Vw_Ret*Port_Rets$Bond_inverse_sigma_hat))+2*cov(na.omit(Port_Rets$Stock_Excess_Vw_Ret*Port_Rets$Stock_inverse_sigma_hat),na.omit(Port_Rets$Bond_Excess_Vw_Ret*Port_Rets$Bond_inverse_sigma_hat)))
k<-sqrt(k_square)
Port_Rets[,Levered_k:=c(rep(NA,36),rep(k,nrow(Port_Rets)-36))]
Port_Rets[,Excess_levered_RP_Ret:=Levered_k*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+Levered_k*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret]
#Port_Rets[,Excess_levered_RP_Ret:=0.02580*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+0.02580*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret]
return(Port_Rets)
}

Port_Rets<-PS2_Q3(Monthly_CRSP_Universe)

#Q4
PS2_Q4<-function(x)
{
Port_Rets<-x  
Pannel_report<-matrix(rep(0,36),ncol=6,byrow=TRUE)
rownames(Pannel_report)<-c("CRSP stocks","CRSP bonds","Value-weighted portfolio","60/40 portfolio","Unlevered RP","Levered RP")
colnames(Pannel_report)<-c("Annualized Mean","t-stat of Annualized Mean","Annualized Standard Deviation","Annualized Sharpe Ratio","Skewness","Excess Kurtosis")

#Port_Rets_Q4<-Port_Rets[(Year>=1930 &Year<=2010)&!(Year==2010  & Month>6),c("Stock_Excess_Vw_Ret","Bond_Excess_Vw_Ret","Excess_Vw_Ret","Excess_60_40_Ret","Excess_Unlevered_RP_Ret","Excess_levered_RP_Ret")]
Port_Rets_Q4<-Port_Rets[(Year>=1930 &Year<=2010),c("Stock_Excess_Vw_Ret","Bond_Excess_Vw_Ret","Excess_Vw_Ret","Excess_60_40_Ret","Excess_Unlevered_RP_Ret","Excess_levered_RP_Ret")]
result_annualized_mean<-apply(Port_Rets_Q4,2,mean)*12
result_annualized_volatility<-apply(Port_Rets_Q4,2,sd)*sqrt(12)

ttest<-function(x)
{
  y<-t.test(x)
  return(y$statistic)
  
}
result_t_statis<-apply(Port_Rets_Q4,2,ttest)
result_annualized_sr<-result_annualized_mean/(result_annualized_volatility)
result_skewness<-apply(Port_Rets_Q4,2,skewness)
result_ex_kurtosis<-apply(Port_Rets_Q4,2,kurtosis)-3

Pannel_report[,1]=result_annualized_mean
Pannel_report[,2]=result_t_statis
Pannel_report[,3]=result_annualized_volatility
Pannel_report[,4]=result_annualized_sr
Pannel_report[,5]=result_skewness
Pannel_report[,6]=result_ex_kurtosis
return(Pannel_report)
}

View(PS2_Q4(Port_Rets))
