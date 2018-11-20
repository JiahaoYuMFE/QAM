library(data.table)
library(readxl)
library(moments)
FF_data <- read_excel("D:/UCLA Master/UCLA Sem3/Quant Asset/HW1/FF_data.xlsx")
FF_data <- data.table(FF_data)
CRSP_Stocks<-fread(file="D:/UCLA Master/UCLA Sem3/Quant Asset/HW1/data/data2.csv")

PS1_Q1<-function(data_input){
CRSP_Stocks<-data_input
CRSP_Stocks$date<-as.Date(as.character(CRSP_Stocks$date),"%Y%m%d")
CRSP_Stocks[,MktCap:=abs(PRC)*SHROUT]
CRSP_Stocks<-CRSP_Stocks[order(CRSP_Stocks$date),]
#RET to NA
CRSP_Stocks[,RET:=as.numeric(CRSP_Stocks$RET)]
#5
CRSP_Stocks_2<-CRSP_Stocks[SHRCD==10 | SHRCD==11,]
CRSP_Stocks_2<-CRSP_Stocks_2[EXCHCD==1|EXCHCD==2|EXCHCD==3]
CRSP_firstmonth<-CRSP_Stocks_2[date=="1925-12-31"& (!is.na(MktCap)),]
weight_firstmonth=na.omit(CRSP_firstmonth$MktCap)/sum(na.omit(CRSP_firstmonth$MktCap))
weight_permno1<-data.table(cbind(CRSP_firstmonth$PERMNO,weight_firstmonth))
Common_PERMNO1<-intersect(CRSP_Stocks_2[date=="1926-1-30"]$PERMNO,CRSP_firstmonth$PERMNO)

cm_weight1<-weight_firstmonth[match(Common_PERMNO1,weight_permno1$V1)]
cm_return1<-CRSP_Stocks_2[date=="1926-1-30"][match(Common_PERMNO1,CRSP_Stocks_2[date=="1926-1-30"]$PERMNO)]$RET
vwretd1<-sum(na.omit(cm_weight1*cm_return1))
ewretd1<-sum(na.omit(1/length(cm_weight1)*cm_return1))
mktcap1<-sum(na.omit(CRSP_firstmonth$MktCap))
#Find rows with valid MktCap, i.e. non-zero price
CRSP_Stocks_2<-CRSP_Stocks_2[!is.na(MktCap)]

CRSP_Stocks_2[,DLRET:=as.numeric(CRSP_Stocks_2$DLRET)]

CRSP_Stocks_2<-CRSP_Stocks_2[!(is.na(DLRET) & is.na(RET))]
CRSP_Stocks_2[,RET_selected:=RET]
CRSP_Stocks_2[is.na(RET),]$RET_selected=CRSP_Stocks_2[is.na(RET),]$DLRET
CRSP_Stocks_2[is.na(DLRET),]$RET_selected=CRSP_Stocks_2[is.na(DLRET),]$RET
CRSP_Stocks_2[!is.na(DLRET)&!is.na(RET),]$RET_selected=(CRSP_Stocks_2[!is.na(DLRET)&!is.na(RET),]$DLRET+1)*(CRSP_Stocks_2[!is.na(DLRET)&!is.na(RET),]$RET+1)-1

Date<-unique(CRSP_Stocks_2$date)

total_mkt_cap<-double()
weight_vw<-double()
for(i in 1:length(Date))
{
  total_mkt_cap<-c(total_mkt_cap,sum(na.omit(CRSP_Stocks_2[date==Date[i]]$MktCap)))

}

Date_mkt_up<-data.table(Date[1:length(Date)],total_mkt_cap)

vwretd<-double()
for(i in 1:(length(Date)-1))
  
{ 
  weight<-CRSP_Stocks_2[date==Date[i]]$MktCap/sum(CRSP_Stocks_2[date==Date[i]]$MktCap)
  weight_permno<-data.table(cbind(CRSP_Stocks_2[date==Date[i]]$PERMNO,weight))
  Common_PERMNO<-intersect(CRSP_Stocks_2[date==Date[i+1]]$PERMNO,CRSP_Stocks_2[date==Date[i]]$PERMNO)
  cm_weight<-weight[match(Common_PERMNO,weight_permno$V1)]
  cm_return<-CRSP_Stocks_2[date==Date[i+1]][match(Common_PERMNO,CRSP_Stocks_2[date==Date[i+1]]$PERMNO)]$RET_selected
  vwretd<-c(vwretd,sum(cm_weight*cm_return))
} 
  

#Equal Weighted

ewretd<-double()
for(i in 1:(length(Date)-1))
  
{ 
  weight<-rep(1/nrow(CRSP_Stocks_2[date==Date[i]]),nrow(CRSP_Stocks_2[date==Date[i]]))
  weight_permno<-data.table(cbind(CRSP_Stocks_2[date==Date[i]]$PERMNO,weight))
  Common_PERMNO<-intersect(CRSP_Stocks_2[date==Date[i+1]]$PERMNO,CRSP_Stocks_2[date==Date[i]]$PERMNO)
  cm_weight<-weight[match(Common_PERMNO,weight_permno$V1)]
  cm_return<-CRSP_Stocks_2[date==Date[i+1]][match(Common_PERMNO,CRSP_Stocks_2[date==Date[i+1]]$PERMNO)]$RET_selected
  ewretd<-c(ewretd,sum(cm_weight*cm_return))
} 

date_vec = unique(CRSP_Stocks$date)
Year_vec = year(date_vec[2:(length(date_vec))])
Firstmonth<-data.table(cbind(1926,1,mktcap1,vwretd1,ewretd1))
Value_Weighted_Return<-data.table(cbind(Year_vec[2:(length(Date))],Month_vec[2:(length(Date))],total_mkt_cap[1:(length(Date)-1)],ewretd,vwretd))

names(Value_Weighted_Return)[names(Value_Weighted_Return) == "V1"] = "Year"
names(Value_Weighted_Return)[names(Value_Weighted_Return) == "V2"] = "Month"
names(Value_Weighted_Return)[names(Value_Weighted_Return) == "V3"] = "Stock_lag_MV"
names(Value_Weighted_Return)[names(Value_Weighted_Return) == "ewretd"] = "Equal-Weighted Returns"
names(Value_Weighted_Return)[names(Value_Weighted_Return) == "vwretd"] = "Value-Weighted Returns"

return(Value_Weighted_Return)
}


#Q2
PS1_Q2<-function(Monthly_CRSP_Stocks,FF_mkt){
Value_Weighted_Return <-Monthly_CRSP_Stocks
FF_data<-FF_mkt
VWRETD_comapre<-Value_Weighted_Return[6:nrow(Value_Weighted_Return)]
VWRETD_comapre[,RF:=FF_data$RF/100]
VWRETD_comapre[,Excess_return:=`Value-Weighted Returns`-RF]
Annualized_mean<-prod(1+VWRETD_comapre$Excess_return)^(12/nrow(Value_Weighted_Return))-1
Annualized_sd<-sqrt(12)*sd(VWRETD_comapre$Excess_return)
Annualized_sharpe_ratio<-Annualized_mean/Annualized_sd
Annualized_skewness<-skewness(VWRETD_comapre$Excess_return)
Annualized_excess_kurtosis<-kurtosis(VWRETD_comapre$Excess_return)-3

FF_mean<-prod(1+FF_data$`Mkt-RF`/100)^(12/nrow(FF_data))-1
FF_sd<-sqrt(12)*sd(FF_data$`Mkt-RF`/100)
FF_sharpe_ratio<-FF_mean/FF_sd
FF_skewness<-skewness(FF_data$`Mkt-RF`/100)
FF_excess_kurtosis<-kurtosis(FF_data$`Mkt-RF`/100)-3

result_matrix<-matrix(c(Annualized_mean,Annualized_sd,Annualized_sharpe_ratio,Annualized_skewness,Annualized_excess_kurtosis,FF_mean,FF_sd,FF_sharpe_ratio,FF_skewness,FF_excess_kurtosis),ncol=2,byrow=FALSE)
rownames(result_matrix)<-c("Annualized Mean","Annualized Standard Deviation","Annualized Sharpe Ratio","Skewness","Excess Kurtosis")
colnames(result_matrix)<-c("Estimated FF Market Excess Return","Actual FF Market Excess Return")
round(result_matrix,4)
return(round(result_matrix,4))
}


#Q3
PS1_Q3<-function(Monthly_CRSP_Stocks,FF_mkt)
{
VWRETD_comapre<-Monthly_CRSP_Stocks
FF_data<-FF_mkt
Corr_q3<-cor(VWRETD_comapre$Excess_return,FF_data$`Mkt-RF`/100)
maximum_absolute_diff<-max(abs(VWRETD_comapre$Excess_return-FF_data$`Mkt-RF`/100))
return(c(Corr_q3,maximum_absolute_diff))
}

