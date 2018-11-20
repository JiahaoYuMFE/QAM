
#QAM HW3
library(data.table)
library(dplyr)
library(zoo)
library(StatMeasures)
library(moments)
library(reshape2)
library(stringi)
CRSP_Stocks<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW1/data/data2.csv")
CRSP_Stocks<-CRSP_Stocks[order(CRSP_Stocks$date)]
CRSP_Stocks$date<-as.Date(as.character(CRSP_Stocks$date),"%Y%m%d")
#Q1
PS3_Q1<-function(CRSP_Stocks)
{
Company_name<-unique(CRSP_Stocks$PERMNO)
CRSP_Stocks[,Month:=month(date)]
#Delete less than 12 lines company
Company_numMon<-CRSP_Stocks %>% group_by(PERMNO) %>% summarise(numMon=n())
CRSP_Stocks<-filter(CRSP_Stocks,!(PERMNO %in%  Company_numMon[Company_numMon[,2]<12,]$PERMNO))

CRSP_Stocks=CRSP_Stocks %>% group_by(PERMNO)%>% mutate(RET_plus=as.numeric(RET)+1, Ranking_Ret=c(rep(NA,10),rollapply(RET_plus,11,prod,na.rm=TRUE)),Ranking_Ret=shift(log(Ranking_Ret),2))
CRSP_Stocks<-CRSP_Stocks %>% group_by(PERMNO) %>% mutate(Lag_Mkt_Cap=abs(shift(PRC*SHROUT)))

CRSP_Stocks<-CRSP_Stocks %>% mutate(Year=year(date))
output<-select(CRSP_Stocks,Year,Month,PERMNO,SHRCD,EXCHCD,PRC,Lag_Mkt_Cap,RET,Ranking_Ret)
output<-output %>% group_by(PERMNO) %>% filter(EXCHCD %in% c(1,2,3)& SHRCD %in% c(10,11) &!is.na(shift(PRC,13))& !is.na(as.numeric(shift(RET,2)))& !is.na(Lag_Mkt_Cap))
output<-output %>% arrange(PERMNO)
output<-mutate_each(output,funs(as.numeric),RET)
return(output)
}

CRSP_Stocks_Momentum<-PS3_Q1(CRSP_Stocks)

#Q2
#DM Portfolio

PS3_Q2<-function(CRSP_Stocks_Momentum)
{output<-CRSP_Stocks_Momentum
output<-output %>% group_by(Year,Month) %>% mutate(DM_decile=decile(Ranking_Ret))
                                    
#Compute KRF decile
compute_decile <- function(x){
  qt_numbers <- c(-Inf, quantile(x[EXCHCD==1]$Ranking_Ret, probs=seq(0, 1, 0.1), na.rm = T)[2:10], Inf) 
  return(as.integer(cut(x$Ranking_Ret, qt_numbers, include.lowest = T)))
}

output<-data.table(output)
output[, KRF_decile := compute_decile(.SD), by=c('Year','Month')]
output<-output %>% select(Year,Month,PERMNO,Lag_Mkt_Cap,RET,DM_decile,KRF_decile)
}


#Q3
CRSP_Stocks_Momentum_decile<-PS3_Q2(CRSP_Stocks_Momentum)
PS3_Q3<-function(CRSP_Stocks_Momentum_decile){
#Monthly Return under two deciles
output<-output %>% group_by(Year,Month,DM_decile) %>% mutate(DM_weight=Lag_Mkt_Cap/sum(Lag_Mkt_Cap),DM_RET=DM_weight*RET)
output<-output %>% group_by(Year,Month,KRF_decile) %>% mutate(KRF_weight=Lag_Mkt_Cap/sum(Lag_Mkt_Cap),KRF_RET=KRF_weight*RET)
Q3_result1<-output %>%  group_by(Year,Month,DM_decile) %>% summarise(DM_RET=sum(na.omit(DM_RET)))
Q3_result2<-output %>%  group_by(Year,Month,KRF_decile) %>% 
  summarise(KRF_RET=sum(na.omit(KRF_RET)))
Q3_result1<-Q3_result1 %>% arrange(Year,Month,DM_decile)
Q3_result2<-Q3_result2 %>% arrange(Year,Month,KRF_decile)
Q3_result<-data.table(Q3_result1,Q3_result2[,'KRF_RET'])
return(Q3_result)
}

library(readxl)
FF_data <-data.table(read_excel("D:/UCLA Master/UCLA Sem3/Quant Asset/HW1/FF_data.xlsx"))
FF_data[, `:=`(Year = as.integer(substring(Date, 1, 4)), Month = as.integer(substring(Date, 5, 6)))]
Q3_result = merge(Q3_result, FF_data, by = c('Year','Month') )
Q3_result<-Q3_result %>% select(Year,Month,DM_decile,DM_RET,KRF_RET,RF)

#Q4
CRSP_Stocks_Momentum_returns<-PS3_Q3(CRSP_Stocks_Momentum_decile)
PS3_Q4<-function(CRSP_Stocks_Momentum_returns)
{Q4_result<-merge(Q3_result, FF_data, by = c('Year','Month') )
Q4_result<-Q4_result %>% mutate(Mkt=`Mkt-RF`+RF.x)
Q4_result<-Q4_result %>% filter(Year<=2013) %>% mutate(Excess_DM_RET=DM_RET-RF.x/100,Excess_KRF_RET=KRF_RET-RF.x/100)
Moment_mean<-Q4_result %>% group_by(DM_decile) %>% summarise(1200*mean(Excess_DM_RET))
names(Moment_mean)<-c("Decile","Mean_Excess_RET")
Moment_SD<-Q4_result %>% group_by(DM_decile) %>% summarise(1200*mean(Excess_DM_RET)/(sd(Excess_DM_RET)*sqrt(12)*100))
names(Moment_SD)<-c("Decile","SD")
Moment_SR<-Q4_result %>%group_by(DM_decile) %>% summarise((sd(Excess_DM_RET)*sqrt(12)*100))
names(Moment_SR)<-c("Decile","SR")
Moment_SK<-Q4_result %>% group_by(DM_decile) %>% summarise(skewness(log(DM_RET+1)))
names(Moment_SK)<-c("Decile","Skewness")
Winner_decile<-Q4_result %>% group_by(Year,Month) %>% filter(DM_decile==10)
Loser_decile<-Q4_result %>% group_by(Year,Month) %>% filter(DM_decile==1)
WML=(Winner_decile$Excess_DM_RET-Loser_decile$Excess_DM_RET)*1200
WML_mean<-mean(WML)
WML_SD<-(sd(WML/1200)*sqrt(12)*100)
WML_SR<-mean(WML)/(sd(WML/1200)*sqrt(12)*100)
WML_Skewness<-skewness(log(WML/1200+1+Winner_decile$RF.x/100))
Q4_final_result<-matrix(c(Moment_mean$Mean_Excess_RET,WML_mean,Moment_SD$SD,WML_SD,Moment_SR$SR,WML_SR,Moment_SK$Skewness,WML_Skewness),nrow=4,byrow=TRUE)
rownames(Q4_final_result)<-c("r-rf","sigma","SR","sk(m)")
colnames(Q4_final_result)<-c("1","2","3","4","5","6","7","8","9","10","WML")
return(Q4_final_result)
}




#Q5

PS3_Q5<-function(CRSP_Stocks_Momentum_Returns)
{
DM_Series<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW3/NEW_m_m_pt_tot.csv.csv") %>% select(Year,Month,decile,DM_Ret)
MY_DM_Series<-Q3_result %>% arrange(Year,Month,DM_decile) %>% select(Year,Month,DM_decile,DM_RET) %>% filter(Year<2017)
names(MY_DM_Series)<-c("Year","Month","decile","MY_DM_RET")
DM_Comp<-merge(DM_Series,MY_DM_Series,by=c("Year","Month","decile"))
DM_Comp_Result<-DM_Comp %>%group_by(decile) %>%summarise(cor(DM_Ret,MY_DM_RET))

FF_Series<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW3/FF.csv")
FF_Series[, `:=`(Year = as.integer(substring(V1, 1, 4)), Month = as.integer(substring(V1, 5, 6)))]
FF_Series<-FF_Series %>% arrange(Year,Month) %>% select(Year,Month,2,3,4,5,6,7,8,9,10,11)
names(FF_Series)<-c("Year","Month","1","2","3","4","5","6","7","8","9","10")
FF_Series<-melt(FF_Series,id.vars=c("Year","Month"))
names(FF_Series)<-c("Year","Month","decile","KRF_RET")

MY_KRF_Series<-Q3_result %>% arrange(Year,Month,DM_decile) %>% select(Year,Month,DM_decile,KRF_RET) %>% filter(Year<2017)
names(MY_KRF_Series)<-c("Year","Month","decile","MY_KRF_RET")
KRF_Comp<-merge(FF_Series,MY_KRF_Series,by=c("Year","Month","decile"))
KRF_Comp_Result<-KRF_Comp %>% group_by(decile) %>% summarise(cor(KRF_RET,MY_KRF_RET))


#DM
Winner_decile_DM<-Q3_result %>% group_by(Year,Month) %>% filter(DM_decile==10,Year<2017) %>% select(Year,Month,DM_decile,DM_RET)
Loser_decile_DM<-Q3_result %>% group_by(Year,Month) %>% filter(DM_decile==1,Year<2017) %>% select(Year,Month,DM_decile,DM_RET)

Winner_decile_DM_HIS<-DM_Series %>% group_by(Year,Month) %>% filter(decile==10)
Loser_decile_DM_HIS<-DM_Series %>% group_by(Year,Month) %>% filter(decile==1)

WML_DM<-data.table(Winner_decile_DM$Year,Winner_decile_DM$Month,Winner_decile_DM$DM_RET-Loser_decile_DM$DM_RET,Winner_decile_DM_HIS$DM_Ret-Loser_decile_DM_HIS$DM_Ret)
names(WML_DM)<-c("Year","Month","WML1","WML2")

#KRF
Winner_decile_KRF<-Q3_result %>% group_by(Year,Month) %>% filter(DM_decile==10,Year<2017) %>% select(Year,Month,DM_decile,KRF_RET)
Loser_decile_KRF<-Q3_result %>% group_by(Year,Month) %>% filter(DM_decile==1,Year<2017) %>% select(Year,Month,DM_decile,KRF_RET)

Winner_decile_KRF_HIS<-FF_Series %>% group_by(Year,Month) %>% filter(decile==10,Year<2017)
Loser_decile_KRF_HIS<-FF_Series %>% group_by(Year,Month) %>% filter(decile==1,Year<2017)

WML_KRF<-data.table(Winner_decile_DM$Year,Winner_decile_DM$Month,Winner_decile_KRF$KRF_RET-Loser_decile_KRF$KRF_RET,Winner_decile_KRF_HIS$KRF_RET-Loser_decile_KRF_HIS$KRF_RET)
names(WML_KRF)<-c("Year","Month","WML1","WML2")

WML_DM_result<-WML_DM %>% summarise(cor(WML1,WML2))
WML_KRF_result<-WML_KRF %>% summarise(cor(WML1,WML2))

Q5_result<-matrix(c(DM_Comp_Result$`cor(DM_Ret, MY_DM_RET)`,WML_DM_result$`cor(WML1, WML2)`,KRF_Comp_Result$`cor(KRF_RET, MY_KRF_RET)`,WML_KRF_result$`cor(WML1, WML2)`),nrow=2,byrow=TRUE)
rownames(Q5_result)<-c("DM","KRF")
colnames(Q5_result)<-c("Decile 1","Decile 2","Decile 3","Decile 4","Decile 5","Decile 6","Decile 7","Decile 8","Decile 9","Decile 10","WML")
return(Q5_result)
}
