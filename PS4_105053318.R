library(data.table)
library(dplyr)
library(purrr)
library(zoo)
library(lubridate)
library(moments)
CRSP<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/crsp.csv")
Linkage <- fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/link.csv",key="LPERMCO")
Compustat_Stocks<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/comp-annual3.csv")
Pension<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/prba.csv")


CRSP = CRSP[EXCHCD %in% c(1,2,3) & SHRCD %in% c(10,11),] #select data based on share codes & exchange codes

CRSP$date = as.Date(as.character(CRSP$date), "%Y%m%d")
CRSP = CRSP[order(date)]
CRSP[ , Year:= as.integer(year(CRSP$date))]
CRSP[ , Month:= as.integer(month(CRSP$date))]
CRSP$PRC = abs(CRSP$PRC)  #use abs value of negative return values
CRSP$RET = as.numeric(CRSP$RET) # set non-numeric returns to NA
CRSP$RETX = as.numeric(CRSP$RETX)
CRSP$DLRET = as.numeric(CRSP$DLRET)
#Now need to decide what return values should be used:
CRSP[is.na(DLRET) & !is.na(RET),
     Ret := RET][!is.na(DLRET) & is.na(RET),
                 Ret := DLRET][!is.na(DLRET) & !is.na(RET),
                               Ret := (1 + RET)*(1 + DLRET) - 1]

CRSP$Mkt_Cap = CRSP$SHROUT * CRSP$PRC #/ 1000 # market cap in millions
CRSP[, lag_Mkt_Cap := shift(Mkt_Cap), by = PERMNO]
# When more than one PERMNO, calculate market cap with total:
CRSP[, PERMNO_weight := lag_Mkt_Cap/sum(lag_Mkt_Cap),
     by = .(PERMCO, date)][,
                           Ret := sum(PERMNO_weight*Ret),
                           by = .(PERMCO, date)][,
                                                 RetX := sum(PERMNO_weight*RETX),
                                                 by = .(PERMCO, date)][,
                                                                       lag_Mkt_Cap := sum(lag_Mkt_Cap),
                                                                       by = .(PERMCO, date)]
# Create fiscal year
CRSP[, fyear := ifelse(Month >= 7, Year-1, Year - 2)]

# Merge Compstat and pension
setkey(Compustat_Stocks, gvkey, datadate)
setkey(Pension, gvkey, datadate)

Compustat_Merged = merge(Compustat_Stocks, Pension, all.x = TRUE)
Compustat_Merged[, datadate := ymd(datadate)]
setnames(Compustat_Merged, c("gvkey"), c("GVKEY"))


####### clean Linkage data:
Linkage = Linkage[LINKPRIM %in% c("P", "C")]
setnames(Linkage, c("gvkey"), c("GVKEY"))
Linkage[LINKENDDT=='E', LINKENDDT := '20171231']
Linkage[, LINKENDDT := ymd(LINKENDDT)][, LINKDT := ymd(LINKDT)]


####### Merge all data
Comp_Link_merge = merge(Compustat_Merged, Linkage, by='GVKEY', all.x = T, allow.cartesian = T)
data_final = merge(CRSP, Comp_Link_merge, by.x=c('PERMNO','fyear'), by.y=c('LPERMNO','fyear'), all = T, allow.cartesian = T)
data_final = data_final[datadate >= LINKDT & (datadate <= LINKENDDT | is.na(LINKENDDT))]

data_final
data_final[,SHE := ifelse(!is.na(seq), seq, ifelse(!is.na(ceq+pstk), ceq+pstk, ifelse(!is.na(at-lt-mib), at-lt-mib, at-lt)))]
data_final[,DT := ifelse(!is.na(txditc), txditc, ifelse(!is.na(itcb+txdb), itcb+txdb, ifelse(!is.na(itcb), itcb, txdb)))]
data_final[,PS := ifelse(!is.na(pstkrv), pstkrv, ifelse(!is.na(pstkl), pstkl, pstk))]
data_final = data_final %>%
  mutate(NegPS = -PS, Negprba = -prba) %>% as.data.table()
data_final[,BE := rowSums(data_final[,c("SHE", "NegPS", "DT", "Negprba")], na.rm=TRUE)]


########## Size Portfolio

Size_CRSP_Stocks=copy(CRSP)
Size_CRSP_Stocks_Company = Size_CRSP_Stocks[, .(Mkt_Cap = sum(Mkt_Cap, na.rm=T), EXCHCD=last(EXCHCD)), .(PERMCO, Year, Month)]

MktCap_NYSE_decile = Size_CRSP_Stocks_Company %>% filter(Month==6, EXCHCD==1) %>% group_by(Year) %>% 
  summarise(Dec.1 = quantile(Mkt_Cap, 0.1,na.rm=TRUE), Dec.2 = quantile(Mkt_Cap, 0.2, na.rm=TRUE), 
            Dec.3 = quantile(Mkt_Cap, 0.3, na.rm=TRUE), Dec.4 = quantile(Mkt_Cap, 0.4, na.rm=TRUE),
            Dec.5 = quantile(Mkt_Cap, 0.5, na.rm=TRUE), Dec.6 = quantile(Mkt_Cap, 0.6, na.rm=TRUE),
            Dec.7 = quantile(Mkt_Cap, 0.7, na.rm=TRUE), Dec.8 = quantile(Mkt_Cap, 0.8, na.rm=TRUE),
            Dec.9 = quantile(Mkt_Cap, 0.9, na.rm=TRUE))

#Portfolio Construction
Portfolio_class = Size_CRSP_Stocks_Company %>% filter(Month==6) #Filter out month equal 6
Portfolio_class = merge(Portfolio_class, MktCap_NYSE_decile, by = "Year")
Portfolio_class = Portfolio_class %>% mutate(Mkt_Decile = ifelse(Mkt_Cap < Dec.1, 1, NA), Mkt_Decile = ifelse(Mkt_Cap > Dec.1, 2, Mkt_Decile),
                                             Mkt_Decile = ifelse(Mkt_Cap > Dec.2, 3, Mkt_Decile), Mkt_Decile = ifelse(Mkt_Cap > Dec.3, 4, Mkt_Decile),
                                             Mkt_Decile = ifelse(Mkt_Cap > Dec.4, 5, Mkt_Decile), Mkt_Decile = ifelse(Mkt_Cap > Dec.5, 6, Mkt_Decile),
                                             Mkt_Decile = ifelse(Mkt_Cap > Dec.6, 7, Mkt_Decile), Mkt_Decile = ifelse(Mkt_Cap > Dec.7, 8, Mkt_Decile),
                                             Mkt_Decile = ifelse(Mkt_Cap > Dec.8, 9, Mkt_Decile), Mkt_Decile = ifelse(Mkt_Cap > Dec.9, 10, Mkt_Decile))

Size_CRSP_Stocks = merge(Size_CRSP_Stocks, Portfolio_class[,c("Mkt_Decile", "Year", "Month", "PERMCO")], by = c("Year", "Month", "PERMCO"), all=TRUE)

#Create Decile Function & Form Portfolios
paste_decile = function(x){
  length = length(x)
  list = which(x>0)
  for (i in list){
    x[(i+1):(i+11)] = x[i]
  }
  return(x[1:length])
}

Size_CRSP_Stocks = Size_CRSP_Stocks %>% group_by(PERMNO) %>%
  mutate(Mkt_Decile = shift(Mkt_Decile, 1), Mkt_Decile = paste_decile(Mkt_Decile)) %>%
  as.data.table()

# Final Results
Size_CRSP_Stocks = Size_CRSP_Stocks %>% filter(Year >= 1973) #Filter the year
Size_Vw_Ret = Size_CRSP_Stocks %>% group_by(Year, Month, Mkt_Decile) %>%
  mutate(Weighted = lag_Mkt_Cap / sum(lag_Mkt_Cap, na.rm=T), Weighted_Ret = Weighted * Ret) %>%
  summarise(Ret_sum = sum(Weighted_Ret, na.rm=T)) %>%
  filter(!is.na(Mkt_Decile))

#Book-to-Market
# --> Obtain the Size decile portfolio return

#Data Accumulate within Company
Value_CRSP_Stocks = copy(CRSP)
Value_CRSP_Stocks_Company = Value_CRSP_Stocks[, .(MktCap = sum(Mkt_Cap, na.rm=T), EXCHCD=last(EXCHCD)), .(PERMCO, Year, Month)]
setnames(Value_CRSP_Stocks_Company, c("Year"), c("Year_rename"))
setnames(Value_CRSP_Stocks_Company, c("PERMCO"), c("PERMCO_rename"))


# Merge Dataset
Compustat_ccm_Value = merge(filter(Value_CRSP_Stocks_Company, Month==12), data_final, by.x = c("PERMCO_rename", "Year_rename"), by.y = c("LPERMCO", "fyear"))
Compustat_ccm_Value = Compustat_ccm_Value %>% mutate(BMRatio = BE / Mkt_Cap)

# Create B/M Decile
BMRatio_NYSE_decile = Compustat_ccm_Value %>% group_by(Year) %>% filter(EXCHCD.x==1) %>%
  summarise(Dec.1 = quantile(BMRatio, 0.1,na.rm=TRUE), Dec.2 = quantile(BMRatio, 0.2, na.rm=TRUE),
            Dec.3 = quantile(BMRatio, 0.3, na.rm=TRUE), Dec.4 = quantile(BMRatio, 0.4, na.rm=TRUE),
            Dec.5 = quantile(BMRatio, 0.5, na.rm=TRUE), Dec.6 = quantile(BMRatio, 0.6, na.rm=TRUE),
            Dec.7 = quantile(BMRatio, 0.7, na.rm=TRUE), Dec.8 = quantile(BMRatio, 0.8, na.rm=TRUE),
            Dec.9 = quantile(BMRatio, 0.9, na.rm=TRUE))

# Portfolio Construction
Portfolio_class = Compustat_ccm_Value
Portfolio_class = merge(Portfolio_class, BMRatio_NYSE_decile, by = "Year")
Portfolio_class = Portfolio_class %>% mutate(BMRatio_Decile = ifelse(BMRatio < Dec.1, 1, NA), BMRatio_Decile = ifelse(BMRatio > Dec.1, 2, BMRatio_Decile),
                                             BMRatio_Decile = ifelse(BMRatio > Dec.2, 3, BMRatio_Decile), BMRatio_Decile = ifelse(BMRatio > Dec.3, 4, BMRatio_Decile),
                                             BMRatio_Decile = ifelse(BMRatio > Dec.4, 5, BMRatio_Decile), BMRatio_Decile = ifelse(BMRatio > Dec.5, 6, BMRatio_Decile),
                                             BMRatio_Decile = ifelse(BMRatio > Dec.6, 7, BMRatio_Decile), BMRatio_Decile = ifelse(BMRatio > Dec.7, 8, BMRatio_Decile),
                                             BMRatio_Decile = ifelse(BMRatio > Dec.8, 9, BMRatio_Decile), BMRatio_Decile = ifelse(BMRatio > Dec.9, 10, BMRatio_Decile))

setnames(Portfolio_class, c("Month.x"), c("Month"))
Value_CRSP_Stocks = merge(Value_CRSP_Stocks, Portfolio_class[,c("BMRatio_Decile", "Year", "Month", "PERMCO")], by = c("Year", "Month", "PERMCO"), all=TRUE)

Value_CRSP_Stocks = Value_CRSP_Stocks %>% group_by(PERMNO) %>% mutate(BMRatio_Decile = shift(BMRatio_Decile, 7), BMRatio_Decile = paste_decile(BMRatio_Decile))

##Q2
# My replicated data
Size_Vw_Ret_dcast = dcast(Size_Vw_Ret, Year+Month~Mkt_Decile, value.var="Ret_sum")
# French's data
FF_Size = fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/Portfolios_Formed_on_ME.CSV")[559:1098, c(1,11,12,13,14,15,16,17,18,19,20)] #Load in Size Portfolio Return Data
# Correlation


Size_correlation = double()
for (i in 1:10){
  Size_correlation[i] = cor(Size_Vw_Ret_dcast[,i+2], select(FF_Size[7:540],i+1) )
}


# French data statistics
french_size_statistics = matrix(, nrow=5, ncol=10)
french_size_statistics[1,] = apply(FF_Size[,2:11], 2, mean, na.rm=TRUE) * 12 / 100

# My data statistics
FF_mkt<-fread("D:/UCLA Master/UCLA Sem3/Quant Asset/HW4/F-F_Research_Data_Factors.CSV")
Size_Vw_Ret_dcast = Size_Vw_Ret_dcast[3:12] - as.numeric(FF_mkt$RF)/100 #Excess Return
size_statistics = matrix(, nrow=5, ncol=10)
size_statistics[1, ] = apply(Size_Vw_Ret_dcast, 2, mean, na.rm=TRUE) * 12
size_statistics[2, ] = apply(Size_Vw_Ret_dcast, 2, sd, na.rm=TRUE) * sqrt(12)
size_statistics[3, ] = size_statistics[1, ] / size_statistics[2, ]
size_statistics[4, ] = apply(Size_Vw_Ret_dcast, 2, skewness, na.rm=TRUE)
size_statistics[5, ] = Size_correlation


################################################################## 
##                          Question 3
##################################################################
# My replicated data
Value_Vw_Ret = Value_CRSP_Stocks %>% group_by(Year, Month, BMRatio_Decile) %>%
  mutate(Weighted = lag_Mkt_Cap / sum(lag_Mkt_Cap, na.rm=T), Weighted_Ret = Weighted * Ret) %>%
  summarise(Ret_sum = sum(Weighted_Ret, na.rm=T)) %>% filter(!is.na(BMRatio_Decile))


# My data statistics
Value_Vw_Ret_dcast= Value_Vw_Ret_dcast[3:12] - FF_mkt$RF #Excess Return
value_statistics = matrix(, nrow=5, ncol=10)
value_statistics[1, ] = apply(Value_Vw_Ret_dcast, 2, mean, na.rm=TRUE) * 12
value_statistics[2, ] = apply(Value_Vw_Ret_dcast, 2, sd, na.rm=TRUE) * sqrt(12)
value_statistics[3, ] = value_statistics[1, ] / value_statistics[2, ]
value_statistics[4, ] = apply(Value_Vw_Ret_dcast, 2, skewness, na.rm=TRUE)
value_statistics[5, ] = Value_correlation


#Question 4
# My replicated data
Value_Vw_Ret_dcast = dcast(Value_Vw_Ret, Year+Month~BMRatio_Decile, value.var="Ret_sum")
# French's data
FF_BMRatio = fread("Portfolios_Formed_on_BE-ME.CSV")[559:1098, c(1,11,12,13,14,15,16,17,18,19,20)] #Load in BMRatio Portfolio Return Data
FF_BMRatio[,2:11] = FF_BMRatio[,2:11] / 100
# Correlation
Value_correlation = double()
for (i in 1:10){
  Value_correlation[i] = cor(Value_Vw_Ret_dcast[,i+2], select(FF_BMRatio,i+1))
}

# My data statistics
Value_Vw_Ret_dcast= Value_Vw_Ret_dcast[3:12] - FF_mkt$RF #Excess Return
value_statistics = matrix(, nrow=5, ncol=10)
value_statistics[1, ] = apply(Value_Vw_Ret_dcast, 2, mean, na.rm=TRUE) * 12
value_statistics[2, ] = apply(Value_Vw_Ret_dcast, 2, sd, na.rm=TRUE) * sqrt(12)
value_statistics[3, ] = value_statistics[1, ] / value_statistics[2, ]
value_statistics[4, ] = apply(Value_Vw_Ret_dcast, 2, skewness, na.rm=TRUE)
value_statistics[5, ] = Value_correlation

############################################################################################################
########       Problem 5
############################################################################################################