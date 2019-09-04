# Loading libraries
library(lubridate)
library(dplyr)
library(forecast)
library(tseries)

data_folder <- paste(getwd(), '/./data/raw/', sep='')
stage_folder <- paste(getwd(), '/./data/stage/', sep='')
simulation_folder <-paste(getwd(), '/./data/simulation/', sep='')

#Getting patient's admissions
dfadmission<-read.csv(paste(data_folder,"occupation_admission2.csv",sep=''), sep=';', header=T)
names(dfadmission)<-tolower(names(dfadmission))
names(dfadmission)[4]<-"quantity"
dfadmission[,'dt_entrada_unidade']<-as.POSIXct(strptime(as.character(dfadmission$dt_entrada_unidade), "%d/%m/%Y"), format="%Y-%m-%d")
dfadmission<-arrange(dfadmission,dt_entrada_unidade)

#Getting ANS beneficiaries
dfans<-read.csv(paste(data_folder,"ANS beneficiarios 2000-2017 v1.csv",sep=''), sep=',', header=T)
dfans[,'date']<-as.POSIXct(strptime(as.character(dfans$date), format="%m/%d/%Y"), format="%Y-%m-%d")
dfans[,'quantity']<-dfans$corporate_collective

#Getting unemployment & new jobs (IESS and DIEESE)
dfunemployD<-read.csv(paste(data_folder,"DIEESE unemployment rate - SP metrop - 2000 -2017.csv",sep=''), sep=',', header=T)
dfunemployD[,'date']<-as.POSIXct(strptime(as.character(dfunemployD$date), format="%m/%d/%Y"), format="%Y-%m-%d")

#Getting PIB IPEA/IBGE
dfpib<-read.csv(paste(data_folder,"IBGE IPEA - PIB 1990-2017.csv",sep=''), sep=',', header=T)
dfpib[,'date']<-as.POSIXct(strptime(as.character(dfpib$date), format="%m/%d/%Y"), format="%Y-%m-%d")

#Getting selic
dfselic<-read.csv(paste(data_folder,"BACEN Selic 2000-2017.csv",sep=''), sep=',', header=T)
dfselic[,'date']<-as.POSIXct(strptime(as.character(dfselic$date), format="%d/%m/%Y"), format="%Y-%m-%d")
dfselic<-subset(dfselic,format(date,'%d')=='01')
dfselic<-group_by(dfselic, date)%>%summarize(value=max(value))

###########################
#ANS dataset needs to be organized based on a lag of -6 quarters

#Smoothing ANS curve per month
vmonths<-data.frame(date=seq.POSIXt(from=ISOdate(2000,3,01),to=ISOdate(2017,06,01),by='month'))
dfansmonth<-data.frame(date=as.POSIXct(character()),quantity=as.numeric(numeric()),corporate_collective=as.numeric(numeric()),collective_membership=as.numeric(numeric()),collective_not_ident=as.numeric(numeric()),individual=as.numeric(numeric()), total=as.numeric(numeric()))
for (i in 2:nrow(dfans)) {
  vstart<-format(dfans[i-1,1],'%Y%m')
  vend<-format(dfans[i,1],'%Y%m')
  if (i==nrow(dfans)) {
    x<-subset(vmonths,format(date,'%Y%m')>=vstart&format(date,'%Y%m')<=vend)
  } else x<-subset(vmonths,format(date,'%Y%m')>=vstart&format(date,'%Y%m')<vend)
  x<-x$date
  y<-seq(dfans[i-1,2], dfans[i,2], (dfans[i,2] - dfans[i-1,2]) / length(x))
  y1<-seq(dfans[i-1,3], dfans[i,3], (dfans[i,3] - dfans[i-1,3]) / length(x))
  y2<-seq(dfans[i-1,4], dfans[i,4], (dfans[i,4] - dfans[i-1,4]) / length(x))
  y3<-seq(dfans[i-1,5], dfans[i,5], (dfans[i,5] - dfans[i-1,5]) / length(x))
  y4<-seq(dfans[i-1,8], dfans[i,8], (dfans[i,8] - dfans[i-1,8]) / length(x))
  y5<-seq(dfans[i-1,7], dfans[i,7], (dfans[i,7] - dfans[i-1,7]) / length(x))
  
  for (j in 1:length(x)) {
    dfansmonth[nrow(dfansmonth) + 1,'date']<-as.POSIXct(x[j], format='%Y-%m-%d')
    dfansmonth[nrow(dfansmonth),'individual']<-y[j+1]
    dfansmonth[nrow(dfansmonth),'corporate_collective']<-y1[j+1]
    dfansmonth[nrow(dfansmonth),'collective_membership']<-y2[j+1]
    dfansmonth[nrow(dfansmonth),'collective_not_ident']<-y3[j+1]
    dfansmonth[nrow(dfansmonth),'quantity']<-y4[j+1]
    dfansmonth[nrow(dfansmonth),'total']<-y5[j+1]
  }
}

#Smoothing ANS curve per week
vweeks<-data.frame(date=seq.POSIXt(from=ISOdate(2000,3,31),to=ISOdate(2017,06,30),by='week'))
dfansweek<-data.frame(date=as.POSIXct(character()),quantity=as.numeric(numeric()),corporate_collective=as.numeric(numeric()),collective_membership=as.numeric(numeric()),collective_not_ident=as.numeric(numeric()),individual=as.numeric(numeric()), total=as.numeric(numeric()))
for (i in 2:nrow(dfans)) {
  vstart<-format(dfans[i-1,1],'%Y-%m-%d')
  vend<-format(dfans[i,1],'%Y-%m-%d')
  if (i==nrow(dfans)) {
    x<-subset(vweeks,format(date,'%Y-%m-%d')>=vstart&format(date,'%Y-%m-%d')<=vend)
  } else x<-subset(vweeks,format(date,'%Y-%m-%d')>=vstart&format(date,'%Y-%m-%d')<vend)
  x<-x$date
  y<-seq(dfans[i-1,2], dfans[i,2], (dfans[i,2] - dfans[i-1,2]) / length(x))
  y1<-seq(dfans[i-1,3], dfans[i,3], (dfans[i,3] - dfans[i-1,3]) / length(x))
  y2<-seq(dfans[i-1,4], dfans[i,4], (dfans[i,4] - dfans[i-1,4]) / length(x))
  y3<-seq(dfans[i-1,5], dfans[i,5], (dfans[i,5] - dfans[i-1,5]) / length(x))
  y4<-seq(dfans[i-1,8], dfans[i,8], (dfans[i,8] - dfans[i-1,8]) / length(x))
  y5<-seq(dfans[i-1,7], dfans[i,7], (dfans[i,7] - dfans[i-1,7]) / length(x))
  
  for (j in 1:length(x)) {
    dfansweek[nrow(dfansweek) + 1,'date']<-as.POSIXct(x[j], format='%Y-%m-%d')
    dfansweek[nrow(dfansweek),'individual']<-y[j+1]
    dfansweek[nrow(dfansweek),'corporate_collective']<-y1[j+1]
    dfansweek[nrow(dfansweek),'collective_membership']<-y2[j+1]
    dfansweek[nrow(dfansweek),'collective_not_ident']<-y3[j+1]
    dfansweek[nrow(dfansweek),'quantity']<-y4[j+1]
    dfansweek[nrow(dfansweek),'total']<-y5[j+1]
  }
}

vtitle<-"Evolution of ANS Beneficiaries - Sao Paulo metropolitan region"
plot(dfansweek$date, dfansweek$total/1000, ylim=range(1000,10500),type='l', main = vtitle, ylab = "Quantity (/10^3)", xlab='Years')
lines(dfansweek$date,dfansweek$individual/1000, type='l', col='red')
lines(dfansweek$date,dfansweek$collective_membership/1000, type='l', col='green')
lines(dfansweek$date,dfansweek$corporate_collective/1000, type='l',col='blue')
legend("topleft", c('Individual','Corporate Collective', 'Collective per Membership', 'Total'), fill=c("red","green","blue","black"), horiz=TRUE, cex=.5)

#############################
# Generating graph 11 CSV file
#
weeks<-dfansweek$date
dfcsvserie1<-data.frame(date=weeks, quantity_1=as.numeric(dfansweek$total), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=weeks, quantity_1=NA, quantity_2=as.numeric(dfansweek$corporate_collective), quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=weeks, quantity_1=NA, quantity_2=NA, quantity_3=as.numeric(dfansweek$collective_membership), pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie4<-data.frame(date=weeks, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(dfansweek$individual), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3,dfcsvserie4)
write.table(dfcsvseries, file = paste(stage_folder,"Graph11_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader1<-data.frame(date=NA, quantity_1=as.character('Total'), quantity_2=as.character('Corporate Collective'), quantity_3=as.character('Collective per membership'), pred_1=as.character('Individual'), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes='*Source: www.ans.gov.br', y_label='Number of Beneficiaries')
dfcsvheader2<-data.frame(date=NA, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=NA, notes=NA, y_label=NA)
dfcsvheader<-rbind(dfcsvheader1,dfcsvheader2)
write.table(dfcsvheader, file = paste(stage_folder,"Graph11_header.csv",sep=''), sep = ",",row.names=FALSE)
#############################

#Smoothing unemployment curve per week
vweeks<-data.frame(date=seq.POSIXt(from=ISOdate(2000,1,1),to=ISOdate(2017,8,31),by='week'))
dfunemployweek<-data.frame(date=as.POSIXct(character()),quantity=as.numeric(numeric()))
for (i in 2:nrow(dfunemployD)) {
  vstart<-format(dfunemployD[i-1,1],'%Y-%m-%d')
  vend<-format(dfunemployD[i,1],'%Y-%m-%d')
  if (i==nrow(dfunemployD)) {
    x<-subset(vweeks,format(date,'%Y-%m-%d')>=vstart&format(date,'%Y-%m-%d')<=vend)
  } else x<-subset(vweeks,format(date,'%Y-%m-%d')>=vstart&format(date,'%Y-%m-%d')<vend)
  x<-x$date
  if ((dfunemployD[i,2] - dfunemployD[i-1,2])!=0) {
    y<-seq(dfunemployD[i-1,2], dfunemployD[i,2], (dfunemployD[i,2] - dfunemployD[i-1,2]) / length(x))
  } else y<-rep(dfunemployD[i,2],length(x)+1)
  for (j in 1:length(x)) {
    dfunemployweek[nrow(dfunemployweek) + 1,'date']<-as.POSIXct(x[j], format='%Y-%m-%d')
    dfunemployweek[nrow(dfunemployweek),'quantity']<-y[j+1]
  }
}

#################################
# plotting unemployment, ANS and PIB curves
#
UnemployD<-subset(dfunemployD,date>='2000-03-01' & date<='2017-06-30')
Ansmonth<-subset(dfansmonth,date>='2000-03-01' & date<='2017-06-30')
Pib<-subset(dfpib,date>='2000-03-01' & date<='2017-06-30')
Selic<-subset(dfselic,date>='2000-03-01'& date<='2017-06-30')

vtitle<-"Evolution of ANS beneficiaries, GDP, Selic and Unemployment rate - Sao Paulo Metropolitan region"
vnotes<-"Source: www.ans.gov.br, www.ibge.gov.br, www.dieese.org.br; **Note: Unmployment rate>ANS benef: -73% correlation (lag 0); Selic>unemployment rate: 82% correlation (lag -8) "
plot(Ansmonth$date, Ansmonth$quantity/100000, ylim=range(1:70), col='blue', type='l', main = vtitle, sub = vnotes,ylab = "Quantity", xlab='Years')
lines(UnemployD$date, UnemployD$value, type='l', col='red')
lines(Pib$date, Pib$value/10000, type='l', col='green')
lines(Selic$date, Selic$value*10, type='l', col='magenta')
legend("topleft", c('SP ANS Beneficiaries (10^-5)', 'SP Unemployment rate', 'Brazil GDP (10^-4)','Selic (10^1)'), fill=c("blue","red"), horiz=TRUE, cex=.5)

#############################
# Generating graph 12 CSV file
#
months<-Ansmonth$date
dfcsvserie1<-data.frame(date=months, quantity_1=as.numeric(Ansmonth$quantity/100000), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=months, quantity_1=NA, quantity_2=as.numeric(UnemployD$value), quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=months, quantity_1=NA, quantity_2=NA, quantity_3=as.numeric(Pib$value/10000), pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie4<-data.frame(date=months, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(Selic$value*10), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3,dfcsvserie4)
write.table(dfcsvseries, file = paste(stage_folder,"Graph12_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader1<-data.frame(date=NA, quantity_1=as.character('SP ANS Beneficiaries (10^-5)'), quantity_2=as.character('SP Unemployment rate'), quantity_3=as.character('Brazil nominal GDP (10^-4)'), pred_1=as.character('Selic (10^1)'), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes=vnotes, y_label='Number of Beneficiaries')
dfcsvheader2<-data.frame(date=NA, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=NA, notes=NA, y_label=NA)
dfcsvheader<-rbind(dfcsvheader1,dfcsvheader2)
write.table(dfcsvheader, file = paste(stage_folder,"Graph12_header.csv",sep=''), sep = ",",row.names=FALSE)
#############################

# Cross correlation between ANS Benef and Unemployment rate
a<-ccf(UnemployD$value,Ansmonth$quantity, lag.max=40)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line1<-data.frame(col1='Unemployment rate', col2='ANS beneficiaries', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(Pib$value,Ansmonth$quantity, lag.max=40)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line2<-data.frame(col1='GDP', col2='ANS beneficiaries', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(Selic$value,Ansmonth$quantity, lag.max=40)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line3<-data.frame(col1='SELIC rate', col2='ANS beneficiaries', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(Selic$value,UnemployD$value, lag.max=40)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line4<-data.frame(col1='SELIC rate', col2='Unemployment rate', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

#############################
#
# Generating table 1 CSV file
#
vtitle<-'Cross correlation among economic variables and ANS beneficiaries'
vnotes<-''
dflines<-rbind(line1,line2,line3,line4)
write.table(dflines, file = paste(stage_folder,"Table1_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader<-data.frame(col1='Variable A', col2='Variable B', col3='Lag', col4='Correlation', col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA, title=vtitle, notes=vnotes)
write.table(dfcsvheader, file = paste(stage_folder,"Table1_header.csv",sep=''), sep = ",",row.names=FALSE)
#############################

#Filtering & organizing datasets

#Emergency room datasets
dfadmissionER<-subset(dfadmission,cd_setor_atendimento==38 | cd_setor_atendimento==47 | cd_setor_atendimento==37 | cd_setor_atendimento==197)
# Clinic dataset
dfadmissionClinic<-subset(dfadmission,cd_setor_atendimento==2 | cd_setor_atendimento==11| cd_setor_atendimento==34 | cd_setor_atendimento==183 | cd_setor_atendimento==5142 | cd_setor_atendimento==5 | cd_setor_atendimento==5101 | cd_setor_atendimento==12)
# Surgery dataset
dfadmissionSurgery<-subset(dfadmission, cd_setor_atendimento==117 | cd_setor_atendimento==5037)
#
# Organizing Admissions dataset
dfadmissionERweek<-group_by(dfadmissionER,format(as.POSIXct(dt_entrada_unidade,format='%Y-%m-%d'),'%Y%W'))%>%summarize(date=min(dt_entrada_unidade),quantity=sum(quantity))
names(dfadmissionERweek)[1]<-c('week')
dfadmissionERweek<-dfadmissionERweek[!is.na(dfadmissionERweek$week),2:3]

# Organizing Clinic dataset
dfadmissionClinicweek<-group_by(dfadmissionClinic,format(as.POSIXct(dt_entrada_unidade,format='%Y-%m-%d'),'%Y%W'))%>%summarize(date=min(dt_entrada_unidade),quantity=sum(quantity))
names(dfadmissionClinicweek)[1]<-c('week')
dfadmissionClinicweek<-dfadmissionClinicweek[!is.na(dfadmissionClinicweek$week),2:3]

# Organizing Surgery dataset
dfadmissionSurgeryweek<-group_by(dfadmissionSurgery,format(as.POSIXct(dt_entrada_unidade,format='%Y-%m-%d'),'%Y%W'))%>%summarize(date=min(dt_entrada_unidade),quantity=sum(quantity))
names(dfadmissionSurgeryweek)[1]<-c('week')
dfadmissionSurgeryweek<-dfadmissionSurgeryweek[!is.na(dfadmissionSurgeryweek$week),2:3]

######################################
# Setting correct time periods to variables (ANS beneficiaries as predictor variable for ER admissions)
#
# Setting admissions variables  
dfadmissionERperiod<-subset(dfadmissionERweek,date>='2011-01-01' & date<='2016-12-31')
dfadmissionClinicperiod<-subset(dfadmissionClinicweek,date>='2011-01-01' & date<='2016-12-31')
dfadmissionSurgeryperiod<-subset(dfadmissionSurgeryweek,date>='2011-01-01' & date<='2016-12-31')

# creating additional columns
dfadmissionERperiod['week']<-format(as.POSIXct(dfadmissionERperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionClinicperiod['week']<-format(as.POSIXct(dfadmissionClinicperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionSurgeryperiod['week']<-format(as.POSIXct(dfadmissionSurgeryperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionPRED<-data.frame(week=format(seq.POSIXt(from=ISOdate(2017,1,1), to=ISOdate(2017,12,31),by='week'),format='%W'))
dfadmissionPRED['week']<-as.character(dfadmissionPRED$week)

# ANS dataset
Answeek<-subset(dfansweek,date>='2011-01-01' & date<='2016-12-31')
AnsPRED<-subset(dfansweek, date>='2016-12-31')
              
nrow(dfadmissionERperiod)
nrow(dfadmissionClinicperiod)
nrow(dfadmissionSurgeryperiod)
nrow(Answeek)

################################
#
vtitle<-"Evolution of Sao Paulo ANS beneficiaries and Samaritano admissions"
vnotes<-"Source: www.ans.gov.br"
plot(dfadmissionERperiod$date, dfadmissionERperiod$quantity,  ylim=range(-100:8000),col='blue', type='l', main = vtitle, sub = vnotes,ylab = "Quantity", xlab='Years')
lines(dfadmissionSurgeryperiod$date, dfadmissionSurgeryperiod$quantity, type='l', col='red')
lines(Answeek$date, Answeek$quantity/1000, type='l', col='magenta')
legend("topleft", c('SP ANS Beneficiaries (10^-3)', 'ER admisssions', 'Surgery admisssions'), fill=c("magenta","blue", "red"), horiz=TRUE, cex=.5)

#############################
# Generating graph 13 CSV file
#
weeks<-Answeek$date
dfcsvserie1<-data.frame(date=weeks, quantity_1=as.numeric(dfadmissionERperiod$quantity[1:nrow(Answeek)]), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=weeks, quantity_1=NA, quantity_2=as.numeric(dfadmissionSurgeryperiod$quantity[1:nrow(Answeek)]), quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=weeks, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(Answeek$quantity/1000), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3)
write.table(dfcsvseries, file = paste(stage_folder,"Graph13_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader1<-data.frame(date=NA, quantity_1=as.character('ER admissions'), quantity_2=as.character('Surgery admissions'), quantity_3=NA, pred_1=as.character('SP ANS Beneficiaries (10^-3)'), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes=vnotes, y_label='Number of Beneficiaries')
dfcsvheader2<-data.frame(date=NA, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=NA, notes=NA, y_label=NA)
dfcsvheader<-rbind(dfcsvheader1,dfcsvheader2)
write.table(dfcsvheader, file = paste(stage_folder,"Graph13_header.csv",sep=''), sep = ",",row.names=FALSE)

#############################
#
# Cross correlation between ANS Benef and hospital admissions
a<-ccf(Answeek$quantity, dfadmissionERperiod$quantity[2:313], lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line1<-data.frame(col1='ANS beneficiaries', col2='ER admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(Answeek$quantity, dfadmissionSurgeryperiod$quantity[2:313], lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line2<-data.frame(col1='ANS beneficiaries', col2='Surgery admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(Answeek$quantity, dfadmissionClinicperiod$quantity[2:313], lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line3<-data.frame(col1='ANS beneficiaries', col2='Clinical admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(dfadmissionERperiod$quantity, dfadmissionClinicperiod$quantity, lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line4<-data.frame(col1='ER admissions', col2='Clinical admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(dfadmissionERperiod$quantity, dfadmissionSurgeryperiod$quantity, lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line5<-data.frame(col1='ER admissions', col2='Surgery admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

a<-ccf(dfadmissionClinicperiod$quantity, dfadmissionSurgeryperiod$quantity, lag.max=90)
print(c(a$acf[which.max(abs(a$acf))],a$lag[which.max(abs(a$acf))]))
line6<-data.frame(col1='Clinical admissions', col2='Surgery admissions', col3=a$lag[which.max(abs(a$acf))], col4=a$acf[which.max(abs(a$acf))], col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA) 

#############################
#############################
#
# Generating table 2 CSV file
#
vtitle<-'Cross correlation among ANS Beneficiaries and Admissions'
vnotes<-''
dflines<-rbind(line1,line2,line3,line4, line5, line6)
write.table(dflines, file = paste(stage_folder,"Table2_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader<-data.frame(col1='Variable A', col2='Variable B', col3='Lag', col4='Correlation', col5=NA, col6=NA, col7=NA, col8=NA, col9=NA, col10=NA, title=vtitle, notes=vnotes)
write.table(dfcsvheader, file = paste(stage_folder,"Table2_header.csv",sep=''), sep = ",",row.names=FALSE)
#############################

#############################
#
# Generating prediction for ER
#
# Filtering datasets for prediction  
#dfadmissionERperiod<-subset(dfadmissionERweek,date>='2015-01-01' & date<='2016-12-31')
dfadmissionERperiod<-subset(dfadmissionERweek,date>='2011-01-01' & date<='2016-12-31')
dfadmissionClinicperiod<-subset(dfadmissionClinicweek,date>='2011-01-01' & date<='2016-12-31')
dfadmissionSurgeryperiod<-subset(dfadmissionSurgeryweek,date>='2011-01-01' & date<='2016-12-31')

# creating additional columns
dfadmissionERperiod['week']<-format(as.POSIXct(dfadmissionERperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionClinicperiod['week']<-format(as.POSIXct(dfadmissionClinicperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionSurgeryperiod['week']<-format(as.POSIXct(dfadmissionSurgeryperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionPRED<-data.frame(week=format(seq.POSIXt(from=ISOdate(2017,1,1), to=ISOdate(2017,12,31),by='week'),format='%W'))
dfadmissionPRED['week']<-as.character(dfadmissionPRED$week)

# ANS dataset (lag -76) surgery and ER
#Answeek<-subset(dfansweek,date>='2013-05-20' & date<='2015-05-31')
Answeek<-subset(dfansweek,date>='2009-04-20' & date<='2015-05-31')
AnsPRED<-subset(dfansweek,date>='2015-06-25' & date<='2016-06-30')

# creating dummies variables for training dfadmissionperiod
dummiesw<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=54)
for (i in 1:nrow(dfadmissionERperiod)) {
  dummiesw[i,as.numeric(dfadmissionERperiod[i,'week'])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxreg<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=52)
varxregb<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=53)
varxreg[,1:52]<-dummiesw
varxregb[,1:52]<-dummiesw
varxregb[,53]<-Answeek$quantity

# creating dummies variables for dfadmissionPRED (2017)
dummiesw<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=54)
for (i in 1:nrow(dfadmissionPRED)) {
  dummiesw[i,as.numeric(dfadmissionPRED[i,'week'][1])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxregpred<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=52)
varxregpredb<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=53)
varxregpred[,1:52]<-dummiesw
varxregpredb[,1:52]<-dummiesw
varxregpredb[,53]<-AnsPRED$quantity

#############################
#Fitting model for ER admissions without ANS as predictor variable
y <- ts(dfadmissionERperiod$quantity)
fitt1 <- Arima(y, order=c(1,1,2), seasonal=F, xreg=varxreg)
fcast1 <- forecast(fitt1, xreg=varxregpred, h=53)
plot(fcast1, main = "Evolution of Emergency Room admissions and prediction for 2017")

#Fitting model for ER admissions with ANS as predictor variable
y <- ts(dfadmissionERperiod$quantity)
fitt11 <- Arima(y, order=c(1,1,2), seasonal=F, xreg=varxregb)
fcast11 <- forecast(fitt11, xreg=varxregpredb, h=53)
plot(fcast11, main = "Evolution of Emergency Room admissions and prediction for 2017 (with ANS effect)")

# Generating graph 14 CSV file
vtitle<-"Evolution of Emergency Room admissions and prediction for 2017"
weeks<-dfadmissionERperiod$date
weeks1<-seq.POSIXt(from=ISOdate(2017,01,01), to=ISOdate(2017,12,31), by="week")
dfcsvserie1<-data.frame(date=weeks, quantity_1=as.numeric(dfadmissionERperiod$quantity), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(fcast1$mean), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=as.numeric(fcast11$mean), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3)
write.table(dfcsvseries, file = paste(stage_folder,"Graph14_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader<-data.frame(date=NA, quantity_1=as.character("Patent's admissions"), quantity_2=NA, quantity_3=NA, pred_1=as.character("Patent's admissions (model 1)"), pred_2=as.character("Patent's admissions (model 2)"), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes='*Model 1 - seasonal without ANS effect **Model 2 - seasonal with ANS effect', y_label='Number of Patients')
write.table(dfcsvheader, file = paste(stage_folder,"Graph14_header.csv",sep=''), sep = ",",row.names=FALSE)

#Writing ER admissions forecast
ForecastRegister <-data.frame(date=weeks1, pred_1 = as.numeric(fcast1$mean), pred_2=as.numeric(fcast11$mean), pred_3=NA)
write.table(ForecastRegister, file = paste(stage_folder,"dfadmEmergencyforecast2017.csv",sep=''), sep = ";")
ForecastRegister <-data.frame(date=weeks, quantity=as.numeric(dfadmissionERperiod$quantity))
write.table(ForecastRegister, file = paste(stage_folder,"dfadmEmergencyorig.csv",sep=''), sep = ";")

#############################
# Generating prediction for Surgery
#
# Filtering datasets for prediction  
#dfadmissionERperiod<-subset(dfadmissionERweek,date>='2015-01-01' & date<='2016-12-31')
dfadmissionSurgeryperiod<-subset(dfadmissionSurgeryweek,date>='2011-01-01' & date<='2016-12-31')

# creating additional columns
dfadmissionSurgeryperiod['week']<-format(as.POSIXct(dfadmissionSurgeryperiod$date,format='%Y-%m-%d'),'%W')
dfadmissionPRED<-data.frame(week=format(seq.POSIXt(from=ISOdate(2017,1,1), to=ISOdate(2017,12,31),by='week'),format='%W'))
dfadmissionPRED['week']<-as.character(dfadmissionPRED$week)

# ANS dataset (lag -76) surgery and ER
#Answeek<-subset(dfansweek,date>='2013-05-20' & date<='2015-05-31')
Answeek<-subset(dfansweek,date>='2010-10-01' & date<='2016-11-05')
Answeek<-Answeek[,2]
AnsPRED<-subset(dfansweek,date>='2016-11-06' & date<='2017-11-30')
AnsPRED<-AnsPRED[,2]
newvalues<-rep(AnsPRED[length(AnsPRED)],(53-length(AnsPRED)))
AnsPRED<-c(AnsPRED,newvalues)

# creating dummies variables for training dfadmissionperiod
dummiesw<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=54)
for (i in 1:nrow(dfadmissionERperiod)) {
  dummiesw[i,as.numeric(dfadmissionERperiod[i,'week'])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxreg<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=52)
varxregb<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=53)
varxreg[,1:52]<-dummiesw
varxregb[,1:52]<-dummiesw
varxregb[,53]<-Answeek

# creating dummies variables for dfadmissionPRED (2017)
dummiesw<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=54)
for (i in 1:nrow(dfadmissionPRED)) {
  dummiesw[i,as.numeric(dfadmissionPRED[i,'week'][1])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxregpred<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=52)
varxregpredb<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=53)
varxregpred[,1:52]<-dummiesw
varxregpredb[,1:52]<-dummiesw
varxregpredb[,53]<-AnsPRED

#Fitting model for Surgery admissions without ANS as predictor variable
y<-ts(c(dfadmissionSurgeryperiod$quantity))
fitt1 <- Arima(y, order=c(3,1,1), seasonal=F, xreg=varxreg)
fcast1 <- forecast(fitt1, xreg=varxregpred, h=53)
plot(fcast1, main = "Evolution of Surgery admissions and prediction for 2017")

#Fitting model for Surgery admissions with ANS as predictor variable
y <- ts(c(dfadmissionSurgeryperiod$quantity))
fitt11 <- Arima(y, order=c(2,1,2), seasonal=F, xreg=varxregb)
fcast11 <- forecast(fitt11, xreg=varxregpredb, h=53)
plot(fcast11, main = "Evolution of Surgery admissions and prediction for 2017 (with ANS effect)")

# Generating graph 16 CSV file
vtitle<-"Evolution of Surgery admissions and prediction for 2017"
weeks<-dfadmissionSurgeryperiod$date
weeks1<-seq.POSIXt(from=ISOdate(2017,01,01), to=ISOdate(2017,12,31), by="week")
dfcsvserie1<-data.frame(date=weeks, quantity_1=as.numeric(dfadmissionSurgeryperiod$quantity), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(fcast1$mean), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=as.numeric(fcast11$mean), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3)
write.table(dfcsvseries, file = paste(stage_folder,"Graph16_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader<-data.frame(date=NA, quantity_1=as.character("Patent's admissions"), quantity_2=NA, quantity_3=NA, pred_1=as.character("Patent's admissions (model 1)"), pred_2=as.character("Patent's admissions (model 2)"), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes='*Model 1 - seasonal without ANS effect **Model 2 - seasonal with ANS effect', y_label='Number of Patients')
write.table(dfcsvheader, file = paste(stage_folder,"Graph16_header.csv",sep=''), sep = ",",row.names=FALSE)

#Writing Surgery admissions forecast
ForecastRegister <-data.frame(date=weeks1, pred_1 = as.numeric(fcast1$mean), pred_2=as.numeric(fcast11$mean), pred_3=NA)
write.table(ForecastRegister, file = paste(stage_folder,"dfadmSurgeryforecast2017.csv",sep=''), sep = ";")
ForecastRegister <-data.frame(date=weeks, quantity=as.numeric(dfadmissionSurgeryperiod$quantity))
write.table(ForecastRegister, file = paste(stage_folder,"dfadmSurgeryorig.csv",sep=''), sep = ";")
#
#
# (changing lag) ANS dataset (lag -76) surgery and ER
#
# ANS dataset (lag 0) - clinical area
Answeek<-subset(dfansweek,date>='2010-11-22' & date<='2016-12-31')
AnsPRED<-subset(dfansweek,date>='2016-12-29' & date<='2017-12-31')
a<-auto.arima(AnsPRED$quantity)
b<-forecast(a,h=26)
c<-data.frame(date=seq.POSIXt(from=ISOdate(2017,7,7), to=ISOdate(2017,12,31),by='week'),quantity=b$mean)
AnsPRED<-AnsPRED[c(1,2)]
AnsPRED<-rbind(AnsPRED,c)

# creating dummies variables for training dfadmissionperiod
dummiesw<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=54)
for (i in 1:nrow(dfadmissionERperiod)) {
  dummiesw[i,as.numeric(dfadmissionERperiod[i,'week'])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxreg<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=52)
varxregb<-matrix(0,nrow=nrow(dfadmissionERperiod),ncol=53)
varxreg[,1:52]<-dummiesw
varxregb[,1:52]<-dummiesw
varxregb[,53]<-Answeek$quantity

# creating dummies variables for dfadmissionPRED (2017)
dummiesw<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=54)
for (i in 1:nrow(dfadmissionPRED)) {
  dummiesw[i,as.numeric(dfadmissionPRED[i,'week'][1])+1]<-1
}
dummiesw<-dummiesw[,1:52]
varxregpred<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=52)
varxregpredb<-matrix(0,nrow=nrow(dfadmissionPRED),ncol=53)
varxregpred[,1:52]<-dummiesw
varxregpredb[,1:52]<-dummiesw
varxregpredb[,53]<-AnsPRED$quantity

#############################
#Fitting model for Clinical admissions without ANS as predictor variable
y <- ts(dfadmissionClinicperiod$quantity)
fitt1 <- Arima(y, order=c(1,0,0), seasonal=F, xreg=varxreg)
fcast1 <- forecast(fitt1, xreg=varxregpred, h=53)
plot(fcast1, main = "Evolution of Clinical admissions and prediction for 2017")

#Fitting model for Clinical admissions with ANS as predictor variable
y <- ts(dfadmissionClinicperiod$quantity)
fitt11 <- Arima(y, order=c(1,0,0), seasonal=F, xreg=varxregb)
fcast11 <- forecast(fitt11, xreg=varxregpredb, h=53)
plot(fcast11, main = "Evolution of Clinical admissions and prediction for 2017 (with ANS effect)")

# Generating graph 15 CSV file
vtitle<-"Evolution of Clinical admissions and prediction for 2017"
weeks<-dfadmissionClinicperiod$date
weeks1<-seq.POSIXt(from=ISOdate(2017,01,01), to=ISOdate(2017,12,31), by="week")
dfcsvserie1<-data.frame(date=weeks, quantity_1=as.numeric(dfadmissionClinicperiod$quantity), quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie2<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=as.numeric(fcast1$mean), pred_2=NA, pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvserie3<-data.frame(date=weeks1, quantity_1=NA, quantity_2=NA, quantity_3=NA, pred_1=NA, pred_2=as.numeric(fcast11$mean), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA)
dfcsvseries<-rbind(dfcsvserie1,dfcsvserie2,dfcsvserie3)
write.table(dfcsvseries, file = paste(stage_folder,"Graph15_data.csv",sep=''), sep = ",", row.names=FALSE)
dfcsvheader<-data.frame(date=NA, quantity_1=as.character("Patent's admissions"), quantity_2=NA, quantity_3=NA, pred_1=as.character("Patent's admissions (model 1)"), pred_2=as.character("Patent's admissions (model 2)"), pred_3=NA, confinterv1_pred1=NA, confinterv2_pred1=NA, confinterv1_pred2=NA, confinterv2_pred2=NA, confinterv1_pred3=NA, confinterv2_pred3=NA,trend_1=NA, trend_2=NA, title=vtitle, notes='*Model 1 - seasonal without ANS effect **Model 2 - seasonal with ANS effect', y_label='Number of Patients')
write.table(dfcsvheader, file = paste(stage_folder,"Graph15_header.csv",sep=''), sep = ",",row.names=FALSE)

#Writing Clinical admissions forecast
ForecastRegister <-data.frame(date=weeks1, pred_1 = as.numeric(fcast1$mean), pred_2=as.numeric(fcast11$mean), pred_3=NA)
write.table(ForecastRegister, file = paste(stage_folder,"dfadmClinicalforecast2017.csv",sep=''), sep = ";")
ForecastRegister <-data.frame(date=weeks, quantity=as.numeric(dfadmissionClinicperiod$quantity))
write.table(ForecastRegister, file = paste(stage_folder,"dfadmClinicalorig.csv",sep=''), sep = ";")
#

