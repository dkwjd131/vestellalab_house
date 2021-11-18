library(plumber)
library(httr)
library(tidyverse)
library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(sqldf)
library(devtools)
library(DT)



#* @get /Vestella
func_1 <- function(st1,st2,ed1,ed2,x,y,max_dur,t) {

  
  
  st1<-as.numeric(st1)*60
  st2<-as.numeric(st2)*60
  ed1<-as.numeric(ed1)*60
  ed2<-as.numeric(ed2)*60
  
  
  x<-as.numeric(x)
  y<-as.numeric(y)
  max_dur<-as.numeric(max_dur)
  t<-as.numeric(t)

  
  st_table<- r0401[between((as.numeric(hour(r0401$BGNG_RIDE_DTM))*60 + as.numeric(minute(r0401$BGNG_RIDE_DTM))), st1, st2),]
  ed_table<- r0401[between((as.numeric(hour(r0401$BGNG_RIDE_DTM))*60 + as.numeric(minute(r0401$BGNG_RIDE_DTM))), ed1, ed2),]
  
  st_table<-st_table[st_table$ALGH_X==x & st_table$ALGH_Y==y,] # 출근시간에 도착지가 회사인 데이터
  ed_table<-ed_table[ed_table$RIDE_X==x & ed_table$RIDE_Y==y,] # 퇴근시간에 출발지가 회사인 데이터
  
  st_table<-st_table[st_table$TIME<=t,]
  st_table<-st_table[st_table$Duration<=max_dur,]
  
  ed_table<-ed_table[ed_table$TIME<=t,]
  ed_table<-ed_table[ed_table$Duration<=max_dur,]
  

  r0401_st_bus<-st_table[substr(st_table$RIDE_STN1,2,4)=="bus",]
  r0401_st_sub<-st_table[substr(st_table$RIDE_STN1,2,4)=="sub",]
  r0401_ed_bus<-ed_table[substr(ed_table$ALGH_STN_F,2,4)=="bus",]
  r0401_ed_sub<-ed_table[substr(ed_table$ALGH_STN_F,2,4)=="sub",]
  

  
  st_bus<-r0401_st_bus[,-c(1,2,5)]
  st_sub<-r0401_st_sub[,-c(1,2,5)]
  ed_bus<-r0401_ed_bus[,-c(1,2,5)]
  ed_sub<-r0401_ed_sub[,-c(1,2,5)]
  
  
  
  st_sub<-ddply(st_sub,.(RIDE_STN1,RIDE_STN2,RIDE_STN3,RIDE_STN4,RIDE_STN5,ALGH_STN1,ALGH_STN2,ALGH_STN3,ALGH_STN4),transform,avg_TAMT=mean(TTUT_AMT),avg_Dur=mean(Duration),N=length(TIME))
  st_sub<-st_sub[,-c(1,9)]
  st_sub<-unique(st_sub)
  st_sub<-ddply(st_sub,.(RIDE_STN1),transform,max=max(N))
  st_sub<-st_sub[st_sub$N==st_sub$max,]
  st_sub<-ddply(st_sub,.(RIDE_STN1),transform,NN=length(TIME))
  st_sub_tmp<-st_sub[st_sub$NN>1,]
  st_sub<-st_sub[st_sub$NN==1,]
  st_sub_tmp<-ddply(st_sub_tmp,.(RIDE_STN1),transform,num=seq_along(TIME))
  st_sub_tmp<-st_sub_tmp[st_sub_tmp$num==1,]
  st_sub<-st_sub[,-c(34,35,36)]
  st_sub_tmp<-st_sub_tmp[,-c(34,35,36,37)]
  st_sub<-bind_rows(st_sub,st_sub_tmp)
  
  
  
  
  
  st_bus<-ddply(st_bus,.(RIDE_STN1,RIDE_STN2,RIDE_STN3,RIDE_STN4,RIDE_STN5,ALGH_STN1,ALGH_STN2,ALGH_STN3,ALGH_STN4),transform,avg_TAMT=mean(TTUT_AMT),avg_Dur=mean(Duration),N=length(TIME))
  st_bus<-st_bus[,-c(1,9)]
  st_bus<-unique(st_bus)
  st_bus<-ddply(st_bus,.(RIDE_STN1),transform,max=max(N))
  st_bus<-st_bus[st_bus$N==st_bus$max,]
  st_bus<-ddply(st_bus,.(RIDE_STN1),transform,NN=length(TIME))
  st_bus_tmp<-st_bus[st_bus$NN>1,]
  st_bus<-st_bus[st_bus$NN==1,]
  st_bus_tmp<-ddply(st_bus_tmp,.(RIDE_STN1),transform,num=seq_along(TIME))
  st_bus_tmp<-st_bus_tmp[st_bus_tmp$num==1,]
  st_bus<-st_bus[,-c(34,35,36)]
  st_bus_tmp<-st_bus_tmp[,-c(34,35,36,37)]
  st_bus<-bind_rows(st_bus,st_bus_tmp)
  
  
  
  
  
  
  ed_sub<-ddply(ed_sub,.(RIDE_STN2,RIDE_STN3,RIDE_STN4,RIDE_STN5,ALGH_STN1,ALGH_STN2,ALGH_STN3,ALGH_STN4,ALGH_STN_F),transform,avg_TAMT=mean(TTUT_AMT),avg_Dur=mean(Duration),N=length(TIME))
  ed_sub<-ed_sub[,-c(1,9)]
  ed_sub<-unique(ed_sub)
  ed_sub<-ddply(ed_sub,.(ALGH_STN_F),transform,max=max(N))
  ed_sub<-ed_sub[ed_sub$N==ed_sub$max,]
  ed_sub<-ddply(ed_sub,.(ALGH_STN_F),transform,NN=length(TIME))
  ed_sub_tmp<-ed_sub[ed_sub$NN>1,]
  ed_sub<-ed_sub[ed_sub$NN==1,]
  ed_sub_tmp<-ddply(ed_sub_tmp,.(ALGH_STN_F),transform,num=seq_along(TIME))
  ed_sub_tmp<-ed_sub_tmp[ed_sub_tmp$num==1,]
  ed_sub<-ed_sub[,-c(34,35,36)]
  ed_sub_tmp<-ed_sub_tmp[,-c(34,35,36,37)]
  ed_sub<-bind_rows(ed_sub,ed_sub_tmp)

  
  
  
  
  
  ed_bus<-ddply(ed_bus,.(RIDE_STN2,RIDE_STN3,RIDE_STN4,RIDE_STN5,ALGH_STN1,ALGH_STN2,ALGH_STN3,ALGH_STN4,ALGH_STN_F),transform,avg_TAMT=mean(TTUT_AMT),avg_Dur=mean(Duration),N=length(TIME))
  ed_bus<-ed_bus[,-c(1,9)]
  ed_bus<-unique(ed_bus)
  ed_bus<-ddply(ed_bus,.(ALGH_STN_F),transform,max=max(N))
  ed_bus<-ed_bus[ed_bus$N==ed_bus$max,]
  ed_bus<-ddply(ed_bus,.(ALGH_STN_F),transform,NN=length(TIME))
  ed_bus_tmp<-ed_bus[ed_bus$NN>1,]
  ed_bus<-ed_bus[ed_bus$NN==1,]
  ed_bus_tmp<-ddply(ed_bus_tmp,.(ALGH_STN_F),transform,num=seq_along(TIME))
  ed_bus_tmp<-ed_bus_tmp[ed_bus_tmp$num==1,]
  ed_bus<-ed_bus[,-c(34,35,36)]
  ed_bus_tmp<-ed_bus_tmp[,-c(34,35,36,37)]
  ed_bus<-bind_rows(ed_bus,ed_bus_tmp)

  
  

  max<-max(st_sub$avg_TAMT)
  min<-min(st_sub$avg_TAMT)
  difer<-max-min
  st_sub$SC_TAMT<-(st_sub$avg_TAMT-min)/difer*100
  st_sub$SC_TAMT<-100-st_sub$SC_TAMT
  
  
  max<-max(st_bus$avg_TAMT)
  min<-min(st_bus$avg_TAMT)
  difer<-max-min
  st_bus$SC_TAMT<-(st_bus$avg_TAMT-min)/difer*100
  st_bus$SC_TAMT<-100-st_bus$SC_TAMT
  
  
  max<-max(ed_sub$avg_TAMT)
  min<-min(ed_sub$avg_TAMT)
  difer<-max-min
  ed_sub$SC_TAMT<-(ed_sub$avg_TAMT-min)/difer*100
  ed_sub$SC_TAMT<-100-ed_sub$SC_TAMT
  
  
  max<-max(ed_bus$avg_TAMT)
  min<-min(ed_bus$avg_TAMT)
  difer<-max-min
  ed_bus$SC_TAMT<-(ed_bus$avg_TAMT-min)/difer*100
  ed_bus$SC_TAMT<-100-ed_bus$SC_TAMT
  
  
  
  

  max<-max(st_sub$avg_Dur)
  min<-min(st_sub$avg_Dur)
  difer<-max-min
  st_sub$SC_DUR<-(st_sub$avg_Dur-min)/difer*100
  st_sub$SC_DUR<-100-st_sub$SC_DUR
  
  
  
  max<-max(st_bus$avg_Dur)
  min<-min(st_bus$avg_Dur)
  difer<-max-min
  st_bus$SC_DUR<-(st_bus$avg_Dur-min)/difer*100
  st_bus$SC_DUR<-100-st_bus$SC_DUR
  
  
  max<-max(ed_sub$avg_Dur)
  min<-min(ed_sub$avg_Dur)
  difer<-max-min
  ed_sub$SC_DUR<-(ed_sub$avg_Dur-min)/difer*100
  ed_sub$SC_DUR<-100-ed_sub$SC_DUR
  
  
  max<-max(ed_bus$avg_Dur)
  min<-min(ed_bus$avg_Dur)
  difer<-max-min
  ed_bus$SC_DUR<-(ed_bus$avg_Dur-min)/difer*100
  ed_bus$SC_DUR<-100-ed_bus$SC_DUR
  
  
  
  
  
  #st_sub
  tmp1<-sqldf("select * from st_sub, ret where (6371*acos(cos(radians(RIDE_Y))*cos(radians(Y_COORD))*cos(radians(X_COORD)-radians(RIDE_X))+sin(radians(RIDE_Y))*sin(radians(Y_COORD))))<=1")
  #st_bus
  tmp2<-sqldf("select * from st_bus, ret where (6371*acos(cos(radians(RIDE_Y))*cos(radians(Y_COORD))*cos(radians(X_COORD)-radians(RIDE_X))+sin(radians(RIDE_Y))*sin(radians(Y_COORD))))<=0.5")
  #ed_sub
  tmp3<-sqldf("select * from ed_sub, ret where (6371*acos(cos(radians(ALGH_Y))*cos(radians(Y_COORD))*cos(radians(X_COORD)-radians(ALGH_X))+sin(radians(ALGH_Y))*sin(radians(Y_COORD))))<=1")
  #ed_bus
  tmp4<-sqldf("select * from ed_bus, ret where (6371*acos(cos(radians(ALGH_Y))*cos(radians(Y_COORD))*cos(radians(X_COORD)-radians(ALGH_X))+sin(radians(ALGH_Y))*sin(radians(Y_COORD))))<=0.5")
  
  
  result<-rbind(tmp1,tmp2,tmp3,tmp4)
  
  
  
  #Final Score!
  result$FIANL_SC<-result$SC_DUR*0.1+result$SC*0.5+result$SC_TAMT*0.4

  return(result)
}



#* @get /apikey
func_2<-function(){
  
  r<-GET(url="http://localhost:8000/authen2",add_headers(name="dk"))
  hh<-headers(r)
  return(hh)
}

#* @get /authen2
func_3<-function(){
  return("HI")
}
