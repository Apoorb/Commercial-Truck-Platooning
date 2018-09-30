#File Name: Scenario Generator for En-En and En-Ex ramp spacing
#Created by: Apoorba Bibeka
#Creation date: June 19 2017
#Date Modified : June 19 2017
#Purpose:To generate scenario for various EN-EN and EN-Ex ramp spacing
#Last executed:
Sys.time()


rm(list=ls())
dir="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results"
setwd(dir)
#******************************************************************************************************
# Entrance ramp followed by entrance ramp 
Platooning_strategy<-c("RL","FLS")  #Left Lane, Right Lane, Free lane selection       "LL",
`Ramp Configuration`<-c("en-en","en-ex")  #"Entrance Ramp no Auxiliary Lane","Entrance Ramp with Auxiliary Lane"
                            #,"Exit Ramp no Auxiliary Lane","Exit Ramp with Auxiliary Lane",
ramp_spacing<-c(700,2500)
MPR<-c(10,25,50,99.9)
`CACC Gap Setting`<-c(0.6)
`Ramp Volumes`<-600
##For LOS D Maximum service flow rate = 2090 pc/h/ln
# consider 1800 pc/hr/ln LOS D. 2 lanes input volume = 4180
#Consider LOS C for the ramp spacing study 
#freeway_vol<-3260 -1200 pcu/hr
#For LOS C Maximum service flow rate = 1630 pc/h/ln
#For LOS C 2 lanes input volume =3260

freeway_vol<-3260-2*600
freeway_vol_pcu<-freeway_vol
ramo_lane_change<-c("true","false")
#Percentage of trucks 
percent_truck=25

scenario=3
da<-data.frame()

library(plyr)

for(p in Platooning_strategy){
    for(m in MPR){
      for(c in `CACC Gap Setting`){
          for(rv in `Ramp Volumes`){
              for(rsp in ramp_spacing){
               fv=freeway_vol/(((100-percent_truck)/100)+(percent_truck/100)*1.5)  #freeway volume
               fv=round_any(fv,5)
               if(m>99) m=100
               p1<-ifelse(p=="RL","Right Lane","Free Lane Selection")
               rc1<-"en-en"
              temp<-data.frame(scenario,rc1,p1,m,c,fv,rv,rsp)
               da<-rbind(da,temp)
              scenario=scenario+1}
            }
        }
    }
}

da
tmp<-da[da$scenario==15,]
tmp$scenario=1
tmp$m=0.01
da=rbind(da,tmp)
tmp<-da[da$scenario==16,]
tmp$scenario=2
tmp$m=0.01
da=rbind(da,tmp)


da1<-da
da1$rc1="en-en-aux"
da<-rbind(da,da1)
#******************************************************************************************************
# Entrance ramp followed by exit ramp 

da1<-da
da1$rsp<-with(da1,ifelse(rsp==700,1000,2500))
da1$rc1<-with(da1,ifelse(rc1=="en-en","en-ex","en-ex-aux"))
da1$fv<-round_any(fv,5)
da<-rbind(da,da1)
da$scenario<-1:length(da$scenario)

colnames(da)<-c("Scenario","Ramp Configuration","Platooning Strategy","MPR","CACC Gap Setting","Freeway Volumes","Ramp Volumes","Ramp Spacing")
write.csv(da,file="List_sc_Ramp_sp.csv",row.names=FALSE,quote=FALSE)

