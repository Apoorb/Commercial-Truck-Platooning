#File Name: Scenario Generator for Entrance Ramps 
#Created by: Apoorba Bibeka
#Creation date: April 19 2017
#Date Modified : April 19 2017
#Purpose:To generate scenario for entrance ramps 
#Last executed:
Sys.time()


rm(list=ls())
dir="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ent_ramp_results"
setwd(dir)
library(data.table)
Platooning_strategy<-c("RL","FLS")  #Left Lane, Right Lane, Free lane selection       "LL",
`Ramp Configuration`<-c("en_no_aux","en_aux")  #"Entrance Ramp no Auxiliary Lane","Entrance Ramp with Auxiliary Lane"
                            #,"Exit Ramp no Auxiliary Lane","Exit Ramp with Auxiliary Lane",
MPR<-c(10,25,50,99.9)
`CACC Gap Setting`<-c(0.6)
`Ramp Volumes`<-600
#For LOS C Maximum service flow rate = 1630 pc/h/ln
#For LOS C 2 lanes input volume =3260
##For LOS D Maximum service flow rate = 2090 pc/h/ln
# consider 1800 pc/hr/ln LOS D. 2 lanes input volume = 4000
freeway_vol<-c(4000,3260)-`Ramp Volumes` #pcu (4700 pcu= 2*2350 pch/hr/ln :freeway capacity at FF speed of 65 mph)
freeway_vol_pcu<-freeway_vol
ramp_lane_change<-c("false","EntPlatLnChRamp")
#Percentage of trucks 
percent_truck=c(25)
scenario=5
da<-data.frame()

library(plyr)
for(free_vol in freeway_vol){
for(prT in percent_truck){
for(p in Platooning_strategy){
  for(rc in `Ramp Configuration`){
    for(m in MPR){
      for(c in `CACC Gap Setting`){
          for(rv in `Ramp Volumes`){
            for(rlc in ramp_lane_change){
               fv=free_vol/(((100-prT)/100)+(prT/100)*1.5)  #freeway volume
               fv=round_any(fv,5)
               if(m>99) m=100
               if(m<1) m =0
               p1<-ifelse(p=="RL","Right Lane Restriction","Free Lane Selection")
               rc1<-ifelse(rc=="en_no_aux","Without Auxiliary Lane","With Auxiliary Lane")
               rlc<-ifelse(rlc=="false","No action ","Entire platoon change lane on approaching ramp")
              temp<-data.frame(scenario,rlc,p1,rc1,m,c,fv,rv,prT)
              if(rlc=="true"& rc=="en_aux"){
                
              }
              else{
                da<-rbind(da,temp)
              scenario=scenario+1}
          }
        }
      }
    }
  }
}
}
}

#Base case scenarios
tmp<-head(da[da$rc1=="Without Auxiliary Lane" &da$fv==3020,],1)
tmp$scenario=1
tmp$m=0
da=rbind(da,tmp)
tmp<-head(da[da$rc1=="With Auxiliary Lane"&da$fv==3020,],1)
tmp$scenario=2
tmp$m=0
da=rbind(da,tmp)
tmp<-head(da[da$rc1=="Without Auxiliary Lane"&da$fv==2365,],1)
tmp$scenario=3
tmp$m=0
da=rbind(da,tmp)
tmp<-head(da[da$rc1=="With Auxiliary Lane"&da$fv==2365,],1)
tmp$scenario=4
tmp$m=0
da=rbind(da,tmp)


colnames(da)<-c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","CACC Gap Setting","Freeway Volumes","Ramp Volumes","Percent Trucks")
da<-data.table(da)
setorder(da,by="Scenario")
# temp<-da[Scenario>=61& Scenario<=69 &`Ramp lane change`=="true",]
# temp$Scenario<-121:125
# temp$`Ramp lane change`<-"EntPlatLnChRamp" #Entire Platoon Lane change on ramp
# da<-rbindlist(list(da,temp))
# 
# temp<-da[Scenario>=1& Scenario<=10 &`Ramp lane change`=="true",]
# temp$Scenario<-126:130
# temp$`Ramp lane change`<-"EntPlatLnChRamp" #Entire Platoon Lane change on ramp
# da<-rbindlist(list(da,temp))

write.csv(da,file="List_scenarios.csv",row.names=FALSE,quote=FALSE)


scenario=5
da1<-data.frame()
# change formating to input in COM
for(free_vol in freeway_vol){
  for(prT in percent_truck){
for(p in Platooning_strategy){
  for(rc in `Ramp Configuration`){
    for(m in MPR){
      for(c in `CACC Gap Setting`){
        for(rv in `Ramp Volumes`){
          for(rlc in ramp_lane_change){
            fv=free_vol/(((100-prT)/100)+(prT/100)*1.5)  #freeway volume
            fv=round_any(fv,5)
            percent_chgv<-prT*m/100
            percent_hgv=prT-percent_chgv
            temp<-data.frame(scenario,rlc,p,rc,percent_chgv,percent_hgv,c,fv,rv)
            if(rlc=="true"& rc=="en_aux"){
              
            }
            else{
              da1<-rbind(da1,temp)
              scenario=scenario+1}
          }
        }
      }
    }
  }
}
}
}

#Base case scenarios
tmp<-head(da1[da1$rc=="en_no_aux" &da1$fv==3020,],1)
tmp$scenario=1
tmp$percent_chgv=0.01
tmp$percent_hgv=25-tmp$percent_chgv
da1=rbindlist(list(da1,tmp))

tmp<-head(da1[da1$rc=="en_aux"&da1$fv==3020,],1)
tmp$scenario=2
tmp$percent_chgv=0.01
tmp$percent_hgv=25-tmp$percent_chgv
da1=rbindlist(list(da1,tmp))

tmp<-head(da1[da1$rc=="en_no_aux"&da1$fv==2365,],1)
tmp$scenario=3
tmp$percent_chgv=0.01
tmp$percent_hgv=25-tmp$percent_chgv
da1=rbindlist(list(da1,tmp))

tmp<-head(da1[da1$rc=="en_aux"&da1$fv==2365,],1)
tmp$scenario=4
tmp$percent_chgv=0.01
tmp$percent_hgv=25-tmp$percent_chgv
da1=rbindlist(list(da1,tmp))


da1<-data.table(da1)
da1[,rlc:=as.character(rlc)]

colnames(da1)<-c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","percent_chgv","percent_hgv","CACC Gap Setting","Freeway Volumes","Ramp Volumes")
da1<-data.table(da1)
setorder(da1,by="Scenario")

# temp<-da1[Scenario>=61& Scenario<=69 &`Ramp lane change`=="true",]
# temp$Scenario<-121:125
# temp$`Ramp lane change`<-"EntPlatLnChRamp" #Entire Platoon Lane change on ramp
# da1<-rbindlist(list(da1,temp))
# 
# temp<-da1[Scenario>=1& Scenario<=10 &`Ramp lane change`=="true",]
# temp$Scenario<-126:130
# temp$`Ramp lane change`<-"EntPlatLnChRamp" #Entire Platoon Lane change on ramp


da1<-rbindlist(list(da1,temp))
fwrite(da1,file="com_scenarios.csv",row.names=FALSE,quote=FALSE)


