#File Name: Ramp_sp_Q2R
#Created by: Apoorba Bibeka
#Creation date: June 29 2017
#Date Modified : June 29 2017
#Purpose:To analyze the vissim queue results by time intervals results for ramp spacing scenarios  
#Last executed:
Sys.time()

rm(list=ls())
dir="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results"
setwd(dir)
source("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results/summarySE.R")


file="Ramp_sp__results_Tint.csv"
TT_Q_data<-read.csv(file)
TT_Q_data$Scenario_No<-as.character(TT_Q_data$Scenario_No)
sc<-lapply(TT_Q_data$Scenario_No,strsplit,split="Scenario")
sc<-unlist(sc)
sc<-as.numeric(sc[sc!=""])
TT_Q_data$Scenario_No<-as.factor(sc)
summary(TT_Q_data)
library(data.table)
TT_Q_data<-data.table(TT_Q_data)
setnames(TT_Q_data,"Scenario_No","Scenario")
list_sen<-fread("List_sc_Ramp_sp.csv",header=T)
setnames(list_sen,c( "Scenario","Ramp Configuration","Platooning Strategy","MPR (%)","Ramp Spacing (ft)")
         ,c( "Scenario","Ramp Configuration","Platooning Strategy","MPR","Ramp Spacing"))
list_sen<-data.table(list_sen)
final_data<-merge(TT_Q_data,list_sen, by="Scenario")
final_data[,MPR:=as.character(MPR)]
final_data[MPR==10, MPR:="10% MPR"]
final_data[MPR==50, MPR:="50% MPR"]
final_data[MPR==100, MPR:="100% MPR"]
final_data[,avg_spd:=TT_seg_len/(1.47*all_TT)]
final_data[,cavg_spd:=TT_seg_len/(1.47*chgv_TT)]
final_data[cavg_spd==Inf,cavg_spd:=NA]



#install.packages("ggplot2")
library(ggplot2)

final_data$`Ramp Configuration`<-factor(final_data$`Ramp Configuration`)
final_data$`Platooning Strategy`<-factor(final_data$`Platooning Strategy`)
final_data$`Ramp Spacing`<-factor(final_data$`Ramp Spacing`)



#final_data$`Ramp lane change`<-factor(final_data$`Ramp lane change`)
final_data[,MPR:=factor(MPR,levels=c("10% MPR","50% MPR","100% MPR"))]

#These are the cases where the ramp is en-ex and their is an auxilary lane present 
final_data[Max_Queue_Len2==-1000,Max_Queue_Len2:=Max_Queue_Len]



da_que<-summarySE(final_data,"Max_Queue_Len",c("Scenario","Tint"))
da_que<-merge(da_que,list_sen, by=c("Scenario"),all.x=TRUE)


da_que2<-summarySE(final_data,"Max_Queue_Len2",c("Scenario","Tint"))
da_que2<-merge(da_que2,list_sen, by=c("Scenario"),all.x=TRUE)

#lm(Avg_Queue_Len~Scenario,data=da_que)
da_spd<-summarySE(final_data,"avg_spd",c("Scenario","Tint"))
da_spd<-merge(da_spd,list_sen, by=c("Scenario"),all.x=TRUE)

da_chgv_spd<-summarySE(final_data,"cavg_spd",c("Scenario","Tint"))
da_chgv_spd<-merge(da_chgv_spd,list_sen,by=c("Scenario"),all.x=TRUE)


en_en<-grepl("en-en",da_que$`Ramp Configuration`)

# da_que[,Tint:=(Tint/60)-4]
g1<-ggplot(data=da_que[en_en,],aes(x=factor(Tint),y=Max_Queue_Len))
g1<-g1+geom_bar(stat="Identity",width=1)
g1<-g1+theme(axis.text.x = element_text(angle=90,hjust=1))
g1<-g1+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g1<-g1 + scale_x_discrete(breaks=seq(300,3600,300))
g1<-g1+labs(x="1 Minute Time Interval",y="Queue (ft)",title="Queue on the 1st Ramp over Time")

en_en<-grepl("en-en",da_que2$`Ramp Configuration`)
en_ex<-grepl("en-ex",da_que2$`Ramp Configuration`)

g2<-ggplot(data=da_que2[en_en,],aes(x=factor(Tint),y=Max_Queue_Len2))
g2<-g2+geom_bar(stat="Identity",width=1)
g2<-g2+theme(axis.text.x = element_text(angle=90,hjust=1))
g2<-g2+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g2<-g2 + scale_x_discrete(breaks=seq(300,3600,300))
g2<-g2+labs(x="1 Minute Time Interval",y="Queue (ft)",title="Queue on the 2nd Ramp over Time")

en_en<-grepl("en-en",da_spd$`Ramp Configuration`)
en_ex<-grepl("en-ex",da_spd$`Ramp Configuration`)

g3<-ggplot(data=da_spd[en_en,],aes(x=factor(Tint),y=avg_spd))
g3<-g3+geom_bar(stat="Identity",width=1)
g3<-g3+theme(axis.text.x = element_text(angle=90,hjust=1))
g3<-g3+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g3<-g3 + scale_x_discrete(breaks=seq(300,3600,300))
g3<-g3+labs(x="1 Minute Time Interval",y="Speed (Mph)",title="Speed over Time")

en_en<-grepl("en-en",da_chgv_spd$`Ramp Configuration`)
en_ex<-grepl("en-ex",da_chgv_spd$`Ramp Configuration`)

g4<-ggplot(data=da_chgv_spd[en_en,],aes(x=factor(Tint),y=cavg_spd))
g4<-g4+geom_bar(stat="Identity",width=1)
g4<-g4+theme(axis.text.x = element_text(angle=90,hjust=1))
g4<-g4+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g4<-g4 + scale_x_discrete(breaks=seq(300,3600,300))
g4<-g4+labs(x="1 Minute Time Interval",y="Speed (Mph)",title="Connected Trucks Speed over Time")
#*************************************************************************************************************************
en_ex<-grepl("en-ex",da_que$`Ramp Configuration`)
# da_que[,Tint:=(Tint/60)-4]
g5<-ggplot(data=da_que[en_ex,],aes(x=factor(Tint),y=Max_Queue_Len))
g5<-g5+geom_bar(stat="Identity",width=1)
g5<-g5+theme(axis.text.x = element_text(angle=90,hjust=1))
g5<-g5+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g5<-g5 + scale_x_discrete(breaks=seq(300,3600,300))
g5<-g5+labs(x="1 Minute Time Interval",y="Queue (ft)",title="Queue on the 1st Ramp over Time")

en_ex<-grepl("en-ex",da_que2$`Ramp Configuration`)

g6<-ggplot(data=da_que2[en_ex,],aes(x=factor(Tint),y=Max_Queue_Len2))
g6<-g6+geom_bar(stat="Identity",width=1)
g6<-g6+theme(axis.text.x = element_text(angle=90,hjust=1))
g6<-g6+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g6<-g6 + scale_x_discrete(breaks=seq(300,3600,300))
g6<-g6+labs(x="1 Minute Time Interval",y="Queue (ft)",title="Queue on the 2nd Ramp over Time")

en_ex<-grepl("en-ex",da_spd$`Ramp Configuration`)

g7<-ggplot(data=da_spd[en_ex,],aes(x=factor(Tint),y=avg_spd))
g7<-g7+geom_bar(stat="Identity",width=1)
g7<-g7+theme(axis.text.x = element_text(angle=90,hjust=1))
g7<-g7+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g7<-g7 + scale_x_discrete(breaks=seq(300,3600,300))
g7<-g7+labs(x="1 Minute Time Interval",y="Speed (Mph)",title="Speed over Time")

en_ex<-grepl("en-ex",da_chgv_spd$`Ramp Configuration`)

g8<-ggplot(data=da_chgv_spd[en_ex,],aes(x=factor(Tint),y=cavg_spd))
g8<-g8+geom_bar(stat="Identity",width=1)
g8<-g8+theme(axis.text.x = element_text(angle=90,hjust=1))
g8<-g8+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
g8<-g8 + scale_x_discrete(breaks=seq(300,3600,300))
g8<-g8+labs(x="1 Minute Time Interval",y="Speed (Mph)",title="Connected Trucks Speed over Time")




#*****************************************************************************************************************************
#Data Collection Point Results



file="Ramp_sp_results_DC.csv"
dc_data<-read.csv(file)
dc_data$Scenario_No<-as.character(dc_data$Scenario_No)
sc<-lapply(dc_data$Scenario_No,strsplit,split="Scenario")
sc<-unlist(sc)
sc<-as.numeric(sc[sc!=""])
dc_data$Scenario_No<-as.factor(sc)
summary(dc_data)
dc_data<-data.table(dc_data)
setnames(dc_data,"Scenario_No","Scenario")
dc_data<-merge(dc_data,list_sen, by="Scenario")
dc_data[,MPR:=as.character(MPR)]
dc_data[MPR==10, MPR:="10% MPR"]
dc_data[MPR==50, MPR:="50% MPR"]
dc_data[MPR==100, MPR:="100% MPR"]


dc_data$`Ramp Configuration`<-factor(dc_data$`Ramp Configuration`)
dc_data$`Platooning Strategy`<-factor(dc_data$`Platooning Strategy`)
dc_data$`Ramp Spacing`<-factor(dc_data$`Ramp Spacing`)
dc_data$lane<-factor(dc_data$lane)
dc_data$pos<-factor(dc_data$pos)

dc_spd<-summarySE(dc_data,"dc_Avg_spd",c("Scenario","pos","lane"))
dc_spd<-merge(dc_spd,list_sen, by=c("Scenario"),all.x=TRUE)

en_en2<-grepl("en-en",dc_spd$`Ramp Configuration`)
en_ex2<-grepl("en-ex",dc_spd$`Ramp Configuration`)
# da_que[,Tint:=(Tint/60)-4]
DCg1<-ggplot(data=dc_spd[en_en2,],aes(x=factor(pos),y=dc_Avg_spd,fill=lane))
DCg1<-DCg1+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())
DCg1<-DCg1+labs(x="Data Collection Point Location (ft)", y="Average Speed (Mph)",title="Average Instantaneous Speed")
DCg1<-DCg1+scale_fill_discrete(name="lane",breaks=c(1,2),labels=c("Middle/Right","Left"))
DCg1<-DCg1+theme(axis.text.x = element_text(angle=90,hjust=1))
DCg1<-DCg1+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
DCg1<-DCg1 + scale_x_discrete(breaks=seq(0,5000,500))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
DCg2<-ggplot(data=dc_spd[en_ex2,],aes(x=factor(pos),y=dc_Avg_spd,fill=lane))
DCg2<-DCg2+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())
DCg2<-DCg2+labs(x="Data Collection Point Location (ft)", y="Average Speed (Mph)",title="Average Instantaneous Speed")
DCg2<-DCg2+scale_fill_discrete(name="lane",breaks=c(1,2),labels=c("Middle/Right","Left"))
DCg2<-DCg2+theme(axis.text.x = element_text(angle=90,hjust=1))
DCg2<-DCg2+facet_grid(`Platooning Strategy`*`Ramp Spacing`~`Ramp Configuration`*MPR)
DCg2<-DCg2 + scale_x_discrete(breaks=seq(0,5000,500))

pdf("ramp_sp_Q_DC.pdf")
g1
g2
g3
g4
g5
g6
g7
g8
DCg1
DCg2
graphics.off()


#*****************************************************************************************************************************
#Cut Ins



file="cutIn.csv"
cutIn_data<-fread(file)
colnames(cutIn_data)<-c("Scenario","simSec","cutIn","CutVLen","PlatDist","CutVehDist","Speed","Acc")
cutIn_data$Scenario<-as.character(cutIn_data$Scenario)
sc<-lapply(cutIn_data$Scenario,strsplit,split="Scenario")
sc<-unlist(sc)
sc<-as.numeric(sc[sc!=""])
cutIn_data$Scenario<-as.factor(sc)
summary(cutIn_data)
cutIn_data<-cutIn_data[PlatDist>CutVehDist,]
cutIn_data[,Speed:=Speed*2.23694]
cutIn_data[,Acc:=Acc*3.28084]
cutIn_data[,CutVLen:=3.28084*as.numeric(CutVLen)]
cutIn_data[,PlatDist:=3.28084*PlatDist]
cutIn_data[,CutVehDist:=3.28084*CutVehDist]
cutIn_data<-format(cutIn_data,digits = 2,nsmall=2)
cutIn_data<-data.table(cutIn_data)
setorder(cutIn_data,PlatDist)
oldn<-c("Scenario","CutVLen","PlatDist","CutVehDist","Speed","Acc")
newn<-c("Scenario","Length Cut in Vehicle (ft)","Spacing before cut in (ft)","Spacing after cut in (ft)","Speed (Mph)","Acceleration (ft/s2)")
setnames(cutIn_data,oldn,newn)
table(cutIn_data$Scenario)
wfile<-"Pr_cutIn.csv"
fwrite(cutIn_data,wfile)



