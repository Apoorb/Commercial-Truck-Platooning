#File Name: Ttest_TT
#Created by: Apoorba Bibeka
#Creation date: April 12 2017
#Date Modified : April 12 2017
#Purpose:To conduct T-test on travel time 
#Last executed:
Sys.time()

rm(list=ls())
dir="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ent_ramp_results"
setwd(dir)
source("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ent_ramp_results/summarySE_constant_N.R")


file="TT_Qlen_results.csv"
file ="TT_Qlen_results_Tint.csv"
TT_Q_data<-read.csv(file)
TT_Q_data$Scenario_No<-as.character(TT_Q_data$Scenario_No)
sc<-lapply(TT_Q_data$Scenario_No,strsplit,split="Scenario")
sc<-unlist(sc)
sc<-as.numeric(sc[sc!=""])
TT_Q_data$Scenario_No<-as.factor(sc)
# keep_col<-colnames(TT_Q_data)[-length(colnames(TT_Q_data))]
# TT_Q_data<-TT_Q_data[,c(keep_col)]
library(data.table)
TT_Q_data<-data.table(TT_Q_data)

summary(TT_Q_data)
setnames(TT_Q_data,"Scenario_No","Scenario")
list_sen<-fread("List_scenarios.csv",header=T)
final_data<-merge(TT_Q_data,list_sen, by="Scenario")
final_data[,avg_spd:=TT_seg_len/(1.47*all_TT)]

final_data[`Platooning Strategy`=="RL",`Platooning Strategy`:="Right Lane"]
final_data[`Platooning Strategy`=="FLS",`Platooning Strategy`:="Free Lane Selection"]
final_data[`Ramp Configuration`=="en_no_aux", `Ramp Configuration`:="Ent Ramp No Aux Lane"]
final_data[`Ramp Configuration`=="en_aux", `Ramp Configuration`:="Ent Ramp Aux Lane"]
final_data[,MPR:=as.character(MPR)]
final_data[MPR==0, MPR:="0% MPR"]
final_data[MPR==10, MPR:="10% MPR"]
final_data[MPR==25, MPR:="25% MPR"]
final_data[MPR==50, MPR:="50% MPR"]
final_data[MPR==100, MPR:="100% MPR"]




#install.packages("ggplot2")
library(ggplot2)

final_data$`Ramp Configuration`<-factor(final_data$`Ramp Configuration`)
final_data$`Platooning Strategy`<-factor(final_data$`Platooning Strategy`)
final_data$`Ramp lane change`<-factor(final_data$`Ramp lane change`)
final_data[,MPR:=factor(MPR,levels=c("0% MPR","10% MPR","25% MPR","50% MPR","100% MPR"))]
final_data[MPR=="0% MPR",`Platooning Strategy`:="Free Lane Selection"]
remove<-final_data[(MPR=="0% MPR"&`Ramp lane change`=="true"),Scenario]
remove<-unique(remove)
final_data<-final_data[!(Scenario %in% remove),]





#***************************************************************************************************************#
#Creating tables to present the results 
#***************************************************************************************************************#

colnames(final_data)
buf<-final_data

means_spd <- aggregate(avg_spd ~  Scenario, buf, mean)
means_queue_rmp <- aggregate(Max_Queue_Len_Ramp ~  Scenario, buf, mean)
means_queue_fre <- aggregate(Max_Queue_Len_Fre ~  Scenario, buf, mean)

res_dat<-merge(means_spd,list_sen,by="Scenario",all.x=TRUE)
res_dat<-merge(means_queue_rmp,res_dat,by="Scenario",all.x=TRUE)
res_dat<-merge(means_queue_fre,res_dat,by="Scenario",all.x=TRUE)

res_dat<-as.data.table(res_dat)
res_dat[,Scenario:=as.numeric(Scenario)]
res_dat[Scenario %in% c(1,2,5:36),LOS:="LOS D"]
res_dat[Scenario %in% c(3,4,37:68),LOS:="LOS C"]

colnames(res_dat)
res_dat<-res_dat[,.(Scenario,Max_Queue_Len_Ramp,Max_Queue_Len_Fre,avg_spd,`Ramp lane change`,`Platooning Strategy`,`Ramp Configuration`,MPR,`Percent Trucks`,LOS)]
res_dat[,avg_spd:=round(avg_spd,2)]
res_dat[,Max_Queue_Len_Ramp:=round(Max_Queue_Len_Ramp,2)]
res_dat[,Max_Queue_Len_Fre:=round(Max_Queue_Len_Fre,2)]
res_dat<-data.table(res_dat)
res_dat[`Ramp Configuration`=="Ent Ramp Aux Lane",`Ramp Configuration`:="Entrance Ramp with Auxiliary Lane"]
res_dat[`Ramp Configuration`=="Ent Ramp No Aux Lane",`Ramp Configuration`:="Entrance Ramp without Auxiliary Lane"]
setnames(res_dat,c("Platooning Strategy"),c("Lane Restriction Policy"))
res_dat[`Lane Restriction Policy`=="Right Lane",`Lane Restriction Policy`:= "Right Lane Restriction"]
res_dat[MPR==0,`Lane Restriction Policy`:="Free Lane Selection"]

per_truck_label<-c(`50`="50 % Total Trucks",`25`="25 % Total Trucks")
require(xlsx)

write.xlsx(res_dat,"MeanSpdQue.xls","Results")





rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
source("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results/ggplot_fun1.R")


res_dat[,`Ramp Configuration`:=as.factor(`Ramp Configuration`)]
res_dat[,MPR:=as.factor(MPR)]

res_dat[,inter:=interaction(`Ramp Configuration`, `Lane Restriction Policy`)]
Nplt1<-plots_summary_ent_R(data=res_dat[`Ramp lane change`=="No action"],
                           x1=MPR,
                           y1=avg_spd,
                           col1=`Ramp Configuration`,
                           shape1=`Lane Restriction Policy`,
                           group1=inter,
                           facet1=~LOS,
                           ylab1="Speed on Mainlanes (Mph)",
                           title1="Average Speed",
                           cap1="Note: Does not Consider Ramp Lane Change Policy")
Nplt1<-Nplt1+ylim(c(0,70))


Nplt2<-plots_summary_ent_R(data=res_dat[`Ramp lane change`=="No action"],
                           x1=MPR,
                           y1=Max_Queue_Len_Ramp,
                           col1=`Ramp Configuration`,
                           shape1=`Lane Restriction Policy`,
                           group1=inter,
                           facet1=~LOS,
                           ylab1="Queue Length on Ramp (feet)",
                           title1="Average of Maximum Queue - Ramp",
                           cap1="Note: Does not Consider Ramp Lane Change Policy")
Nplt2<-Nplt2+ylim(c(0,3000))
Nplt3<-plots_summary_ent_R(data=res_dat[`Ramp lane change`=="No action"],
                           x1=MPR,
                           y1=Max_Queue_Len_Fre,
                           col1=`Ramp Configuration`,
                           shape1=`Lane Restriction Policy`,
                           group1=inter,
                           facet1=~LOS,
                           ylab1="Queue Length on Mainlanes (feet)",
                           title1="Average of Maximum Queue - Freeway Lanes",
                           cap1="Note: Does not Consider Ramp Lane Change Policy")
Nplt3<-Nplt3+ylim(c(0,3000))

# Nplt2<-ggplot(data=res_dat[`Ramp lane change`=="No action"],aes(x=MPR,y=Max_Queue_Len_Ramp,colour=`Ramp Configuration`, shape = `Lane Restriction Policy`,
#                                                             group=interaction(`Ramp Configuration`, `Lane Restriction Policy`)))+theme_bw(base_size=20)+geom_line(size=1)+geom_point(colour="black",size=4)+facet_grid(~LOS)+
#   scale_color_manual(values=rainbow12equal[c(7,1)])+labs(x="Market Penetration Rate of Connected Trucks",y="Queue Length (ft)",title="Average of Maximum Queue Length -Ramp",caption="No Ramp Lane Change Policy")+scale_shape_manual(values=c(0,3))
# 

res_dat[,inter1:=interaction(`Ramp Configuration`, `Lane Restriction Policy`)]
Nplt4<-plots_summary_ent_R1(data=res_dat[LOS=="LOS D"],
                           x1=MPR,
                           y1=avg_spd,
                           col1=NA,
                           shape1=`Ramp lane change`,
                           group1=NA,
                           facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                           ylab1="Speed on Mainlanes (Mph)",
                           title1="Average Speed",cap1="Note: LOS D")
Nplt4<-Nplt4+ylim(c(0,70))
ggsave("P1.tiff",plot=Nplt4,device="tiff",width=6,height=7,units="in")


Nplt5<-plots_summary_ent_R1(data=res_dat[LOS=="LOS D"],
                           x1=MPR,
                           y1=Max_Queue_Len_Ramp,
                           col1=NA,
                           shape1=`Ramp lane change`,
                           group1=NA,
                           facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                           ylab1="Queue Length on Ramp (feet)",
                           title1="Average of Maximum Queue - Ramp",
                           cap1="Note: LOS D")
Nplt5<-Nplt5+ylim(c(0,3000))

Nplt6<-plots_summary_ent_R1(data=res_dat[LOS=="LOS D"],
                           x1=MPR,
                           y1=Max_Queue_Len_Fre,
                           col1=NA,
                           shape1=`Ramp lane change`,
                           group1=NA,
                           facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                           ylab1="Queue Length on Mainlanes (feet)",
                           title1="Average of Maximum Queue - Freeway Lanes",
                           cap1="Note: LOS D")
Nplt6<-Nplt6+ylim(c(0,3000))


Nplt7<-plots_summary_ent_R1(data=res_dat[LOS=="LOS C"],
                            x1=MPR,
                            y1=avg_spd,
                            col1=NA,
                            shape1=`Ramp lane change`,
                            group1=NA,
                            facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                            ylab1="Speed on Mainlanes (Mph)",
                            title1="Average Speed",cap1="Note: LOS C")
Nplt7<-Nplt7+ylim(c(0,70))


Nplt8<-plots_summary_ent_R1(data=res_dat[LOS=="LOS C"],
                            x1=MPR,
                            y1=Max_Queue_Len_Ramp,
                            col1=NA,
                            shape1=`Ramp lane change`,
                            group1=NA,
                            facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                            ylab1="Queue Length on Ramp (feet)",
                            title1="Average of Maximum Queue - Ramp",
                            cap1="Note: LOS C")
Nplt8<-Nplt8+ylim(c(0,3000))

Nplt9<-plots_summary_ent_R1(data=res_dat[LOS=="LOS C"],
                            x1=MPR, 
                            y1=Max_Queue_Len_Fre,
                            col1=NA,
                            shape1=`Ramp lane change`,
                            group1=NA,
                            facet1=~`Ramp Configuration`~`Lane Restriction Policy`,
                            ylab1="Queue Length (ft)",
                            title1="Average of Maximum Queue - Freeway Lanes",
                            cap1="Note: LOS C")
Nplt9<-Nplt9+ylim(c(0,3000))

#***************************************************************************************************************#
# Cut In 
CutIn<-fread("cutIn.csv")

names(CutIn)<-c("Scenario","SimSec","No","LeadVehLen","Platoon_spacing (m)","SpacingAfterCutin (m)","Speed(m/s)","Acc(m/s2)","Leader_State","Odometer")
CutIn[,cuts:=sum(No),by=Scenario]
CutIn[,cuts:=cuts/5]
CutIn<-unique(CutIn,by="Scenario")
CutIn<-CutIn[,.(Scenario,cuts)]
CutIn[,Scenario:=tstrsplit(Scenario,"Scenario",keep="2")]
CutIn[,Scenario:=as.numeric(Scenario)]
CutIn<-merge(res_dat,CutIn,all.x=TRUE)
CutIn[is.na(CutIn$cuts),cuts:=0]


Nplt10<-plots_summary_ent_R(data=CutIn[`Ramp lane change`=="No action"],
                           x1=MPR,
                           y1=cuts,
                           col1=`Ramp Configuration`,
                           shape1=`Lane Restriction Policy`,
                           group1=inter,
                           facet1=~LOS,
                           ylab1="Frequency of Cut-Ins",
                           title1="Cut-in Maneuver",
                           cap1="Note: Does not Consider Ramp Lane Change Policy")
Nplt10<-Nplt10+ylim(c(0,130))


Nplt11<-plots_summary_ent_R(data=CutIn[`Ramp lane change`=="Entire platoon change lane on approaching ramp"],
                            x1=MPR,
                            y1=cuts,
                            col1=`Ramp Configuration`,
                            shape1=`Lane Restriction Policy`,
                            group1=inter,
                            facet1=~LOS,
                            ylab1="Frequency of Cut-Ins",
                            title1="Cut-in Maneuver",
                            cap1="Note: Considers Ramp Lane Change Policy")
Nplt11<-Nplt11+ylim(c(0,130))


pdf("Entrance_ramp.pdf",onefile = TRUE,width=12,height=12,family="Helvetica")
Nplt1
Nplt2
Nplt3
Nplt4
Nplt5
Nplt6
Nplt7
Nplt8
Nplt9
Nplt10
Nplt11
graphics.off()
ggsave("P1.tiff",plot=Nplt1,device="tiff",width=6,height=7,units="in")
ggsave("P2.tiff",plot=Nplt2,device="tiff",width=6,height=7,units="in")
ggsave("P3.tiff",plot=Nplt3,device="tiff",width=6,height=7,units="in")
ggsave("P4.tiff",plot=Nplt4,device="tiff",width=6,height=7,units="in")
ggsave("P5.tiff",plot=Nplt5,device="tiff",width=6,height=7,units="in")
ggsave("P6.tiff",plot=Nplt6,device="tiff",width=6,height=7,units="in")
ggsave("P7.tiff",plot=Nplt7,device="tiff",width=6,height=7,units="in")
ggsave("P8.tiff",plot=Nplt8,device="tiff",width=6,height=7,units="in")
ggsave("P9.tiff",plot=Nplt9,device="tiff",width=6,height=7,units="in")
ggsave("P10.tiff",plot=Nplt10,device="tiff",width=6,height=7,units="in")
ggsave("P11.tiff",plot=Nplt11,device="tiff",width=6,height=7,units="in")


#***************************************************************************************************************#
da_que<-summarySE(final_data,"Max_Queue_Len","Scenario")
da_que<-merge(da_que,list_sen, by="Scenario")

da_spd<-summarySE(final_data,"avg_spd","Scenario")
da_spd<-merge(da_spd,list_sen, by="Scenario")

da_min_spd<-summarySE(final_data,"min_spd","Scenario")
da_min_spd<-merge(da_min_spd,list_sen, by="Scenario")

da_que[,CIupper:=round(Max_Queue_Len+ci,2)]
da_que[,CIlower:=round(Max_Queue_Len-ci,2)]
da_spd[,CIupper:=round(avg_spd+ci,2)]
da_spd[,CIlower:=round(avg_spd-ci,2)]
da_min_spd[,CIupper:=round(min_spd+ci,2)]
da_min_spd[,CIlower:=round(min_spd-ci,2)]



#*****************************************************************************************************
#Plots for average queue length 
g1<-ggplot(data=da_que[Scenario %in% 1:60,],aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g1<-g1+  geom_errorbar(aes(ymin=Max_Queue_Len-ci, ymax=Max_Queue_Len+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
g1<-g1+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g1<-g1+labs(x="Market Penetration Rate of Connected Trucks", y="Average  Queue Length (ft)",title="Average Maximum Queue Length")
g1<-g1+theme(strip.text.y=element_text(size=9))+ylim(-500,2500)
g1<-g1+  geom_text(aes(label = CIlower, y = Max_Queue_Len-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = Max_Queue_Len+ci), vjust = -1.5,size=2,position=position_dodge(1)) 


#Plots for Avg spd 
g2<-ggplot(data=da_spd[Scenario %in% 1:60,],aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g2<-g2+  geom_errorbar(aes(ymin=avg_spd-ci, ymax=avg_spd+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
g2<-g2+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g2<-g2+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")
g2<-g2+theme(strip.text.y=element_text(size=9))+ylim(0,80)
g2<-g2+  geom_text(aes(label = CIlower, y = avg_spd-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = avg_spd+ci), vjust = -1.5,size=2,position=position_dodge(1)) 
#*****************************************************************************************************


#BoxPlots LoS -D 
#*****************************************************************************************************

final_data0<-final_data[Scenario %in% 1:60,]
colnames(final_data0)
buf<-final_data0[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data0, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g3<-ggplot(data=final_data0,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g3<-g3+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g3<-g3+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g3<-g3+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length (LOS-D)")+theme_bw()
g3<-g3+theme(strip.text=element_text(size=12),axis.title=element_text(size=12),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.title = element_text(size=12),legend.text=element_text(size=12))

colnames(final_data0)
buf<-final_data0[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data0, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g4<-ggplot(data=final_data0,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g4<-g4+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g4<-g4+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g4<-g4+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles (LOS-D)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))




#BoxPlots LoS -D and 50 % trucks
#*****************************************************************************************************

final_data1<-final_data[Scenario %in% 1:60 &`Percent Trucks`==50,]
colnames(final_data1)
buf<-final_data1[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data1, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g5<-ggplot(data=final_data1,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g5<-g5+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g5<-g5+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g5<-g5+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length (LOS-D and 50% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g5<-g5+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


library(gridExtra)
pdf("TestPlot.pdf",onefile = TRUE,width=12,height=12,family="Helvetica")
g5
graphics.off()

colnames(final_data1)
buf<-final_data1[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data1, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g6<-ggplot(data=final_data1,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g6<-g6+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g6<-g6+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g6<-g6+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles (LOS-D and 50% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g6<-g6+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


#BoxPlots LoS -D and 25 % trucks
#*****************************************************************************************************

final_data2<-final_data[Scenario %in% 1:60 &`Percent Trucks`==25,]
colnames(final_data2)
buf<-final_data2[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data2, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g7<-ggplot(data=final_data2,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g7<-g7+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g7<-g7+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g7<-g7+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length (LOS-D and 25% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g7<-g7+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


buf<-final_data2[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data2, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g8<-ggplot(data=final_data2,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g8<-g8+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g8<-g8+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g8<-g8+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles (LOS-D and 25% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g8<-g8+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))



#BoxPlots LoS -C and 50 % trucks
#*****************************************************************************************************

final_data3<-final_data[Scenario %in% 61:120 &`Percent Trucks`==50,]
colnames(final_data3)
buf<-final_data3[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data3, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g9<-ggplot(data=final_data3,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g9<-g9+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g9<-g9+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g9<-g9+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length (LOS-C and 50% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g9<-g9+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


colnames(final_data3)
buf<-final_data3[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data3, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g10<-ggplot(data=final_data3,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g10<-g10+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g10<-g10+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g10<-g10+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles (LOS-C and 50% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g10<-g10+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


#BoxPlots LoS -C and 25 % trucks
#*****************************************************************************************************


final_data4<-final_data[Scenario %in% 61:120 &`Percent Trucks`==25,]
colnames(final_data4)
buf<-final_data4[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data4, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g11<-ggplot(data=final_data4,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g11<-g11+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g11<-g11+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g11<-g11+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length (LOS-C and 25% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g11<-g11+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))


buf<-final_data4[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data4, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g12<-ggplot(data=final_data4,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g12<-g12+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g12<-g12+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g12<-g12+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles (LOS-C and 25% Trucks)")+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
g12<-g12+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))



#BoxPlots LoS -C
#*****************************************************************************************************

final_data5<-final_data[Scenario %in% 61:120,]
buf<-final_data5[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data5, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g13<-ggplot(data=final_data5,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g13<-g13+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g13<-g13+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 100),position = position_dodge(1))
g13<-g13+labs(x="Market Penetration Rate of Connected Trucks", y="Average  Queue Length (ft)",title="Average Maximum Queue Length")+theme_bw()


colnames(final_data5)
buf<-final_data5[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data5, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)

g14<-ggplot(data=final_data5,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Percent Trucks`+`Platooning Strategy`~`Ramp Configuration`)
g14<-g14+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g14<-g14+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 2),position = position_dodge(1))
g14<-g14+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")+theme_bw()


#*****************************************************************************************************
#Make footnotes 
#*****************************************************************************************************
makeFootnote <- function(footnoteText =
                           format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}
#*****************************************************************************************************


#BoxPlots LoS -C + Scenarios 120- 125 included
#*****************************************************************************************************
final_data6<-final_data[Scenario %in% c(2:10,62:70,122:125,127:130),]
final_data6[,`Ramp lane change`:=factor(`Ramp lane change`,levels(final_data$`Ramp lane change`)[c(2,3,1)])]
buf<-final_data6[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks","Freeway Volumes")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Max_Queue_Len ~  Scenario, final_data6, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)
vol_label<-c(`2130`="LOS C",`2720`="LOS D")
g15<-ggplot(data=final_data6,aes(x=factor(MPR),y=Max_Queue_Len,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Freeway Volumes`~.,labeller = labeller(`Freeway Volumes`=vol_label))+theme_bw(base_size = 20)
g15<-g15+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g15<-g15+geom_text(data = means, aes(label = round(Max_Queue_Len,digits=1), y = Max_Queue_Len + 180),position = position_dodge(1),size=6)
g15<-g15+labs(caption="No Auxiliary Lane + Right Lane Restriction",x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length")
g15<-g15+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))
g15<-g15+theme (plot.caption=element_text(hjust=1, vjust=0.5))

final_data6<-final_data[Scenario %in% c(2:10,62:70,122:125,127:130),]
final_data6[,`Ramp lane change`:=factor(`Ramp lane change`,levels(final_data$`Ramp lane change`)[c(2,3,1)])]
buf<-final_data6[,c("Scenario","Ramp lane change","Platooning Strategy","Ramp Configuration","MPR","Percent Trucks","Freeway Volumes")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data6, mean)
means<-merge(means,buf,by="Scenario",all.x=TRUE)
means<-data.table(means)
vol_label<-c(`2130`="LOS C",`2720`="LOS D")
g16<-ggplot(data=final_data6,aes(x=factor(MPR),y=avg_spd,fill=`Ramp lane change`))+geom_boxplot(colour="light blue",stat="boxplot",position=position_dodge(1))+facet_grid(`Freeway Volumes`~.,labeller = labeller(`Freeway Volumes`=vol_label))
g16<-g16+stat_summary(fun.y=mean, colour="black", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g16<-g16+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd + 4),position = position_dodge(1),size=8)
g16<-g16+labs(caption="No Auxiliary Lane + Right Lane Restriction",x="Market Penetration Rate of Connected Trucks",y="Average Speed (Mph)",title="Average Speed (Mph)")+theme_bw(base_size = 18)
g16<-g16+scale_fill_discrete(breaks=c("false","true","EntPlatLnChRamp"),labels=c("false","true","Entire Platoon Changes Lane"))
g16<-g16+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))
g16<-g16+theme (plot.caption=element_text(hjust=1, vjust=0.5))


# 
# 
# library(gridExtra)
# pdf("myplots1.pdf",onefile = TRUE,width=12,height=12,family="Helvetica")
# g1
# g2
# g3
# g4
# g5
# g6
# g7
# g8
# g9
# g10
# g11
# g12
# g13
# g14
# g15
# g16
# graphics.off()

pdf("Plot2.pdf",onefile = TRUE,width=12,height=12,family="Helvetica")
tiff("Plot2.tiff")

# g15
g16
graphics.off()




