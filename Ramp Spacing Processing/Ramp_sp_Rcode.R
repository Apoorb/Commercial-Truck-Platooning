#File Name: Ramp_sp_Rcode.R
#Created by: Apoorba Bibeka
#Creation date: June 21 2017
#Date Modified : June 21 2017
#Purpose:To analyze the vissim results for ramp spacing scenarios  
#Last executed:
Sys.time()

rm(list=ls())
dir="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results"
setwd(dir)
source("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results/summarySE_constant_N.R")


file="Ramp_sp_results.csv"
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
#**********************************************************************************************
list_sen<-fread("List_sc_Ramp_sp.csv",header=T)
list_sen<-data.table(list_sen)
setnames(list_sen,c("MPR (%)","Ramp Spacing (ft)"),c("MPR","Ramp Spacing"))
final_data<-merge(TT_Q_data,list_sen, by="Scenario")
final_data[,MPR:=as.numeric(MPR)]
final_data[MPR==0.01,MPR:=0]
# final_data[,MPR:=as.character(MPR)]
# 
# final_data[MPR==10, MPR:="10% MPR"]
# final_data[MPR==0, MPR:="0% MPR"]
# final_data[MPR==25, MPR:="25% MPR"]
# final_data[MPR==50, MPR:="50% MPR"]
# final_data[MPR==100, MPR:="100% MPR"]
final_data[,avg_spd:=TT_seg_len/(1.47*all_TT)]

# final_data[,avg_spd:=TT_seg_len/(1.47*Avg_TT)]
# final_data[,min_spd:=TT_seg_len/(1.47*Max_TT)]
# final_data[,cavg_spd:=TT_seg_len/(1.47*Avg_TT_CHGV)]
# final_data[,cmin_spd:=TT_seg_len/(1.47*Max_TT_CHGV)]

#install.packages("ggplot2")
library(ggplot2)

final_data$`Ramp Configuration`<-factor(final_data$`Ramp Configuration`)
final_data$`Platooning Strategy`<-factor(final_data$`Platooning Strategy`)
final_data$`Ramp Spacing`<-factor(final_data$`Ramp Spacing`)

#final_data$`Ramp lane change`<-factor(final_data$`Ramp lane change`)
# final_data[,MPR:=factor(MPR,levels=c("0% MPR","10% MPR","25% MPR","50% MPR","100% MPR"))]

final_data[final_data==-1000]<- NA 
# setnames(final_data,c("Max_Queue_Len","Max_Queue_Len2"),c("Avg_Queue_Len","Avg_Queue_Len2"))
setnames(final_data,c("Max_Queue_Len_ramp1","Max_Queue_Len_freln1","Max_Queue_Len_ramp2","Max_Queue_Len_freln2")
         ,c("amQrmp1_","amQfre1_","amQrmp2_","amQfre2_"))
#These are the cases where the ramp is en-ex and their is an auxilary lane present 
# final_data[Avg_Queue_Len2==-1000,Avg_Queue_Len2:=Avg_Queue_Len]
# final_data[,avg_queue2_fin:=mean(Avg_Queue_Len2),by=Scenario]

final_data[,avg_spd_fin:=mean(avg_spd),by=Scenario]
final_data[,amQrmp1:=mean(amQrmp1_),by=Scenario]
final_data[,amQfre1:=mean(amQfre1_),by=Scenario]
final_data[,amQrmp2:=mean(amQrmp2_),by=Scenario]
final_data[,amQfre2:=mean(amQfre2_),by=Scenario]


final_data[`Ramp Configuration`=="en-en",`Ramp Configuration`:="Double Entrance Ramp without Auxiliary Lane"]
final_data[`Ramp Configuration`=="en-en-aux",`Ramp Configuration`:="Double Entrance Ramp with Auxiliary Lane"]
final_data[`Ramp Configuration`=="en-ex",`Ramp Configuration`:="Entrance-Exit Ramp without Auxiliary Lane"]
final_data[`Ramp Configuration`=="en-ex-aux",`Ramp Configuration`:="Entrance-Exit Ramp with Auxiliary Lane"]

setnames(final_data,c("Platooning Strategy"),c("Lane Restriction Policy"))
final_data[`Lane Restriction Policy`=="Right Lane",`Lane Restriction Policy`:= "Right Lane Restriction"]

final_data1<-unique(final_data,by="Scenario")
# final_data1$MPR<-revalue(final_data1$MPR, c("10% MPR"=10, "50% MPR"=50, "100% MPR"=100))
final_data1<-final_data1[,.(Scenario,`Ramp Configuration`,`Lane Restriction Policy`,
            `Ramp Spacing`,MPR,avg_spd_fin,amQrmp1,amQfre1,amQrmp2,amQfre2)]
require(xlsx)
write.xlsx(final_data1,"res_rmp_sp.xls",row.names = FALSE)


#**********************************************************************************************

#**********************************************************************************************

rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")

source("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/R_code_truck_platoon/ramp_spacing_results/ggplot_fun1.R")

final_data1[,inter:=interaction(`Ramp Spacing`,`Lane Restriction Policy`)]
final_data1[,`Ramp Configuration`:=as.factor(`Ramp Configuration`)]
setnames(final_data1,c("Ramp Spacing"),c("Ramp Spacing (ft)"))

Nplt1<-plots_summary(data=final_data1[grepl("Double",`Ramp Configuration`),],
              x1=MPR,
              y1=avg_spd_fin,
              col1=`Ramp Spacing (ft)`,
              shape1=`Lane Restriction Policy`,
              group1=inter,
              facet1=~`Ramp Configuration`,
              ylab1="Speed (Mph)",
              title1="Average Speed",
              cap1="Note: LOS C and Does not Consider Ramp Lane Change Policy")
Nplt1<-Nplt1+ylim(c(0,70))


Nplt2<-plots_summary(data=final_data1[grepl("Double Entrance Ramp without Auxiliary Lane",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQrmp1,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 1st Ramp",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy\nAuxiliary Lane Cases have Queue Counters only at the Second Ramp")
Nplt2<-Nplt2+ylim(c(0,200))

Nplt3<-plots_summary(data=final_data1[grepl("Double Entrance Ramp without Auxiliary Lane",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQfre1,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 1st Ramp Freeway Lanes",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy\nAuxiliary Lane Cases have Queue Counters only at the Second Ramp")
Nplt3<-Nplt3+ylim(c(0,200))

Nplt4<-plots_summary(data=final_data1[grepl("Double",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQrmp2,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 2nd Ramp",
                     cap1="Note: LOS C and Does not Consider Ramp Lane Change Policy")
Nplt4<-Nplt4+ylim(c(0,200))

Nplt5<-plots_summary(data=final_data1[grepl("Double",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQfre2,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 2nd Ramp Freeway Lanes",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt5<-Nplt5+ylim(c(0,200))

#***************************************************************************************************************#
# Cut In 
CutIn<-fread("cutIn.csv")

names(CutIn)<-c("Scenario","SimSec","No","LeadVehLen","Platoon_spacing (m)","SpacingAfterCutin (m)","Speed(m/s)","Acc(m/s2)","Leader_State","Odometer")
CutIn[,cuts:=sum(No),by=Scenario]
CutIn[,cuts:=cuts/5]
CutIn<-unique(CutIn,by="Scenario")
CutIn<-CutIn[,.(Scenario,cuts)]
CutIn[,Scenario:=tstrsplit(Scenario,"Scenario",keep="2")]
CutIn[,Scenario:=as.factor(Scenario)]
CutIn<-merge(final_data1,CutIn,all.x=TRUE)
CutIn[MPR==0.01,MPR:=0]
CutIn[is.na(CutIn$cuts),cuts:=0]



Nplt6<-plots_summary(data=CutIn[grepl("Double",`Ramp Configuration`),],
                     x1=MPR,
                     y1=cuts,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Number of Cut-in Maneuver",
                     title1="Cut-in Maneuver",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt6<-Nplt6+ylim(c(0,130))
# Nplt1<-ggplot(data=final_data1[grepl("en-en",`Ramp Configuration`),],aes(x=MPR,y=avg_spd_fin,colour=`Ramp Spacing`, shape = `Platooning Strategy`,
# group=interaction(`Ramp Spacing`,`Platooning Strategy`)))+geom_line(size=1)+geom_point(colour="black",size=2)+facet_wrap(~`Ramp Configuration`)+
#   scale_color_manual(values=rainbow12equal[c(7,1)])+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed",caption="No Ramp Lane Change Policy")+theme_bw()+scale_shape_manual(values=c(0,3))
# 


pdf("Double_EntranceRamp.pdf")
Nplt1
Nplt2
Nplt3
Nplt4
Nplt5
Nplt6
graphics.off()


ggsave("P1.tiff",plot=Nplt1,device="tiff",width=6,height=8.5,units="in")
ggsave("P2.tiff",plot=Nplt2,device="tiff",width=6,height=5,units="in")
ggsave("P3.tiff",plot=Nplt3,device="tiff",width=6,height=5,units="in")
ggsave("P4.tiff",plot=Nplt4,device="tiff",width=6,height=8.5,units="in")
ggsave("P5.tiff",plot=Nplt5,device="tiff",width=6,height=8.5,units="in")
ggsave("P6.tiff",plot=Nplt6,device="tiff",width=6,height=8.5,units="in")


Nplt7<-plots_summary(data=final_data1[grepl("Exit",`Ramp Configuration`),],
                     x1=MPR,
                     y1=avg_spd_fin,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Speed (Mph)",
                     title1="Average Speed",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt7<-Nplt7+ylim(c(0,70))


Nplt8<-plots_summary(data=final_data1[grepl("Entrance-Exit Ramp without Auxiliary Lane",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQrmp1,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 1st Ramp",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy\nAuxiliary Lane Cases have Queue Counters only at the Second Ramp")
Nplt8<-Nplt8+ylim(c(0,200))

Nplt9<-plots_summary(data=final_data1[grepl("Entrance-Exit Ramp without Auxiliary Lane",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQfre1,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 1st Ramp Freeway Lanes",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy\nAuxiliary Lane Cases have Queue Counters only at the Second Ramp")
Nplt9<-Nplt9+ylim(c(0,200))

Nplt10<-plots_summary(data=final_data1[grepl("Exit",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQrmp2,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 2nd Ramp",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt10<-Nplt10+ylim(c(0,200))

Nplt11<-plots_summary(data=final_data1[grepl("Exit",`Ramp Configuration`),],
                     x1=MPR,
                     y1=amQfre2,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Queue Length (ft)",
                     title1="Average of Maximum Queue - 2nd Ramp Freeway Lanes",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt11<-Nplt11+ylim(c(200))





Nplt12<-plots_summary(data=CutIn[grepl("Exit",`Ramp Configuration`),],
                     x1=MPR,
                     y1=cuts,
                     col1=`Ramp Spacing (ft)`,
                     shape1=`Lane Restriction Policy`,
                     group1=inter,
                     facet1=~`Ramp Configuration`,
                     ylab1="Number of Cut-in Maneuver",
                     title1="Cut-in Maneuver",
                     cap1="Note:  LOS C and Does not Consider Ramp Lane Change Policy")
Nplt12<-Nplt12+ylim(c(0,130))

pdf("Entrance-Exit_Ramp.pdf")
Nplt7
Nplt8
Nplt9
Nplt10
Nplt11
Nplt12
graphics.off()


ggsave("P7.tiff",plot=Nplt7,device="tiff",width=6,height=8.5,units="in")
ggsave("P8.tiff",plot=Nplt8,device="tiff",width=6,height=5,units="in")
ggsave("P9.tiff",plot=Nplt9,device="tiff",width=6,height=5,units="in")
ggsave("P10.tiff",plot=Nplt10,device="tiff",width=6,height=8.5,units="in")
ggsave("P11.tiff",plot=Nplt11,device="tiff",width=6,height=8.5,units="in")
ggsave("P12.tiff",plot=Nplt12,device="tiff",width=6,height=8.5,units="in")








#***************************************************************************************************************#

da_que<-summarySE(final_data,"Avg_Queue_Len","Scenario")
da_que<-merge(da_que,list_sen, by="Scenario")
da_que2<-summarySE(final_data,"Avg_Queue_Len2","Scenario")
da_que2<-merge(da_que2,list_sen, by="Scenario")
#lm(Avg_Queue_Len~Scenario,data=da_que)
da_spd<-summarySE(final_data,"avg_spd","Scenario")
da_spd<-merge(da_spd,list_sen, by="Scenario")
da_min_spd<-summarySE(final_data,"min_spd","Scenario")
da_min_spd<-merge(da_min_spd,list_sen, by="Scenario")
da_que$CIupper=0
da_que$CIlower=0
da_que2$CIupper=0
da_que2$CIlower=0
da_spd$CIupper=0
da_spd$CIlower=0

da_que[,CIupper:=round(Avg_Queue_Len+ci,2)]
da_que[,CIlower:=round(Avg_Queue_Len-ci,2)]
da_que2[,CIupper:=round(Avg_Queue_Len2+ci,2)]
da_que2[,CIlower:=round(Avg_Queue_Len2-ci,2)]
da_spd[,CIupper:=round(avg_spd+ci,2)]
da_spd[,CIlower:=round(avg_spd-ci,2)]


da_min_spd$CIlower=0
da_min_spd$CIupper=0
da_min_spd[,CIupper:=round(min_spd+ci,2)]
da_min_spd[,CIlower:=round(min_spd-ci,2)]


en_en<-grepl("en-en",da_que$`Ramp Configuration`)
en_ex<-grepl("en-ex",da_que$`Ramp Configuration`)

#*****************************************************************************************************************************
#Plots for average Maximum queue length en-en Ramps
g1<-ggplot(data=da_que[en_en,],aes(x=factor(MPR),y=Avg_Queue_Len,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g1<-g1+scale_fill_discrete(name="Ramp Spacing (ft)")
g1<-g1+  geom_errorbar(aes(ymin=Avg_Queue_Len-ci, ymax=Avg_Queue_Len+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g1<-g1+scale_fill_manual(values=c("#CCCCCC","#666666"))
g1<-g1+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 1st Ramp")
g1<-g1+theme(strip.text.y=element_text(size=9))+ylim(-500,1700)
g1<-g1+  geom_text(aes(label = CIlower, y = Avg_Queue_Len-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = Avg_Queue_Len+ci), vjust = -1.5,size=2,position=position_dodge(1)) 
g1<-g1+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))

#*****************************************************************************************************************************



colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Avg_Queue_Len ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g1a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),],aes(x=factor(MPR),y=Avg_Queue_Len,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g1a<-g1a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                    shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g1a<-g1a+geom_text(data = means, aes(label = round(Avg_Queue_Len,digits=1), y = Avg_Queue_Len + 100),position = position_dodge(1))
g1a<-g1a+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 1st Ramp")
g1a<-g1a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g1a<-g1a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))

#*****************************************************************************************************************************
#Plots for average queue length 2 en-en Ramps
g2<-ggplot(data=da_que2[en_en,],aes(x=factor(MPR),y=Avg_Queue_Len2,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g2<-g2+scale_fill_discrete(name="Ramp Spacing (ft)")
g2<-g2+  geom_errorbar(aes(ymin=Avg_Queue_Len2-ci, ymax=Avg_Queue_Len2+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g2<-g2+scale_fill_manual(values=c("#CCCCCC","#666666"))
g2<-g2+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 2nd Ramp")
g2<-g2+theme(strip.text.y=element_text(size=9))+ylim(-150,1700)
g2<-g2+  geom_text(aes(label = CIlower, y = Avg_Queue_Len2-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = Avg_Queue_Len2+ci), vjust = -1.5,size=2,position=position_dodge(1)) 



colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Avg_Queue_Len2 ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g2a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),],aes(x=factor(MPR),y=Avg_Queue_Len2,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g2a<-g2a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g2a<-g2a+geom_text(data = means, aes(label = round(Avg_Queue_Len2,digits=1), y = Avg_Queue_Len2 + 100),position = position_dodge(1))
g2a<-g2a+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 2nd Ramp")
g2a<-g2a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g2a<-g2a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))

#*****************************************************************************************************************************
#Plots for Avg spd en-en Ramps
g3<-ggplot(data=da_spd[en_en,],aes(x=factor(MPR),y=avg_spd,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g3<-g3+scale_fill_discrete(name="Ramp Spacing (ft)")
g3<-g3+  geom_errorbar(aes(ymin=avg_spd-ci, ymax=avg_spd+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g3<-g3+scale_fill_manual(values=c("#CCCCCC","#666666"))
g3<-g3+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")
g3<-g3+theme(strip.text.y=element_text(size=9))+ylim(0,80)
g3<-g3+  geom_text(aes(label = CIlower, y = avg_spd-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = avg_spd+ci), vjust = -1.5,size=2,position=position_dodge(1)) 
g3<-g3+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))



colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g3a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-en-aux","en-en"),],aes(x=factor(MPR),y=avg_spd,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g3a<-g3a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g3a<-g3a+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd -2),position = position_dodge(1))
g3a<-g3a+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")
g3a<-g3a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g3a<-g3a+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))
g3a<-g3a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))

#*****************************************************************************************************************************
#Plots for Min spd en-en Ramps
# g4<-ggplot(data=da_min_spd[en_en,],aes(x=factor(MPR),y=min_spd,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
# g4<-g4+scale_fill_discrete(name="Ramp Spacing (ft)")
# g4<-g4+  geom_errorbar(aes(ymin=min_spd-ci, ymax=min_spd+ci),
#                        width=0.5,                    # Width of the error bars
#                        position=position_dodge(1))
# #g4<-g4+scale_fill_manual(values=c("#CCCCCC","#666666"))
# g4<-g4+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Min Average Speed (Mph) All Vehicles")
# g4<-g4+theme(strip.text.y=element_text(size=9))+ylim(0,80)
# g4<-g4+  geom_text(aes(label = CIlower, y = min_spd-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
#   geom_text(aes(label = CIupper, y = min_spd+ci), vjust = -1.5,size=2,position=position_dodge(1)) 
# 
# 




#*****************************************************************************************************************************
#Plots for average Maximum queue length en-ex Ramps
g5<-ggplot(data=da_que[en_ex,],aes(x=factor(MPR),y=Avg_Queue_Len,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g5<-g5+scale_fill_discrete(name="Ramp Spacing (ft)")
g5<-g5+  geom_errorbar(aes(ymin=Avg_Queue_Len-ci, ymax=Avg_Queue_Len+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g1<-g1+scale_fill_manual(values=c("#CCCCCC","#666666"))
g5<-g5+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 1st Ramp")
g5<-g5+theme(strip.text.y=element_text(size=9))+ylim(-500,1700)
g5<-g5+  geom_text(aes(label = CIlower, y = Avg_Queue_Len-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = Avg_Queue_Len+ci), vjust = -1.5,size=2,position=position_dodge(1)) 





colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Avg_Queue_Len ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g5a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),],aes(x=factor(MPR),y=Avg_Queue_Len,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g5a<-g5a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g5a<-g5a+geom_text(data = means, aes(label = round(Avg_Queue_Len,digits=1), y = Avg_Queue_Len + 100),position = position_dodge(1))
g5a<-g5a+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 1st Ramp")
g5a<-g5a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g5a<-g5a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))

#*****************************************************************************************************************************
#Plots for average queue length 2 en-ex Ramps
g6<-ggplot(data=da_que2[en_ex,],aes(x=factor(MPR),y=Avg_Queue_Len2,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g6<-g6+scale_fill_discrete(name="Ramp Spacing (ft)")
g6<-g6+  geom_errorbar(aes(ymin=Avg_Queue_Len2-ci, ymax=Avg_Queue_Len2+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g2<-g2+scale_fill_manual(values=c("#CCCCCC","#666666"))
g6<-g6+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 2nd Ramp")
g6<-g6+theme(strip.text.y=element_text(size=9))+ylim(-150,1700)
g6<-g6+  geom_text(aes(label = CIlower, y = Avg_Queue_Len2-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = Avg_Queue_Len2+ci), vjust = -1.5,size=2,position=position_dodge(1)) 




colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(Avg_Queue_Len2 ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g6a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),],aes(x=factor(MPR),y=Avg_Queue_Len2,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g6a<-g6a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g6a<-g6a+geom_text(data = means, aes(label = round(Avg_Queue_Len2,digits=1), y = Avg_Queue_Len2 + 100),position = position_dodge(1))
g6a<-g6a+labs(x="Market Penetration Rate of Connected Trucks", y="Average Maximum Queue Length (ft)",title="Average Maximum Queue Length 2nd Ramp")
g6a<-g6a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g6a<-g6a+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))
g6a<-g6a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))

#*****************************************************************************************************************************
#Plots for Avg spd en-ex Ramps
g7<-ggplot(data=da_spd[en_ex,],aes(x=factor(MPR),y=avg_spd,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g7<-g7+scale_fill_discrete(name="Ramp Spacing (ft)")
g7<-g7+  geom_errorbar(aes(ymin=avg_spd-ci, ymax=avg_spd+ci),
                       width=.5,                    # Width of the error bars
                       position=position_dodge(1))
#g3<-g3+scale_fill_manual(values=c("#CCCCCC","#666666"))
g7<-g7+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")
g7<-g7+theme(strip.text.y=element_text(size=9))+ylim(0,80)
g7<-g7+  geom_text(aes(label = CIlower, y = avg_spd-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = avg_spd+ci), vjust = -1.5,size=2,position=position_dodge(1)) 





colnames(final_data)
buf<-final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),c("Scenario","Ramp Spacing","Platooning Strategy","Ramp Configuration","MPR")]
buf<-unique(buf,by="Scenario")
means <- aggregate(avg_spd ~  Scenario, final_data, mean)
means<-merge(means,buf,by="Scenario",all.y=TRUE)
means<-data.table(means)

g7a<-ggplot(data=final_data[`Ramp Configuration` %in% c("en-ex-aux","en-ex"),],aes(x=factor(MPR),y=avg_spd,fill=factor(`Ramp Spacing`)))+geom_boxplot(colour="black",stat="boxplot",position=position_dodge(1))+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g7a<-g7a+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                      shape=18, size=3,show_guide = FALSE,position=position_dodge(1))
g7a<-g7a+geom_text(data = means, aes(label = round(avg_spd,digits=1), y = avg_spd -2),position = position_dodge(1))
g7a<-g7a+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Average Speed (Mph) All Vehicles")
g7a<-g7a+theme_bw() + scale_fill_discrete("Ramp Spacing (ft)")
g7a<-g7a+theme(strip.text=element_text(size=16,face="bold"),axis.text.y=element_text(size=16),axis.text.x = element_text(angle=45,hjust=1,size=16),legend.text=element_text(size=16),title = element_text(size=16,face="bold"))
g7a<-g7a+theme(strip.text=element_text(size=12,face="bold"),axis.text.y=element_text(size=12),axis.text.x = element_text(angle=45,hjust=1,size=12),legend.text=element_text(size=12),title = element_text(size=12,face="bold"))


#*****************************************************************************************************************************
#Plots for Min spd en-ex Ramps
g8<-ggplot(data=da_min_spd[en_ex,],aes(x=factor(MPR),y=min_spd,fill=factor(`Ramp Spacing`)))+geom_bar(colour="white",stat="Identity",width=1,position=position_dodge())+facet_grid(`Platooning Strategy`~`Ramp Configuration`)
g8<-g8+scale_fill_discrete(name="Ramp Spacing (ft)")
g8<-g8+  geom_errorbar(aes(ymin=min_spd-ci, ymax=min_spd+ci),
                       width=0.5,                    # Width of the error bars
                       position=position_dodge(1))
#g4<-g4+scale_fill_manual(values=c("#CCCCCC","#666666"))
g8<-g8+labs(x="Market Penetration Rate of Connected Trucks",y="Speed (Mph)",title="Min Average Speed (Mph) All Vehicles")
g8<-g8+theme(strip.text.y=element_text(size=9))+ylim(0,80)
g8<-g8+  geom_text(aes(label = CIlower, y = min_spd-ci), vjust = 1.5,size=2,position=position_dodge(1)) +
  geom_text(aes(label = CIupper, y = min_spd+ci), vjust = -1.5,size=2,position=position_dodge(1)) 


library(gridExtra)
pdf("rmp_sp_plots.pdf",onefile = TRUE)
print(g1)
print(g1a)

print(g2)
print(g2a)

print(g3)
print(g3a)

# print(g4)
print(g5)
print(g5a)

print(g6)
print(g6a)

print(g7)
print(g7a)

# print(g8)
graphics.off()
#install.packages("agricolae")
library(agricolae)
model<-aov(avg_spd~Scenario,data=final_data)

# summary(model)
# test<-HSD.test(model,"Scenario", group=TRUE,console=TRUE)
# test




