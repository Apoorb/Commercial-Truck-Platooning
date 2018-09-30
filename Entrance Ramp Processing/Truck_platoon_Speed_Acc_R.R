#File Name:  TRuck Platooning Phase 2 Speed_Acc_summary
#Created by: Apoorba Bibeka
#Creation date: Feb 7 2016
#Date Modified : April 03 2017
#Purpose:To get the summary statistics of speed and acceleration 
#Last executed:
Sys.time()

#Note 1: vehicle_record$VehDecRoute==2 condition works because in VISSIM network VehDecRoute 2 
# means that the vehicle is on the freeway. 
#Note 2:  



#1 Housekeeping
ls()
rm(list=ls())
ls()


#Set the current directory to the directory where all the data files are kept

setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/Commercial_platooning/VISSIM_Merging")

#Load data.table package for fread to work
library(data.table)

#fread by default create data table. Change the data table to data frame
options(datatable.fread.datatable=FALSE)

#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("scenario*.fzp"))


#Skip the first row   fread_  skip=1
#File has header 		fread header=true



speed_acc_summary <-data.frame(Scenario=character(length(files)),Minimum_speed=numeric(length(files)), Quartile_1_speed=numeric(length(files)),Median_speed=numeric(length(files)),Quartile_3_speed=numeric(length(files)),Maximum_speed=numeric(length(files)),Mean_speed=numeric(length(files)),Stdev_speed=numeric(length(files)), Minimum_acc=numeric(length(files)), Quartile_1_acc=numeric(length(files)),Median_acc=numeric(length(files)),Quartile_3_acc=numeric(length(files)),Maximum_acc=numeric(length(files)),SS_Mean_acc=numeric(length(files)),Stdev_acc=numeric(length(files)),stringsAsFactors=FALSE)



#install.packages("fBasics")
library(fBasics)
library(ggplot2)

#Iterate over all the files 

i=1

file<-list.files(pattern=glob2rx("scenario1a*.fzp"))

#loop over files to extract all the raw data files and save them in a data frame (pla_len_1, ....)
for(file in files){
  temp=strsplit(file,"scenario")[[1]][2]
  no=strsplit(temp,"_")[[1]][1]
  assign(paste("scenario",no,sep=""),fread(file,header=TRUE, skip=1))
  
  fi_name=paste("scenario",no,sep="")
  #convert file name to data frame
  vehicle_record <-(get(fi_name))
  
  rm(list=paste("scenario",no,sep=""))

#Rename Column names
colnames(vehicle_record)<-c("vehID","simtime_sec","SPEED","ACCELERATION","Veh_type","VehDecRoute","odom","fd","link_lane")
vehicle_record<-na.omit(vehicle_record)
da<-vehicle_record[vehicle_record$VehDecRoute==2,]          #routing decision 2 is freeway traffic, 201 are connected hgv
# vehicle_record<-vehicle_record[vehicle_record$VehDecRoute==2,]
# 
# vehicle_record<-vehicle_record[vehicle_record$Veh_type==201,]
# #============================================================================
# 
# #get the per time as leader 
# summary_pla<-vehicle_record
# rm(vehicle_record)
#Create a subset for only HGV 
# LDV_record <-summary_pla[,c("SPEED","ACCELERATION")]
# rm(summary_pla)
#     speed_acc_summary[i,1]<-paste("Scenario",no,sep="_")
#         
#     speed_acc_summary[i,c(2,3,4,5,6,7,8)]<-basicStats(LDV_record$SPEED)[c("Minimum","1. Quartile","Median","3. Quartile","Maximum","Mean","Stdev"),]
#         
#     speed_acc_summary[i,c(9,10,11,12,13,15)]<-basicStats(LDV_record$ACCELERATION)[c("Minimum","1. Quartile","Median","3. Quartile","Maximum","Stdev"),]
#     LDV_record$ACCELERATION<-(LDV_record$ACCELERATION)^2
# 	 speed_acc_summary[i,14]<-basicStats( LDV_record$ACCELERATION)["Mean",]
# 	 speed_acc_summary[i,14]<-  mapply(sqrt,speed_acc_summary[i,14])
# 	 
# 	 i=i+1


	 
	 
	 # Plot the space-time diagram 
	 
	 # da$lane<-sapply(strsplit(da$link_lane,"-"),"[",2)
	 # da$lane<-as.numeric(da$lane)
	
	 da1<-da[(da$simtime_sec>=1000 & da$simtime_sec<=1800),]
	 da1$odom<-da1$odom*0.000189394 # feet to miles
	 da1$Veh_type<-factor(da1$Veh_type)
	 vid<-unique(da1$vehID)
	 
	 

	 ##################################################################################
	 # Plot the trajectories of different vehicles to analyze the fluctuation in speed 
	 buf_pt<-ggplot(da1,aes(x=odom,y=SPEED,colour=factor(vehID)))+geom_line()
	 buf_pt<-buf_pt+scale_colour_discrete(guide=FALSE)
	 buf_pt<-buf_pt+xlab("Distance (Mi)")+ylab("Speed (Mph)")+ggtitle(paste("Scenario",no,sep=" "))
	 assign(paste("a",no,sep=""),buf_pt)
	 

	 # rm(LDV_record)
	 
}







multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(a1a,a2a,a3a,a4a,a5a,a6a,cols=2)

multiplot(a7a,a8a,a9a,cols=2)



s<-strsplit(speed_acc_summary$Scenario, "[^[:digit:]]")
sc_no<-as.numeric(unlist(s))
sc_no<-unique(sc_no[!is.na(sc_no)])
#speed_acc_summary<-cbind(speed_acc_summary,sc_no)
speed_acc_summary$Scenario<-sc_no
	#input the list of sen
	list_sen<-fread("List_scenarios.csv",header=T)
	final_data<-merge(speed_acc_summary,list_sen, by.x="Scenario")

#file="/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/Speed_acc_summary.csv"
file="C:/Users/a-bibeka/Dropbox/TTI_Projects/Commer  cial_platooning/R_code_truck_platoon/stability.csv"


write.table(final_data, file,sep=",",append = F,row.names=FALSE)





