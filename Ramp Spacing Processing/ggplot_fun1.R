


plots_summary<-function(data1,x1,y1,col1,shape1,group1,facet1,xlab1="% of Trucks Operating in Platoons",ylab1,title1,cap1){
  rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
  ggplt<-ggplot(data=data1,aes_(x=substitute(x1),y=substitute(y1),colour=substitute(col1), shape = substitute(shape1),group=substitute(group1)
    ))+geom_line(size=1,aes_(linetype=substitute(col1)))+geom_point(colour="black",size=2)+facet_wrap(facet1,nrow=2,scales="free")+
    scale_color_manual(values=rainbow12equal[c(8,2)])+labs(x=xlab1,y=ylab1,title=title1,caption=cap1)+theme_bw(base_size=12)+scale_shape_manual(values=c(0,3),name="Lane Restriction\nPolicy")+theme(strip.text = element_text(size=12))+theme(plot.caption =element_text(hjust=0))
  ggplt
}


plots_summary_ent_R<-function(data1,x1,y1,col1,shape1,group1,facet1,xlab1="% of Trucks Operating in Platoons",ylab1,title1,cap1){
  rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
  ggplt<-ggplot(data=data1,aes_(x=substitute(x1),y=substitute(y1),colour=substitute(col1), shape = substitute(shape1),group=substitute(group1)))+geom_line(size=1,aes_(linetype=substitute(col1)))+geom_point(colour="black",size=4)+facet_wrap(facet1,scales="free",nrow=2)+
    scale_color_manual(values=rainbow12equal[c(7,1)])+labs(x=xlab1,y=ylab1,title=title1,caption=cap1)+scale_shape_manual(values=c(0,3))+theme_bw(base_size = 12)+theme(plot.caption =element_text(hjust=0))
  
  ggplt
}


plots_summary_ent_R1<-function(data1,x1,y1,col1,shape1,group1,facet1,xlab1="% of Trucks Operating in Platoons",ylab1,title1,cap1){
  rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
  ggplt<- ggplot(data=data1,aes_(x=substitute(x1),y=substitute(y1),colour=substitute(col1), shape = substitute(shape1),group=substitute(group1)))+theme_bw(base_size=12)+geom_line(size=1,aes_(linetype=substitute(col1)))+geom_point(colour="black",size=4)+facet_wrap(facet1,scales="free")+
    scale_color_manual(values=rainbow12equal[c(7,1)])+labs(x=xlab1,y=ylab1,title=title1,caption=cap1)+scale_shape_manual(values=c(2,4),name="Ramp Lane\nChange Policy",breaks=c("No action","Entire platoon change lane on approaching ramp"),labels=c("No action  \n","Entire platoon\nchange\nlane on\napproaching\nramp"))+theme(plot.caption =element_text(hjust=0))
  
  ggplt
  }

