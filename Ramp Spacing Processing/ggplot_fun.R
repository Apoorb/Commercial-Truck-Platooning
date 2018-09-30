


plots_summary<-function(data1,x1,y1,col1,shape1,facet1,facet2,xlab1,ylab1,title1,cap1){
  rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
  ggplt<-ggplot(data=data1,aes_(x=substitute(x1),y=substitute(y1),colour=substitute(col1), shape = substitute(shape1)
    ))+geom_line(size=1)+geom_point(colour="black",size=2)+facet_wrap(~as.name(facet1))+
    scale_color_manual(values=rainbow12equal[c(7,1)])+labs(x=xlab1,y=ylab1,title=title1,caption=cap1)+theme_bw()+scale_shape_manual(values=c(0,3))
  ggplt
}


group=interaction(substitute(col1),substitute(shape1))