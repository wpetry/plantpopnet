###########################################################
## Map tagged Plantago in plots
###########################################################
## Preliminaries

library(gdata)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cowplot)

###########################################################
## Read in data, then clean for plotting
###########################################################
census.dat<-read.xls("~/Documents/Plantago demography/Census data/2015 data & photos/Individual plant census 2015.xls",sheet="MERGED",na.strings=c("-99999",""))

## prepare data for plotting (each row is a unique tag with coordinates and total leaf number)
census.dat %<>%
  select(site_code,transect,plot,plant_id,x_coord,y_coord,no_leaves) %>%
  group_by(site_code,transect,plot,plant_id,x_coord,y_coord) %>%
  summarize(no_leaves=sum(no_leaves))

###########################################################
## Build map for each plot
###########################################################

map_plla<-function(s,t,p){
  if(is.numeric(t)){
    t<-paste0("T",t)
  }
  if(is.numeric(p)){
    p<-paste0("P",p)
  }
  if(sum((census.dat %>% filter(site_code==s,transect==t,plot==p))["plant_id"],na.rm=T)){
    ggplot(data=census.dat %>% filter(site_code==s,transect==t,plot==p)
           ,aes(x=x_coord,y=y_coord))+
      geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=50),color="transparent",fill="grey95")+
      geom_segment(aes(x=0,xend=0,y=0,yend=50),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=10,xend=10,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=20,xend=20,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=30,xend=30,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=40,xend=40,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=50,xend=50,y=0,yend=50),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=0,yend=0),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=10,yend=10),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=20,yend=20),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=30,yend=30),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=40,yend=40),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=50,yend=50),color="grey50",linetype="solid",size=0.25)+
      geom_point(aes(size=no_leaves),shape=19,color="grey50")+
      geom_text(aes(y=y_coord-1,label=plant_id),fontface="bold")+
      ggtitle(paste(s,t,p))+
      scale_size_continuous(limits=range((census.dat %>% filter(site_code==s))$no_leaves))+
      labs(x="x coordinate (cm)",y="y coordinate (cm)",size="Total leaf number")+
      coord_cartesian(xlim=c(0,50),ylim=c(0,50))+
      theme_classic()
  }else{
    ggplot(data=census.dat,aes(x=x_coord,y=y_coord))+
      geom_rect(aes(xmin=0,xmax=50,ymin=0,ymax=50),color="transparent",fill="grey95")+
      geom_segment(aes(x=0,xend=0,y=0,yend=50),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=10,xend=10,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=20,xend=20,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=30,xend=30,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=40,xend=40,y=0,yend=50),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=50,xend=50,y=0,yend=50),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=0,yend=0),color="grey50",linetype="solid",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=10,yend=10),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=20,yend=20),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=30,yend=30),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=40,yend=40),color="grey50",linetype="dashed",size=0.25)+
      geom_segment(aes(x=0,xend=50,y=50,yend=50),color="grey50",linetype="solid",size=0.25)+
      annotate("text",x=25,y=25,label="NO PLANTS",fontface="bold",size=10)+
      ggtitle(paste(s,t,p))+
      labs(x="x coordinate (cm)",y="y coordinate (cm)",size="Total leaf number")+
      coord_cartesian(xlim=c(0,50),ylim=c(0,50))+
      theme_classic()
  }
}

