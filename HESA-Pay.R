## Data from

## https://docs.google.com/spreadsheets/d/1z4ffKuH6AW0iWIhTMA1RrAjaBziS2LEKzUeUD7FMVx0/edit#gid=2002317430

## read in data

pay<-read.csv("academicPay.csv",stringsAsFactors=FALSE)
str(pay)

pay<-subset(pay,mfAcademicPayRatio>0.75)

Falmouth.mf.ratio<-subset(pay,University=="Falmouth University")$mfAcademicPayRatio

## Plot distribution of pay ratios

library(ggplot2)
library(grid)

png("mfPayRatio.png",width = 600, height = 480)
g<-ggplot(pay,aes(x=mfAcademicPayRatio,fill=..x..),alpha=0.2)+
        #geom_histogram(binwidth=0.02)+
        geom_histogram(binwidth=0.02)+
        coord_flip()+
        #scale_x_reverse()+
        scale_fill_gradient("Ratio", low = "green", high = "red")+
        #geom_vline(aes(xintercept=Falmouth.mf.ratio),   # Ignore NA values for mean
                   #color="red", linetype="dashed", size=1)+
        labs(x = "Ratio of male academic pay to female academic pay",y = "Number of Universities")+
        geom_segment(aes(x =1.0 , y = 7, xend = 1.0, yend = 25),color="black",linewidth=20,linetype="dashed")+
        annotate("text", x = Falmouth.mf.ratio, y = 4., label = "Falmouth",color="blue",size=8)+
        geom_segment(aes(x = Falmouth.mf.ratio, y = 1.5, xend = Falmouth.mf.ratio, yend = 0),color="blue",linewidth=20, arrow = arrow(type="closed",length = unit(0.4, "cm"),))+
        theme(axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=16))+
        theme(axis.title.x = element_text(size=16,vjust=-.5),
              axis.title.y=element_text(size=16,vjust=1.2))+
        theme(legend.text=element_text(size=14),
              legend.title=element_text(size=14))
g
dev.off() 

falmouth<-subset(pay,pay$University=="Falmouth University")

library(dplyr)
arrange(pay,mfAcademicPayRatio)

pay$deviance<-abs(1-pay$mfAcademicPayRatio)

arrange(pay,deviance)

ggplot(pay,aes(x=deviance,fill=..x..),alpha=0.2)+
        #geom_histogram(binwidth=0.02)+
        geom_histogram(binwidth=0.02)+
        scale_fill_gradient("Deviation", low = "green", high = "red")+
        geom_vline(aes(xintercept=0.00),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1)+
        labs(x = "Deviation from gender parity of pay of all academics",y = "Count")