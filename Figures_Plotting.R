#Source Data_Cleaning.R first
source(file = "Data_Cleaning.R")

###### Here's where you should stop the script and move over to plotting 

#A Plot-
Aplot<-ggplot(data=fullData, mapping=aes(x=Month, y=SCP_Value,fill=Treatment))+geom_boxplot(outlier.shape=NA, position="identity",alpha=0.5)+facet_wrap(~Block, ncol=3)+scale_fill_manual(values=c("green", "purple"))+theme_bw()+geom_point(col="grey", alpha=0.5)+theme(axis.text.x=element_text(angle=45, vjust=0.9, hjust=1))+geom_line(data=fullData, mapping = aes(x=Month, y=MeanMonthlySCP, col=Treatment, group=Treatment))+scale_color_manual(values=c("green","purple"))+ylab("Mean Supercooling Point (\u00B0C)")+theme(text=element_text(family="Times New Roman"))

Aplot 

#B plot-Range and Variance plots across time 
rangeplot<-ggplot(data=subset, mapping=aes(x=Month, y=MonthlyRange, col=Treatment, group=Treatment))+theme_bw()+facet_wrap(~Block, ncol=3)+theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ylab("Temperature Range (\u00B0C)")+theme(text=element_text(family="Times New Roman"))+geom_line(size=1.5, aes(alpha=Treatment))+scale_color_manual(values=c("green","purple"))+scale_alpha_manual(values=c(0.7, 0.5))+geom_point(shape=21, fill="black",size=2.5)+theme(axis.text.x=element_text(face="bold", size=10))

MonthVar<-ggplot(data=subset, mapping=aes(x=Month, y=Var, fill=Treatment,col=Treatment, group=Treatment))+facet_wrap(~Block, ncol=3)+theme_bw()+geom_line(size=1.5, aes(alpha=Treatment))+geom_point(shape=21,fill="black", size=2.5)+scale_color_manual(values=c("green","purple"))+theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ylab("Temperature Variance")+scale_fill_manual(values=c("green","purple"))+theme(text=element_text(family="Times New Roman"))+scale_alpha_manual(values=c(0.7, 0.5))+theme(axis.text.x=element_text(face="bold", size=10))


rangeplot/MonthVar



#C plot 

MinCplot<-ggplot(data=join, mapping=aes(x=TwoWeekMinTemp, y=SCP_Value, col=Treatment, group=Treatment))+geom_point(col="grey")+stat_smooth(method="lm", formula=y~x, geom="smooth",size=1.5, aes(linetype=Treatment))+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Minimum Temperature (\u00B0C)")+theme(plot.subtitle =element_text(face="bold"))+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("solid","longdash"))+theme(axis.text.x=element_text(face="bold", size=10))

MaxCplot<-ggplot(data=join, mapping=aes(x=TwoWeekMaxTemp, y=SCP_Value, col=Treatment, group=Treatment))+geom_point(col="grey")+stat_smooth(method="lm", formula=y~x, geom="smooth", size=1.5, aes(linetype=Treatment))+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Maximum Temperature (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10), legend.position="none")+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))
MaxCplot/MinCplot

#D Plot-Supercooling Point vs Monthly Range 

#Plotting the control groups
ggplot(data=filt, mapping=aes(x=MonthlyRange, y=SCP_Value, color=Block))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("green", "orange", "purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))


#plotting the sheltered roups
ggplot(data=filt2, mapping=aes(x=MonthlyRange, y=SCP_Value))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("green", "orange", "purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=10))

#Plotting everything together with facet_wrap
ggplot(data=join, mapping=aes(x=MonthlyRange, y=SCP_Value, color=Block))+geom_point()+stat_smooth(method="lm", geom="smooth", se=FALSE)+scale_color_manual(values=c("blue", "#ff7f0e", "darkgreen"))+theme_bw()+facet_wrap(~Treatment)+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Monthly Range (\u00B0C)")+theme(text=element_text("Times New Roman", "bold", size=10))+scale_linetype_manual(values=c("longdash", "longdash"))+theme(axis.text.x=element_text(face="bold", size=12))+theme(strip.text.x=element_text(size=12))



#Supplementary Plots-Plot Env Temperature vs. Time. So First find the ibutton logger temperatures that were experienced, maybe across every X time period and do the mean/raw temperature experienced across time. Readouts imply that monthly mean and monthly min might not be very useful as they're visually very similar, but there may be a sig difference in the monthly maximum temperature occurred between treatments.Even if we don't see a visually striking difference, there is however evidence from previous literature that the minimum temperatures experienced do seem to have an effect on it. 
WeeklyMaxplot<-ggplot(data=subset, mapping=aes(x=Date, y=WeeklyMax, col=Treatment))+geom_line()+facet_wrap(~Block)+theme_bw()+scale_color_manual(values=c("darkgreen", "purple"))+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+theme(text=element_text("Times New Roman")) +ylab("Weekly Maximum Temperatures (\u00B0C)")

maxlineplot<-ggplot(data=weeklytemps, aes(x=Date, y=maxdiff))+geom_link2(lwd=0.5,aes(color=after_stat(y<0)))+scale_color_manual(values=c("purple", "darkgreen"))+facet_wrap(~Block)+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")+xlab("Month")
maxlineplot

WeeklyMaxplot

WeeklyMaxplot/maxlineplot


#Minimum temperature plots 

minlineplot<-ggplot(data=weeklytemps, aes(x=Date, y=mindiff))+geom_link2(lwd=0.5,aes(color=after_stat(y<0)))+scale_color_manual(values=c("purple", "darkgreen"))+facet_wrap(~Block)+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+ylab("Difference in Temperatures (\u00B0C)")+theme(text=element_text("Times New Roman"))+theme(legend.position="None")+xlab("Month")


WeeklyMinplot<-WeeklyMaxplot<-ggplot(data=subset, mapping=aes(x=Date, y=WeeklyMin, col=Treatment))+geom_line()+facet_wrap(~Block)+scale_color_manual(values=c("darkgreen", "purple"))+theme_bw()+theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))+theme(text=element_text("Times New Roman"))+ylab("Weekly Minimum Temperatures (\u00B0C)")

WeeklyMinplot/minlineplot

#Supplementary Plot-Plot SCP to 2 Week Mean Temperature Period
#Main argument- treatments are affecting the temperature, and the temperature is affecting the SCP. Graphs 1-3 are arguing this.
Cplot<-ggplot(data=join, mapping=aes(x=TwoWeekMeanTemp, y=SCP_Value, col=Treatment))+geom_point(col="grey")+facet_grid(~Month.x, scales="free")+stat_smooth(method="lm", formula=y~x, geom="smooth")+scale_color_manual(values=c("green","purple"))+theme_bw()+ylab("Mean Supercooling Point (\u00B0C)")+xlab("Two Week Mean Temperature (\u00B0C)")+theme(axis.text.x=element_text(angle=45, vjust=0.5))+theme(plot.subtitle  =element_text(face="bold"))+theme(text=element_text("Times New Roman"))


