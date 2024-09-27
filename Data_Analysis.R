source("Data_Cleaning.R")



library(lme4)
library(nlme)


anova<-aov(data=fullData, formula = MeanSCP~Treatment*Month+Block)#Final Nested ANOVA design, same as: MeanSCP~Block+Treatment+Month+Treatment:Month. So does block interact with month, or only treatment?
summary(anova)



#Single-predictor Linear Regression


NovControl<-fullData%>%
  filter(Month=="November", Treatment=="Control")
Ncontrol<-glm(data=NovControl, formula=SCP_Value~MonthlyMean)
summary(Ncontrol)

NCjoin<-join%>%
  filter(Month.x=="January", Treatment=="Control")
NCjoin<-glm(data=NCjoin, formula=SCP_Value~TwoWeekMeanTemp)
summary(NCjoin)


NovTreat<-fullData%>%
  filter(Month=="November",Treatment=="Sheltered")
NTreat<-glm(data=NovTreat, formula=SCP_Value~MonthlyMean)
summary(NTreat)

DecControl<-fullData%>%
  filter(Month=="December", Treatment=="Control")
DControl<-glm(data=DecControl, formula=SCP_Value~MonthlyMean)
summary(DControl)

DecTreat<-fullData%>%
  filter(Month=="December", Treatment=="Sheltered")
DTreat<-glm(data=DecTreat, formula=SCP_Value~MonthlyMean)
summary(DTreat)


JanControl<-fullData%>%
  filter(Month=="January", Treatment=="Control")
JControl<-glm(data=JanControl, formula=SCP_Value~MonthlyMean)
summary(JControl)

JanTreat<-fullData%>%
  filter(Month=="January", Treatment=="Sheltered")
JTreat<-glm(data=JanTreat, formula=SCP_Value~MonthlyMean)
summary(JTreat)


FebControl<-fullData%>%
  filter(Month=="February", Treatment=="Control")
FControl<-lm(data=FebControl, formula=SCP_Value~MonthlyMean)
out<-summary(FControl) #Significant
coef(FControl)
out$coefficients

FebTreat<-fullData%>%
  filter(Month=="February", Treatment=="Sheltered")
FTreat<-glm(data=FebTreat, formula=SCP_Value~MonthlyMean)
out<-summary(FTreat) #Significant
coef(FTreat)
out$coefficients

MarControl<-fullData%>%
  filter(Month=="March", Treatment=="Control")
MControl<-glm(data=MarControl, formula=SCP_Value~MonthlyMean)
summary(MControl)

MarTreat<-fullData%>%
  filter(Month=="March", Treatment=="Sheltered")
MTreat<-glm(data=MarTreat, formula=SCP_Value~MonthlyMean)
summary(MTreat)


AprControl<-fullData%>%
  filter(Month=="April", Treatment=="Control")
AControl<-glm(data=AprControl, formula=SCP_Value~MonthlyMean)
summary(AControl) #Significant

AprTreat<-fullData%>%
  filter(Month=="April", Treatment=="Sheltered")
ATreat<-glm(data=AprTreat, formula=SCP_Value~MonthlyMean)
summary(ATreat)



#Test for normality using QQ Plot
qqnorm(stats$SCP_Value, pch=1, frame=FALSE)
qqline(stats$SCP_Value, col="steelblue", lwd=2)

#Test for normality using Shapiro-Wilks Test
shapiro.test(stats$SCP_Value)


#Linear Regression
linmod<-lm(data=fullData, formula=MeanSCP~Treatment+Month+Block)
summary(linmod)


