# Data Analysis for Tetrastichus planipennisi field experiments-Supercooling Point and Emergence Data

#Loading in Packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(moments)
library(gridExtra)
library(forecast)
library(pracma)
library(DescTools)
library(viridis)
library(TukeyC)
library(ggpubr)
library(patchwork)
library(ggforce)


#Read in data
stats<-read.csv("SCP_All_Sites_2020_2021.csv")
ibutton<-read.csv("iButton_All_2020_2021.csv")
emerge<-read.csv("Emergence_2020_2021.csv")


#Data Transformation and Dataframes

#Editing Stats to add treatment labels
#all(stats$Month %in% month.name) # Validate month names
stats$Month<- factor(stats$Month, levels=c("November", "December", "January", "February", "March", "April"))
edit_stats<-stats%>%
  mutate(Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Sheltered", no="Control"), Block=substr(Jar, start=1, stop=1))%>%
  mutate(Treatment=as.factor(Treatment))

#Summarized version of stats for MeanSCP
meanSCP<-stats%>%
  group_by(Jar,Month)%>%
  summarise(MeanSCP=mean(SCP_Value))%>%
  mutate(Block=substr(Jar, 1,1),Treat=substr(Jar,3, nchar(Jar)), Treatment=ifelse(Treat=="Dense",yes = "Treatment", no="Control"),month=match(Month, month.name))%>% #Month=as.POSIXct(month,format="%m")
  ungroup()%>%
  select(Treatment, Block, Month, MeanSCP)

datalogger<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  filter(Block==1)

ecloseedit<-emerge%>%
  group_by(Group,Treatment)%>%
  summarize(Tets=sum(Tetrastichus), Failed=sum(Tetrastichus.Failed.to.Eclose))

maxtemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MaxC=max(Temp),DateTime=DateTime)

#Subset data for highs/lows
mintemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])%>%
  group_by(Group, Month, MonthNum)%>%
  summarise(MinC=min(Temp))


#Mean temp for 24 hrs, 1 week, 2 weeks, 1 month
meantemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Day=yday(DateTime))%>%
  group_by(Group, Day)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime, Week=week(DateTime), Month=month(DateTime))

meanmonthlytemps<-ibutton%>% #CHECK for tempreadouts
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Month=month(DateTime))%>%
  group_by(Group, Month)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp),DateTime=DateTime, Week=week(DateTime), Month=month(DateTime))


#Do plots for weeks and months-*CHECK for tempreadouts
meanweeklytemps<-ibutton%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(ibutton$Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  select(-X)%>%
  group_by(Group)%>%
  mutate(Week=week(DateTime))%>%
  group_by(Group,Week)%>%
  summarise(MinC=min(Temp),Mean=mean(Temp), MaxC=max(Temp))


#Combining the above dataframes into one large one. So we may be able to archive the other ones
tempreadouts<-ibutton%>% #Take the ibutton datalogger data
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>% #Filter the data by these group names
  group_by(Group)%>% #Group by Group
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>% #Create new variables 
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Sheltered")))%>% #Create new column of Treatment that will be Control or Treatment
  dplyr::select(-X)%>% #Delete the X column
  mutate(Week=1+ as.numeric(Date - as.Date("2021-11-18")) %/% 7)%>% #Create a week column that is weekly intervals from the start date of the experiment.
  group_by(Group)%>% #Group by Group (Again?)
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])%>% #Create a MonthNum, Month Column
  ungroup()%>%
  group_by(Group, Date)%>%
  mutate(DailyMean=mean(Temp), DailyMin=min(Temp), DailyMax=max(Temp))%>% #Calculate Daily Mean, Min, and Max of temperature
  ungroup()%>%
  group_by(Group,Week)%>%
  mutate(WeeklyMean=mean(Temp), WeeklyMin=min(Temp), WeeklyMax=max(Temp), WeekVar=var(Temp), WeeklyRange=(max(Temp)-(min(Temp))))%>% #Calculate Weekly Mean, Min, and Max of temperature
  ungroup()%>%
  group_by(Group, Month)%>%
  mutate(MonthlyMean=mean(Temp), MonthlyMin=min(Temp), MonthlyMax=max(Temp), Var=var(Temp), MonthlyRange=(max(Temp)-(min(Temp)))) #Calculate Monthly Mean, Min, and Max of temperature

#RENAME: ex is going to be data transformation for the two week mean prior to collection dates.

ex<-tempreadouts%>%
  mutate(TwoWeekDate=case_when(month(Date)=='11'~'2021-11-16', month(Date)=='12'~'2021-12-17', month(Date)=='1'~'2022-01-15', month(Date)=='2'~'2022-02-14', month(Date)=='3'~'2022-03-16', month(Date)=='4'~'2022-04-16'))%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Block, Month, Treatment)%>%
  mutate(TwoWeekTemp=case_when(Date>TwoWeekDate~Temp))%>%
  mutate(TwoWeekMeanTemp=mean(TwoWeekTemp, na.rm=TRUE))%>%
  drop_na()%>%
  ungroup()%>%
  group_by(Block, Date, Treatment)%>%
  mutate(TwoWeekMinTemp=min(TwoWeekTemp, na.rm=TRUE), TwoWeekMaxTemp=max(TwoWeekTemp, na.rm=TRUE))%>%
  group_by(Block, TwoWeekDate, Treatment)%>%
  mutate(TwoWeekMinTemp=mean(TwoWeekMinTemp), TwoWeekMaxTemp=mean(TwoWeekMaxTemp))%>%
  summarize(Block=Block, Treatment=Treatment, Month=Month, TwoWeekDate=TwoWeekDate, TwoWeekMinTemp=TwoWeekMinTemp, TwoWeekMeanTemp=TwoWeekMeanTemp, TwoWeekMaxTemp=TwoWeekMaxTemp, Var=Var, MonthlyRange=MonthlyRange)%>%
  distinct()%>%
  mutate(TwoWeekDate=ymd(TwoWeekDate))


durationtemps<-ibutton%>%
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>% #Filter the data by these group names
  group_by(Group)%>% #Group by Group
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.numeric(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>% #Create new variables 
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Sheltered")))%>% #Create new column of Treatment that will be Control or Treatment
  dplyr::select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum], Week=week(Date))%>%
  mutate(Period=cut(Date, "7 days"))%>%
  ungroup()%>%
  group_by(Group, Period)%>%
  mutate(WeeklyMean=mean(Temp), WeeklyMin=min(Temp), WeeklyMax=max(Temp))%>%
  filter(MonthNum=="11"|MonthNum=="12"|MonthNum=="1"|MonthNum=="2"|MonthNum==3|MonthNum==4)%>%
  mutate(Group=factor(Group, levels=c("5_1_5_D", "5_1_1_U", "11_1_5_D", "11_1_1_U", "11_2_4_D", "11_2_1_U")))%>%
  arrange(Block)

filttemp<-tempreadouts%>%
  filter(Date=="2022-1-1")


temps<-tempreadouts%>%
  select(2, 4, 5, 6,7, 8,15)%>%
  group_by(Date)%>%
  distinct()%>%
  group_by(Date,Block)%>%
  mutate(tempdiff=max(WeeklyMax))%>%
  group_by(Date,Block, Treatment)%>%
  mutate(tempdiff=WeeklyMax-tempdiff)


#Pull out the weekly maximum temperature differences between control and treatments
maxtempdiff<-tempreadouts%>%
  select(2, 4, 5, 6,7, 8,9,12)%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Date)%>%
  distinct()%>% 
  group_by(Date,Block)%>% #For every control/treatment of a block on a date... 
  mutate(tempdiff=max(DailyMax))%>% #Get the highest temperature between the two 
  group_by(Date,Block, Treatment)%>% #Then for every control/treatment
  mutate(tempdiff=DailyMax-tempdiff)%>% #Subtract that temperature from its temperature-the ones with maximum temperatures should be 0, the other should be lower than 0 as it's smaller. 
  group_by(Block,Week)%>%
  mutate(weekdiff=mean(tempdiff))%>%
  group_by(Date,Block,Treatment)%>%
  summarize(tempdiff=min(tempdiff), Treatment=Treatment, DailyMax=DailyMax, Week=Week, weekdiff=weekdiff )#Trying to eliminate the duplicates/reduce the amount of values for easier plotting 



monthtemps<-tempreadouts%>%
  select(2, 4, 5, 6,8,18)%>%
  group_by(Date)%>%
  distinct()%>%
  group_by(Month,Block)%>%
  summarize(Group=Group, Block=Block, Treatment=Treatment, MonthlyMean=MonthlyMean, Month=Month)%>%
  distinct() #MonthlyMean is the Monthly Mean Temperatures


#pull out the weekly minimum temperature differences between control and treatments
mintempdiff<-tempreadouts%>%
  select(2, 4, 5, 6,7,8,9,11)%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Date)%>%
  distinct()%>% 
  group_by(Date,Block)%>% #For every control/treatment of a block on a date... 
  mutate(tempdiff=min(DailyMin))%>% #Get the lowest temperature between the two 
  group_by(Date,Block, Treatment)%>% #Then for every control/treatment
  mutate(tempdiff=-(tempdiff-DailyMin))%>% #Subtract that temperature from its temperature-the ones with minimum temperatures should be 0, the other should be lower than 0 as it's larger. 
  group_by(Date,Block,Treatment)%>%
  summarize(tempdiff=min(tempdiff), Treatment=Treatment, DailyMin=DailyMin, Week=Week) #Trying to eliminate the duplicates/reduce the amount of values for easier plotting 


#Take out the temperature readings for May? Not sure if I still need this, probably need to delete
tempdiff<-tempreadouts%>%
  select(2,4,6,7, 8,9,14)%>%
  distinct()%>%
  group_by(Week, Treatment)%>%
  summarize(MeanWeeklyMin=mean(WeeklyMin))%>%
  ungroup()%>%
  group_by(Week)%>%
  mutate(MeanWeeklyMindiff=diff(MeanWeeklyMin))



#Pulling out temperature differences and two-week min and average temperatures
#Step 1-Pull out the temperature data for Controls and Treatment at X time scale
#Step 2-Calculate the difference between Controls and Treatment at a particular time step 
#Step 3- rebind this to the dataset-dependent on the time scale of the data, not the treatments


tempreadouts$Block<-factor(tempreadouts$Block, levels=c("1", "2","3"))


#Calculating the min and average temperature for the two weeks preceding the sample dates.
#So the two week date comes from the stats object, but it needs to also be attached to the tempreadouts df so that you can calculate 
twoweeks<-edit_stats%>%
  mutate(CollectedDate=mdy(CollectedDate), TwoWeekDate=mdy(TwoWeekDate), MonthNum=month(CollectedDate), Block=as.factor(Block))


#Join the twoweeks dataframe to the monthtemps dataframe so that we can do the CPlot with all of the temperatures, not just the two week lag. 

twoweeks<-twoweeks%>%
  left_join(monthtemps,by=c("Month", "Treatment", "Block"))

mintwoweeks<-ibutton%>%
  filter(Group=="5_1_1_U" | Group== "5_1_5_D" | Group=="11_1_1_U" | Group=="11_1_5_D" | Group=="11_2_1_U" | Group== "11_2_4_D")%>%
  group_by(Group)%>%
  mutate(Group=as.factor(Group),Temp=as.numeric(Temp), Block=as.factor(Block), DateTime=mdy_hm(DateTime), Date=date(DateTime))%>%
  ungroup()%>%
  mutate(Treatment=as.factor(case_when(str_detect(Group, "U") ~ "Control", TRUE ~ "Treatment")))%>%
  dplyr::select(-X)%>%
  group_by(Group)%>%
  mutate(MonthNum=month(DateTime), Month=month.name[MonthNum])

#for loop for each month, where you iterate through each month, and then filter between the dates of twoweeks' CollectedDate and TwoWeekDate

p<-unique(mintwoweeks$MonthNum)[-c(7,8,9)]
counter<-0
y<-list()
x<-list()
data<-data.frame(matrix(NA, nrow=6, 6))

#For loop to extract mean temperature for each Jar for two weeks preceding collection date. Can rerun this to get the min temp instead.
for(i in p){
  counter<-counter+1
  scpedit<-twoweeks%>%
    filter(MonthNum==i)
  
  edit<-mintwoweeks%>%
    filter(MonthNum==i)%>%
    filter(Date <=as.Date(unique(scpedit$CollectedDate)) & Date >= as.Date(unique(scpedit$TwoWeekDate)))%>%
    mutate(Day=day(Date))%>%
    group_by(Group, Day)%>%
    summarize(Min=min(Temp))%>%
    ungroup()%>%
    group_by(Group)%>%
    summarize(MeanMin=sum(Min)/14)
  
  x[[counter]]<-edit$Group
  
  colnames(data)<-x[[counter]]
  
  data[counter,]<-edit$MeanMin
}

data$MonthNum<-unique(twoweeks$MonthNum)

meantemp<-data%>%
  pivot_longer(cols = 1:6, names_to = "Jar", values_to = "Mean")%>%
  mutate(Treatment=as.factor(case_when(str_detect(Jar, "U") ~ "Control", TRUE ~ "Sheltered")), Block=as.factor(rep(x=c(2,2,3,3,1,1), times=6)), Mean=as.numeric(Mean))

meantempfilt<-data%>%
  pivot_longer(cols = 1:6, names_to = "Jar", values_to = "Mean")%>%
  filter(MonthNum=="2")%>%
  mutate(Treatment=as.factor(case_when(str_detect(Jar, "U") ~ "Control", TRUE ~ "Sheltered")), Block= as.factor(c(2,2,3,3,1,1)))

joined<-left_join(twoweeks, meantemp,by=c("Treatment", "Block", "MonthNum")) 

fullData<-joined%>%
  group_by(Block, Jar.y)%>%
  mutate(MeanSCP=mean(SCP_Value))%>% #MeanSCP is mean SCP per jar/block
  group_by(Block, Treatment,Month)%>%
  mutate(MeanMonthlySCP=mean(SCP_Value))%>% #MeanMonthlySCP is mean SCP per treatment/block/month
  mutate(Month=factor(Month, levels=c("November", "December", "January", "February", "March", "April")))%>%
  group_by(Month, Treatment,Block)%>%
  mutate(MeanSCP=mean(SCP_Value)) 

join<-fullData%>%
  left_join(ex, by=c("TwoWeekDate", "Treatment", "Block"))%>%
  mutate(Month=Month.x)


#For B Plot
#Plot weekly temperatures and the differences between them 
#Let's grab the weekly temperatures and clean up the dataset a bit
weeklytemps<-tempreadouts%>%
  select(Group,Date,Block, Treatment,Week, Month, WeeklyMax,WeeklyMin)%>%
  distinct()%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")%>%
  group_by(Block, Week)%>%
  mutate(maxtemp=max(WeeklyMax), mintemp=min(WeeklyMin))%>%
  group_by(Block, Date)%>%
  mutate(maxdiff=diff(WeeklyMax),mindiff=diff(WeeklyMin))%>%
  summarize(maxdiff=maxdiff, Date=Date, Block=Block, mindiff=mindiff)%>%
  mutate(Group=ifelse(mindiff<=0, "Control", "Sheltered"))%>%
  distinct()
#Filter tempreadouts for a smaller subset of data (November-April)
subset<-tempreadouts%>%
  select(Group,Date,Block, Treatment,Week, Month, WeeklyMax,WeeklyMin, Var, WeekVar, WeeklyRange, MonthlyRange)%>%
  distinct()%>%
  filter(Month=="November"|Month=="December"|Month=="January"|Month=="February"|Month=="March"|Month=="April")

subset$Month<- factor(subset$Month, levels=c("November", "December", "January", "February", "March", "April"))

#Combining join and subset to have the range and SCP values in the same dataset
tempreadoutfilt<-subset%>%
  group_by(Month, Treatment, Block)%>%
  summarize(Var=Var, MonthlyRange=MonthlyRange)%>%
  distinct()


combined<-subset%>%
  summarize(Month=Month,Block=Block, Treatment=Treatment, MonthlyRange=MonthlyRange, Var=Var )%>%
  distinct()%>%
  left_join(join, by=c("Month", "Block","Treatment"))


tempreadouts$Month<- factor(tempreadouts$Month, levels=c("November", "December", "January", "February", "March", "April"))

filt<-join%>%
  filter(Treatment=="Control")
filt2<-join%>%
  filter(Treatment=="Sheltered")
linreg<-lm(data=filt, formula = SCP_Value~MonthlyRange)

