collegeexpansion<-read.csv("F:/collegeexpansion.csv",header=T)
install.packages("mlbench")
library(mlbench)
library(ggplot2)
#Figure 1
ggplot(collegeexpansion)+geom_bar(aes(Birthyear,fill=Education),position="dodge")+
labs(title=paste("Barplot assorted by Education Level & grouped by Birthyear"))
#figure2
ggplot(collegeexpansion)+geom_bar(aes(Birthyear,fill=Education),position="fill")+
labs(title=paste("Barplot:Proportion of different Education Levels"))
#·Ö×é
attach(collegeexpansion)
treat<-subset.data.frame(collegeexpansion,Birthyear>=1981)
control<-subset.data.frame(collegeexpansion,Birthyear<1981)
#Figure 3
barcontrol<-ggplot(control)+
geom_bar(aes(x=Education,fill=Education),show.legend = FALSE,width=1)+
theme(aspect.ratio=1)+labs(x=NULL,y=NULL)
barcontrol+coord_flip()+labs(title=paste("Barplot:Before College Expansion"))
#Figure 4
barcontrol+coord_polar()+labs(title=paste("Polar Coordinates:Before College Expansion"))
#Figure 5
bartreat<-ggplot(treat)+
geom_bar( aes(x=Education,fill=Education),show.legend = FALSE,width=1 )+
  theme(aspect.ratio=1)+labs(x=NULL,y=NULL)
bartreat+coord_flip()+labs(title=paste("Barplot:After College Expansion"))
#Figure 6
bartreat+coord_polar()+labs(title=paste("Polar Coordinates:After College Expansion"))

#Figure 7
collegerate<-aggregate(College~Birthyear,collegeexpansion,mean)
collegerate
ggplot(collegerate,aes(x=Birthyear,y=College))+ geom_point()+geom_smooth()+
geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Rate of obtaining college education"))
#Figure 8
employrate<-aggregate(Employ~Birthyear,collegeexpansion,mean)
employrate
ggplot(employrate,aes(x=Birthyear,y=Employ))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Employment Rate"))

#Figure 9
nonagremployrate<-aggregate(Nonagremploy~Birthyear,collegeexpansion,mean)
nonagremployrate
ggplot(nonagremployrate,aes(x=Birthyear,y=Nonagremploy))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Nonagremployment Rate"))

#Figure 10
meanlnw<-aggregate(Lnw~Birthyear,collegeexpansion,mean)
meanlnw
ggplot(meanlnw,aes(x=Birthyear,y=Lnw))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Mean of Annual Salary Logrithm"))

#Figure 11
meanhappiness<-aggregate(Happiness~Birthyear,collegeexpansion,mean)
meanhappiness
ggplot(meanhappiness,aes(x=Birthyear,y=Happiness))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Mean of Happiness"))
# Figure 12
ggplot(collegeexpansion,aes(x=Birthyear,y=Employ,color=Education))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Employment Rate of different Education Levels"))

# Figure 13
ggplot(collegeexpansion,aes(x=Birthyear,y=Nonagremploy,color=Education))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Non-agricultural Employment Rate of different Education Levels"))

# Figure 14
ggplot(collegeexpansion,aes(x=Birthyear,y=Lnw,color=Education))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Lnw of different Education Levels"))
#Figure 15
ggplot(collegeexpansion,aes(x=Birthyear,y=Lnw,colour=College==1))+geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Comparision Lnw whether obtained college education or not "))
#Figure 16
college<-subset.data.frame(collegeexpansion,College==1)
noncollege<-subset.data.frame(collegeexpansion,College==0)

par(mfrow=c(1,2))
hist(college$Lnw,breaks=10,xlim=c(6,14),ylim=c(0,400),col="white")
hist(noncollege$Lnw,breaks=10,xlim=c(6,14),ylim=c(0,800),col="grey")
par(mfrow=c(1,1))

#Figure 17
boxplot(college$Lnw,noncollege$Lnw)

#Figure 18
ggplot(collegeexpansion,aes(x=Birthyear,y=Happiness,color=Education))+ geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Mean of Life-Happiness"))
#Figure 19
ggplot(collegeexpansion,aes(x=Birthyear,y=Happiness,colour=College==1))+geom_point()+geom_smooth()+
  geom_vline(aes(xintercept=1981),colour="#BB0000",linetype="dashed")+
  labs(title=paste("Comparision of Happiness whether obtained college education or not "))










