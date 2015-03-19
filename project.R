# github testing
#grouping the countries into income status and HDI rank
#think about more on whether to predict or what can we say about HDI?


library(ggplot2)
hdi<-read.csv(file ="C:/Users/Kaylee/Documents/MA882/project/dataset compiled.csv", header=T)
head(hdi)

#transfer each variables into numerical variables
HDI<-as.numeric(hdi$HDI.100)
immi<-as.numeric(hdi$Immigration)
unemp<-as.numeric(as.character(hdi$Unemploy))
LFP.female<-as.numeric(hdi$LFP_female)
LFP.male<-as.numeric(hdi$LFP_male)
SOS<-as.numeric(hdi$share.of.seats)
forest.change<-as.numeric(hdi$forest_change)
urban<-as.numeric(hdi$urban)
smoker<-as.numeric(hdi$smoker_male)
health<-as.numeric(hdi$health_expend)
internet<-as.numeric(hdi$internet_user)
income<-as.numeric(hdi$income)
HDIrank<-as.numeric(hdi$HDIrank)

#visualizing data using ggplot2
#boxplot
box.immi1<-qplot(factor(income), immi, data=hdi, xlab="Income Category", ylab="HDI*100",
                 geom="boxplot", margins=TRUE)
box.immi1<-box.immi1+theme_bw()
box.immi1

box.immi2<-qplot(factor(HDIrank), immi, data=hdi, xlab="HDI Rank", ylab="Immigration Rate",
                 geom="boxplot")
box.immi2<-box.immi2+theme_bw()
box.immi2

box.unemploy<-qplot(factor(income),unemp, data-hdi, xlab="Income Category",
                    ylab="Unemployment Rate",geom="boxplot")
box.unemploy<-box.unemploy+theme_bw()
box.unemploy

box.health<-qplot(factor(income), health, data=hdi, xlab="Income Category",
                  ylab="Health Expenditure", geom="boxplot")
box.health<-box.health+theme_bw()
box.health

box.urban<-qplot(factor(income), urban, data=hdi, xlab="Income Category", 
                 ylab="Urban Population (% Total Population)", geom="boxplot")
box.urban<-box.urban+theme_bw()
box.urban


#scatter plot
#forest change
plot.forest<-qplot(forest.change, HDI.100, data=hdi, xlab="Forest Change from 2004 to 2013", 
                   ylab="HDI index", colour=factor(income))#+stat_smooth(method="lm")
plot.forest<-plot.forest+theme_bw()
plot.forest<-plot.forest+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot.forest
#immigration population rate
plot.immi<-qplot(immi, HDI.100, data=hdi, xlab="Immigration Rate", ylab="HDI index",
                 colour=factor(income))+stat_smooth(method="lm")
plot.immi<-plot.immi+theme_bw()
plot.immi
#urban population rate
plot.urban<-qplot(urban, HDI.100, data=hdi, xlab="Urban Population Rate",
                  ylab="HDI index", colour=factor(income))+stat_smooth(method="lm")
plot.urban<-plot.urban+theme_bw()
plot.urban
#total health expenditure
plot.health<-qplot(health,HDI, data-hdi, xlab="Total Health Expenditure (% GDP)",
                   ylab="HDI*100",colour=factor(income))+stat_smooth(method="lm")
plot.health<-plot.health+theme_bw()
plot.health


#clean data y is the data frame
#y$education[y$education==9999999999]<-NA
#y$education[y$education==0]<-NA
#y$education[y$education<0]<-NA
