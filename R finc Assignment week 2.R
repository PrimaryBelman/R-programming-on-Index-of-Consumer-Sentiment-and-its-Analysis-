library(ggplot2)
library(tidyverse)
#install.packages("tidyverse")

###reading and extracting the file data
getwd()
setwd("C://Backup//DataPartition//RIT Course Work//FINC//Assignment 2")

df_main<-read.csv("sca-tableall-on-2022-Nov-09.csv",header=F)
df_first<-read.csv(file='sca-table1-on-2022-Nov-09.csv',header=F)
df_second<-read.csv(file='sca-table2-on-2022-Nov-09.csv',header=F)
df_third<-read.csv(file='sca-table3-on-2022-Nov-09.csv',header=F)
df_fourth<-read.csv(file='sca-table4-on-2022-Nov-09.csv',header=F)
df_fifth<-read.csv(file='sca-table5-on-2022-Nov-09.csv',header=F)


##cleaning the dataframes
colnames(df_first)<-c('Month','Year','Index','V')
df_first<-df_first%>%  filter(!row_number() %in% c(1, 2))
df_first<-df_first[,1:3]
row.names(df_first)<-NULL
head(df_first)
str(df_first)
df_first$Index<-as.numeric(df_first$Index)
df_first$Year<-as.factor(df_first$Year)
df_first$Month<-as.factor(df_first$Month)


colnames(df_second)<-c('Month','Year','Lower Third','Middle Third','Upper Third','V')
df_second<-df_second%>%  filter(!row_number() %in% c(1, 2))
df_second<-df_second[,1:5]
head(df_second)
str(df_second)
df_second$Year<-as.factor(df_second$Year)
df_second$Month<-as.factor(df_second$Month)
df_second$`Lower Third`<-as.numeric(df_second$`Lower Third`)
df_second$`Middle Third`<-as.numeric(df_second$`Middle Third`)
df_second$`Upper Third`<-as.numeric(df_second$`Upper Third`)



colnames(df_third)<-c('Month','Year','18-34','35-54','55+','V')
df_third<-df_third%>%  filter(!row_number() %in% c(1, 2))
df_third<-df_third[,1:5]
df_third$Year<-as.factor(df_third$Year)
df_third$Month<-as.factor(df_third$Month)
df_third$`18-34`<-as.numeric(df_third$`18-34`)
df_third$`35-54`<-as.numeric(df_third$`35-54`)
df_third$`55+`<-as.numeric(df_third$`55+`)


head(df_third)
str(df_third)

head(df_fourth)

colnames(df_fourth)<-c('Month','Year','North East','North Central','South','West','V')
df_fourth<-df_fourth%>%  filter(!row_number() %in% c(1, 2))
df_fourth<-df_fourth[,1:6]
df_fourth$Year<-as.factor(df_fourth$Year)
df_fourth$Month<-as.factor(df_fourth$Month)
df_fourth$`North East`<-as.numeric(df_fourth$`North East`)
df_fourth$`North Central`<-as.numeric(df_fourth$`North Central`)
df_fourth$South<-as.numeric(df_fourth$South)
df_fourth$West<-as.numeric(df_fourth$West)

colnames(df_fifth)<-c('Month','Year','Personal Finance Current','Personal Finance Expected','Business Condition 12 Months','Business Condition 5 Years','Buying Conditions','Current Index','Expected Index','V')
df_fifth<-df_fifth%>%  filter(!row_number() %in% c(1, 2))
df_fifth<-df_fifth[,1:9]
df_fifth$Year<-as.factor(df_fifth$Year)
df_fifth$Month<-as.factor(df_fifth$Month)
df_fifth$`Personal Finance Current`<-as.numeric(df_fifth$`Personal Finance Current`)
df_fifth$`Personal Finance Expected`<-as.numeric(df_fifth$`Personal Finance Expected`)
df_fifth$`Business Condition 12 Months`<-as.numeric(df_fifth$`Business Condition 12 Months`)
df_fifth$`Business Condition 5 Years`<-as.numeric(df_fifth$`Business Condition 5 Years`)
df_fifth$`Buying Conditions`<-as.numeric(df_fifth$`Buying Conditions`)
df_fifth$`Current Index`<-as.numeric(df_fifth$`Current Index`)
df_fifth$`Expected Index`<-as.numeric(df_fifth$`Expected Index`)

head(df_fourth)
head(df_fifth)

##EDA

##Let us check the average index by year

df_first_groupyr<-df_first%>%group_by(Year)%>%select(Index,Year)%>%filter(Year %in% c('2015','2016','2017','2018','2019','2020','2021','2022'))%>%summarize(Index)
#is.na(df)
df_first_groupyr

str(df_first$Year)

ggplot(data=df_first_groupyr,aes(x=Year,y=Index,group=1))+geom_point()+geom_line()
##We can see that Consumer purchasing power increased in the year 2018 and then fell down drastically.
##This could be due to COVID

head(df_second)

df_second_groupyr<-df_second%>%group_by(Year)%>%select(`Lower Third`,`Middle Third`,`Upper Third`)%>%filter(Year %in% c('2015','2016','2017','2018','2019','2020','2021','2022'))%>%summarize(`Lower Third`,`Middle Third`,`Upper Third`)
df_second_groupyr


ggplot(data=df_second_groupyr)+geom_point()+geom_line()

