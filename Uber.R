 # Data Analysis Using UBER dataset

library(ggplot2) #visualization matplotlib in case of python
library(ggthemes) #add-on with ggplot
library(dplyr) #data manipulation in case of pandas in pythom
library(lubridate) #date and time 
library(scales) #grapical scalling
library(tidyr) #tidy data
library(DT) #table formatted result

# Reading the chunk of data 
setwd('C:\\Users\\Lenovo\\Documents\\R\\Uber-dataset')
apr_data <- read.csv("uber-raw-data-apr14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")



#Combining all the data 
data_2014<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)



#visualize the data 
head(data_2014)


#structure
str(data_2014)


#Summary statistics 
summary(data_2014)



#what are your primary observation from date time we get time frame
#start analysis

data_2014$Date.Time<-as.POSIXct(data_2014$Date.Time,format = "%m/%d/%Y %H:%M:%S")

summary(data_2014)

#Extract time from date time
data_2014$Time<- format(as.POSIXct(data_2014$Date.Time,format = "%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time) #formatting
data_2014$day <- format(day(data_2014$Date.Time))  #day
data_2014$month <- format(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- format(year(data_2014$Date.Time))  #no meaning as we have data of 2014 only
data_2014$dayofweek <- format(wday(data_2014$Date.Time, label=TRUE))

#hour minute second 

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


#lets start visualization 

#plotting the trip by hours in a day 

hour_data <- data_2014 %>%
  group_by(hour) %>%
  summarise(Total=n())   #grouping the data wrt hour and count 

#see in a tabluar form 
datatable(hour_data)

#visualize the data

ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat="identity",fill="black" , color = "blue")+
  ggtitle("Trips By Hour")
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#most operations happens during 15 to 21 hour aka 3pm to 9pm  #fiding 
  
month_hour_data <- data_2014 %>%
    group_by(month,hour) %>%
    summarise(Total=n())


#see in a tabluar form 
datatable(month_hour_data)

#plot the same 

ggplot(month_hour_data,aes(hour,Total,fill = month))+
  geom_bar(stat="identity")+
  ggtitle("Trips By Hour and month")+
  scale_y_continuous(labels = comma)

#finding from graph------->  september has more rides than ever 

sept_hour <- data_2014 %>%
  group_by(hour,month) %>%
  filter(month=="sep") %>%
  summarise(Total= n())

ggplot(sept_hour,aes(hour,Total,fill = hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips By Hour and month for september")+
  scale_y_continuous(labels = comma)

#finding -3  ---> aroung 6 pm 70000 the most and do same for april

apr_hour <- data_2014 %>%
  group_by(hour,month) %>%
  filter(month=="apr") %>%
  summarise(Total= n())

ggplot(apr_hour,aes(hour,Total,fill = hour))+
  geom_bar(stat="identity")+
  ggtitle("Trips By Hour and month for april")+
  scale_y_continuous(labels = comma)


#around 50000 rides on 5 p.m. in april
#plot the data grouped by day 

day_data <- data_2014 %>%
  group_by(day) %>%
  summarise(Total=n())

datatable(day_data)

#visualization
ggplot(day_data,aes(day,Total))+
  geom_bar(stat="identity",fill="blue" , color = "black")+
  ggtitle("Trips By day")
theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#more or less uniform distribution rather than 31 because many month wont have 31

#do a month and day grouping 

month_day_data <- data_2014 %>%
  group_by(month,day) %>%
  summarise(Total=n())


#see in a tabluar form 
datatable(month_day_data)

#plot the same 

ggplot(month_day_data,aes(day,Total,fill = month))+
  geom_bar(stat="identity")+
  ggtitle("Trips By Day and month")+
  scale_y_continuous(labels = comma)

#finding data worth of 183 days
#sept data 

sept_day <- data_2014 %>%
  group_by(day,month) %>%
  filter(month =="sep") %>%
  summarise(Total= n())

ggplot(sept_day,aes(day,Total,fill = day))+
  geom_bar(stat="identity")+
  ggtitle("Trips By Day and month for september")+
  scale_y_continuous(labels = comma)

#13 has highest number of rides 

#monthly trend
month_data <- data_2014 %>% group_by(month) %>% summarise(Total = n())
datatable(month_data)

#aug and sept has highest kind of rides find why this happens to repeat 
#ask the company wheather it is due marketing or internal or envo effect

ggplot(month_data,aes(month,Total,fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips by month")+
  
  scale_y_continuous(labels = comma)

#month-weekday
month_weekday_data <- data_2014 %>% group_by(month,dayofweek) %>% summarise(Total = n())
datatable(month_weekday_data)

ggplot(month_weekday_data,aes(month,Total,fill=dayofweek))+
  geom_bar(stat="identity")+
  ggtitle("Trips By month and weekday")+
  
  scale_y_continuous(labels = comma)

#only for weekday
weekday_data <- data_2014 %>% group_by(dayofweek) %>% summarise(Total = n())
datatable(weekday_data)

ggplot(weekday_data,aes(dayofweek,Total,fill=dayofweek))+
  geom_bar(stat="identity")+
  ggtitle("Trips By  weekday")+
  
  scale_y_continuous(labels = comma)

#any reason why thrusday and friday has more traffic 

#analysis of bases

ggplot(data_2014,aes(Base))+
  geom_bar(fill="darkred")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by bases")

#it seems B02512 is not much profitable
ggplot(data_2014,aes(Base,fill=month))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by Bases and Month")

#events in sept in b02764 and event in apr and may in b02617 must be observed.

ggplot(data_2014,aes(Base,fill=dayofweek))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by Bases and Dayofweek")

#nothing suspi.......
day_and_hour<- data_2014 %>%
  group_by(day,hour) %>%
  dplyr::summarise(Total= n())
datatable(day_and_hour)

ggplot(day_and_hour,aes(day,hour,fill=Total))+
  geom_tile(color="white")+
  ggtitle("heat map by hor and day")












