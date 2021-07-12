---
title: "Cyclistic BIKE-SHARE Case Study"
author: "Pei I Shih"
date: "2021/7/12"
output: html_document
---

## Summary
This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It’s originally based on the case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman ( [Found Here](https://artscience.blog/home/divvy-dataviz-case-study) ). I will be using the *Divvy dataset* for the case study. The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: **“In what ways do members and casual riders use Divvy bikes differently?”**

## Scenario 
Cyclistic is a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, marketing analyst team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

## Characters and teams
●	Cyclistic: A bike-share program that features more than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day.

●	Lily Moreno: The director of marketing and your manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels.

●	Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy. 

●	Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the recommended marketing program.

## About the company
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## Data cleaning
I have cleaned my data and save the process at my [github](https://github.com/Pei-I/MyPortfolio/blob/main/cyclistic.R). 

## Set up environment
```{r}
library(tidyverse)
library(lubridate)
library(sqldf)
```

## Import data
```{r}
all_trips_v3 <- read.csv("D:/file/all_trips_v3.csv")
#this is the combined data from 2020 Q2 to 2021 Q1 after data cleaning
```

## Conduct descriptive analysis


### 1. Compare members and casual users

```{r}
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)

```

### 2. See the average ride time by each day for members vs casual users

```{r}
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
```

### 3. Compare the number of rides for members vs casual users 
```{r}
count(all_trips_v3, member_casual)
```

### 4. Analyze ridership data by type and weekday

```{r}
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE, locale = "English")) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()		#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)						# sorts

```


### 5. Assign a name for the analyzed data frame

```{r}
V<-all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE, locale = "English")) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)    
```

### 6. Notice that the days of the week are out of order. Let's fix that.

```{r}
V$weekday <- ordered(V$weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

```


### 7. Visualize the number of rides by rider type

```{r}


ggplot(data = V, mapping = aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col( position = "dodge")+
  labs(title = "Cyclistic BIKE-SHARE: Weekday VS. Number of Rides", 
       subtitle = "Comparison of Rider Type", 
       caption = "Data collected from https://divvy-tripdata.s3.amazonaws.com/index.html")


ggplot(data = V, mapping = aes(x = weekday, y = average_duration, fill = member_casual))+
  geom_col( position = "dodge")+
  labs(title = "Cyclistic BIKE-SHARE: Weekday VS. Avg_Duration", 
       subtitle = "Comparison of Rider Type", 
       caption = "Data collected from https://divvy-tripdata.s3.amazonaws.com/index.html")
```

### 8. Export summary file

```{r}
write.csv(V,'result.csv')
```

## Summarizing recommendations for the business
After analyzing Cyclistic BIKE-SHARE data, I found some insights that might help their marketing strategy.


#### **Insight 1 : Rider Type** 

* The number of annual members is much larger than that of casual riders. 

#### **Insight 2 : Trip Duration**

* The trip duration of annual members is approximately 15-20 minutes, while that of casual riders is around 40-50 minutes. 

* I suppose the reason is because, for annual members, their purpose is commute to work. For casual riders, they prefer to use this service to relax or exercise.  

#### **Insight 3 : Number of Rides**

* There is no obvious difference in the number of rides of annual members from day to day; while the number of rides of casual riders increases on weekends, especially on Saturday. 

* I think that's because people like to go out and relax on weekends. 

## Idea for Clyclistic BIKE-SHARE

Due to the trend of casual riders using more shared bicycles on weekends, Cyclistic can add a new type of membership that only applies to weekends.





