#Load libraies
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)


#Loading Datasets
apr20 <- read.csv("202004-divvy-tripdata.csv")
may20 <- read.csv("202005-divvy-tripdata.csv")
jun20 <- read.csv("202006-divvy-tripdata.csv")
jul20 <- read.csv("202007-divvy-tripdata.csv")
aug20 <- read.csv("202008-divvy-tripdata.csv")
sep20 <- read.csv("202009-divvy-tripdata.csv")
oct20 <- read.csv("202010-divvy-tripdata.csv")
nov20 <- read.csv("202011-divvy-tripdata.csv")
dec20 <- read.csv("202012-divvy-tripdata.csv")
jan21 <- read.csv("202101-divvy-tripdata.csv")
feb21 <- read.csv("202102-divvy-tripdata.csv")
mar21 <- read.csv("202103-divvy-tripdata.csv")

#check column names of each datasets consistency
colnames(apr20)
colnames(may20)
colnames(jun20)
colnames(jul20)
colnames(aug20)
colnames(sep20)
colnames(oct20)
colnames(nov20)
colnames(dec20)
colnames(jan21)
colnames(feb21)
colnames(mar21)

#Check data structures and data types for all data frames
str(apr20)
str(may20)
str(jun20)
str(jul20)
str(aug20)
str(sep20)
str(oct20)
str(nov20)
str(dec20)
str(jan21)
str(feb21)
str(mar21)

#columns have inconsistency of data in dec20,jan21,feb21,mar21 datasets. We need to convert from char to double using mutate() function
dec20 <-  mutate(dec20, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
jan21 <-  mutate(jan21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
feb21 <-  mutate(feb21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))
mar21 <-  mutate(mar21, start_station_id = as.double(start_station_id),
                 end_station_id = as.double(end_station_id))

#let's check whether the changes took place
is.double(dec20$start_station_id)
is.double(dec20$end_station_id)
is.double(jan21$start_station_id)
is.double(jan21$end_station_id)
is.double(feb21$start_station_id)
is.double(feb21$end_station_id)
is.double(mar21$start_station_id)
is.double(mar21$end_station_id)

# combining all datasets into one dataframe
all_trips <- bind_rows(apr20, may20, jun20, jul20, aug20, sep20, oct20, nov20, dec20, jan21, feb21, mar21)

# check structure of the new dataframe
str(all_trips)

#all looks good
#remove the columns which is not required
all_trips <- all_trips %>%
  select(-c(start_lat:end_lng))
glimpse(all_trips)

#rename columns for better readibility
all_trips <- all_trips %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(all_trips)

#adding columns that we can use for aggregation function
#column for day of the week the trip started
all_trips$day_of_the_week <- format(as.Date(all_trips$start_time),'%a')

#column for month when the trip started
all_trips$month <- format(as.Date(all_trips$start_time),'%b_%y')

#column for time of the day when the trip started
#Time element needs to be extracted from start_time. However, as the times must be in POSIXct
#(only times of class POSIXct are supported in ggplot2), a two-step conversion is needed. 
#First the time is converted to a character vector, effectively stripping all the date information. 
#The time is then converted back to POSIXct with today’s date – the date is of no interest to us,
#only the hours-minutes-seconds are.
all_trips$time <- format(all_trips$start_time, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")

#column for trip duration in min
all_trips$trip_duration <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/60

# check the dataframe
glimpse(all_trips)

# checking for trip lengths less than 0
nrow(subset(all_trips,trip_duration < 0))

#checking for testrides that were made by company for quality checks
nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "Test"))

# remove negative trip durations 
all_trips_v2 <- all_trips[!(all_trips$trip_duration < 0),]

#remove test rides
all_trips_v2<- all_trips_v2[!((all_trips_v2$start_station_name %like% "TEST" | all_trips_v2$start_station_name %like% "test")),]

#check dataframe
glimpse(all_trips_v2)

# checking count of distinct values
table(all_trips_v2$customer_type)
#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_v2, sum), c("customer_type", "total_trip_duration(mins)"))

#Analyzing and visualize the datasets
# statictical summary of trip_duration for all trips
summary(all_trips_v2$trip_duration)

#statistical summary of trip_duration by customer_type
all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

#Total number of customer type and day in a week
# fix the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations
all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Apr_20", "May_20", "Jun_20", "Jul_20", "Aug_20", "Sep_20", "Oct_20",
                                                           "Nov_20", "Dec_20", "Jan_21", "Feb_21", "Mar_21"))
all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))


#Visualize the set of total trip by cistomer and day of week
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#average number of trips by customer types
all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))

#visualize
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#viz of average number trip duraton by customer type by each week
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")

#viz of average trip duration by customer type vs month
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))

#viz of ride type vs number of trips by customer
all_trips_v2 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")


#Classic bikes are predominantly used by members. Docked bikes are in most demand and equally used by both members as well as casual riders. Electric bikes are more favored by members. If electric bikes costs the highest among all 3 types, it would be a financially sound move to increase their fleet while reducing docked bikes, as they are already preferred by members who make up for the majority of the trips.

#Note: Data is not available on the quantity of fleet across each type of bikes

#Creating a csv file of the clean data for futher analysis or visualizations in other tools like SQL, Tableau
clean_data <- aggregate(all_trips_v2$trip_duration ~ all_trips_v2$customer_type + all_trips_v2$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)


#Key Takeaways
#Casual riders made 41% of total trips contributing to 66% of total trip duration between Apr'20 - Mar'21. Member riders make up 59% of total trips contributing to 34% of total trip duration between Apr'20 - Mar'21

#Usage (based on trip duration) of bikes by casual riders is almost twice that of member riders.

#Casual customers use bikeshare services more during weekends, while members use them consistently over the entire week.

#Average trip duration of casual riders is more than twice that of member rider over any given day of the week cumulatively.

#Casual riders ride longer during first half of the year compared to the second half, while members clock relatively similar average trip duration month over month.

#Casual riders prefer docked bikes the most while classic bikes are popular among members.

#Recommendations
#Provide attractive promotions for casual riders on weekdays so that casual members use the bikeshare services ore uniformly across the entire week.

#Offer discounted membership fee for renewals after the first year. It might nudge casual riders to take up membership.





