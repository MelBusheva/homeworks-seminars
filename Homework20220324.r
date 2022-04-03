#Problem 1

FactorialFunction <- function(InputNumber){
   InputNumber<- as.integer(InputNumber)
   for(k in 1:(InputNumber-1)){
    if(InputNumber > 0){
     InputNumber <- as.numeric(InputNumber*k) 
     Result <- InputNumber
    } else if (InputNumber == 0){
      Result <- 1
    } else{
      Result <- NaN
    }
  }
  return(Result)
}


#Problem 2

inputVector<-c(2,5,7,35,78,99)

SDFunction <- function(inputVector){
  Average <- mean(inputVector)
  SquaredDifference<-(inputVector- Average)^2
  Average2 <- sum(SquaredDifference)/(length(SquaredDifference)-1) #looked it up on the internet and saw that it is like that in, originally was mean(SquaredDifference)
  SD <- sqrt(Average2)
return(SD)
}

#Problem 3

library(nycflights13)
library(tidyverse)

# 5.6.7 Exercises

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
####1.Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:####
#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
#A flight is always 10 minutes late.
#A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
#99% of the time a flight is on time. 1% of the time it’s 2 hours late.
#Which is more important: arrival delay or departure delay?####
  
#I saw the solution but I don't think I totally get it

#2.Come up with another approach that will give you####

#original
not_cancelled %>% 
  count(dest)
#alternative
not_cancelled%>%
  group_by(dest)%>%
  summarise(n())
#original
not_cancelled %>% 
  count(tailnum, wt = distance)
#alternative
not_cancelled%>%
  group_by(tailnum)%>%
  summarise(sum(distance))

#3. If dep_delay is NA, it means that this flight is cancelled. Subsequently it will not have an arrival time as well.

#4.
cancelled <-flights%>%
  filter(is.na(dep_delay))%>%
  group_by(month, day)%>%
  summarise(CancelledPerDay = n())

flights%>%
  group_by(day)%>%
  mutate(n(is.na(dep_delay)))%>%
  summarise(AverageDelay = mean(dep_delay))%>%
  

ggplot(data = cancelled, mapping = aes(x = CancelledPerDay)) + 
  geom_freqpoly(binwidth = 10)

ggplot(data = cancelled, mapping = aes(x = CancelledPerDay, y = month)) + 
  geom_point(alpha = 1/10)


DelayAvg <- not_cancelled%>%
  group_by(month,day)%>%
  summarise(AverageDelay=mean(dep_delay))

ggplot(data = DelayAvg, mapping = aes(x = AverageDelay)) + 
  geom_freqpoly(binwidth = 10)
  
ggplot(data = DelayAvg, mapping = aes(x = AverageDelay, y = month)) + 
  geom_point(alpha = 1/10)

#5.
CarrierDest <- flights %>% 
  group_by(carrier, dest) %>%
  summarise(NrFlights = n(), AverageDelay = round(mean(dep_delay, na.rm = T), 2))%>%
  mutate(AvgDelayPerFlight = round((AverageDelay/NrFlights),2))

CarrierDest%>%
  filter(NrFlights > 200)%>%
ggplot(CarrierDest, mapping = aes(x = NrFlights, y = AverageDelay))+
  geom_point()+
  geom_smooth(se = FALSE)



#6. If it is set to TRUE, it will essentially order the groups by the total number of observations, or, in other words, the rows in the new count 
#column will be arranged in descending order.

# 5.7.1 Exercises

#1.Just every operation starts working on a group basis rather than on the whole data.

#2.
by_tailnum <- flights %>%
  mutate( onTime = %>%
  group_by(tailnum)%>%
  filter(rank(desc(dep_delay)) < 10)

#3.What time of day should you fly if you want to avoid delays as much as possible?

#4.For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.

#5.Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag(), explore how the delay of a flight is related to the delay of the immediately preceding flight.

#6.Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?
  
#7.Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.

#8.For each plane, count the number of flights before the first delay of greater than 1 hour.
#####Problem 4#####


#4.1 
by_dest <- flights %>%
  select(carrier,dest) %>%
  group_by(carrier) %>%
  count(dest) %>%
  arrange(carrier,desc(n))%>%
  summarise(MaxDest= first(dest), NrFlights = max(n))

#4.2 
by_delay <- flights %>%
  select(carrier, dep_delay)%>%
  group_by(carrier)%>%
  summarise(MaxDepDelay = max(dep_delay, na.rm = T))

#4.3 
by_plane <- flights %>%
  select(tailnum, distance)%>%
  group_by(tailnum)%>%
  filter(!is.na(tailnum))%>%
  summarise(TotalDistance = sum(distance, na.rm = T))%>%
  arrange(desc(TotalDistance))%>%
  slice_head(n = 3)
  
by_plane2 <- flights %>%
  select(tailnum, distance)%>%
  group_by(tailnum)%>%
  filter(!is.na(tailnum))%>%
  summarise(TotalDistance = sum(distance, na.rm = T))%>%
  arrange(TotalDistance)%>%
  slice_head(n = 3)

#4.4 
by_month <- flights%>%
  select(month, day, flight, carrier, distance, dep_time, dep_delay, arr_delay)

February <- by_month %>%
  select(month, day, flight,dep_time)%>%
  filter(month == 2 & !is.na(dep_time))%>%
  group_by(day)%>%
  summarise(FirstFlight = first(flight), LastFlight = last(flight))

#4.5 

March <- by_month%>%
  select(month, carrier, distance)%>%
  filter(month == 3)%>%
  group_by(carrier)%>%
  summarise(TotalMiles = sum(distance))%>%
  arrange(desc(TotalMiles))%>%
  slice_head()

#Which flew the least?

March2 <- by_month%>%
  select(month, carrier, distance)%>%
  filter(month == 3)%>%
  group_by(carrier)%>%
  summarise(TotalMiles = sum(distance))%>%
  arrange(TotalMiles)%>%
  slice_head()

#4.6 

DelaysMonth <- by_month%>%
  select(month, dep_delay)%>%
  filter(dep_delay > 60 & !is.na(dep_delay))%>%
  group_by(month)%>%
  summarise(NrDelays = n())%>%
  arrange(desc(NrDelays))%>%
  slice_head()

#4.7 

TimebwFlights <- flights%>%
  select(month,day,flight, dep_time)%>%
  filter(!is.na(dep_time))%>%
  mutate(DepTime= (((dep_time%/%100)*60) + dep_time%%100)%%1440, Difference = DepTime- lag(DepTime))%>%
  summarise(AverageTime = mean(Difference >0, na.rm = T))


#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.

