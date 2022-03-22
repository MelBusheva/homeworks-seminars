# Problem 1
Bet <- 1
sumLenght <- NULL
for (i in 1:1000) {
  Budget <- 100
  t <- while (Budget > 0) {
    Budget <- Budget - Bet
    winOrLoss <- sample(c(0, 1), 1, replace = TRUE, prob = c(0.514, 0.486))
    if (winOrLoss == 0) {
      Bet <- Bet * 2
    } else {
      Bet <- c(-1)  
    }
  }
  sumLenght <- sumLenght + length(t)}
averageLenght <- sumLenght / 1000

I am not sure about this task. I can't run the code properly for some reason.

#Do all the exercises:
# 5.2.4 Exercises 
library(nycflights13)
library(tidyverse)
view(flights)
  # 1
  delay <- filter(flights, arr_delay >= 120)
  flewTo <- filter(flights, dest %in% c("IAH", "HOU"))
  carrier <- filter(flights, carrier %in% c("UA", "AA", "DL"))
  summerDep <- filter(flights, month %in% c(7, 8, 9))
  arriveAndDeparture <- filter(flights, dep_delay <= 0 & arr_delay > 120)
  madeUpFlight <- filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
  midnight <- filter(flights, dep_time <= 600 | dep_time == 2400)
  # 2 The expression `between(x, left, right)` is equivalent to `x >= left & x <= right`.
    # From the previous exercise we can rewrite summerDep as summerDep <- filter(flights, between(month, 7, 9))
  # 3
  missingDep <- filter(flights, is.na(dep_time))
    # The arrival time is also missing for these rows. Those might be cancelled flights.
  # 4
    # NA^0 is TRUE, because every number on the power of 0 equals 1, and 1 is often represented with TRUE in the programming languages
    # NA | TRUE is always TRUE, because every time we write a combination, including "| TRUE", we receive TRUE. It's only enough one of the valuables to be TRUE in order the whole statement to be true
    # Because every statement including "& FALSE" equals FALSE
    # The reason that `NA * 0 != 0` is that $0 \times \infty$ and $0 \times -\infty$ are undefined. R represents undefined results as `NaN`, which is an abbreviation of "[not a number]
# 5.3.1 Exercises 
  # 1 For example, to sort the data frame by departure time in ascending order but `NA` values first, we run arrange(flights, desc(is.na(dep_time)), dep_time)
  Since `desc(is.na(dep_time))` is either `TRUE` when `dep_time` is missing, or `FALSE`, when it is not, the rows with missing values of `dep_time` will come first, since `TRUE > FALSE`.
  # 2
  a <- arrange(flights, desc(dep_delay))
  b <- arrange(flights, dep_time)
  # 3 
  c <- arrange(flights, desc(distance * 60 / air_time))
  # 4
  d <- arrange(flights, desc(distance))
  e <- arrange(flights, distance)
# 5.4.1 Exercises 
  # 1
  select(flights, dep_time, dep_delay, arr_time, arr_delay)
  select(flights, 4, 6, 7, 9)
  select(flights, starts_with("dep_"), starts_with("arr_"))
  # 2 The function ignores dublication and doen't print an error
  # 3 It is especially useful for programming with selecting functions. 
  any_of() doesn't check for missing variables. It is especially useful with negative selections, when you would like to make sure a variable is removed.
  m <- c("year", "month", "day", "variable_not_in_the_dataframe")
  select(flights, any_of(m))
  # 4 Yes. `select()` consistent regardless of whether the table is stored as an R data frame or in a database. 
  To change the behavior, we need to  rewrite it as select(flights, contains("TIME", ignore.case = FALSE))
# 5.5.2 Exercises 
  # 1
  flights_times <- mutate(flights,
  dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
  sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
  sched_dep_time %% 100) %% 1440
  )
  #2
  I expect that `air_time = arr_time - dep_time`.
  flights_airtime <-
  mutate(flights,
    dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
    arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
    air_time_diff = air_time - arr_time + dep_time
  )
  It turns out that there are many flights for which `air_time != arr_time - dep_time`.
  I can think of two reasons why `air_time` would not equal `arr_time - dep_time`.
1.  The flight passes midnight, so `arr_time < dep_time`.
2.  The flight crosses time zones, and the total air time will be off by hours (multiples of 60). 
Since time-zones and crossing midnight only affects the hour part of the time, all values of `air_time_diff` should be divisible by 60.
I'll visually check this hypothesis by plotting the distribution of `air_time_diff`.
  ggplot(flights_airtime, aes(x = air_time_diff)) +
    geom_histogram(binwidth = 1)
  This is not the case.
  To fix these time-zone issues, I would want to convert all the times to a date-time to handle overnight flights, and from local time to a common time zone.
  The `tzone` column of `nycflights13::airports` gives the time-zone of each airport.
  But that still leaves the other differences unexplained. 
  Rereading the documentation for the table, it appears that the `air_time` variable refers to flight time, which is defined as the time between wheels-off (take-off) and wheels-in (landing).
  But the flight time does not include time spent on the runway taxiing to and from gates.
  Now I see that `air_time <= arr_time - dep_time`, supposing that the time zones of `arr_time` and `dep_time` are in the same time zone.
  # 3 
  dep_time - dep_delay == sched_dep_time
  flights_deptime <-
    mutate(flights,
           dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
           sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                   sched_dep_time %% 100) %% 1440,
           dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
    )
  filter(flights_deptime, dep_delay_diff != 0)
  `dep_delay_diff` doesn't equal zero for all rows. The discrepancies could be because a flight was scheduled to depart 
before midnight, but was delayed after midnight.
ggplot(
  filter(flights_deptime, dep_delay_diff > 0),
  aes(y = sched_dep_time_min, x = dep_delay_diff)
) +
  geom_point()
  The only cases in which the departure delay is not equal to the difference
in scheduled departure and actual departure times is due to a quirk in how these
columns were stored.

Honestly, I couldn't understand tast 2 and 3. I used the internet for guidance but still couldn't get it.
  # 4
  flights_delayed <- mutate(flights, 
                          dep_delay_min_rank = min_rank(desc(dep_delay))
  # 5
  It prints 2  4  6  5  7  9  8 10 12 11, which equals c(1 + 1, 2 + 2, 3 + 3, 1 + 4, 2 + 5, 3 + 6, 1 + 7, 2 + 8, 3 + 9, 1 + 10)
  Apparently it ads every number of the first vector to every number to the second vector, but because the first one is shorter
  it starts repeating the numbers and adding them to the rest of the numbers of the second vector
  When adding two vectors, R recycles the shorter vector's values to create a vector of the same length as the longer vector.
  The code also raises a warning that the shorter vector is not a multiple of the longer vector.
  # 6 `sin()`, `cos()`, `tan()`

  