library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)
#Problem 1
# 1.1 SMA
data <- c(0, 2, 4, 6, 7, 3, 2, 9, 17, 14, 15, 0, 4, 3)
for (i in 1:length(data)){
  mean(data[i:(i+3)])}
  #I struggle only to store the results
# 1.2 cor
m <- c(41, 19, 23, 40, 55, 57, 33)
n <- c(94, 60, 74, 71, 82, 76, 61)
Nominator <- length(m)*(sum(m, n)-sum(m)*sum(n))
Denominator <- sqrt((length(m)*sum(m^2)-sum(m)^2)*(length(m)*sum(n^2)-sum(n)^2))
CorrelationCoef = Nominator/Denominator

#Problem 2
x = (1:100)
prime_numbers=c()
for (i in 2:100) {
  if (any(x == i)) {
    prime_numbers = c(prime_numbers, i)
    x = c(x[(x %% i) != 0], i)
  }
}
print(prime_numbers)
# I looked for this one in the interned. I got the logic behind it,
# but I couldn't understand what is exactly happening in x = c(x[(x %% i) != 0], i)
# I get the idea, but i don't understand the code itself here

#Problem 3
library(tidyquant)
#1-5
data <- tidyquant::tq_get("FB",
                          get = "stock.prices",
                          from = "2019-01-01",
                          to   = "2021-04-01")%>%
  dplyr::select(symbol, date, adjusted)

Dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                        to = ymd('2021-04-01'), 
                                        by = 'days'), 3),
                    Symbol = rep("FB", 822))

Join <- Dates %>%
  dplyr::left_join(data, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

JoinNew <- Join %>%
  mutate(EMA26 = EMA(adjusted, n = 26),
         EMA12 = EMA(adjusted, n = 12),
         MACD = EMA12 - EMA26,
         LagMACD = lag(MACD),
         SignalLine = EMA(MACD, 9),
         LagSignalLine = lag(SignalLine)) %>%
  ungroup() %>%
  filter(!is.na(LagSignalLine)) %>%
  mutate(Crossed = case_when(LagMACD > LagSignalLine & MACD < SignalLine ~ "crossed from above",
                             LagMACD < LagSignalLine & MACD > SignalLine ~ "crossed from below",
                             TRUE ~ "no cross"),
         Signal = case_when(Crossed == "crossed from above" ~ "sell",
                            Crossed == "crossed from below" ~ "buy",
                            TRUE ~ "do nothing"))
#6
StockPrices = c(rep(100, 10), rep(130, 10), rep(110, 10), rep(90, 10), rep(95, 10), rep(80, 10), rep(75, 10), rep(93, 10), rep(120, 10), rep(85, 10))
DatesAndPrices <- data.frame(Date = seq.Date(from = as.Date("2019-01-01"), 
                                   by = "days",
                                   length.out = 100), 
                   Symbol = "StockA",
                   Price = sample(StockPrices, 100))
Benchmark <- DatesAndPrices %>%
  mutate(Signal = case_when(Price > 100 ~ "sell",
                             Price < 100 ~ "buy",
                             TRUE ~ "hold"))
# I will use some example that was created with the code
# The first one was with prices 85, 75, 75, 75, 110
x <- c(85, 75, 75, 75)
4*110-(100+85+75+75+75)==30
# The result is 130
100*110/mean(x)==141.9355
#MACD beats the benchmark
#Second one 90, 95, 75, 100, 90, 85, 93, 75, 110
y <- c(100, 90, 95, 75, 90, 85, 93, 75)
8*110-sum(y) == 177
# Result 277
100*110/mean(y) == 125.1778
#The Benchmark beats
#Third one 100, 95, 80, 110
z <- c(95, 80)
3*100-sum(z) == 125
#Resukt 225
100*110/mean(z) == 125.7143
#Benchmark beats again

# I tried to do this with a while loop, but I didn't succeed. The Price stayed the same.
#Here is what I thought about:
Budget <- 100
while (Budget > Price) {
  StockPrices = c(rep(100, 10), rep(130, 10), rep(110, 10), rep(90, 10), rep(95, 10), rep(80, 10), rep(75, 10), rep(93, 10), rep(120, 10), rep(85, 10))
  Price = sample(StockPrices, 1)
  if (Budget > Price) {
    Bought = Budget
    Bought = Bought + Price
  } else {
    Bought = 0
  }
Results <- c(Results, Bought)
}

