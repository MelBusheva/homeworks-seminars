library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

#Problem 1#
data <- tidyquant::tq_get(c("FB", "AMZN", "NFLX", "AAPL", "GOOG", "SPY"),
                          get = "stock.prices",
                          from = "2019-01-01",
                          to   = "2021-04-01")%>%
  dplyr::select(symbol, date, adjusted)


Dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                        to = ymd('2021-04-01'), 
                                        by = 'days'), 3),
                    Symbol = c(rep("FB", 822), rep("AMZN", 822), rep("NFLX", 822), rep("AAPL", 822), rep("GOOG", 822), rep("SPY", 822)))

Join <- Dates %>%
  dplyr::left_join(data, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

table <- Join %>%
  pivot_wider(names_from = "Symbol",
              values_from = "adjusted")

data2<-bind_cols(table) %>%
  mutate(ReturnFB = (FB-lag(FB))/lag(FB),
         ReturnAMZN = (AMZN-lag(AMZN))/lag(AMZN),
         ReturnNFLX = (NFLX-lag(NFLX))/lag(NFLX),
         ReturnAAPL = (AAPL-lag(AAPL))/lag(AAPL),
         ReturnGOOG = (GOOG-lag(GOOG))/lag(GOOG)) %>%
  mutate(ReturnSPY = (SPY-lag(SPY))/lag(SPY)) %>%
  filter(!is.na(ReturnFB), !is.na(ReturnAMZN), !is.na(ReturnNFLX), !is.na(ReturnAAPL), !is.na(ReturnGOOG)) %>%
  select(ReturnFB, ReturnAMZN, ReturnNFLX, ReturnAAPL, ReturnGOOG,ReturnSPY)

fitFB <- lm(data2$ReturnFB~data2$ReturnSPY)
resultFB <- summary(fitFB)
fitAMZN <- lm(data2$ReturnAMZN~data2$ReturnSPY)
resultAMZN <- summary(fitAMZN)
fitNFLX <- lm(data2$ReturnNFLX~data2$ReturnSPY)
resultNFLX <- summary(fitNFLX)
fitAAPL <- lm(data2$ReturnAAPL~data2$ReturnSPY)
resultAAPL <- summary(fitAAPL)
fitGOOG <- lm(data2$ReturnGOOG~data2$ReturnSPY)
resultGOOG <- summary(fitGOOG)

data3 <- data2 %>%
  mutate(βFB = resultFB$coefficients[2,1],
         βAMZN = resultAMZN$coefficients[2,1],
         βNFLX = resultNFLX$coefficients[2,1],
         βAAPL = resultAAPL$coefficients[2,1],
         βGOOG = resultGOOG$coefficients[2,1]) %>%
  mutate(αFB = ReturnFB-0.0304-βFB*(ReturnSPY-0.0304),
         αAMZN = ReturnAMZN-0.0304-βAMZN*(ReturnSPY-0.0304),
         αNFLX = ReturnNFLX-0.0304-βNFLX*(ReturnSPY-0.0304),
         αAAPL = ReturnAAPL-0.0304-βAAPL*(ReturnSPY-0.0304),
         αGOOG = ReturnGOOG-0.0304-βGOOG*(ReturnSPY-0.0304))
#I took the return rate of the treasury bill on 9.06 from this site:
# https://ycharts.com/indicators/10_year_treasury_rate
#Problem 1#
