library(tidyverse)
library(tidyquant)
library(lubridate)
library(dplyr)

data <- tidyquant::tq_get(c("FB", "SPY"),
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

data<-bind_cols(fin_data,fin_data1) %>%
  select(adjusted...3, adjusted...6) %>%
  mutate(ReturnAppl = (adjusted...3-lag(adjusted...3))/lag(adjusted...3)) %>%
  mutate(ReturnSPY = (adjusted...6-lag(adjusted...6))/lag(adjusted...6)) %>%
  filter(!is.na(ReturnAppl)) %>%
  select(ReturnAppl,ReturnSPY)

regression <- lm(ReturnAppl~ReturnSPY - 1, data=data)
summary(regression)
fin_data<-fin_data %>%
  mutate(SMA20 = SMA(adjusted, n=20),
         SD20...)



#pivot_wider like pivot tables in excell to put the prices below the name of the stock
#we can run the regression with it names from symbol values from adjusted