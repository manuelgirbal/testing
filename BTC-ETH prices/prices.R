library(tidyverse)
library(lubridate)
library(plotly)

#ETH and BTC daily price (from https://coinmarketcap.com/):
btcprice <- read.csv("BTC-ETH prices/btc-usd-max.csv")
ethprice <- read.csv("BTC-ETH prices/eth-usd-max.csv")

btcprice <- as_tibble(btcprice) %>%
  mutate(DATES = as_date(snapped_at)) %>% 
  group_by(DATES) %>% 
  mutate(btcprice = round(mean(price),0)) %>% 
  select(DATES, btcprice)

ethprice <- as_tibble(ethprice) %>%
  mutate(DATES = as_date(snapped_at)) %>% 
  group_by(DATES) %>% 
  mutate(ethprice = round(mean(price),0)) %>% 
  select(DATES, ethprice)            


#Merging tables:
DATES <- btcprice$DATES
data <- tibble(DATES)

data <- data %>%
  left_join(btcprice, by = "DATES") %>% 
  left_join(ethprice, by = "DATES")

data[is.na(data)] <- 0

#Computing daily variation:
data_variation <- data %>%
  arrange(DATES) %>% 
  mutate(BTC_VAR = round((btcprice/lag(btcprice)-1)*100,1),
         ETH_VAR = round((ethprice/lag(ethprice)-1)*100,1))


is.na(data_variation) <- sapply(data_variation, is.infinite)
data_variation[is.na(data_variation)]<-0


#Plot 1:
ggplot(data, aes(DATES)) + 
  geom_line(aes(y = btcprice, colour = "btcprice")) + 
  geom_line(aes(y = ethprice, colour = "ethprice")) +
  ylab("USD Value") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


#Plot 2:

plot_ly(data_variation, x = ~DATES, y = ~BTC_VAR, type = 'scatter', mode = 'lines') %>%
  layout(title = "",
         yaxis = list(title = "Daily Variation" ,
                      zeroline = FALSE))

plot_ly(data_variation, x = ~DATES, y = ~ETH_VAR, type = 'scatter', mode = 'lines') %>%
  layout(title = "",
         yaxis = list(title = "Daily Variation" ,
                      zeroline = FALSE))

