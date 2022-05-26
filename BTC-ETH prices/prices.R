library(tidyverse)
library(lubridate)

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
data <- btcprice %>% 
  left_join(ethprice, by = "DATES")


#Data with variation: ### ROTO ###
data_variation <- data %>%
  arrange(DATES) %>% 
  mutate(BTC_VAR = round((btcprice/lag(btcprice)-1)*100,1),
         ETH_VAR = round((ethprice/lag(ethprice)-1)*100,1))

data_variation[is.na(data_variation)] <- 0


#Plot:
ggplot(data, aes(DATES)) + 
  geom_line(aes(y = btcprice, colour = "btcprice")) + 
  geom_line(aes(y = ethprice, colour = "ethprice")) +
  ylab("USD Value") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

