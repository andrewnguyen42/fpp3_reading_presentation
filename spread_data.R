library(tidyr)
library(purrr)
library(broom)
library(feasts)

quantmod::getSymbols.FRED(auto.assign=FALSE)

tickers <- c('BAMLC0A1CAAA','BAMLC0A2CAA','BAMLC0A3CA','BAMLC0A4CBBB', 'BAMLH0A1HYBB', 'BAMLH0A2HYB', 'BAMLH0A3HYC')

corporate_spreads <- map(tickers, .id = 'rating', quantmod::getSymbols.FRED, auto.assign=FALSE) %>%
  map_dfr(tidy)

spread_ts <- corporate_spreads %>%
  pivot_wider(names_from = series, values_from = value) %>%
  rename(AAA = BAMLC0A1CAAA
         , AA = BAMLC0A2CAA
         , A = BAMLC0A3CA
         , BBB = BAMLC0A4CBBB
         , BB = BAMLH0A1HYBB
         , B = BAMLH0A2HYB
         , CCC = BAMLH0A3HYC) %>%
  mutate(date = index, .keep = 'unused') %>%
  pivot_longer(-date, names_to = 'rating') %>%
  mutate(spread = value * 100, .keep = 'unused' #yield to bps
         , spread_diff = spread - lag(spread)
         , rating = factor(rating, levels = c('AAA', 'AA', 'A', 'BBB', 'BB', 'B', 'CCC'))) %>% 
  filter(!is.na(spread_diff)) %>% 
  tsibble(key = rating)

feasts::autoplot(spread_ts, spread)

spread_ts %>%
  features(spread, features = list(mean = mean, sd = sd, quantile))

spread_ts %>% features(spread, feat_acf)
spread_ts %>% features(spread, feat_pacf)

