library(yfR)
library(ggplot2)
library(tidyverse)

df <- yf_get(tickers = 'PPC.JO', first_date = '2021-06-01', last_date = Sys.Date(), freq_data = 'monthly')
summary(df)
df_plot <- df %>% select(ref_date, ret_adjusted_prices)
ggplot(df_plot, aes(x = ref_date, y = ret_adjusted_prices))+
  geom_line()+
  labs(title = 'PPC Limited',
       subtitle = 'Price analysis',
       x = 'ref date',
       y = 'return adjusted price')