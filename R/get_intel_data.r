library('TTR')
library('quantmod')
library('ggplot2')
library('zoo')
library('dplyr')

#get intel stock data from yahoo finance
df_stocks = getSymbols('INTC',from='2018-01-01',to='2023-11-27',src='yahoo',
                       auto.assign=FALSE)

str(df_stocks)

# Plotting the closing value of intel stock for each day
ggplot(df_stocks, aes(x = index(df_stocks), y = INTC.Close)) +
  geom_line(size = 0.7) +
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  labs(title = "Intel Stock Close Prices", x = "Date", y = "Close") +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5))


# Calculate rolling mean and rolling standard deviation
df_stocks$RollingMean = rollapply(df_stocks$INTC.Close,
                                  width = 30, FUN = mean, fill = NA, align = "right")
df_stocks$RollingStd = rollapply(df_stocks$INTC.Close,
                                 width = 30, FUN = sd, fill = NA, align = "right")


#plot the closing price and rolling mean for each day
ggplot(df_stocks, aes(x = index(df_stocks), y = INTC.Close)) +
  geom_line(size = 0.7) +
  geom_line(aes(y = RollingMean), color = "blue", size = 0.7) +
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  theme(text = element_text(size = 11)) +
  labs(title = "Intel Stock Price", x = "Date", y = "Price") +
  theme(plot.title = element_text(hjust = 0.5))

#compute the Zscore
df_stocks$ZScore = (df_stocks$INTC.Close - 
                      df_stocks$RollingMean)/df_stocks$RollingStd

#generate the buy/sell signals
#if the Zscore is less than -1 -  buy the stocks
#if the Zscire is more than 1 - sell the stocks
thresh = 1
df_stocks$Signal = 0  # 0 indicates no action
df_stocks$Signal[df_stocks$ZScore > thresh] = -1  # Buy signal
df_stocks$Signal[df_stocks$ZScore < -thresh] = 1  # Sell signal

#compute daily returns i.e. the percentage change in the closing value of the stocks
df_stocks$Daily.Returns = diff(df_stocks$INTC.Close, 
                               lag = 1) / lag(df_stocks$INTC.Close, 1)

#compute strategic returns
df_stocks$Strategic.Returns = lag(df_stocks$Signal, 1)*df_stocks$Daily.Returns

#cumulative product of Strategic returns
df_stocks$Strategic.Returns[is.na(df_stocks$Strategic.Returns)] = 0
df_stocks$Cumulative.Returns = cumprod(1+df_stocks$Strategic.Returns)

#plot the buy and sell indicators
ggplot(df_stocks, aes(x = index(df_stocks), y = INTC.Close)) +
  geom_line(size = 0.7, color = "black") +
  geom_line(aes(y = RollingMean), color = "blue", size = 0.7) +
  geom_point(data = df_stocks[df_stocks$Signal == 1,], aes(x = index(df_stocks[df_stocks$Signal == 1,]), y = INTC.Close), shape = 17, color = "green", size = 1.2) +
  geom_point(data = df_stocks[df_stocks$Signal == -1,], aes(x = index(df_stocks[df_stocks$Signal == -1,]), y = INTC.Close), shape = 17, color = "red", size = 1.2) +
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  theme(text = element_text(size = 12)) +
  labs(title = "Buy / Sell Indicators", x = "Date", y = "Price") +
  theme(plot.title = element_text(hjust = 0.5))