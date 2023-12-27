#computing the hold position
vol =as.numeric(initial_investment/df_stocks$INTC.Close[1])
df_stocks$Hold_Portfolio = df_stocks$INTC.Close*vol

#comparing the mean reversion portfolio vs hold portfolio
ggplot(df_stocks, aes(x = index(df_stocks), y = Portfolio)) +
  geom_line(size = 0.7, color = "black") +
  geom_line(aes(y = Hold_Portfolio), color = "red", size = 0.7) +
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  theme(text = element_text(size = 11)) +
  labs(title = "Mean Reversion vs Long Term Investment", x = "Date", y = "Price") +
  theme(plot.title = element_text(hjust = 0.5))