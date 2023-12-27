#settings an initial investment of $10000
initial_investment = 10000

#computing the daily portfolio value for our initial investment
df_stocks$Portfolio = df_stocks$Cumulative.Returns*initial_investment

#plotting our portfolio
ggplot(df_stocks, aes(x = index(df_stocks), y = Portfolio)) +
  geom_line(size = 0.7, color = "black")+
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  theme(text = element_text(size = 11)) +
  labs(title = "Portfolio Value", x = "Date", y = "Price") +
  theme(plot.title = element_text(hjust = 0.5))