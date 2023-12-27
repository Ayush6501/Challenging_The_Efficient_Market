Strategy_A = c(as.numeric(df_stocks$Portfolio))
Strategy_B = c(as.numeric(df_stocks$Hold_Portfolio))

#checking the normality of the portfolios and their differences
normality_test_mean_reversion = shapiro.test(Strategy_A)
normality_test_hold = shapiro.test(Strategy_B)

# neither the 2 portfolios nor their differences have a normal distribution
print(normality_test_mean_reversion$p.value)
print(normality_test_hold$p.value)

#checking if data is continuous - numeric data is continuous as it can take any numeric value
print(is.numeric(Strategy_A))
print(is.numeric(Strategy_B))

#visualizing the difference between the two groups
ggplot(df_stocks, aes(x = index(df_stocks), y = (df_stocks$Portfolio-df_stocks$Hold_Portfolio))) +
  geom_line(size = 0.7, color = "black")+
  theme(panel.background = element_rect(fill = "wheat", color = "black")) +
  theme(text = element_text(size = 11)) +
  labs(title = "Difference Betwen two groups", x = "Date", y = "Difference") +
  theme(plot.title = element_text(hjust = 0.5))

# carrying out the Wilcoxon signed rank test
result_WSRT = wilcox.test(Strategy_A, Strategy_B, paired = TRUE, alternative = "two.sided")
print(result_WSRT)
print(result_WSRT$p.value)

#carrying out the Mann Whitney U / Wilcoxon rank-sum test
result_MWUT = wilcox.test(Strategy_A,Strategy_B)
print(result_MWUT)
print(result_MWUT$p.value)