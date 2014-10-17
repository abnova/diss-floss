df <- flossData

df$age <- df[["Development Team Size"]]

g <- ggplot(df, aes(x=age)) + 
  geom_histogram(aes(y=(log10(..count..+1))), binwidth=0.05) + 
  scale_x_continuous(trans="log", limits=c(1,99))