data("diamonds")
head(diamonds)

ggplot(aes(x=x,y=price),data=diamonds) +
  geom_point()

cor.test(diamonds$x,diamonds$price)
cor.test(diamonds$y,diamonds$price)
cor.test(diamonds$z,diamonds$price)

ggplot(aes(x=depth,y=price),data=diamonds) +
  geom_point()

ggplot(aes(x=depth,y=price),data=diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(43,79,2))

cor.test(diamonds$depth,diamonds$price)

ggplot(aes(x=carat,y=price),data=diamonds) +
  geom_point() +
  xlim(0,quantile(diamonds$carat,0.99)) +
  ylim(0,quantile(diamonds$price,0.99))

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x=volume,y=price),data=diamonds) +
  geom_point()
library(plyr)
count(diamonds$volume == 0)



with(subset(diamonds,volume > 0 & volume < 800),cor.test(volume,price))

ggplot(aes(x=volume,y=price),data=subset(diamonds,volume > 0 & volume < 800)) +
  geom_point(alpha = 1/100) +
  geom_smooth(method = 'lm')

library(dplyr)

diamondsByClarity <- group_by(diamonds,clarity)
diamonds_By_Clarity <- summarise(diamondsByClarity,
                                 mean_price = mean(price),
                                 median_price = median(price),
                                 min_price = min(price),
                                 max_price = max(price),
                                 n = n())
head(diamonds_By_Clarity,8)

diamondsByColor <- group_by(diamonds,color)
diamondsByColor <- summarise(diamondsByColor,
                             mean_price = mean(price),
                             median_price = median(price),
                             min_price = min(price),
                             max_price = max(price),
                             n = n())

p1 <- ggplot(data = diamonds_By_Clarity,aes(x=clarity,y=mean_price)) + geom_col()

p2 <- ggplot(data = diamondsByColor,aes(x=color,y=mean_price)) + geom_col()

library(gridExtra)
grid.arrange(p2,p1)

