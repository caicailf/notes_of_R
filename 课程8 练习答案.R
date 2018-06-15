data("diamonds")
qplot(x=price,data=diamonds,fill=cut) +
  facet_wrap(~color) +
  scale_x_log10() +
  scale_fill_brewer(type = 'qual')

ggplot(aes(x=table,y=price),data=diamonds) +
  geom_point(aes(color = cut)) +
  scale_x_continuous(breaks = seq(50,80,2)) +
  scale_color_brewer(type = 'qual')

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x=volume,y=price),data=diamonds) +
  geom_point(aes(color=clarity)) +
  xlim(0,quantile(diamonds$volume,0.99)) +
  scale_y_log10() +
  scale_color_brewer(type = 'div')

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

ggplot(aes(x=tenure,y=prop_initiated),
       data=subset(pf,!is.na(pf$prop_initiated))) +
  geom_line(aes(color=year_joined.bucket),stat = 'summary',fun.y=median)


ggplot(aes(x=tenure,y=prop_initiated),
       data=subset(pf,!is.na(pf$prop_initiated))) +
  geom_smooth(aes(color=year_joined.bucket),stat = 'summary',fun.y=median) 

summary(subset(pf,pf$year_joined.bucket == '(2012,2014]'))


ggplot(aes(x=cut,y = price/carat),
       data=diamonds) +
  geom_jitter(aes(color=color),alpha = 1/2) +
  facet_wrap(~clarity) +
  scale_color_brewer(type = 'div')
