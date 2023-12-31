library(ggplot2)

d <- read.csv(file='input/data-rental.csv')

p <- ggplot(data=d, aes(x=Area, y=Y)) +
  theme_bw(base_size=18) +
  geom_point(shape=1, size=2) +
  scale_x_continuous(breaks=seq(from=20, to=120, by=20))
ggsave(p, file='output/fig7-1-left.png', dpi=300, w=4, h=3)

p <- ggplot(data=d, aes(x=Area, y=Y)) +
  theme_bw(base_size=18) +
  geom_point(shape=1, size=2) +
  scale_x_log10(breaks=c(1,2,5,10,20,50,100)) + scale_y_log10(breaks=c(10,20,50,100,200,500,1000,2000)) +
  coord_cartesian(xlim=c(9, 120)) 
ggsave(p, file='output/fig7-1-right.png', dpi=300, w=4, h=3)
