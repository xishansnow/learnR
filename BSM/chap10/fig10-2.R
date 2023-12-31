library(ggplot2)

d <- read.csv(file='input/data-mix1.csv')
dens <- density(d$Y)

bw <- 0.5
p <- ggplot(data=d, aes(x=Y)) +
  theme_bw(base_size=18) +
  geom_histogram(binwidth=bw, colour='black', fill='white') +
  geom_density(eval(bquote(aes(y=..count..*.(bw)))), alpha=0.35, colour=NA, fill='gray20') +
  geom_rug(sides='b') +
  scale_y_continuous(breaks=seq(from=0, to=10, by=2)) +
  labs(x='Y') + xlim(range(dens$x))
ggsave(file='output/fig10-2.png', plot=p, dpi=300, w=4, h=3)
