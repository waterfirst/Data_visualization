

rm(list=ls())

options(warn=-1) # 경고메세지 무시하기
# readr 패키지 로드
library(readr)
library(tidyverse)
library(data.table) #for loading and mapping data



df3 <- df2 %>%  pivot_longer(cols = 1:40, names_to = "x", values_to = "z") %>%
  mutate(y= rep(seq(1:40), 30),  x= rep(seq(1:30), each=40), z=(z-min(z))/10000)


p1 <- df3 %>% ggplot(aes(x=x, y=y, col=z))+geom_tile()+
  scale_colour_gradientn(colours=c("navy","blue", "green", "yellow", "orange", "red"))+
  labs(x="x", y="y", title=paste( "WSI-", file_names[1])) +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
p1


ggsave("sample.png",dpi=300, width=8, height=6, p)
dev.copy(png,"plot.png", width = dev.size("px")[1], height = dev.size("px")[2])
dev.off()
