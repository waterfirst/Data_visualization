

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


########################

p <- ggplot(df1, aes(x, y, z= z)) +
  stat_contour(geom="polygon",aes(fill=stat(level))) +
  scale_fill_distiller(palette = "Spectral", direction = -1)
ggplotly(p)


p <- ggplot(df1, aes(x, y, z= z, colour=stat(level))) +
  geom_contour() +
  scale_colour_distiller(palette = "YlGn", direction = 1)

ggplotly(p)


# 여러개 파일 불러와서 2d 이미지 저장하기
temp <- list.files(path=path, pattern="*.csv")

files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)
file_names <- gsub(".csv", "", temp )

# 파일 이름별로 데이터 프레임 변수 생성
for (file in files) {
  # 파일 이름에서 확장자 제거
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # 데이터 프레임 변수 생성
  assign(file_name, read.csv(file, skip = 1, fileEncoding = "UTF-16", sep = ",", header = F)) 
  
}




for (i in 1:length(temp)) {
  df <- eval(parse(text=file_names[i]))
  df1 <- df %>% slice(-1) %>% select(-801)%>% 
    pivot_longer(cols = 1:800, names_to = "x", values_to = "z") %>% 
    mutate(y= rep(seq(1:800), 600),  x= rep(seq(1:600), each=800), z=(z-min(z))/1000)
  df1 %>% ggplot(aes(x=x, y=y, col=z))+geom_tile()+
    scale_colour_gradientn(colours=c("navy","blue", "green", "yellow", "orange", "red"))+
    labs(x="x", y="y", title=paste( "WSI-", file_names[i])) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  dev.copy(png,paste(file_names[i],".png"), width = dev.size("px")[1], height = dev.size("px")[2])
  dev.off()
  
}

