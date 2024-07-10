# 필요한 라이브러리 로드
library(ggplot2)
library(dplyr)


# 데이터 생성
data <- data.frame(
  name = c("아담", "셋", "에노스", "게난", "마할랄렐", "야렛", "에녹", "므두셀라", "라멕", "노아", "셈", "아르박삿", "셀라", "에벨", "벨렉", "르우", "스룩", "나홀", "데라", "아브라함"),
  birth = c(0, 130, 235, 325, 395, 460, 622, 687, 874, 1056, 1558, 1658, 1693, 1723, 1757, 1787, 1819, 1849, 1878, 2008),
  life = c(930, 912, 905, 910, 895, 962, 365, 969, 777, 950, 600, 438, 433, 464, 239, 239, 230, 148, 205, 175),
  group = c(rep("노아 이전", 10), rep("노아 이후", 10))
)

# 데이터 처리
data <- data %>%
  mutate(end = birth + life,
         label_position = birth + life / 2,
         name_life = paste0(name, "(", life, ")"))

# 그래프 생성
ggplot(data, aes(y = reorder(name, -birth))) +
  geom_segment(aes(x = birth, xend = end, yend = name, color = group), size = 5) +
  geom_text(aes(x = birth, label = birth), hjust = 1.2, size = 3) +
  geom_text(aes(x = label_position, label = name_life), vjust = 0.5, size = 3) +
  geom_text(aes(x = end, label = end), hjust = -0.2, size = 3) +
  geom_vline(xintercept = 1656, color = "red", linetype = "dashed") +
  geom_text(aes(x = 1656, y = 20, label = "대홍수"), angle = 90, vjust = -0.5, color = "red", size = 4) +
  scale_x_continuous(breaks = seq(0, 2200, by = 200), limits = c(-100, 2300)) +
  scale_color_manual(values = c("노아 이전" = "gray", "노아 이후" = "yellow2")) +
  labs(title = "성경 연대기: 아담부터 아브라함까지",
       x = "연도", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")