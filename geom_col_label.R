
rm(list=ls())

df <- tibble::tribble(
  ~angle,  ~`4.3`,  ~`3.8`,  ~`3.3`,  ~`2.8`,  ~`2.3`,  ~`1.8`,  ~`1.3`,
  0L,   0.999,   0.999,       1,       1,       1,       1,       1,
  5L,       1,       1,   0.999,   0.988,   0.963,   0.923,    0.88,
  10L,    0.91,   0.866,   0.821,   0.774,    0.73,   0.685,    0.64,
  15L,   0.668,   0.621,   0.577,   0.533,    0.49,   0.449,   0.407,
  20L,   0.424,   0.382,   0.339,   0.294,   0.252,   0.207,   0.162,
  25L,   0.182,   0.139,   0.096,   0.056,   0.028,   0.014,   0.011,
  30L,   0.011,    0.01,    0.01,   0.009,   0.009,    0.01,   0.009,
  35L,   0.008,   0.008,   0.008,   0.008,   0.008,   0.008,   0.008,
  40L,   0.007,   0.007,   0.007,   0.007,   0.007,   0.007,   0.007,
  45L,   0.006,   0.006,   0.005,   0.006,   0.005,   0.003,   0.002,
  50L,   0.005,   0.005,   0.004,   0.003,   0.002,   0.001,   0.001,
  55L,   0.006,   0.003,   0.002,   0.001,   0.001,   0.001,       0,
  60L,   0.005,   0.002,   0.001,   0.001,   0.001,       0,       0,
  65L,   0.004,   0.003,   0.001,   0.001,       0,       0,       0,
  70L,   0.003,   0.002,   0.002,       0,       0,       0,       0
)

library(tidyverse)

df %>% pivot_longer(-1, names_to = "BM_to_PDL", values_to = "cut_off") %>% 
  
  ggplot(aes(x=angle, y=cut_off, col=BM_to_PDL))+
  geom_point()+geom_smooth(se=F, method = "gam")+
  theme_bw()

df %>% pivot_longer(-1, names_to = "BM_to_PDL", values_to = "cut_off") %>% 
  mutate(BM_to_PDL = as.numeric(BM_to_PDL)) %>% 
  filter(angle %in% c(0, 10, 15)) %>% 
  mutate(angle = as.factor(angle)) %>% 
  ggplot(aes(x=BM_to_PDL, y=cut_off*100, label=cut_off*100, fill=angle))+
  geom_col(position="dodge")+
  geom_label(position= position_dodge(0.4))+
  theme_bw()+
  labs(y="휘도 상대비(%)", x="BM~PDL 이격거리")



df %>% pivot_longer(-1, names_to = "BM_to_PDL", values_to = "cut_off") %>% 
  mutate(BM_to_PDL = as.numeric(BM_to_PDL)) %>% 
  filter(angle %in% c(0, 10, 15)) %>% 
  mutate(angle = as.factor(angle)) %>% 
  ggplot(aes(x=BM_to_PDL, y=cut_off*100, label=cut_off*100, fill=angle))+
  geom_col(position="dodge")+
  
  geom_text(aes(label = cut_off*100, y=cut_off*100+3), position = position_dodge(0.5))+
  theme_bw()+
  labs(y="휘도 상대비(%)", x="BM~PDL 이격거리")


df %>% pivot_longer(-1, names_to = "BM_to_PDL", values_to = "cut_off") %>% 
  mutate(BM_to_PDL = as.numeric(BM_to_PDL)) %>% 
  filter(angle %in% c(0, 10, 15)) %>% 
  mutate(angle = as.factor(angle)) %>% 
  pivot_wider(names_from = angle, values_from = cut_off) %>% 
  mutate(across(c(2:4), ~.x/.x[4]*100)) %>% 
  pivot_longer(-1, names_to = "angle", values_to = "cut_off") %>% 
  mutate(angle = as.factor(angle)) %>% 
  
  ggplot(aes(x=BM_to_PDL, y=cut_off, label=cut_off, col=angle,fill=angle))+
  geom_col(position="dodge")+
  #geom_point()+
  geom_text(aes(label = round(cut_off,1), y=cut_off+3), position = position_dodge(0.5))+
  theme_bw()+
  labs(y="휘도 상대비(%)", x="BM~PDL 이격거리")
