library(readxl)
library(tidyverse)
library(writexl)
library(stringr)
library(gt)
library(showtext) # 한글
library(viridis) #특정 color 묶음
library(patchwork)
showtext_auto()

getwd()
paths <- c("D:/Private_Documents/!!2024/")


setwd("D:/Private_Documents/!!2024/")
paths <- getwd()

library(purrr)

file_1 <- structure(list(file = c("non_lcf_dms.xlsx"
), sheet = excel_sheets("non_lcf_dms.xlsx")))


non_lcf <-
  map2(file_1$file, file_1$sheet, ~ read_excel(path = .x, sheet = .y, range = "A7:IK408")) %>%
  list_rbind(names_to = "type") %>%
  mutate(type = rep(c(file_1$sheet), each = n()/length(file_1$sheet)))

non_lcf %>% rename(wavelength =  `Wavelength /nm`) %>%
  mutate(type = str_remove(type, "raw_spectrum_")) %>%
  separate(type,into=c("wad", "direction"),
           sep="_", convert = TRUE, extra = "merge") %>%
  rename_with(~ tolower(gsub(", P=0°...", "_", .x, fixed = TRUE))) -> non_lcf2

x <- c(0:60)
y <- c("_white", "_red", "_green", "_blue")
name <- do.call(paste0, expand.grid(x,y))

colnames(non_lcf2)[4:247] <- name

df2 <- non_lcf2 %>%
  pivot_longer(cols = c(4:247), names_to = "color", values_to = "tr") %>%
  separate(color, into=c("angle", "color"), sep="_") %>%
  mutate(angle = as.numeric(angle)) %>%
  filter(color =="green") %>%
  filter(direction %in% c("L")) %>%
  filter(angle ==60) %>%
  filter(angle %in% seq(0, 60, by=10)) %>%
  filter(wavelength %in% seq(500, 600, by=1)) %>%
  filter(wad == "uWAD2")


