
rm(list=ls())
dev.off()

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(data.table) #for loading and mapping data
library(ggplot2)  #helps visualize data

#getwd() #displays working directory
# Input data files are available in the read-only "../input/" directory
setwd("D:/Non_Documents/AI/R/data/자전거") #sets your working directory to simplify data load 

#여러개의 csv 파일 한번에 읽어오기
dir()
alltrips <- list.files(pattern = "\\.csv$") %>% map_df(~read_csv(.))

str(alltrips)
head(alltrips)
tail(alltrips)
glimpse(alltrips)
summary(alltrips)


## 행/열 수, 결측치, 멤버쉽 종류 (케주얼/멤버)

cat("#1 Total rows: ", nrow(alltrips)) #1

paste("#2 Columns with NAs")
colSums(is.na(alltrips)) #2

paste("#3 number of categories per membership") #3
unique(alltrips$member_casual)

#날짜/시간 처리
alltrips$date <- as.Date(alltrips$started_at) #The default format is yyyy-mm-dd
alltrips$month <- as.numeric(format(as.Date(alltrips$date), "%m"))
alltrips$day <- as.numeric(format(as.Date(alltrips$date), "%d"))
alltrips$year <- as.numeric(format(as.Date(alltrips$date), "%Y"))
alltrips$ride_hour <- as.numeric(format(alltrips$started_at, "%H"))
alltrips$day_of_week <- format(as.Date(alltrips$date), "%A")
alltrips$week_day <- wday(alltrips$date) 

#자전거를 탄 시간 계산 (초)
alltrips$ride_length <- difftime(alltrips$ended_at,alltrips$started_at)


glimpse(alltrips)


## 자전거를 탄 시간이 음수인 행 갯수
paste("Bikes with negative ride length:", nrow(filter(alltrips, ride_length < 0)))

## 출발지가 HQ_QR인 곳 없음
paste("Station HQ QR records:", nrow(filter(alltrips, start_station_name=="HQ QR")))

## 자전거 탄 시간이 양수인 것만 다시 변수에 넣음음
goodtrips <- alltrips %>% filter(alltrips$ride_length > 0)
rm(alltrips) #free up some memory - delete unused dataframe



## 자전거 탄 시간을 초 -> 분으로 바꿈
goodtrips$trip_duration_minutes <- round(as.numeric(goodtrips$ride_length/60), digits=3)
## 주중을 다시 표시  Create column to identify business days
goodtrips$weekdays <- wday(goodtrips$date)

# 년-월만 표시 A column to work with months in sequence
goodtrips$yr_month <- paste(goodtrips$year, sprintf("%02d", goodtrips$month), sep="-")

## 자전거 탄 시간 통계
summary(goodtrips$trip_duration_minutes)
### 5.8~18분 사이에 50%가 몰려있음, 평균 10분 자전거 탐, 평균이 19분인데 이상점때문에 높음


#return to work directory (need RW permissions)
setwd("~")
getwd()
dir()
# 자전거 탄 시간 표준편차
std_dist <- sd(goodtrips$trip_duration_minutes)

# 이상점 제거 remove outliers from data for chart
charting <- goodtrips %>% filter(trip_duration_minutes > -(std_dist) & trip_duration_minutes < std_dist)
# create chart
hist(charting$trip_duration_minutes, main = "Overall utilization", breaks=50)
## 2시간 초과는 이상치로 보임

# 멤버쉽 종류별 히스토그램 그리기
ggplot(charting) +
  geom_histogram(aes(x=trip_duration_minutes, color=member_casual)) +
  facet_wrap(~member_casual)


# 시간에 따라 사용자 수는 증가하는가?
#filter only members and summarize it, then plot
goodtrips %>% filter(member_casual=="member") %>% 
  group_by(yr_month, member_casual) %>% summarize(members_count=n()) %>%
  ggplot(aes(yr_month, members_count)) +
  labs(title = "Member subscriptions in 12 months",
       x="Month",
       y="Members") +
  geom_col(fill="red") +
  theme(axis.text.x = element_text(angle = 90))

## 여름엔 증가하고, 겨울엔 감소하는 듯 (자전거니)


## 멤버쉽별 거래 건수 / 비율
deals <- goodtrips %>% count(member_casual, name="transactions")
deals$as_pct <- deals$transactions / sum(deals$transactions) * 100
deals %>% gt()

## 시간대별 히스토그램
ggplot(charting, aes(x=ride_hour, fill=member_casual)) +
  geom_histogram(position="identity", alpha=0.4, bins=24) +
  labs(
    title="Peak demand hours by membership",
    x="Hour",
    y="Rides")


## 변수 제거
rm(charting) #done with this
rm(std_dist) #done also

## 멤버
volume <- aggregate(goodtrips$trip_duration_minutes ~ goodtrips$member_casual, FUN = sum)
volume$as_pct <- volume$`goodtrips$trip_duration_minutes` / sum(volume$`goodtrips$trip_duration_minutes`) *100
volume

## 주중/주말 멤버쉽별 평균 자전거 탄 시간
goodtrips %>% 
  group_by(member_casual) %>%
  summarise(week_use = mean(trip_duration_minutes[weekdays<6]),
            weekend_use = mean(trip_duration_minutes[weekdays>5]))

## 주중/주말 멤버쉽별 전체 자전거 탄 시간
trips <- goodtrips %>% group_by(member_casual) %>%
  summarize(workdays_use=sum(trip_duration_minutes[weekdays<6]), 
            weekend_use=sum(trip_duration_minutes[weekdays>5])) 
trips
#done with this
rm(deals)
rm(trips)
rm(volume)



# Ridership data by membership type and weekday -----
biz_simulation <- goodtrips %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and day_of_week
  summarise(number_of_rides = n(), volume = sum(trip_duration_minutes))
# 1- Business value at flat rate of US$2/10min ($0.2/min)? -----
biz_simulation$biz_value <- biz_simulation$volume * 0.1
# 2- Updated value with membership incentive of 20%? -----
rtio <- data.frame(ratio=rep(c(1,1.2), each=nrow(biz_simulation)/2), 
                   member_casual=rep(c("member", "casual"), each=nrow(biz_simulation)/2))
#3 - combining it all
upd_simulation <- biz_simulation %>% inner_join(rtio)
upd_simulation$upd_value <- upd_simulation$biz_value * upd_simulation$ratio
#4 - show final comparison with updated figures
upd_simulation %>% 
  group_by(member_casual) %>%  #groups by usertype and day_of_week
  summarise(base_fee=sum(biz_value),
            casual_fee = sum(upd_value),
            added_value = casual_fee - base_fee)
#housekeeping
rm(rtio) #done
rm(biz_simulation) 
rm(upd_simulation)
