library(tidyverse)
library(gapminder)

# 1. 대륙별 나라수는 몇 나라인가?

str(gapminder)

max(gapminder$year)

gapminder |> filter(year == 2007) |>  group_by(continent) |>  summarise(n= n())


# 2. 인구수가 많은 상위 10개 나라를 뽑아서 가장 최근 연도에서 나라별 인구수와 기대 수명을 구하라.


# 
# 3. 연도별 기대수명이 가장 빠르게 증가하는 나라는 어디인가?
#   
# 4. 2000년도 대륙별 1인당 gpd의 평균과 표준편차는 어떻게 되는가?
#   
# 5. 인구수 데이터를 표준화(평균 0, 표준편차 1) 하라. 
# 
# 6. 1인당 gpd 데이터를 정규화(1과 0 사이로 만듦) 하라


