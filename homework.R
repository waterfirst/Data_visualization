library(tidyverse)
library(gapminder)
library(datatoys)


# 1. 2007년 대륙별 나라수는 몇 나라인가?

str(gapminder)

max(gapminder$year)

gapminder |> filter(year == 2007) |>  group_by(continent) |>  summarise(n= n())


# 2. 가장 최근 연도에서 인구수가 많은 상위 10개 나라를 뽑아서 나라별 인구수와 기대 수명을 구하라.
#(이때 인구수는 13.2억명, 기대수명은 73세로 단위를 맞추어라. )

gapminder |> filter(year == 2007) |> arrange(-pop) |> slice(1:10) |> 
  group_by(country) |> summarise(인구수_억명 = round(pop/100000000,1), 기대수명_세 = round(lifeExp) ) |> 
  arrange(-인구수_억명)

gapminder |> 
  filter(country != "Kuwait") |> 
  ggplot(aes(x=year, y= gdpPercap, col= country))+
  geom_line()+
  theme(legend.position = "none")+
  facet_wrap(~continent,)


# 
# 3. 연도별 기대수명이 가장 빠르게 증가하는 나라는 어디인가?
gapminder |>  select(country, year, lifeExp) |> filter(year %in% c(1952, 2007)) |> 
  pivot_wider(names_from = year,  values_from = lifeExp) |> 
  mutate(ratio = (`2007`- `1952`)/(2007-1952)) |> arrange(-ratio) 

diff(1:10, 2)

x <- cumsum(cumsum(1:10))
diff(x, lag = 2)
#   
# 4. 2000년도 대륙별 1인당 gpd의 평균과 표준편차는 어떻게 되는가?
gapminder |> filter(year == 2002) |> group_by(continent) |> 
  summarise(avg = mean(gdpPercap, na.rm=T), `σ`= sd(gdpPercap, na.rm=T))

#   
# 5. 기대수명 데이터를 표준화(평균 0, 표준편차 1) 하라. 

nor_sd = function(x){
  result = (x - mean(x)) / sd(x)
  return(result)
}


gapminder |> mutate(life_nor = nor_sd(lifeExp) ) |> 

  ggplot(aes(x=year, y= life_nor, col= country))+
  geom_line()+
  theme(legend.position = "none")+
  facet_wrap(~continent,)


# 
# 6. 1인당 gpd 데이터를 정규화(1과 0 사이로 만듦) 하라


nor_minmax = function(x){
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
}

gapminder |> 
  
  filter(country != "Kuwait") |> 
  mutate(gdp_sd = nor_minmax(gdpPercap) ) |> 
 
  ggplot(aes(x=year, y= gdp_sd, col= country))+
  geom_line()+
  theme(legend.position = "none")+
  facet_wrap(~continent,)

