library(tidyverse)
library(plotly)
library(gapminder)
library(crosstalk)
library(leaflet)
library(flipbookr)

gapminder %>%
  ggplot(aes(x=year,
             y=lifeExp,
             group=country)) +
  geom_line()


p <- gapminder %>%
  ggplot(aes(x=year,
             y=lifeExp,
             group=country)) +
  geom_line()

ggplotly(p)



p <- gapminder %>%
  ggplot(aes(
    x = year,
    y = lifeExp,
    group = country,
    text = paste0("대륙: ", continent, "\n",
                  "국가: ", country))) +
  geom_line()

ggplotly(p, tooltip = "text")



gm_highlight <- highlight_key(gapminder,
                              ~country)

life_g <- gm_highlight %>%
  ggplot(aes(
    x=year,
    y=lifeExp,
    group=country,
    text=paste0("대륙: ", continent, "\n",
                "국가: ", country))) +
  geom_line()

life_gg <- ggplotly(life_g, tooltip = "text")
highlight(life_gg, on = "plotly_click",
          selectize = TRUE,
          dynamic = TRUE,
          persistent = TRUE)



gapminder_cjk <- gapminder %>%
  filter(country %in% c("China", "Japan",
                        "Korea, Rep."))

gapminder_sd <- SharedData$new(gapminder_cjk,
                               ~country)

life_g <- gapminder_sd %>%
  ggplot(aes(
    x = year,
    y = lifeExp,
    group = country,
    text = paste0("대륙: ", continent, "\n",
                  "국가: ", country))) +
  geom_line() +
  geom_point() +
  facet_wrap(~country)

ggplotly(life_g, tooltip = "text")




life_g <- gapminder %>%
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp,
    group = country, color=country,
    text=paste0("대륙: ", continent, "\n",
                "국가: ", country))) +
  geom_point(alpha = 0.2) +
  geom_point() +
  facet_wrap(~continent, ncol = 2) +
  scale_x_sqrt() +
  theme(legend.position = "none")

ggplotly(life_g, tooltip="text")



life_g <- gapminder %>%
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp,
    group = country, color=country,
    text=paste0("대륙: ", continent, "\n",
                "국가: ", country))) +
  geom_point(alpha = 0.2) +
  geom_point(aes(frame = year), color="red") +
  facet_wrap(~continent, ncol = 2) +
  scale_x_sqrt() +
  theme(legend.position = "none")

ggplotly(life_g, tooltip="text")




gapminder %>% filter( country %in% c("Korea, Rep.","Korea, Dem. Rep.", "China", "United States", "Japan")) %>%
  ggplot(aes(x=year, y=lifeExp, group = country, col=country))+
geom_line() +  geom_point(size=1.5) +
  # scale_x_date(date_breaks="1 week", date_labels="%m-%d") +
  # scale_y_continuous(labels=scales::percent) +
  theme_bw(base_family="NanumGothic") +

  labs(x="", y="기대수명", color="") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1))



gapminder %>% filter( country %in% c("Korea, Rep.","Korea, Dem. Rep.", "China", "United States", "Japan")) %>%
  ggplot(aes(x=year, y=lifeExp, group = country, col=country))+
  geom_line() +  geom_point(size=1.5) +
  # scale_x_date(date_breaks="1 week", date_labels="%m-%d") +
  # scale_y_continuous(labels=scales::percent) +
  theme_bw(base_family="NanumGothic") +

  labs(x="", y="기대수명", color="") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~country, scale="free")



library(gganimate)

life <- gapminder %>% filter( country %in% c("Korea, Rep.","Korea, Dem. Rep.", "China", "United States", "Japan")) %>%
  ggplot(aes(x=year, y=lifeExp, group = country, col=country))+
  geom_line(alpha=0.3, linewidth=1.5) +
  geom_point(aes(frame = `year`), size=3.5) +
  # scale_x_date(date_breaks="1 week", date_labels="%m-%d") +
  # scale_y_continuous(labels=scales::percent) +
  theme_bw(base_family="NanumGothic") +

  labs(x="", y="기대수명", color="") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text=element_text(size=16, color="black"),
        legend.text=element_text(size=18),
        plot.title = element_text(size=22))

gganimate(life)
gganimate(life, "기대수명.gif", ani.width = 640, ani.height = 480)




library(tidyverse); library(palmerpenguins); library(ggiraph); library(patchwork)
color_palette <- thematic::okabe_ito(3)

scatter_gg <- penguins |> drop_na() |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species, data_id = species)) +
  geom_point_interactive(size = 1) +
  labs(title = "펭귄의 지느러미 길이와 몸무게",
       x = "지느러미 길이 (mm)", y = "몸무게 (g)", color = "종") +
  theme_minimal(base_size = 5) +
  theme(legend.position = "top") +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(labels = scales::comma)

boxplot_gg <- penguins |> drop_na() |>
  ggplot(aes(x = species, y = body_mass_g, fill = species,  data_id = species)) +
  geom_boxplot_interactive() +
  geom_jitter_interactive(aes(col = species)) +
  labs(title = "펭귄 종별 몸무게",
       x = "펭귄 종명", y = "몸무게 (g)", color = "종") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()

girafe(
  ggobj = boxplot_gg + plot_spacer() + scatter_gg + plot_layout(widths = c(0.45, 0.1, 0.45)),
  options = list(
    opts_hover(css = ''), opts_hover_inv(css = "opacity:0.1;"),
    opts_sizing(rescale = FALSE)
  ))



library(dplyr)
library(ggplot2)
library(patchwork)
dat <- gapminder::gapminder |>
  janitor::clean_names() |>
  mutate(
    # Reformat continent as a character instead of as a factor
    # (will be important later)
    id = levels(continent)[as.numeric(continent)],
    continent = forcats::fct_reorder(continent, life_exp)
  )

color_palette <- thematic::okabe_ito(5)
names(color_palette) <- unique(dat$continent)
base_size <- 18
mean_life_exps <- dat |>
  group_by(continent, year, id) |>
  summarise(mean_life_exp = mean(life_exp)) |>
  ungroup()


line_chart <- mean_life_exps |>
  ggplot(aes(x = year, y = mean_life_exp, col = continent)) +
  geom_line(linewidth = 2.5) +
  geom_point(size = 4) +
  theme_minimal(base_size = base_size) +
  labs(
    x = element_blank(),
    y = 'Life expectancy (in years)',
    title = 'Life expectancy over time'
  ) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) +
  scale_color_manual(values = color_palette)
line_chart

library(ggiraph)

line_chart <- mean_life_exps |>
  ggplot(aes(x = year, y = mean_life_exp, col = continent, data_id = id)) +
  geom_line_interactive(linewidth = 2.5) +
  geom_point_interactive(size = 4) +
  theme_minimal(base_size = base_size) +
  labs(
    x = element_blank(),
    y = 'Life expectancy (in years)',
    title = 'Life expectancy over time'
  ) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) +
  scale_color_manual(values = color_palette)

girafe(ggobj = line_chart)
girafe(
  ggobj = line_chart,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 6,
  width_svg = 9
)



library(crosstalk)
library(leaflet)
library(DT)

# Wrap data frame in SharedData
sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])

# Create a filter input
filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250)

# Use SharedData like a dataframe with Crosstalk-enabled widgets
bscols(
  leaflet(sd) %>% addTiles() %>% addMarkers(),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)



library(ggwordcloud)

yoon_tbl |>
  ggplot(aes(label = 키워드, size = n, color = I("red"))) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 80) +
  facet_wrap(~ 연도) +
  theme_minimal() +
  labs(title = "윤석열 대통령 연도별 워드 클라우드")
