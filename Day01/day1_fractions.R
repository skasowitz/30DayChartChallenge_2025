library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggtext)
library(colorspace)
library(showtext)

font_add('fa6-brands',
         here::here('D:/30DayChartChallenge2025/fonts/Font Awesome 6 Brands-Regular-400.otf'))

showtext_auto(enable = TRUE)

df <- data.frame(
  year = c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022),
  total_pop = c(282216952,288125973,293638158,299398484,304059724,308745538,
                313914040,318857056,323127513,327167434,331501080,333261756), # US Population
  cws_pop = c(249671249,255443289,262690043,262690043,269911707,276607387,
              282534910,284099832,276969134,284075868,287798584,289330482), # Population on public water system
  fluor_pop = c(161924080,172209735,180632481,184028038,195545109,204283554,
                210655401,211393167,201565162,207426535,209145650,209135866) # Population on fluoridated drinking water
) |>
  mutate(perc_cws = round(cws_pop/total_pop, 3),
         perc_fluor = round(fluor_pop/total_pop, 3),
         perc_cws_fluor = round(fluor_pop/cws_pop, 3))


# Caption -----------------------------------------------------------------

author <- "Seth Kasowitz"
citation <- "National Center for Chronic Disease Prevention and Health Promotion (U.S.). Division of Oral Health"

# Social media icons
linkedin <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
github   <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bluesky  <- str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")

cap_text <- str_glue("Graphic: {author} | {bluesky} skasowitz.bsky.social | {linkedin} seth-kasowitz | {github} skasowitz \n
                     Source: {citation}") 


method_annotation <- 
  tribble(
    ~x, ~y, ~label,
    2016, 0.9, "2016: Introduction of a new methodology for estimating the population on the community water system."
  )

df |>
  pivot_longer(starts_with('perc')) |>
  ggplot() +
  geom_ribbon(aes(x = year, ymin = 0.5, ymax = value, fill = name)) +
  annotate("segment", x = 2016, xend = 2016, y = 1, yend = 0.857) +
  ggrepel::geom_label_repel(data = method_annotation,
                            aes(x, y, label = str_wrap(label, 28)), min.segment.length = 10,
                            nudge_x = 1, nudge_y = 0.075) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0.5,1)) +
  scale_x_continuous(n.breaks = 12) +
  scale_fill_discrete_sequential(palette = 'Blues 2', order = 3:1,
                                 labels = c(
                                   'Percent of population on Community Water Source (CWS)',
                                   'Percent of population on fluoridated CWS',
                                   'Percent of population on fluoridated water'
                                 )) +
  labs(caption = cap_text,
       x = '', fill = '', y = '',
       title = "Access to Fluoridated Water by United States Population") +
  theme_minimal() +
  theme(plot.caption = element_markdown(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 28),
        legend.position = 'inside',
        legend.position.inside = c(0.2,0.9))
