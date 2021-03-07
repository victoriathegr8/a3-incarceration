library(tidyverse)
library(maps)
library(lintr)
library(patchwork)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# What is the proportion of black and white jail prison population compared to
# total prison population for the most recent year?
most_recent <- incarceration %>%
  filter(year == max(year))
black_jail_prop <- (sum(most_recent$black_jail_pop, na.rm = T) /
                      sum(most_recent$total_jail_pop, na.rm = T))
white_jail_prop <- (sum(most_recent$white_jail_pop, na.rm = T) /
                      sum(most_recent$total_jail_pop, na.rm = T))

diff_jail_prop <- black_jail_prop - white_jail_prop

# What is the proportion of black and white populations aged 15-64 in regards
# to the total population for the most recent year?
black_pop <- sum(most_recent$black_pop_15to64) /
  sum(most_recent$total_pop_15to64)
white_pop <- sum(most_recent$white_pop_15to64) /
  sum(most_recent$total_pop_15to64)

# What is the difference between the above two variables?
black_jail_diff <- black_pop - black_jail_prop
white_jail_diff <- white_pop - white_jail_prop

# How many states have a black jail proportion higher than the total black
# population proportion for the most recent year?
higher_black_proportion <- most_recent %>%
  group_by(state) %>%
  summarise(black_jail_proportion = (sum(black_jail_pop, na.rm = T) /
                                       sum(total_jail_pop, na.rm = T))) %>%
  filter(black_jail_proportion > black_pop) %>%
  nrow()

# What year had the highest black prison admission rate?
year_most_black_prison_adm <- incarceration %>%
  group_by(year) %>%
  summarise(black_prison_admission_rate = sum(black_prison_adm, na.rm = T) /
              sum(total_prison_adm, na.rm = T)) %>%
  filter(black_prison_admission_rate ==
           (max(black_prison_admission_rate, na.rm = T))) %>%
  pull(year)

# What state had the highest black prison admission rate for the year 2013
# (the year the Black Lives Matter movement began)?
state_most_black_prison_adm <- incarceration %>%
  filter(year == 2013) %>%
  group_by(state) %>%
  summarise(black_prison_admission_rate =
              sum(black_prison_adm, na.rm = T) /
              sum(total_prison_adm, na.rm = T)) %>%
  filter(black_prison_admission_rate ==
           (max(black_prison_admission_rate, na.rm = T))) %>%
  pull(state)

# Trends over time chart
# Black jail population over time in Wisconsin, Michigan, Pennsylvania,
# New York, and South Carolina
top_states <- incarceration %>%
  filter(year > 1994 & year < 2017) %>%
  filter(state == "WI" | state == "MI" |
           state == "PA" | state == "NY" | state == "SC") %>%
  group_by(year, state) %>%
  summarise(black_prison_proportion =
              sum(black_prison_pop, na.rm = T) /
              sum(total_prison_pop, na.rm = T))

black_prison_prop_chart <- top_states %>%
  ggplot(aes(x = year, y = black_prison_proportion,
             group = state, color = state)) +
  geom_line() +
  labs(title = "% Black Of Total Prison Population over Time",
       y = "Black Prison Population Proportion")

# Variable comparison chart
black_jail_chart <- ggplot(data = incarceration) +
  geom_point(mapping = aes(x = white_pop_15to64, y = black_jail_pop)) +
  labs(title = "White Population Aged 15-64 vs. Black Jail Population",
       x = "White Population Aged 15-64",
       y = "Black Jail Population")

# Map
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

midwest <- c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI")

map_data <- county_shapes %>%
  left_join(most_recent, by = "fips") %>%
  filter(state %in% midwest)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

white_jail_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  labs(title = "White Jail Population\nin the Midwest in 2018",
       fill = "White Jail Population")
black_jail_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  labs(title = "Black Jail Population\nin the Midwest in 2018",
       fill = "Black Jail Population")

combined_maps <- white_jail_map + black_jail_map +
  plot_layout(widths = 4, heights = 4)
