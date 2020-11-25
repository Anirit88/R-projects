library("stringr")
library("tidyverse")
library("ggplot2")
library("dbplyr")
install.packages("maps")
install.packages("mapproj")
install.packages("patchwork")
library(maps)

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Filtering out my data to fit the questions i will be addressing in this assignment
usable_data <- select(incarceration,year,fips,state,county_name,total_pop_15to64,female_pop_15to64,male_pop_15to64)

#summary data for 5 values intended to be calculated

#total number of males and female inmates from each state between the ages 15 and 64,
#with difference amongst them in the most recent year (2018)
total_inmates2018  <- usable_data %>%
  filter(year == max(year))%>%
  summarise(
  avg_male = mean(male_pop_15to64),
  avg_female = mean(female_pop_15to64),
  difference = avg_female - avg_male
)

#total number of males and female inmates from each state between the ages 15 and 64,
#with difference amongst them in the earliest year (1970)
total_inmates1970  <- usable_data%>%
  filter(year == min(year))%>%
  summarise(
  avg_male = mean(male_pop_15to64),
  avg_female = mean(female_pop_15to64),
  difference = avg_female - avg_male
)

#State with the maximum incarceration for males
max_males_incarcerated <- usable_data %>%
  filter(male_pop_15to64 == max(male_pop_15to64))%>%
  select(state, male_pop_15to64)

#State with the maximum incarceration for females
max_females_incarcerated <- usable_data %>%
  filter(female_pop_15to64 == max(female_pop_15to64))%>%
  select(state, female_pop_15to64)

#ratio of female to male incarsiration rates
usable_data <- mutate(
  usable_data,
  ratio = female_pop_15to64/male_pop_15to64
)

#Trend over time chart for the most recent year and the earliest year of recorded 
#incarceration rates for males VS females.

trend_chart_male <- ggplot(data = usable_data) +
  geom_line(mapping = aes(x= year,  y = male_pop_15to64 )) +
  labs(
  title = " incarceration rates over the years",
  x = "year",
  y= "population"
  )

trend_chart_female <- ggplot(data = usable_data) +
  geom_line(mapping = aes(x= year,  y = female_pop_15to64 )) +
  labs(
    title = "female incarceration rates over the years",
    x = "year",
    y= "population"
  )
  
#Variable comparison chart
#Female comparison
comparison_chart_female  <- ggplot(data = usable_data) +
  geom_point(mapping = aes(x = total_pop_15to64, y = female_pop_15to64,
                           color = year)) +
  scale_x_continuous() +
  scale_y_continuous() +
  labs(
    title = "comparison on what percent of incarcerated people are women",
    x = "total population",
    y= "female population"
  )

#Male comparison
comparison_chart_male  <- ggplot(data = usable_data) +
  geom_line(mapping = aes(x = total_pop_15to64, y = male_pop_15to64,
                           color = year)) +
  scale_x_continuous() +
  scale_y_continuous() +
  labs(
    title = "comparison on what percent of incarcerated people are men",
    x = "total population",
    y= "male population"
  )

#Map for  data
#filter out data for 2018
female_rates <-  usable_data%>%
  filter(year == 2018)%>%
  select(fips, ratio)

# Join incarsirated data to the U.S. shapefile
state_shape <- map_data("state")%>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- state_shape %>%
  left_join(female_rates, by = "fips")
# Defining a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

map  <-  ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = ratio),
    color = "white",
    size = .1
  ) +
  coord_map()+
  blank_theme +
  scale_fill_continuous(low = "Red", high = "#132B43") +
  labs(fill = "female to male ratio") +
  ggtitle("female to male incarceration rates ratio")   
  
  