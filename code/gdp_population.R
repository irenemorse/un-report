#####################################################
## Setup Section
#####################################################

# creating this so my dir aligns with leaners

library(tidyverse)
library(ggplot2)

#####################################################
## Day 1 Work: Intro to R and Plotting
#####################################################

gapminder_data <- read.csv("data/gapminder_data.csv")

#gapminder_data <- read_csv("~/Desktop/un-report/gapminder_data.csv")

ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", size="Population (in millions)")+
  theme_classic()
ggsave("gdp_percap.pdf")

#####################################################
## Day 2 Work: Data Manipulation and Cleaning
#####################################################

gapminder_data <- read_csv("data/gapminder_data.csv")
glimpse(gapminder_data)
View(gapminder_data)

summarize(gapminder_data, avgLifeExp = mean(lifeExp))

# what is the maximum lifeExp?
summarize(gapminder_data, maxLifeExp = max(lifeExp))

# pipes
# %>%
# means "and then do this thing"

# option 1 without a pipe
summarize(gapminder_data, avgLifeExp = mean(lifeExp))
# option 2 with a pipe
gapminder_data %>% summarize(avgLifeExp = mean(lifeExp))

# save the summary table
gapminder_data_summary <- gapminder_data %>% 
  summarize(avgLifeExp = mean(lifeExp))

gapminder_data %>% filter(year == 2007) %>% 
  summarize(avgLifeExp07 = mean(lifeExp))

# find the earliest year in the data using summarize() and min()
gapminder_data %>% summarize(startpoint = min(year))
# filter the data to 1952 only and find avg GDP per capita
gapminder_data %>% filter(year == 1952) %>% 
  summarize(avgGDPpercap = mean(gdpPercap))

# calculate life expectancy by year
gapminder_data %>% group_by(year) %>% 
  summarize(avg = mean(lifeExp))

# calculate average life expectancy by continent
gapminder_data %>% group_by(continent) %>%
  summarize(avg = mean(lifeExp), 
            min = min(lifeExp), 
            max = max(lifeExp))

# adding columns to dataset using mutate
gapminder_data %>% mutate(gdp = gdpPercap * pop)
View(gapminder_data)

# we have a column called pop
# use mutate to create a col for popInMillions
gapminder_data %>% mutate(popInMillions = pop/100000)

# filter to select rows
# select to select cols
gapminder_data %>% select(pop, year)
gapminder_data %>% select(-continent)

# print a dataframe with country, continent, year, and lifeExp
gapminder_data %>% select(country, continent, year, lifeExp)
gapminder_data %>% select(year, starts_with("c"))

# print dataframe of all cols that end in the letter "p"
gapminder_data %>% select(ends_with("p"))

gapminder_data %>% select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# save a dataframe that contains only the Americas in 2007
# logical symbols: & is for AND
# |
# is for OR
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

# cleaning messy data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, 
         col_names = c("region","country","year","series",
                       "value","footnotes","source"))

glimpse(co2_emissions_dirty)

# select the country, year, series, and value columns
co2_emissions_dirty %>% select(country, year, series, value) %>%
  print(n=50)

co2_emissions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)

# filter down to 2005 and drop the year column
co2_emissions <- co2_emissions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

# previous we created gapminder for 2007
# now we have co2 emissions for 2005
# Google how to join two data frames using tidyverse
inner_join(gapminder_data, co2_emissions, by="country")

anti_join(gapminder_data, co2_emissions, by="country")

View(gapminder_data)
View(co2_emissions)

co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
         col_names = c("region","country","year",
                       "series","value","footnotes","source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

anti_join(gapminder_data, co2_emissions, by="country")

unique(gapminder_data$country)
unique(co2_emissions$country)

# address Puerto Rico
gapminder_data <- gapminder_data %>% 
  mutate(country = recode(country, 
                          "Puerto Rico" = "United States"))

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")
glimpse(gapminder_co2)

gapminder_co2 %>% group_by(continent) %>% 
  summarize(avgLifeExp = mean(lifeExp))

gapminder_co2 %>% 
  filter(continent=="Americas") %>%
  mutate(region = ifelse(country == "United States" | country == "Canada" | country == "Mexico", "north", "south")) %>%
  View()

# I want the Americas in 2007
gapminder_co2 <- gapminder_co2 %>% 
  filter(continent=="Americas" & year==2007)

# write out new clean dataset as csv
write_csv(gapminder_co2, "data/gapminder_co2.csv")
