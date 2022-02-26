library(tidyverse)
library(ggExtra)
library(ggplot2)

jail_data <- read.csv('https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv')

# What is the State / County with the highest % of incarcerated adults
jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000) %>%
  mutate(jailed_percentage = total_prison_pop / total_pop_15to64) %>% 
  arrange(desc(jailed_percentage)) %>%
  summarize(state, county_name, jailed_percentage) %>%
  head(10)

# What is the average incarceration rate for adults nationally
average_jail_percentage <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(total_pop_15to64) & !is.na(total_prison_pop)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(total_pop_15to64), prison_pop = sum(total_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)
# 0.60 % (not to be confused with 60%)

# What is the State with the highest % of incarcerated adults
highest_prison_pop_state <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(total_pop_15to64) & !is.na(total_prison_pop)) %>%
  group_by(state) %>%
  summarize(state_pop = sum(total_pop_15to64), prison_pop = sum(total_prison_pop)) %>%
  summarize(state, jailed_percentage = round(prison_pop / state_pop * 100, 3)) %>%
  arrange(desc(jailed_percentage)) %>%
  top_n(1) %>%
  pull(state, jailed_percentage)

# What is the national prison rate for white people in 2016?
white_national_prison_rate <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(white_prison_pop) & !is.na(white_pop_15to64)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(white_pop_15to64), prison_pop = sum(white_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)

# What is the national prison rate for black people in 2016?
black_national_prison_rate <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(black_prison_pop) & !is.na(black_pop_15to64)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(black_pop_15to64), prison_pop = sum(black_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)

# What is the national prison rate for hispanic people in 2016?
latinx_national_prison_rate <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(latinx_prison_pop) & !is.na(latinx_pop_15to64)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(latinx_pop_15to64), prison_pop = sum(latinx_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)

# What is the national prison rate for native americans in 2016?
native_national_prison_rate <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(native_prison_pop) & !is.na(native_pop_15to64)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(native_pop_15to64), prison_pop = sum(native_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)

# What is the national prison rate for asian americans in 2016?
aapi_national_prison_rate <- jail_data %>% filter(year == 2016) %>% 
  filter(total_pop_15to64 > 1000 & !is.na(aapi_prison_pop) & !is.na(aapi_pop_15to64)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(aapi_pop_15to64), prison_pop = sum(aapi_prison_pop)) %>%
  summarize(jailed_percentage = prison_pop / total_pop * 100) %>%
  pull(jailed_percentage) %>%
  round(3)

## Chart 1: number of people in prison / total population by race over time.

# filtering data for each race
asian_prison_rate_by_year <- jail_data %>% 
  filter(year <= 2016 & year >= 1970) %>% 
  filter(!is.na(aapi_prison_pop) & !is.na(aapi_pop_15to64) & aapi_pop_15to64 > 100 
         & total_prison_pop > 100) %>%
  group_by(year) %>%
  summarize(prison_rate_of_population = sum(aapi_prison_pop) / sum(aapi_pop_15to64) * 10000, 
            race = 'Asian American / Pacific Islander')

black_prison_rate_by_year <- jail_data %>% 
  filter(year <= 2016 & year >= 1970) %>% 
  filter(!is.na(black_prison_pop) & !is.na(black_pop_15to64) & black_pop_15to64 > 100 
         & total_prison_pop > 100) %>%
  group_by(year) %>%
  summarize(prison_rate_of_population = sum(black_prison_pop) / sum(black_pop_15to64) * 10000, 
            race = 'Black')

hispanic_prison_rate_by_year <- jail_data %>% 
  filter(year <= 2016 & year >= 1970) %>% 
  filter(!is.na(latinx_prison_pop) & !is.na(latinx_pop_15to64) & latinx_pop_15to64 > 100 
         & total_prison_pop > 100) %>%
  group_by(year) %>%
  summarize(prison_rate_of_population = sum(latinx_prison_pop) / sum(latinx_pop_15to64) * 10000, 
            race = 'Hispanic')

white_prison_rate_by_year <- jail_data %>% 
  filter(year <= 2016 & year >= 1970) %>% 
  filter(!is.na(white_prison_pop) & !is.na(white_pop_15to64) & white_pop_15to64 > 100 
         & total_prison_pop > 100) %>%
  group_by(year) %>%
  summarize(prison_rate_of_population = sum(white_prison_pop) / sum(white_pop_15to64) * 10000, 
            race = 'White')

native_prison_rate_by_year <- jail_data %>% 
  filter(year <= 2016 & year >= 1970) %>% 
  filter(!is.na(native_prison_pop) & !is.na(native_pop_15to64) & native_pop_15to64 > 100 
         & total_prison_pop > 100) %>%
  group_by(year) %>%
  summarize(prison_rate_of_population = sum(native_prison_pop) / sum(native_pop_15to64) * 10000, 
            race = 'Native American')

# join data of each race
time_data <- asian_prison_rate_by_year %>% 
  union(black_prison_rate_by_year) %>%
  union(hispanic_prison_rate_by_year) %>%
  union(white_prison_rate_by_year) %>%
  union(native_prison_rate_by_year)

# plot jail rate over time
jail_rate_over_time_plot <- ggplot(time_data, aes(x = year, y = prison_rate_of_population, color = race)) +
  geom_line() + 
  geom_point(size=1) +
  guides(color = guide_legend(title = "Race")) +
  ggtitle("National Jail Rate per 10,000 population from 1990 to 2016") +
  xlab("Year") + 
  ylab("Jail Rate per 10,000 population")

# Chart 2: Comparison between prison rate % of race 
#   and county population
#   and total prison pop
#   and total prison admission
asian_correlation_data <- jail_data %>% 
  filter(year == 2016) %>% 
  filter(!is.na(aapi_prison_pop) & !is.na(aapi_pop_15to64) & aapi_pop_15to64 > 100 & total_prison_pop > 100) %>%
  mutate(prison_rate = aapi_prison_pop / total_prison_pop * 100) %>%
  mutate(population_rate = aapi_pop_15to64 / total_pop_15to64 * 100) %>%
  summarize(prison_rate, population_rate, total_prison_pop, total_pop_15to64, total_prison_adm, 
            race = 'Asian American / Pacific Islander')

black_correlation_data <- jail_data %>% 
  filter(year == 2016) %>% 
  filter(!is.na(black_prison_pop) & !is.na(black_pop_15to64) & black_pop_15to64 > 100 & total_prison_pop > 100) %>%
  mutate(prison_rate = black_prison_pop / total_prison_pop * 100) %>%
  mutate(population_rate = black_pop_15to64 / total_pop_15to64 * 100) %>%
  summarize(prison_rate, population_rate, total_prison_pop, total_pop_15to64, total_prison_adm, 
            race = 'Black')

hispanic_correlation_data <- jail_data %>% 
  filter(year == 2016) %>% 
  filter(!is.na(latinx_prison_pop) & !is.na(latinx_pop_15to64) & latinx_pop_15to64 > 100 & total_prison_pop > 100) %>%
  mutate(prison_rate = latinx_prison_pop / total_prison_pop * 100) %>%
  mutate(population_rate = latinx_pop_15to64 / total_pop_15to64 * 100) %>%
  summarize(prison_rate, population_rate, total_prison_pop, total_pop_15to64, total_prison_adm, 
            race = 'Hispanic')

native_correlation_data <- jail_data %>% 
  filter(year == 2016) %>% 
  filter(!is.na(native_prison_pop) & !is.na(native_pop_15to64) & native_pop_15to64 > 100 & total_prison_pop > 100) %>%
  mutate(prison_rate = native_prison_pop / total_prison_pop * 100) %>%
  mutate(population_rate = native_pop_15to64 / total_pop_15to64 * 100) %>%
  summarize(prison_rate, population_rate, total_prison_pop, total_pop_15to64, total_prison_adm, 
            race = 'Native American')

white_correlation_data <- jail_data %>% 
  filter(year == 2016) %>% 
  filter(!is.na(white_prison_pop) & !is.na(white_pop_15to64) & white_pop_15to64 > 100 & total_prison_pop > 100) %>%
  mutate(prison_rate = white_prison_pop / total_prison_pop * 100) %>%
  mutate(population_rate = white_pop_15to64 / total_pop_15to64 * 100) %>%
  summarize(prison_rate, population_rate, total_prison_pop, total_pop_15to64, total_prison_adm, 
            race = 'White')

correlation_data <- asian_correlation_data %>% 
  union(black_correlation_data) %>% 
  union(hispanic_correlation_data) %>%
  union(native_correlation_data) %>% 
  union(white_correlation_data)

# plot the correlation chart 
p <- ggplot(correlation_data, aes(x = total_prison_pop, y = prison_rate, color = race)) +
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  labs(title = "Total Prison Population vs.\nPrison Rate by Race 2016") +
  geom_smooth() +
  xlab("Prison Population") + 
  ylab("% of Prisoners") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  ) 

# Plot the scatter plot with marginal histograms
correlation_plot <- ggMarginal(p, groupColour = TRUE, groupFill = TRUE)

## Plot 3, a map using fips.
fips_by_county <- maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") 

fip_locations <- map_data("county") %>% inner_join(fips_by_county) %>% summarize(fips, group, long, lat)

#set up 'a minimalist theme'
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

# Black jail rate map
black_jail_rate_by_county <- jail_data %>%
  filter(year == 2016) %>%
  filter(!is.na(black_prison_pop) & !is.na(total_prison_pop)) %>%
  filter(state != 'AK' & state != 'HI') %>%
  group_by(fips) %>%
  summarize(prison_pop_percent = mean(black_prison_pop / total_prison_pop) * 100)

black_jail_rate_fips <- fip_locations %>% left_join(black_jail_rate_by_county, by="fips")

black_jail_rate_map <- ggplot(black_jail_rate_fips) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prison_pop_percent),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + 
  scale_fill_continuous(low = "#fff2f5", high = "#610014") +
  labs(fill = "% of Prisoners\n That Are Black") +
  ggtitle("Black Prison Population 2016") +
  blank_theme

# White jail rate map
white_jail_rate_by_county <- jail_data %>%
  filter(year == 2016) %>%
  filter(!is.na(white_prison_pop) & !is.na(total_prison_pop)) %>%
  filter(state != 'AK' & state != 'HI') %>%
  group_by(fips) %>%
  summarize(prison_pop_percent = mean(white_prison_pop / total_prison_pop) * 100)

white_jail_rate_fips <- fip_locations %>% left_join(white_jail_rate_by_county, by="fips")

white_jail_rate_map <- ggplot(white_jail_rate_fips) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prison_pop_percent),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#CBC3E3", high = "#301934") +
  labs(fill = "% of Prisoners\n That Are White") +
  ggtitle("White Prison Population 2016") +
  blank_theme

# Asian American/Pacific Islander jail rate map
aapi_jail_rate_by_county <- jail_data %>%
  filter(year == 2016) %>%
  filter(!is.na(aapi_prison_pop) & !is.na(total_prison_pop)) %>%
  filter(state != 'AK' & state != 'HI') %>%
  group_by(fips) %>%
  summarize(prison_pop_percent = mean(aapi_prison_pop / total_prison_pop) * 100)

aapi_jail_rate_fips <- fip_locations %>% left_join(aapi_jail_rate_by_county, by="fips")

aapi_jail_rate_map <- ggplot(aapi_jail_rate_fips) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prison_pop_percent),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#ADD8E6", high = "#00008b") +
  labs(fill = "% of Prisoners \nThat Are Asian \nAmericans/Pacific \nIslanders") +
  ggtitle("Asian American/Pacific Islander Prison Population 2016") +
  blank_theme

# Hispanic jail rate map
latinx_jail_rate_by_county <- jail_data %>%
  filter(year == 2016) %>%
  filter(!is.na(latinx_prison_pop) & !is.na(total_prison_pop)) %>%
  filter(state != 'AK' & state != 'HI') %>%
  group_by(fips) %>%
  summarize(prison_pop_percent = mean(latinx_prison_pop / total_prison_pop) * 100)

latinx_jail_rate_fips <- fip_locations %>% left_join(latinx_jail_rate_by_county, by="fips")

latinx_jail_rate_map <- ggplot(latinx_jail_rate_fips) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prison_pop_percent),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#90ee90", high = "#013220") +
  labs(fill = "% of Prisoners \nThat Are Hispanic") +
  ggtitle("Hispanic Prison Population 2016") +
  blank_theme

# native americans jail rate map
native_jail_rate_by_county <- jail_data %>%
  filter(year == 2016) %>%
  filter(!is.na(native_prison_pop) & !is.na(total_prison_pop)) %>%
  filter(state != 'AK' & state != 'HI') %>%
  group_by(fips) %>%
  summarize(prison_pop_percent = mean(native_prison_pop / total_prison_pop) * 100)

native_jail_rate_fips <- fip_locations %>% left_join(native_jail_rate_by_county, by="fips")

native_jail_rate_map <- ggplot(native_jail_rate_fips) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prison_pop_percent),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#fda172", high = "#893101") +
  labs(fill = "% of Prisoners \nThat Are Native \nAmerican") +
  ggtitle("Native American Prison Population 2016") +
  blank_theme
