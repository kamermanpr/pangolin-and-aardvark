############################################################
#                                                          #
#                   Pangolins: Figures                     #
#                                                          #
############################################################

#--- Load packages ---#
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(sf) 

#--- Create figures directory ---#
if(!dir.exists('figures')){
    dir.create('figures')
}

#--- Import ---#
pangolin <- read_csv('data/pangolin-data.csv') %>% 
    clean_names()

pangolin_predation <- read_csv('data/pangolin-data_drinking-and-predation.csv') %>% 
    clean_names()

#--- Inspect ---# 
dim(pangolin)
names(pangolin)
glimpse(pangolin)
slice_head(pangolin, n = 10)
slice_tail(pangolin, n = 10)

dim(pangolin_predation)
names(pangolin_predation)
glimpse(pangolin_predation)
slice_head(pangolin_predation, n = 2)
slice_tail(pangolin_predation, n = 2)

#--- Plot 1 ---#
# Where in ZA are all the sightings (by province) (all animals, dead or alive)

## Extract data
pangolins_za <- pangolin %>% 
    filter(!is.na(province_region)) %>% 
    mutate(province_region = str_to_title(province_region)) %>% 
    mutate(country = str_to_title(country)) %>% 
    filter(country =='South Africa') %>% 
    select(province_region) %>% 
    group_by(province_region) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(total = sum(count),
           percent = round(100 * (count / total), 0)) %>% 
    arrange(desc(percent)) %>% 
    rename(province = province_region)

## Import ZA shape file
za <- read_sf('africa_shpZA/zaf_admin1.shp') %>% 
    select(ADM1_EN) %>%
    ## Reduce size
    st_simplify(preserveTopology = TRUE,
                dTolerance = 0.03) 

## Check province names
za$ADM1_EN

## Fix province names
za$ADM1_EN[za$ADM1_EN == 'Nothern Cape'] <- 'Northern Cape'
za$ADM1_EN[za$ADM1_EN == 'KwaZulu-Natal'] <- 'Kwazulu-Natal'

## Calculate province centroids
centroids_za <- as.data.frame(st_coordinates(st_centroid(za))) %>% 
    mutate(province = za$ADM1_EN)

## Join data_prov and centroids_za
data_prov <- centroids_za %>% 
    left_join(pangolins_za)

## Rename X and Y
data_prov <- data_prov %>% 
    rename(longitude = X,
           latitude = Y)

## Join za and data_prov 
za2 <- za %>% 
    rename(province = ADM1_EN) %>% 
    left_join(data_prov) %>% 
    mutate(count = str_replace_na(count),
           count = str_replace(count, pattern = 'NA', replacement = ' ')) %>% 
    mutate(count = as.numeric(count))

p1 <- ggplot(data = za2) +
    geom_sf(aes(fill = count),
            show.legend = FALSE) +
    geom_text(aes(x = longitude, 
                  y = latitude,
                  label = count),
              size = 6) +
    labs(title = NULL) +
    scale_fill_gradient(high = '#993404', 
                        low = '#ffffd4',
                        na.value = '#E5E5E5') +
    #scale_fill_manual(values = pal) +
    theme_map(base_size = 18) +
    theme(plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'))

ggsave(filename = 'figures/pangolins_sightings-by-province.png',
       plot = p1,
       height = 6, 
       width = 8)

#--- Plot 2 ---#
# Day vs night (alive animals only)

## Extract data
pangolin_time <- pangolin %>% 
    filter(alive_dead == 'alive') %>% 
    mutate(year = factor(year(dmy(year)))) %>% 
    mutate(time = case_when(
        str_detect(time, 'afternoon') ~ 'afternoon',
        str_detect(time, 'sunset') ~ 'afternoon',
        str_detect(time, 'evening') ~ 'night-time',
        str_detect(time, 'daylight') ~ 'daytime (not specified)',
        TRUE ~ time
    )) %>% 
    group_by(year, time) %>% 
    summarise(count = n()) %>% 
    group_by(year) %>% 
    mutate(total_by_year = sum(count),
           proportion_by_year = count / total_by_year) %>% 
    ungroup() %>% 
    complete(year, time, fill = list(count = 0,
                                     proportion_by_year = 0)) %>% 
    fill(total_by_year, .direction = 'down') %>% 
    mutate(time = factor(time,
                         levels = c('morning', 'daytime (not specified)',
                                    'afternoon', 'night-time'),
                         ordered = TRUE))

## Plot
p2 <- ggplot(data = pangolin_time) +
    aes(x = year, 
        y = proportion_by_year,
        fill = time) +
    geom_col(colour = '#999999') +
    labs(title = NULL,
         x = 'Year',
         y = 'Proportion of sightings') +
    scale_fill_brewer() +
    scale_x_discrete(breaks = c('2010', '2012', '2014', '2016', '2018')) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_time-of-sighting_year.png',
       plot = p2,
       height = 6, 
       width = 8)

# Presentation plot
p2b <- ggplot(data = pangolin_time) +
    aes(x = year, 
        y = proportion_by_year,
        fill = time) +
    geom_col(colour = '#FFFFFF') +
    labs(title = NULL,
         x = 'Year',
         y = 'Proportion of sightings') +
    scale_fill_tableau(palette = 'Classic Color Blind') +
    scale_x_discrete(breaks = c('2010', '2012', '2014', '2016', '2018')) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_time-of-sighting_year_presentation.png',
       plot = p2b,
       height = 6, 
       width = 8)

#--- Plot 3 ---#
# Day vs night by season (alive animals only)

## Extract data
pangolin_season <- pangolin %>% 
    filter(alive_dead == 'alive') %>% 
    select(year, time) %>% 
    mutate(month = month(dmy(year))) %>% 
    mutate(season = case_when(
        month == 12 |
            month == 1 |
            month == 2 ~ 'summer',
        month == 3 |
            month == 4 |
            month == 5 ~ 'autumn',
        month == 6 |
            month == 7 |
            month == 8 ~ 'winter',
        month == 9 |
            month == 10 |
            month == 11 ~ 'spring'
    )) %>% 
    mutate(time = case_when(
        str_detect(time, 'afternoon') ~ 'afternoon',
        str_detect(time, 'sunset') ~ 'afternoon',
        str_detect(time, 'evening') ~ 'night-time',
        str_detect(time, 'daylight') ~ 'daytime (not specified)',
        TRUE ~ time
    )) %>% 
    group_by(season, time) %>% 
    summarise(count = n()) %>% 
    group_by(season) %>% 
    mutate(total_by_season = sum(count),
           proportion_by_season = count / total_by_season) %>% 
    ungroup() %>% 
    complete(season, time, fill = list(proportion_by_season = 0)) %>% 
    mutate(time = factor(time,
                         levels = c('morning', 'daytime (not specified)',
                                    'afternoon', 'night-time'),
                         ordered = TRUE)) %>% 
    mutate(season = factor(season, 
                           levels = c('summer', 'autumn', 'winter', 'spring'),
                           ordered = TRUE))

## Plot
p3 <- ggplot(data = pangolin_season) +
    aes(x = season, 
        y = proportion_by_season,
        fill = time) +
    geom_col(colour = '#999999') +
    labs(title = NULL,
         x = 'Season',
         y = 'Proportion of sightings') +
    scale_fill_brewer() +
    scale_x_discrete(labels = c('Summer', 'Autumn', 'Winter', 'Spring')) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_time-of-sighting_season.png',
       plot = p3,
       height = 6, 
       width = 8)

# Presentation plot
p3b <- ggplot(data = pangolin_season) +
    aes(x = season, 
        y = proportion_by_season,
        fill = time) +
    geom_col(colour = '#FFFFFF') +
    labs(title = NULL,
         x = 'Season',
         y = 'Proportion of sightings') +
    scale_fill_tableau(palette = 'Classic Color Blind') +
    scale_x_discrete(labels = c('Summer', 'Autumn', 'Winter', 'Spring')) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_time-of-sighting_season_presentation.png',
       plot = p3b,
       height = 6, 
       width = 8)

#--- Plot 4 ---#
# Number of social media posts by year (all animals, dead or alive)

## Extract data
pangolin_posts <- pangolin %>% 
    select(year) %>% 
    mutate(year = factor(year(dmy(year)))) %>% 
    group_by(year) %>% 
    summarise(count = n()) 

## PLot
p4 <- ggplot(data = pangolin_posts) +
    aes(x = year,
        y = count) +
    geom_col(fill = '#4E79A7') +
    geom_text(aes(label = count),
              vjust = -1,
              size = 6) +
    labs(title = NULL,
         x = 'Year',
         y = 'Number of postings') +
    scale_x_discrete(breaks = c('2010', '2012', '2014', '2016', '2018')) +
    scale_y_continuous(limits = c(0, 170)) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_number-of-posts.png',
       plot = p4,
       height = 6, 
       width = 8)

#--- Plot 5 ---#
# Number of drinking events

## Plot 
p5 <- pangolin_predation %>% 
    select(month, drinking) %>% 
    mutate(month = month.abb[1:12]) %>%
    mutate(month = factor(month,
                          levels = month.abb[1:12],
                          ordered = TRUE)) %>% 
    ggplot(data = .) + 
    aes(x = month, 
        y = drinking) +
    geom_col(colour = '#999999',
             fill = '#4E79A7') +
    labs(title = NULL,
         x = 'Month',
         y = 'Count') +
    scale_y_continuous(limits = c(0, 6),
                       breaks = c(0, 2, 4, 6)) +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_drinking_month.png',
       plot = p5,
       height = 6, 
       width = 8)

#--- Plot 6 ---#
# Number of predation episodes

## Extract data
predation <- pangolin_predation %>% 
    select(month, lion, leopard, hyena) %>% 
    mutate(month = month.abb[1:12]) %>% 
    pivot_longer(cols = -month,
                 names_to = 'predator',
                 values_to = 'count')

## Plot 
p6 <- predation %>% 
    mutate(month = factor(month,
                          levels = month.abb[1:12],
                          ordered = TRUE)) %>% 
    ggplot(data = .) + 
    aes(x = month, 
        y = count,
        fill = predator) +
    geom_col(colour = '#999999') +
    labs(title = NULL,
         x = 'Month',
         y = 'Count') +
    scale_y_continuous(limits = c(0, 7),
                       breaks = c(0, 2, 4, 6)) +
    scale_fill_brewer() +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.background = element_rect(fill = '#FFFFFF',
                                         colour = '#FFFFFF'),
          plot.subtitle = element_text(size = 14),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(size = 0.5),
          axis.ticks = element_line(size = 0.5))

ggsave(filename = 'figures/pangolins_predation_month.png',
       plot = p6,
       height = 6.5, 
       width = 8)
