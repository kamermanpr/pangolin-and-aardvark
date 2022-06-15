############################################################
#                                                          #
#  Pangolins: Generate a table of URL links (Supplement 3) #
#                                                          #
############################################################

#--- Load packages ---#
library(dplyr)
library(readr)

#--- Import data ---#
pangolins <- read_csv('data/pangolin-data.csv') %>% 
    clean_names()

#--- Extract links ---# 
links <- pangolins %>% 
    mutate(Species = 'Pangolin') %>% 
    select(number, Species, country, province_region, link) %>% 
    rename(`ID` = number,
           Country = country,
           `Province/region` = province_region,
           URL = link) %>% 
    mutate(URL = ifelse(is.na(URL),
                        yes = 'Missing',
                        no = URL))

#--- Write to file ---#
write_csv(x = links, 
          file = 'supplement-3.csv')
