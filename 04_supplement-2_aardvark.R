############################################################
#                                                          #
#  Aardvark: Generate a table of URL links (Supplement 2)  #
#                                                          #
############################################################

#--- Load packages ---#
library(dplyr)
library(readr)

#--- Import data ---#
aardvark <- read_csv('data/aardvark-data.csv') %>% 
    clean_names()

#--- Extract links ---# 
links <- aardvark %>% 
    mutate(Species = 'Aardvark') %>% 
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
          file = 'supplement-2.csv')
