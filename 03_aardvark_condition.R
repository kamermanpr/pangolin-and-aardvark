############################################################
#                                                          #
#                 Aardvark body condition                  #
#                                                          #
############################################################

#--- Load packages ---#
library(dplyr)
library(forcats)
library(lubridate)
library(stringr)
library(readr)
library(readxl)
library(janitor)
library(irr)
library(gtsummary)
library(parameters)

#--- Create figures directory ---#
if(!dir.exists('figures')){
    dir.create('figures')
}

#--- Import ---#
aardvark <- read_csv('data/aardvark-data.csv') %>% 
    clean_names() %>% 
    filter(!is.na(link)) %>%
    mutate(link = str_squish(link))

link <- read_csv('data/aardvark_links-with-image-numbers.csv') %>% 
    filter(!is.na(link)) %>%
    mutate(link = str_squish(link))

condition <- read_csv('data/body-condition_LM_NW_combined.csv') %>% 
    clean_names()

#--- Join datasets ---#
data_joined <- aardvark %>% 
    select(number, link, alive_dead, year, time) %>% 
    inner_join(link, by = 'link') %>% 
    inner_join(condition, by = 'image_number') %>% 
    filter(alive_dead == 'alive') %>% 
    relocate(image_number, .after = number)
    
#--- Process ---#
data_cleaned <- data_joined %>% 
    # Choose columns
    select(number, image_number, year, time,
           leith_1_to_4, nora_1_to_4) %>% 
    # Time of day (day vs night)
    mutate(time = case_when(
        str_detect(time, 'evening') ~ 'Night-time',
        TRUE ~ 'Daytime'
    )) %>% 
    # Time of year 
    mutate(month = month(dmy(year))) %>% 
    mutate(season = case_when(
        month == 12 |
            month == 1 |
            month == 2 ~ 'Summer',
        month == 3 |
            month == 4 |
            month == 5 ~ 'Autumn',
        month == 6 |
            month == 7 |
            month == 8 ~ 'Winter',
        month == 9 |
            month == 10 |
            month == 11 ~ 'Spring'
    )) %>% 
    select(-year, -month) %>% 
    # Clean-up Leith's columns
    mutate(leith_1_to_4 = as.numeric(str_remove(leith_1_to_4,
                                                pattern = 'Not able to see'))) %>% 
    relocate(season, .after = image_number)

#--- Agreement ---#
# Raw scores (Leith and Nora)
agree(ratings = select(data_cleaned, leith_1_to_4, nora_1_to_4) %>% 
          filter(complete.cases(.)))

kappa2(ratings = select(data_cleaned, leith_1_to_4, nora_1_to_4) %>% 
          filter(complete.cases(.)),
       weight = 'equal')

## Poor agreement between reviewers, best bet is probably to dichotomise 
## condition into good vs poor and get a third reviewer to decide which
## of the first two reviewers was "correct".

#--- Output file for reviewer 3 ---#
# Process data
reviewer_three <- data_cleaned %>% 
    select(number, image_number, leith_1_to_4, nora_1_to_4) %>% 
    # Dichotomise (sob!)
    ## Reviewer 1: Leith
    mutate(reviewer_1 = case_when(
        leith_1_to_4 >= 3 ~ 'Good',
        leith_1_to_4 < 3 ~ 'Poor'
    )) %>% 
    ## Reviewer 2: Nora
    mutate(reviewer_2 = case_when(
        nora_1_to_4 >= 3 ~ 'Good',
        nora_1_to_4 < 3 ~ 'Poor'
    )) %>% 
    # Choose required columns
    select(number, image_number, reviewer_1, reviewer_2) %>% 
    # Agreement
    mutate(summary = case_when(
        is.na(reviewer_1) & !is.na(reviewer_2) |
            !is.na(reviewer_1) & is.na(reviewer_2) ~ 'Only one reviewer',
        is.na(reviewer_1) & is.na(reviewer_2) ~ 'No reviews',
        reviewer_1 == 'Good' & reviewer_2 == 'Good' |
            reviewer_1 == 'Poor' & reviewer_2 == 'Poor' ~ 'Reviewers agree',
        reviewer_1 == 'Good' & reviewer_2 == 'Poor' |
            reviewer_1 == 'Poor' & reviewer_2 == 'Good' ~ 'Reviewers disagree'
    )) %>% 
    mutate(reviewer_3 = '')
    
# Save data
write_csv(x = reviewer_three,
          file = 'data/reviewer_3_template.csv')

#--- Import and process reviewer_3_complete.xlsx data ---#
data_rev3 <- read_xlsx('data/reviewer_3_complete.xlsx',
                       sheet = 1)

# Check that reviewer 3 agrees with reviewer 1/2 when there is only one reviewer
data_rev3 %>% 
    mutate(one_reviewer = case_when(
        summary == 'Only one reviewer' & reviewer_2 == reviewer_3 ~ 'Agree',
        summary == 'Only one reviewer' & reviewer_2 != reviewer_3 ~ 'Disagree',
        summary == 'Only one reviewer' & reviewer_1 == reviewer_3 ~ 'Agree',
        summary == 'Only one reviewer' & reviewer_1 != reviewer_3 ~ 'Disagree'
    )) %>% 
    filter(one_reviewer == 'Disagree') # Length = 0, i.e., all agree

# Collate data
data_collated <- data_rev3 %>% 
    mutate(condition_collated = case_when(
        summary == 'Reviewers agree' ~ reviewer_1,
        summary == 'Reviewers disagree' ~ reviewer_3,
        summary == 'Only one reviewer' ~ reviewer_3
    )) 

# Join data_collated () to data_cleaned (time and season data)
data_final <- data_cleaned %>% 
    select(1:4) %>% 
    left_join(data_collated) %>% 
    select(1:4, 9) %>% 
    filter(!is.na(condition_collated))

#--- Tabulated summary ---#
data_final %>% 
    select(-c(1:2)) %>% 
    tbl_summary()

#--- Condition vs day/night sighting ---#
# Plot
png(filename = 'figures/aardvark_condition.png',
    width = 7, height = 6.5, res = 300, units = 'in')
mosaicplot(xtabs(~condition_collated + time, 
                 data = data_final),
           cex = 1,
           color = c('#FFFFFF', '#666666'),
           main = NULL,
           xlab = 'Physical condition',
           ylab = 'Time of day')
dev.off()

# Model
mod_time <- glm(factor(time) ~ factor(condition_collated), 
             data = data_final, 
             family = binomial())

model_parameters(model = mod_time, 
                 exponentiate = TRUE) 