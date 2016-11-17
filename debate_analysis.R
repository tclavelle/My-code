###################################################################
##
## Presidential Debates Analysis
##
###################################################################

### Ideas:
##
## Frequency of Trump's favorite words over time with the debates ('Tremendous', 'Great', 'Mexico', etc)
## 

## Load libraries
library(tidyverse)
library(wordcloud2)

## Load debate data
pres <- read_csv(file = '../../Google Drive/Project Data/fun-data/debate.csv')
prime <- read_csv(file = '../../Google Drive/Project Data/fun-data/primary_debates_cleaned.csv')

## Calculate word frequency

# Trump
trump <- pres %>%
  filter(Speaker == 'Trump') %>%
  select(Speaker, Date, Line, Text) %>%
  mutate(Debate = 'Presidential') %>%
  left_join(prime %>%
              filter(Speaker == 'Trump') %>%
              select(Speaker, Date, Line, Text) %>%
              mutate(Debate = 'Primary'))
  
# Clinton
clinton <- pres %>%
  filter(Speaker == 'Clinton') %>%
  select(Speaker, Date, Line, Text) %>%
  mutate(Debate = 'Presidential') %>%
  left_join(prime %>%
              filter(Speaker == 'Clinton') %>%
              select(Speaker, Date, Line, Text) %>%
              mutate(Debate = 'Primary')) 