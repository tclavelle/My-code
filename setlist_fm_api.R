#------------------------------------------------------------------------------------------------------
#  Live Tracks Shiny App code
#  By: Tyler Clavelle
#------------------------------------------------------------------------------------------------------

library(httr)
library(tidyverse)
library(XML)
library(xml2)
library(RCurl)
library(data.table)
library(lubridate)

### Phish artist id
mbid <- 'e01646f2-2a04-450d-8bf2-0d993082e058'

pgs <- c(1:88)

all_shows <- lapply(pgs, FUN = function(c) {
  
  # First page only with this link, adjust as necessary or loop for multiple (176 pages for Phish)
  xmlLocation <- paste('api.setlist.fm/rest/0.1/artist/',mbid,'/setlists.xml?p=', c ,sep='')
  
  xmlLocationPhish <- paste('api.setlist.fm/rest/0.1/artist/',mbid,'/setlists.xml?p=80' ,sep='')
  phish2<-read_xml(x = getURL(xmlLocationPhish))
  
  
  # read xml information from API
  phish <- read_xml(x = getURL(xmlLocation))
  
  # extract all sets
  sets <- xml_children(xml_find_all(phish, "//sets"))
  
  ## Tour date info
  venue_info <- xml_find_all(phish, 'setlist/@id | setlist/@eventDate | //city/@name | //city/@state | //venue/@id | //venue/@name | //coords/@lat | //coords/@long') %>%
    xml_text() %>%
    matrix(ncol = 8, byrow = T) %>%
    as.data.frame(stringsAsFactors = F) %>%
    rename(date = V1,
           show_id = V2,
           venue_id = V3,
           venue    = V4,
           city     = V5,
           state    = V6,
           lat      = V7,
           long     = V8)
  
  all_sets <- lapply(sets, FUN = function(x) {
    # for(a in 1:length(sets)) {
      # x <- sets[[a]]
    # x = sets[[12]]
    # get show id
    show_id <- xml_find_all(x, '../../@id') %>% 
      xml_text()
    
    # get set name
    set_name <- xml_find_all(x, '@name' ) %>% 
      xml_text()
    
    if(length(set_name)==0) {set_name <- 'non-set'}
    
    # get songs
    songs <- xml_find_all(x, 'song/@name | song/info' ) %>% 
      xml_text()
    
    # make data frame of set
    set_df <- data_frame(show_id = rep(show_id, times = length(songs)),
                         set     = rep(set_name, times = length(songs)),
                         song    = songs)
    # print(a)
  }) %>%
    bind_rows() %>%
    left_join(venue_info)

  # store page results in master list
  # all_shows[[a]] <- set_df
  # print(a)
})

all <- bind_rows(all_shows) %>%
  separate(date, into = c('day','month','year'), sep = '-')
  

write_csv(all, path = 'phish_beta.csv')

