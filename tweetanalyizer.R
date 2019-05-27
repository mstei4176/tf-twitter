library(rtweet)
library(RMySQL)
library(tidyverse)
library(keyring)
library(purrrlyr)

#system("ssh -L 3306:localhost:3306 10.0.1.121")

############################################################
# Chunk  - 1 - Loading Tweets   
############################################################
# Set variables to be used in keyring.
kr_name <- "thievingfox_keyring"
kr_service <- "myTest2"
kr_username <- "mstein"

# Create a keyring and add an entry using the variables above
# kb <- keyring::backend_file$new()
# # Prompt for the keyring password, used to unlock keyring
# kb$keyring_create(kr_name)
# # Prompt for the credential to be stored in the keyring
# kb$set(kr_service, username=kr_username, keyring=kr_name)
# # Lock the keyring
# kb$keyring_lock(kr_name)

## Reading RSS addresses from SQL ##
conLoc <- dbConnect(dbDriver("MySQL"), 
                    user=kr_username, 
                    password=keyring::backend_file$new()$get(service = kr_service,
                                                             user = kr_username,
                                                             keyring = kr_name), 
                    dbname=kr_service, 
                    host='127.0.0.1', 
                    port = 3306)

sql <- "select * FROM TwitterByTimeLines;"
TwitterTLS <- dbGetQuery(conLoc,sql)
TwitterTLS <- TwitterTLS %>%
  select(user_id, created_at, screen_name, text, display_text_width,
         media_expanded_url, media_expanded_url, ext_media_expanded_url, lang) %>%
  filter(lang == "en") %>%
  as_tibble() %>%
  distinct()

dbDisconnect(conLoc)

############################################################
# Chunk  - 2 - Cleaning Tweets   
############################################################





############################################################
# Chunk  - 3 - Timeline for Tweets   
############################################################

## plot time series of tweets
rt %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )






