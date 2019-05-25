library(rtweet)
library(RMySQL)
library(tidyverse)
library(keyring)
library(purrrlyr)

############################################################
# Chunk  - 1 - Reading handles   
############################################################
# Set variables to be used in keyring.
kr_name <- "thievingfox_keyring"
kr_service <- "myTest2"
kr_username <- "mstein"

## Reading RSS addresses from SQL ##
conLoc <- dbConnect(dbDriver("MySQL"), 
                    user=kr_username, 
                    password=keyring::backend_file$new()$get(service = kr_service,
                                                             user = kr_username,
                                                             keyring = kr_name), 
                    dbname=kr_service, 
                    host='127.0.0.1', 
                    port = 3306)

sql <- "select TwitterHandle FROM competitors;"
TwitterHandles <- dbGetQuery(conLoc,sql)
sql <- "select screen_name FROM TwitterNewsSites;"
TwitterInfluencer <- dbGetQuery(conLoc,sql)
#TwitterHandles <- as.list(TwitterHandles)
TwitterHandles <- TwitterHandles[c(44:nrow(TwitterHandles)),]
#TwitterHandles <- as.data.frame(TwitterHandles)
TwitterInfluencer <- TwitterInfluencer %>%
  as_tibble() %>%
  distinct()

#dbWriteTable(conLoc, "TwitterNewsSites", TwitterInfluencer, overwrite = TRUE)

dbDisconnect(conLoc)

#TwitterInfluencer.lst <- as.list(TwitterInfluencer)

############################################################
# Chunk  - 2 - Setting up Twitter  
############################################################
token <- create_token(
  app = "thievingfox",
  consumer_key = "IDt6AdectPd2uA0tz4iHWVGV6",
  consumer_secret = "czZlmRqDRszJSRTMSIh38m1HVPOTW8yDHBoaIBVYUjy42NIVlg",
  access_token="254714599-6agwyh2WXrUFQfta4PSTl3r0ycS244uwnQyCFhz0" ,
  access_secret="fQGRqDB2xlb4Ard00SLD7K0tTnGmGYN2gjxmhToi8gcjJ"
)

start.date <- as.character(Sys.Date()-2)
end.date <- as.character(Sys.Date()-1)
############################################################
# Chunk  - 3 - Twitter Search Scrape Candidates    
############################################################
## WRITING INITIAL SET TO LOCAL DB ##

tmls2 <- get_timelines(TwitterHandles, n = 3200)
flatTMLS <- rtweet::flatten(tmls2)

conLoc <- dbConnect(dbDriver("MySQL"), 
                    user=kr_username, 
                    password=keyring::backend_file$new()$get(service = kr_service,
                                                             user = kr_username,
                                                             keyring = kr_name), 
                    dbname=kr_service, 
                    host='127.0.0.1', 
                    port = 3306)

screennames <- flatTMLS %>% 
  select(screen_name) %>%
  distinct()

dbWriteTable(conLoc, "TwitterTimelinesflatV2", flatTMLS, overwrite = TRUE)
gc()
dbDisconnect(conLoc)

############################################################
# Chunk  - 4 - Initital Twitter Search Scrape others    
############################################################
## WRITING INITIAL SET TO LOCAL DB ##


screennames.lst <- TwitterInfluencer %>%
  mutate(group = round(seq_along(1:1504)/10,0)) %>%
  slice_rows("group") %>%
  purrrlyr::by_slice(dmap, paste, collapse = ",") %>%
  unnest() %>%
  mutate(names = strsplit(screen_name, ",")) %>%
  select(3)



GetTwitterTimelines <- function(users) {
  get_timelines(users, n = 100, parse = TRUE,
                check = TRUE)
}

for (i in 1:151) {
  conLoc <- dbConnect(dbDriver("MySQL"), 
                      user=kr_username, 
                      password=keyring::backend_file$new()$get(service = kr_service,
                                                               user = kr_username,
                                                               keyring = kr_name), 
                      dbname=kr_service, 
                      host='127.0.0.1', 
                      port = 3306)
  
  tweetsfound <- map_df(screennames.lst[[1]][i], GetTwitterTimelines)
  tweetsfound <- rtweet::flatten(tweetsfound)
  Sys.sleep(sample(10:36,1))
  dbWriteTable(conLoc, "TwitterByTimeLines", tweetsfound, append = TRUE)
  gc()
  dbDisconnect(conLoc)
}


############################################################
# Chunk  - 5 - Since Twitter Search Scrape others    
############################################################
## WRITING INITIAL SET TO LOCAL DB ##

GetTwitterSinceIDTimelines <- function(user, sinceID) {
  get_timeline(user, n = 3000, since_id = sinceID, parse = TRUE,
                check = TRUE)
}

repeat {
  for (i in 1:151) {
  
    names <- unlist(screennames.lst[[1]][i])
  
    for (j in 1:length(names)) {
      conLoc <- dbConnect(dbDriver("MySQL"), 
                        user=kr_username, 
                        password=keyring::backend_file$new()$get(service = kr_service,
                                                                 user = kr_username,
                                                                 keyring = kr_name), 
                        dbname=kr_service, 
                        host='127.0.0.1', 
                        port = 3306)

      sql <- paste0("select * FROM TwitterByTimeLines WHERE screen_name ='",names[j],"';")
      screen_name.db <- dbGetQuery(conLoc,sql)
    
      sinceID <- screen_name.db %>%
        mutate(status_id <- as.numeric(status_id)) %>%
        filter(status_id == max(status_id)) %>%
        select(status_id) %>%
        mutate(status_id = as.character(status_id))

      tweetsfound <- map_df(names[j], GetTwitterSinceIDTimelines, sinceID)
      tweetsfound <- rtweet::flatten(tweetsfound)
    
      dbWriteTable(conLoc, "TwitterByTimeLines", tweetsfound, append = TRUE)
      gc()
      dbDisconnect(conLoc)
    }
    Sys.sleep(sample(1:60,1))
  }

}

# 
# cnnbrk <- rtweet::flatten(cnnbrk)
# dbWriteTable(conLoc, "TwitterByTimeLines", cnnbrk, append = TRUE)
# gc()
# 


# get_timelines(TwitterInfluencer.small.lst, n = 3000)

# 
# tmls <- get_timelines(TwitterInfluencer.small.lst, n = 3000)
# flatTMLS <- rtweet::flatten(tmls)
# 
# 
# conLoc <- dbConnect(dbDriver("MySQL"), 
#                     user=kr_username, 
#                     password=keyring::backend_file$new()$get(service = kr_service,
#                                                              user = kr_username,
#                                                              keyring = kr_name), 
#                     dbname=kr_service, 
#                     host='127.0.0.1', 
#                     port = 3306)
# 
# dbWriteTable(conLoc, "TwitterInfluencers", flatTMLS, append = TRUE)
# gc()
# dbDisconnect(conLoc)
# 


