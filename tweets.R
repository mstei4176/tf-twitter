library(rtweet)
library(RMySQL)
library(tidyverse)
library(keyring)


count <- function(x) {length(na.omit(x))}

foo <- function() {
  warning("uh-oh...", immediate.=TRUE)
  flush.console()
  Sys.sleep(sample(3,1, replace=TRUE))
  "done"
}

dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  
  # =================== Replace NA's with previous non-NA value =====================
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}

############################################################
# Chunk  - 2 - Reading competitors   
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
dbDisconnect(conLoc)
#TwitterHandles <- as.list(TwitterHandles)
TwitterHandles <- TwitterHandles[c(44:nrow(TwitterHandles)),]

############################################################
# Chunk  - 4 - Setting up Twitter  
############################################################

#TwitterSearchTerms <- c(TwitterHandles,Keywords)

token <- create_token()

  

#setup_twitter_oauth(consumerKey, consumerSecret, access_token="254714599-6agwyh2WXrUFQfta4PSTl3r0ycS244uwnQyCFhz0", access_secret="fQGRqDB2xlb4Ard00SLD7K0tTnGmGYN2gjxmhToi8gcjJ")
start.date <- as.character(Sys.Date()-2)
end.date <- as.character(Sys.Date()-1)


############################################################
# Chunk  - 5 - Twitter Search Scrape    
############################################################
# cnn <- get_followers("ewarren")

tweets <- search_tweets(
  TwitterHandles[1], n = 18000, include_rts = FALSE
)

############################################################
# Chunk  - 5 - Twitter Search Scrape    
############################################################
## get most recent 3200 tweets posted by Donald Trump's account
tmls <- get_timelines(TwitterHandles, n = 3200)

## data frame where each observation (row) is a different tweet
tmls

## users data for realDonaldTrump is also retrieved
users_data(tmls)

## count observations for each timeline
table(tmls$screen_name)

## plot the frequency of tweets for each user over time
tmls %>%
  #dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


## WRITING INITIAL SET TO LOCAL DB ##
dbWriteTable(conLoc, "Twitter_Timelines", data.frame(tmls),append = TRUE)
dbDisconnect(conLoc)
rm(Handle.list, HFTC.df, conLoc,tmls,sql,i, Keywords,end.date, start.date, TwitterHandles,TwitterSearchTerms,consumerKey, consumerSecret, count, foo, dupsBetweenGroups )

