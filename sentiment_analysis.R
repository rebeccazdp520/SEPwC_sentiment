suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(tidyr)
library(argparse)
library(ggpubr)
})

#Define global variables
utils::globalVariables(c(
  "language", "word", "content", "id", "created_at",
  "sentiment", "score", "value", "method", "n"
))


load_data<-function(filename) {
  #Read the csv file into the variable "data"
  data <- read.csv(filename)

  #Remove the HTML formatting from the content column
  data$content <- gsub("<[^>]+>", "", data$content)

  #Update the date format to a compatible format
  data$created_at <- ymd_hms(data$created_at)

  #Only get the English toots and then update the data
  data <- data %>% filter(language == "en")
  return()
}

word_analysis<-function(toot_data, emotion) {
  #Get the NRC sentiment lexicon
  nrc <- get_sentiments("nrc")
  #Only load the emotion sentiment
  nrc <- nrc %>% filter(sentiment == emotion)
  #Split all words into separate lines and link to id
  words <- toot_data %>% unnest_tokens(word, content)
  #Join to the NRC Lexicon using `inner_join()` so that only words that appear in both lists are kept
  emotion_words <- words %>% inner_join(nrc, by = "word")
  #Count how many times each word appears and get a top ten
  top_words <- emotion_words %>%
    group_by(word) %>% #Group rows by word
    summarise(n = n()) %>% #Counts rows in each group
    arrange(desc(n)) %>% #Sorts descending
    slice_head(n = 10) #Selects top ten

  #Rejoin with the original data to bring back the `id`, `created_at`, and `sentiment` columns
  result <- top_words %>% inner_join(emotion_words, by = "word")
  return()
}

sentiment_analysis<-function(toot_data) {

  return()

}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}
