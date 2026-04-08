suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(tidyr)
library(argparse)
library(ggpubr)
})



load_data <- function(filename) {
  #Load the file into a variable called data. force the id column to be type character
  data <- read.csv(filename, colClasses = c("id" = "character"))
  #Clean up the HTML tags in the 'content' column
  data$content <- gsub("<[^>]+>", "", data$content)
  #Update the 'created_at' column date format 
  data$created_at <- ymd_hms(data$created_at)
  #Only get the English toots
  data <- data[data$language == "en", ]
  #Return the data
  return(data)
}

word_analysis<-function(toot_data, emotion) {
  #Load the NRC lexicon
  nrc <- get_sentiments("nrc")
  #Filter the sentiments to emotions
  nrc <- nrc[nrc$sentiment == emotion, ]
  #Tokenise the toot text into individual words.
  words <- toot_data |> unnest_tokens(word, content)
  #Use an inner_join to keep only the words that appear in nrc and toots
  emotion_words <- words |> inner_join(nrc, by = "word")
  #Sorty by the mostcommon and then select the top 10
  top_words <- emotion_words |>
     group_by(word) |>
     summarise(n = n()) |>
     arrange(desc(n)) |>
     slice_head(n = 10)

  result <- top_words %>% inner_join(emotion_words, by = "word")
    return(result)
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
