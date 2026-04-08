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
  return(data)
}

word_analysis<-function(toot_data, emotion) {

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
