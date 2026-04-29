suppressPackageStartupMessages({
  library(sentimentr)
  library(tidytext)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(argparse)
  library(ggpubr)
  library(ggplot2)
})

load_data <- function(filename) {
  # Load the file into a variable called data
  # Force the id column to be type character
  data <- read.csv(filename, colClasses = c("id" = "character"))
  # Clean up the HTML tags in the 'content' column
  data$content <- gsub("<[^>]+>", "", data$content)
  # Update the 'created_at' column date format
  data$created_at <- ymd_hms(data$created_at)
  # Only get the English toots
  data <- data |> filter(.data$language == "en")
}

word_analysis <- function(toot_data, emotion) {
  nrc <- get_sentiments("nrc")
  nrc <- nrc |> filter(.data$sentiment == emotion, )

  words <- toot_data |>
    unnest_tokens(.data$word, .data$content) |>
    inner_join(nrc, by = "word")

  word_counts <- words |>
    count(.data$word, sort = TRUE) |>
    slice_head(n = 10)

  result <- words |>
    # Re-attach the 'n' counts
    inner_join(word_counts, by = "word") |>
    # RESTORE THE COLUMNS
    select("id", "created_at", "sentiment", "word", "n") |>
    # Keep only 1 row per word
    distinct(.data$word, .keep_all = TRUE) |>
    # Sort by highest count
    arrange(desc(.data$n))

  result
}

sentiment_analysis <- function(toot_data) {
  words <- toot_data |> unnest_tokens(.data$word, .data$content)

  afinn <- get_sentiments("afinn")
  afinn_result <- words |>
    inner_join(afinn, by = "word") |>
    group_by(.data$id, .data$created_at) |>
    summarise(sentiment = sum(.data$value), .groups = "drop") |>
    arrange(desc(.data$created_at)) |>
    mutate(method = "afinn")

  nrc <- get_sentiments("nrc") |>
    filter(.data$sentiment %in% c("positive", "negative"))
  nrc_result <- words |>
    inner_join(nrc, by = "word", relationship = "many-to-many") |>
    mutate(score = ifelse(sentiment == "positive", 1, -1)) |>
    group_by(.data$id, .data$created_at) |>
    summarise(sentiment = sum(.data$score), .groups = "drop") |>
    arrange(desc(.data$created_at)) |>
    mutate(method = "nrc")

  bing <- get_sentiments("bing")
  bing_result <- words |>
    inner_join(bing, by = "word") |>
    mutate(score = ifelse(.data$sentiment == "positive", 1, -1)) |>
    group_by(.data$id, .data$created_at) |>
    summarise(sentiment = sum(.data$score), .groups = "drop") |>
    arrange(desc(.data$created_at)) |>
    mutate(method = "bing")

  bind_rows(afinn_result, nrc_result, bing_result)

}

main <- function(args) {
  toot_data <- load_data(args$filename)
  message(paste("Running analysis for emotion:", args$emotion))
  word_data <- word_analysis(toot_data, args$emotion)
  print(word_data)
  sentiment_data <- sentiment_analysis(toot_data)
  plot_file <- args$output
  if (is.null(plot_file)) {
    plot_file <- args$plot
  }
  if (!is.null(plot_file)) {
    p <- ggplot(
      sentiment_data,
      aes(x = .data$created_at, y = .data$sentiment, colour = .data$method)
    ) +
      geom_point() +
      labs(
        title = paste("Sentiment Over Time (Focus:", args$emotion, ")"),
        x = "Time",
        y = "Sentiment Score"
      )
    ggsave(plot_file, plot = p)
  }
}


if (sys.nframe() == 0) {

  # main program, called via Rscript
  parser <- ArgumentParser(
    prog = "Sentiment Analysis"
  )
  parser$add_argument("filename",
                      help = "the file to read the toots from")
  parser$add_argument("--emotion",
                      default = "anger",
                      choices = c("anger", "anticipation", "disgust", "fear",
                                  "joy", "sadness", "surprise", "trust",
                                  "negative", "positive"),
                      help = "The NRC emotion category to analyze")
  parser$add_argument("-v", "--verbose",
                      action = "store_true",
                      help = "Print progress")
  parser$add_argument("-p", "--plot",
                      help = "Plot something. Give the filename")

  args <- parser$parse_args()
  # This checks if you are in RStudio; if so, it asks you for the emotion
  if (interactive() && args$emotion == "anger") {
    cat("Choices: anger, anticipation, disgust, fear, joy, sadness, trust\n")
    user_input <- readline(prompt = "Enter emotion (Enter for 'anger'): ")
    if (user_input != "") {
      args$emotion <- user_input
    }
  }
  main(args)
}
