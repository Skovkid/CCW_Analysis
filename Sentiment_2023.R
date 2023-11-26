library(syuzhet)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis/CCW_EXP_REV_2023/EN/GS/Session 2/TXT/")

# Assuming your text files are in a directory called "text_files"
files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store sentiment scores
sentiment_scores2023 <- data.frame(file = character(), score = numeric(), stringsAsFactors = FALSE)

for (file in files) {
  # Read the text content from each file
  text_content <- readLines(file)
  
  # Calculate sentiment using syuzhet
  sentiment2023 <- get_sentiment(text_content, method = "syuzhet")
  
  # Sum up the sentiment scores for the file
  total_sentiment2023 <- sum(sentiment2023)
  
  # Add the file and its total sentiment score to the data frame
  sentiment_scores2023 <- rbind(sentiment_scores2023, data.frame(file = basename(file), score = total_sentiment2023))
}

# Rank the files by sentiment score
sentiment_scores2023 <- sentiment_scores2023 %>%
  arrange(score)

# Plot the data
ggplot(sentiment_scores2023, aes(x = reorder(file, score), y = score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "File", y = "Sentiment Score", title = "Sentiment Analysis of Governmental expert review in 2023")

ggsave()
