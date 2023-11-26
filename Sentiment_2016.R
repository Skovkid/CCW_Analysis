library(syuzhet)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis/CCW_REV_5/EN/General statements/TXT/")

# Assuming your text files are in the current working directory
files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# Initialize two empty data frames: one for sentiment scores, another for files without keywords
sentiment_scores_2016 <- data.frame(file = character(), score = numeric(), stringsAsFactors = FALSE)
files_without_keywords <- character()

keywords <- c("(LAWS)","LAWS", "Lethal autonomous weapons system","Lethal Autonomous Weapons Systems","autonomous weapon system","lethalautonomous weapons", "Autonomous Weapons")

for (file in files) {
  # Read the text content from each file
  text_content <- tolower(paste(readLines(file), collapse = " "))
  
  # Check if text contains any of the keywords
  if (any(str_detect(text_content, regex(keywords, ignore_case = TRUE)))) {
    # Calculate sentiment using syuzhet
    sentiment_2016 <- get_sentiment(text_content, method = "syuzhet")
    
    # Sum up the sentiment scores for the file
    total_sentiment_2016 <- sum(sentiment_2016)
    
    # Add the file and its total sentiment score to the data frame
    sentiment_scores_2016 <- rbind(sentiment_scores_2016, data.frame(file = basename(file), score = total_sentiment_2016))
  } else {
    # Add the file to the list of files without keywords
    files_without_keywords <- c(files_without_keywords, basename(file))
  }
}

# Rank the files by sentiment score
sentiment_scores_2016 <- sentiment_scores_2016 %>%
  arrange(desc(score))

# Plot the data with a bar chart
ggplot(sentiment_scores_2016, aes(x = reorder(file, score), y = score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "File", y = "Sentiment Score", title = "Sentiment Scores for LAWS")

# Print out the names of the files that didn't mention any of the keywords
print(files_without_keywords)

