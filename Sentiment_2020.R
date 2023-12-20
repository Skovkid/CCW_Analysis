library(syuzhet)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis/CCW_EXP_REV_2020/EN/TXT/")

# Assuming your text files are in a directory called "text_files"
files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store sentiment scores
sentiment_scores2020 <- data.frame(file = character(), score = numeric(), stringsAsFactors = FALSE)

for (file in files) {
  # Read the text content from each file
  text_content <- readLines(file)
  
  # Calculate sentiment using syuzhet
  sentiment2020 <- get_sentiment(text_content, method = "syuzhet")
  
  # Sum up the sentiment scores for the file
  total_sentiment2020 <- sum(sentiment2020)
  
  # Add the file and its total sentiment score to the data frame
  sentiment_scores2020 <- rbind(sentiment_scores2020, data.frame(file = basename(file), score = total_sentiment2020))
}

# Rank the files by sentiment score
sentiment_scores2020 <- sentiment_scores2020 %>%
  arrange(score)

# Plot the data
ggplot(sentiment_scores2020, aes(x = reorder(file, score), y = score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "File", y = "Sentiment Score", title = "Sentiment Of Governmental expert review in 2020")







### Step 1 Data cleaning #####



# Extracting country names from the file names
sentiment_scores2020$Country <- gsub("_.+$", "", sentiment_scores2020$file) # Remove everything after the underscore
sentiment_scores2020$Country <- gsub("\\d", "", sentiment_scores2020$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores2020$Country <- countrycode(sentiment_scores2020$Country, "country.name", "iso2c")


# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores2020.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores2020$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores2020$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores2020$Country[na_rows[na_files == "United_States_1.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "United_States_2.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "United_States_3.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "Costa_Rica.txt"]] <- "CR"
sentiment_scores2020$Country[na_rows[na_files == "UK.txt"]] <- "GB"
sentiment_scores2020$Country[na_rows[na_files == "Multiple states.txt"]] <- "MS"



#Removing unnecessary empty txt file
sentiment_scores2020 <- na.omit(sentiment_scores2020)



#Combining the scores

sentiment_scores2020<- sentiment_scores2020 %>%
  group_by(Country) %>%
  summarise(total_score = sum(score, na.rm = TRUE))  # Sum scores, removing NA values


