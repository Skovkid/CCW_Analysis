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
  labs(x = "File", y = "Sentiment Score", title = "Sentiment Scores for CCW Reeview in 2016")

# Print out the names of the files that didn't mention any of the keywords
print(files_without_keywords)




########################################## Data cleaning############################


# Extracting country names from the file names
sentiment_scores_2016$Country <- gsub("_.+$", "", sentiment_scores_2016$file) # Remove everything after the underscore
sentiment_scores_2016$Country <- gsub("\\d", "", sentiment_scores_2016$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores_2016$Country <- countrycode(sentiment_scores_2016$Country, "country.name", "iso2c")

# Renaming the column
sentiment_scores_2016 <- rename(sentiment_scores_2016, Country = file)

# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores_2016.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores_2016$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores_2016$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores_2016$Country[na_rows[na_files == "EU.txt"]] <- "EU"

sentiment_scores2021$CountryCode <- NULL  # This will remove the column 'CountryCode' from the dataframe
sentiment_scores2021$file <- NULL


#Let's make sentiment analysis of all the EU countries
EU_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
                  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
                  "PL", "PT", "RO", "SK", "SI", "ES", "SE")

EU_df <- data.frame(Country = EU_countries)
EU_df$score <- 30.30
sentiment_scores_2016$CountryCode <- NULL  # This will remove the column 'CountryCode' from the dataframe
sentiment_scores_2016$file <- NULL

#Now we add only the missing values, ie, the ones who hadn't said anything individually


# Keep all rows from sentiment_scores_2016 and then add the missing countries from EU_df
missing_countries <- anti_join(EU_df, sentiment_scores_2016, by = "Country")
sentiment_scores_2016_EU <- bind_rows(sentiment_scores_2016, missing_countries)



# Create a bar chart
ggplot(data = sentiment_scores_2016_EU, aes(x = Country, y = score)) + 
  geom_bar(stat = "identity") +  # Use stat="identity" to use the actual values in the 'score' column
  theme_minimal() +  # Optional: Use a minimal theme for a nice appearance
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate the x axis labels to make them readable
  labs(x = "Country", y = "Sentiment Score", title = "Sentiment Scores CCW 5th review (Unbundled)")  # Label the axes and the chart

