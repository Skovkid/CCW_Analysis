library(syuzhet)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis/CCW_REV_6/EN/TXT/")

# Assuming your text files are in a directory called "text_files"
files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty data frame to store sentiment scores
sentiment_scores2021 <- data.frame(file = character(), score = numeric(), stringsAsFactors = FALSE)

for (file in files) {
  # Read the text content from each file
  text_content <- readLines(file)
  
  # Calculate sentiment using syuzhet
  sentiment2021 <- get_sentiment(text_content, method = "syuzhet")
  
  # Sum up the sentiment scores for the file
  total_sentiment2021 <- sum(sentiment2021)
  
  # Add the file and its total sentiment score to the data frame
  sentiment_scores2021 <- rbind(sentiment_scores2021, data.frame(file = basename(file), score = total_sentiment2021))
}

# Rank the files by sentiment score
sentiment_scores2021 <- sentiment_scores2021 %>%
  arrange(score)

# Plot the data
ggplot(sentiment_scores2021, aes(x = reorder(file, score), y = score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "File", y = "Sentiment Score", title = "Sentiment Scores of The 6th Review")





###################################### Step 1 Data cleaning #####



# Extracting country names from the file names
sentiment_scores2021$Country <- gsub("_.+$", "", sentiment_scores2021$file) # Remove everything after the underscore
sentiment_scores2021$Country <- gsub("\\d", "", sentiment_scores2021$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores2021$Country <- countrycode(sentiment_scores2021$Country, "country.name", "iso2c")

# Renaming the column
sentiment_scores2021 <- rename(sentiment_scores2021, Country = file)

# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores2021.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores2021$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores2021$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores2021$Country[na_rows[na_files == "Group_of_13.txt"]] <- "G13"
sentiment_scores2021$Country[na_rows[na_files == "SouthAfrica.txt"]] <- "SA"
sentiment_scores2021$Country[na_rows[na_files == "EU.txt"]] <- "EU"


# Define the vector of 'G13' full country names
g13_countries <- c("Argentina","Costa Rica", "Ecuador", "El Salvador", "Kazakhstan", "Nigeria", "Panama", "Peru",
                   "Philippines", "Sierra Leone", "State of Palestine", "Uruguay", "Guatemala")

# Convert the country names to ISO country codes
g13_country_codes <- countrycode(g13_countries, "country.name", "iso2c")

# Create a dataframe with these codes
g13_df <- data.frame(CountryCode = g13_country_codes)

g13_df$score <- 8.15

sentiment_scores2021 <- left_join(sentiment_scores2021,g13_df)

# Assuming your dataframe is called df and you have the columns 'Country' and 'CountryCode'

# Replace 'Country' with 'CountryCode' where 'Country' is "G13"
sentiment_scores2021$Country[sentiment_scores2021$Country == "G13"] <- as.character(sentiment_scores2021$CountryCode[sentiment_scores2021$Country == "G13"])
#Drop Country code
sentiment_scores2021$CountryCode <- NULL  # This will remove the column 'CountryCode' from the dataframe
sentiment_scores2021$file <- NULL

#Let's make sentiment analysis of all the EU countries
EU_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
                  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
                  "PL", "PT", "RO", "SK", "SI", "ES", "SE")

EU_df <- data.frame(Country = EU_countries)
EU_df$score <- 67.20


#Now we add only the missing values, ie, the ones who hadn't said anything individually


# Keep all rows from sentiment_scores2021 and then add the missing countries from EU_df
missing_countries <- anti_join(EU_df, sentiment_scores2021, by = "Country")
sentiment_scores2021_EU <- bind_rows(sentiment_scores2021, missing_countries)



# Create a bar chart
ggplot(data = sentiment_scores2021_EU, aes(x = Country, y = score)) + 
  geom_bar(stat = "identity") +  # Use stat="identity" to use the actual values in the 'score' column
  theme_minimal() +  # Optional: Use a minimal theme for a nice appearance
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate the x axis labels to make them readable
  labs(x = "Country", y = "Sentiment Score", title = "Sentiment Scores CCW 6th review (Unbundled)")  # Label the axes and the chart

