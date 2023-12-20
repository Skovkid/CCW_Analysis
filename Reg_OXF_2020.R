#    REGRESSION FOR 2020 SENTIMENT SCORE      #


# FOR THIS REGRESSION, THE 2020 DATA WILL BE USED

library(ggplot2)
library(dplyr)
library(countrycode)


setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis")

#Prepare senitmentscore for merge

names(sentiment_scores2020)[names(sentiment_scores2020) == "CountryCode"] <- "Country"
names(sentiment_scores2020)[names(sentiment_scores2020) == "total_score"] <- "score"

#Merge sentiment score with AI index

Oxf_sentiment_scores_2020 <- full_join(Oxf_AI_2020,sentiment_scores2020)

#Assigning the multiple states data point with their average AI score (63.25)

Oxf_sentiment_scores_2020$AI_Index[is.na(Oxf_sentiment_scores_2020$AI_Index) & Oxf_sentiment_scores_2020$Country == "MS"] <- 63.25


Oxf_sentiment_scores_2020 <- na.omit(Oxf_sentiment_scores_2020)


##################################### Adding the nations that were apart of MS

# Specified countries
MS_countries <- c('Belgium', 'Chile', 'Ireland', 'Luxembourg', 'Mexico', 'New-Zealand')

# Assign a score of 83.00 to these countries
MS_scores <- rep(83.00, length(MS_countries))

# Create a dataframe
MS_countries <- data.frame(Country = MS_countries, Score = scores)
#ISOCODE
MS_countries$Country <- countrycode(MS_countries$Country, "country.name", "iso2c")

MS_countries <- left_join(MS_countries,Oxf_AI_2020)

names(MS_countries)[names(MS_countries) == "Score"] <- "score"

#Merge with oxf

Oxf_sentiment_scores_2020 <- full_join(Oxf_sentiment_scores_2020,MS_countries)

#######################################################################################




##############
#Regression


#model <- lm(y ~ x, data=data)

Oxf_reg_2020 <- lm(score ~ AI_Index, data = Oxf_sentiment_scores_2020)

summary(Oxf_reg_2020)
confint(Oxf_reg_2020)

Oxf_conf_intervals_2020 <- confint(Oxf_reg_2020)
view(Oxf_conf_intervals_2020)



#Scatterplot
ggplot(Oxf_sentiment_scores_2020, aes(x=AI_Index, y=score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="CCW Governemtnal Expert (2020, Unbundled)", x="AI Score", y="Sentiment")


############# Control variables

Oxf_mult_var_reg_2020 <-full_join(Oxf_sentiment_scores_2020,joined_multivariate_2020)
Oxf_mult_var_reg_2020 <- na.omit(Oxf_mult_var_reg_2020)

Oxf_mult_var_reg_2020_lm <- lm(score ~ AI_Index + Military + NY.GDP.MKTP.PP.CD + Scaled_HCI, data = Oxf_mult_var_reg_2020)
summary(Oxf_mult_var_reg_2020_lm)

Oxf_Vif_2020 <- vif(Oxf_mult_var_reg_2020_lm)
print(Oxf_Vif_2020)





########################### Colored graph



# Create new columns for color and size, and a label for the countries
Oxf_sentiment_scores_2020 <- Oxf_sentiment_scores_2020 %>%
  mutate(
    color = case_when(
      Country %in% g7_countries ~ "blue",
      Country %in% red_countries ~ "red",
      TRUE ~ "grey"
    ),
    size = case_when(
      Country %in% g7_countries ~ 5,
      Country %in% red_countries ~ 5,
      TRUE ~ 2
    ),
    label = if_else(Country %in% c(g7_countries, red_countries), as.character(Country), NA_character_)
  )


# Plot with color-blind friendly colors, different sizes, and labels for G7 and red countries
ggplot(Oxf_sentiment_scores_2020, aes(x = AI_Index, y = score)) +
  geom_point(aes(color = color, size = size)) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3) +  # Add labels with ggrepel for better placement
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Single regression line
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "grey" = "grey")) +
  labs(title = "CCW Governmental Expert 2020") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend for size

