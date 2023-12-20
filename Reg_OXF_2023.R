#    REGRESSION FOR 2023 SENTIMENT SCORE      #


# FOR THIS REGRESSION, THE 2023 DATA WILL BE USED

library(ggplot2)
library(dplyr)
library(countrycode)


setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis")

#Start by unbundling g52_df

names(g52_df)[names(g52_df) == "CountryCode"] <- "Country"

#Merging with AI_Index

Oxf_g52_AI <- full_join(g52_df,Oxf_AI_2023)

#Remove NA
Oxf_g52_AI<- na.omit(Oxf_g52_AI)


#Merge g52 with the original sentiment score

#Rename column in sentiment_score2023 !!!!!!!!!!!!!!!(This has a permanent effect. If you do the other regression, this will be messed up!!!!!)!!!!
names(sentiment_scores2023)[names(sentiment_scores2023) == "CountryCode"] <- "Country"

#Join tables

Oxf_sentiment_scores_2023 <- full_join(Oxf_g52_AI,sentiment_scores2023)

#Remove file column
Oxf_sentiment_scores_2023$file <- NULL

#Add all the neat little scores together to make one observation per country

Oxf_sentiment_scores_2023<- Oxf_sentiment_scores_2023 %>%
  group_by(Country) %>%
  summarise(total_score = sum(score, na.rm = TRUE))


#Join with AI index again
Oxf_sentiment_scores_2023 <- left_join(Oxf_sentiment_scores_2023,Oxf_AI_2023)


#Remove all 
Oxf_sentiment_scores_2023<- na.omit(Oxf_sentiment_scores_2023)

#Regression


#model <- lm(y ~ x, data=data)

Oxf_reg_2023 <- lm( total_score ~ AI_Index, data = Oxf_sentiment_scores_2023)

summary(Oxf_reg_2023)
confint(Oxf_reg_2023)

Oxf_conf_intervals_2023 <- confint(Oxf_reg_2023)
#view(Oxf_conf_intervals_2023)

#Scatterplot
ggplot(Oxf_sentiment_scores_2023, aes(x=AI_Index, y=total_score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="CCW Governemtnal Expert (2023, Unbundled)", x="AI Score", y="Sentiment")



############### Control Variables

Oxf_mult_var_reg_2023 <-full_join(Oxf_sentiment_scores_2023,joined_multivariate_2022)
Oxf_mult_var_reg_2023 <- na.omit(Oxf_mult_var_reg_2023)



Oxf_mult_var_reg_2023_lm <- lm(total_score ~ AI_Index + Military + NY.GDP.MKTP.PP.CD + Scaled_HCI, data = Oxf_mult_var_reg_2023)
summary(Oxf_mult_var_reg_2023_lm)

Oxf_Vif_2023 <- vif(Oxf_mult_var_reg_2023_lm)
print(Oxf_Vif_2023)



########################### Colored graph



# Create new columns for color and size, and a label for the countries
Oxf_sentiment_scores_2023 <- Oxf_sentiment_scores_2023 %>%
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
ggplot(Oxf_sentiment_scores_2023, aes(x = AI_Index, y = total_score)) +
  geom_point(aes(color = color, size = size)) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3) +  # Add labels with ggrepel for better placement
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Single regression line
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "grey" = "grey")) +
  labs(title = "CCW Governmental Expert 2023") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend for size
