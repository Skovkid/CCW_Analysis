#    REGRESSION FOR 2021 SENTIMENT SCORE      #


# FOR THIS REGRESSION, THE 2021 DATA WILL BE USED

library(ggplot2)
library(dplyr)
library(countrycode)


setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis")

#Merge sentiment score with AI index

Oxf_sentiment_scores_2021 <- full_join(Oxf_AI_2021,sentiment_scores2021_EU)

Oxf_sentiment_scores_2021 <- na.omit(Oxf_sentiment_scores_2021)



#Regression


#model <- lm(y ~ x, data=data)

Oxf_reg_2021 <- lm(score ~ AI_Index, data = Oxf_sentiment_scores_2021)

summary(Oxf_reg_2021)
confint(Oxf_reg_2021)

Oxf_conf_intervals_2021 <- confint(Oxf_reg_2021)
view(Oxf_conf_intervals_2021)

#Scatterplot
ggplot(Oxf_sentiment_scores_2021, aes(x=AI_Index, y=score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="CCW 6th Review (2021, Unbundled)", x="AI Score", y="Sentiment")



########### Control Variables

Oxf_mult_var_reg_2021 <-full_join(Oxf_sentiment_scores_2021,joined_multivariate_2021)
Oxf_mult_var_reg_2021 <- na.omit(Oxf_mult_var_reg_2021)

Oxf_mult_var_reg_2021_lm <- lm(score ~ AI_Index + + Military + NY.GDP.MKTP.PP.CD + Scaled_HCI, data = Oxf_mult_var_reg_2021)
summary(Oxf_mult_var_reg_2021_lm)

Oxf_Vif_2021 <- vif(Oxf_mult_var_reg_2021_lm)
print(Oxf_Vif_2021)


############# Colored graph

# Create new columns for color and size, and a label for the countries
Oxf_sentiment_scores_2021 <- Oxf_sentiment_scores_2021 %>%
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
ggplot(Oxf_sentiment_scores_2021, aes(x = AI_Index, y = score)) +
  geom_point(aes(color = color, size = size)) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3) +  # Add labels with ggrepel for better placement
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Single regression line
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "grey" = "grey")) +
  labs(title = "CCW 6th review Unbundled") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend for size
