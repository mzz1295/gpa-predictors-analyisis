### Cleaning the data
### Yes or no columns will be assigned, Yes = 1, No = 0 
### For Sex, Male = 1, Female = 0 
### For Major Category, STEM = 1, Business = 2, Arts = 3, Medicine = 4
### For Reading Hours, 0 - 0.5 hours = 1; 0.5 - 1 hours = 2; 1-2 hours = 3; 2 - 4 hours = 4; 4 - 6 hours = 5;
student_data <- read.csv('Anonymous Survey (For College Students) 2.csv')
student_data$Timestamp <- NULL ### Timestamp column should be NULL
custom_labels <- c("plays_chess", "sex", "plays_sports", "reading_hours_per_week", "speak_more_than_one_language", "major_category", "plays_instrument","plays_videogames", "eats_fast_food", "drink_alchohol_regularly", "social_person", "GPA")
colnames(student_data) <- custom_labels
student_data$plays_chess <- as.numeric(student_data$plays_chess == "Yes")
student_data$sex <- as.numeric(student_data$sex == "Male")
student_data$plays_sports <- as.numeric(student_data$plays_sports == "Yes")
student_data$speak_more_than_one_language <- as.numeric(student_data$speak_more_than_one_language == "Yes")
student_data$plays_videogames <- as.numeric(student_data$plays_videogames == "Yes")
student_data$plays_instrument <- as.numeric(student_data$plays_instrument == "Yes")
student_data$eats_fast_food <- as.numeric(student_data$eats_fast_food == "Yes")
student_data$drink_alchohol_regularly <- as.numeric(student_data$drink_alchohol_regularly == "Yes")
student_data$social_person <- as.numeric(student_data$social_person == "Yes")
student_data$major_category <- ifelse(student_data$major_category == "STEM", 1,
                                      ifelse(student_data$major_category == "Business", 2,
                                      ifelse(student_data$major_category == "Arts", 3, 4)))
student_data$reading_hours_per_week <- ifelse(student_data$reading_hours_per_week == "0 - 0.50 hours", 1,
                                      ifelse(student_data$reading_hours_per_week == "0.50 - 1 hours", 2,
                                             ifelse(student_data$reading_hours_per_week == "1 - 2 hours", 3,
                                                    ifelse(student_data$reading_hours_per_week == "2 - 4 hours", 4, 5))))

##############
#### Bootstrap the data 
##############
install.packages("boot")
library(boot)
set.seed(123)
n_boot <- 57 
bootstrap_sample <- student_data[sample(1:nrow(student_data), n_boot, replace = TRUE), ]
library(dplyr)
View(bootstrap_sample)
student_data <- bind_rows(student_data, bootstrap_sample)
View(student_data)
###############                              
#### Correlation of all variables
###############

### Chess and GPA
chess_GPA <- student_data[, c("plays_chess", "GPA")]
corrplot(cor(chess_GPA), method = "square", tl.cex = 0.8)
### Instrument and GPA 
instrument_GPA <- student_data[, c("plays_instrument", "GPA")]
corrplot(cor(instrument_GPA), method = "square", tl.cex = 0.8)
### Alcohol Drinker and GPA
alcohol_GPA <- student_data[, c("drink_alchohol_regularly", "GPA")]
corrplot(cor(alcohol_GPA), method = "square", tl.cex = 0.8)
### Social Person and GPA
social_GPA <- student_data[, c("social_person", "GPA")]
corrplot(cor(social_GPA), method = "square", tl.cex = 0.8)
### Speak more than one langauge and GPA
language_GPA <- student_data[, c("speak_more_than_one_language", "GPA")]
corrplot(cor(language_GPA), method = "square", tl.cex = 0.8)
### Plays Video Games and GPA
games_GPA <- student_data[, c("plays_videogames", "GPA")]
corrplot(cor(games_GPA), method = "square", tl.cex = 0.8)
### Correlation between Reading Hours and GPA
reading_hours_GPA <- student_data[, c("reading_hours_per_week", "GPA")]
corrplot(cor(reading_hours_GPA), method = "square", tl.cex = 0.8)
### Major vs GPA distribution
library(ggplot2)
major_GPA <- student_data[, c("major_category", "GPA")]
cor_value <- cor(major_GPA$major_category, major_GPA$GPA)
ggplot(major_GPA, aes(x = factor(major_category), y = GPA,)) +
  geom_boxplot(fill = 'lightblue')+
  labs(title = "GPA distribution by major", x = "Major Category", y = "GPA")+
  theme_minimal()

### Bar chart analysis of Chess and GPA
library(dplyr)
gpa_summary <- student_data %>%
  group_by(plays_chess) %>%
  summarise(avg_GPA = mean(GPA, na.rm = TRUE))
gpa_summary$plays_chess <- factor(gpa_summary$plays_chess, levels = c(0,1), labels = c("No", "Yes"))
ggplot(gpa_summary, aes(x = plays_chess, y = avg_GPA, fill = plays_chess)) +
  geom_bar(stat = "identity") +
  labs(title = "Average GPA by Playing Chess", x = "Plays Chess", y = "Average GPA") +
  scale_fill_manual(values = c("tomato","steelblue"))+
  theme_minimal()
############
#### T test for all variables
############

### For Chess and GPA
t_test_result_chess <- t.test(GPA ~ plays_chess, data = student_data)
t_test_result_chess
### Instrument and GPA
t_test_results_instrument <- t.test(GPA ~ plays_instrument, data = student_data)
t_test_results_instrument
### Video Games and GPA 
t_test_results_games <- t.test(GPA ~ plays_videogames, data = student_data)
t_test_results_games
### Stem major vs non stem major and GPA
stem_nonstem <- student_data[, c("GPA", "major_category")]
stem_nonstem$STEM_vs_NonSTEM <- ifelse(stem_nonstem$major_category == 1, 1, 0)
View(stem_nonstem)
t_test_results_major <- t.test(GPA ~ STEM_vs_NonSTEM, data = stem_nonstem)
t_test_results_major
### Language and GPA
t_test_results_language <- t.test(GPA ~ speak_more_than_one_language, data = student_data)
t_test_results_language

##########
#### Regression Analysis
##########
### Reading hours predicts GPA 
model_readinghours <- lm(GPA ~ reading_hours_per_week, data = student_data)
summary(model_readinghours)
### If plays Chess
model_chess <- lm(GPA ~ plays_chess, data = student_data)
summary(model_chess)
### To add if student plays instrument as well 
model_chess_instrument <- lm(GPA ~ plays_chess + plays_instrument, data = student_data)
summary(model_chess_instrument)
### To add if speaks more than one language 
model_chess_language <- lm(GPA ~ plays_chess + plays_instrument + speak_more_than_one_language, data = student_data)
summary(model_chess_language)
### Seems to decrease the Adjusted R^2 
### What if we only look at major to GPA 
model_major <- lm(GPA ~ major_category, data = student_data)
summary(model_major)
### Lower than chess but higher than adding other variables 
### only looking at chess and playing video games 
model_videogames <- lm(GPA ~ plays_chess + plays_videogames, data = student_data)
summary(model_videogames)
### See if video games and Reading Hours gives a better R^2
model_readinghours_games <- lm(GPA ~ reading_hours_per_week + plays_videogames, data = student_data)
summary(model_readinghours_games)
### Now all three variables 
model_readinghours_games_chess <- lm(GPA ~ plays_chess + plays_videogames + reading_hours_per_week, data = student_data)
summary(model_readinghours_games_chess)
### What if you are a social person do you have a higher GPA?
model_social <- lm(GPA ~ social_person, data = student_data)
summary(model_social)
### Well if being social does not help, what about being drinking alchohol
model_alcohol <- lm(GPA ~ drink_alchohol_regularly, data = student_data)
summary(model_alcohol)
### Lets look at how well does the model best adjusted R^2 with playing video games and playing chess predicts GPA 
library(ggplot2)
ggplot(student_data, aes(x = predicted, y = residuals)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(title = "Residuals vs Predicted Values", x = "Predicted GPA", y = "Residuals") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  theme_minimal()
### Lets do a regression model and predict GPAs using readings hours 
student_data$predicted_gpa_reading <- predict(model_readinghours, newdata = student_data)
student_data$residuals_reading <- residuals(model_readinghours)
### Lets graph actual vs predicted
ggplot(student_data, aes(x = predicted_gpa_reading, y = residuals_reading)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(title = "Residuals vs Predicted Values", x = "Predicted GPA", y = "Residuals") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  theme_minimal()
### Now the regression model of predicting GPA using Chess, Reading Hours, and Video Games
student_data$predicted_gpa_reading_chess_games <- predict(model_readinghours_games_chess, newdata = student_data)
student_residuals_reading_chess_games <- residuals(model_readinghours_games_chess)    
### Now plot predicted vs actual 
ggplot(student_data, aes(x = predicted_gpa_reading_chess_games, y = student_residuals_reading_chess_games)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(title = "Residuals vs Predicted Values", x = "Predicted GPA", y = "Residuals") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  theme_minimal()
