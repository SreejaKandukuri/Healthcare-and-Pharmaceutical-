library(dplyr)
library(readr)
library(psych)

# Read the dataset
df<- read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Northeastern - Analytics\\ALY 6150 Healthcare Data\\EMRsample2022.csv")


# Display descriptive statistics
describe(df)

#EDA
#age distribution
ggplot(df, aes(x = Age)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Age", x = "Age Group", y = "Count")

#AGE AND ILLNESS
ggplot(df, aes(x = Age, fill = `Severity.of.Illness`)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of Severity of Illness by Age", x = "Age Group", y = "Severity.of.Illness")

#CORRELATION MATRIX 
library(ggplot2)
library(ggcorrplot)
# Select numeric variables
numeric_vars <- df %>% select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)


# Create a numeric Age variable by taking mid-points of age ranges
df <- df %>%
  mutate(Age_numeric = case_when(
    Age == "0-10" ~ 5,
    Age == "11-20" ~ 15,
    Age == "21-30" ~ 25,
    Age == "31-40" ~ 35,
    Age == "41-50" ~ 45,
    Age == "51-60" ~ 55,
    Age == "61-70" ~ 65,
    Age == "71-80" ~ 75,
    Age == "81-90" ~ 85,
    Age == "91-100" ~ 95,
    TRUE ~ NA_real_
  ))

# Convert Severity.of.Illness to a binary variable
df <- df %>%
  mutate(Severity_of_Illness_Extreme = ifelse(`Severity.of.Illness` == "Extreme", 1, 0))

# Logistic regression with Age as the predictor
logistic_model_age <- glm(Severity_of_Illness_Extreme ~ Age_numeric, data = df, family = binomial)

# Display the summary of the model
summary(logistic_model_age)

#3

# Logistic regression with Age and additional controls
logistic_model_controls <- glm(Severity_of_Illness_Extreme ~ Age_numeric + Hospital_code + `City_Code_Hospital` + Department + Ward_Type + Admission_Deposit, data = df, family = binomial)

# Display the summary of the model
summary(logistic_model_controls)

#4
# Load necessary libraries
library(caret)
library(e1071)
library(dplyr)
library(readr)

# Load the dataset

# Convert the age categories to numeric values by taking the midpoint of the age ranges
df <- df %>%
  mutate(Age_numeric = case_when(
    Age == "0-10" ~ 5,
    Age == "11-20" ~ 15,
    Age == "21-30" ~ 25,
    Age == "31-40" ~ 35,
    Age == "41-50" ~ 45,
    Age == "51-60" ~ 55,
    Age == "61-70" ~ 65,
    Age == "71-80" ~ 75,
    Age == "81-90" ~ 85,
    Age == "91-100" ~ 95,
    TRUE ~ NA_real_
  ))

# Convert Severity.of.Illness to a binary factor variable
df <- df %>%
  mutate(Severity_of_Illness_Extreme = factor(ifelse(`Severity.of.Illness` == "Extreme", "Yes", "No")))

# Prepare the data
set.seed(123)
index <- createDataPartition(df$Severity_of_Illness_Extreme, p = 0.7, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Train a logistic regression model
logistic_model_ml <- train(Severity_of_Illness_Extreme ~ Age_numeric + Hospital_code + `City_Code_Hospital` + Department + Ward_Type + Admission_Deposit,
                           data = train_data, method = "glm", family = "binomial")

# Predictions
predictions <- predict(logistic_model_ml, newdata = test_data)

# Convert predictions to factors to match the levels of the actual values
predictions <- factor(predictions, levels = levels(test_data$Severity_of_Illness_Extreme))

# Confusion matrix and other metrics
conf_matrix <- confusionMatrix(predictions, test_data$Severity_of_Illness_Extreme)

# Display the confusion matrix and other metrics
conf_matrix

#5

# Convert Length_of_stay to numeric by taking mid-points of ranges
df <- df %>%
  mutate(Length_of_stay_numeric = case_when(
    Length_of_stay == "0-10" ~ 5,
    Length_of_stay == "11-20" ~ 15,
    Length_of_stay == "21-30" ~ 25,
    Length_of_stay == "31-40" ~ 35,
    Length_of_stay == "41-50" ~ 45,
    Length_of_stay == "51-60" ~ 55,
    Length_of_stay == "61-70" ~ 65,
    Length_of_stay == "71-80" ~ 75,
    Length_of_stay == "81-90" ~ 85,
    Length_of_stay == "91-100" ~ 95,
    TRUE ~ NA_real_
  ))

# Logistic regression with LOS as the predictor
logistic_model_los <- glm(Severity_of_Illness_Extreme ~ Length_of_stay_numeric, data = df, family = binomial)

# Display the summary of the model
summary(logistic_model_los)


