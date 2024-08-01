# Load necessary library

library(dplyr)

# Read the dataset
data <- read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Northeastern - Analytics\\ALY 6150 Healthcare Data\\assignment2a.csv")

# Logistic regression for the overall OR
overall_model <- glm(cvd ~ obesity, data = data, family = binomial)
summary(overall_model)

# Calculate the Odds Ratio (OR)
exp(coef(overall_model))

# Logistic regression stratified by age (young)
young_data <- data %>% filter(age == 0)
young_model <- glm(cvd ~ obesity, data = young_data, family = binomial)
summary(young_model)

# Calculate the Odds Ratio (OR) for young
exp(coef(young_model))

# Logistic regression stratified by age (old)
old_data <- data %>% filter(age == 1)
old_model <- glm(cvd ~ obesity, data = old_data, family = binomial)
summary(old_model)

# Calculate the Odds Ratio (OR) for old
exp(coef(old_model))

#1 B
# Load the dataset from CSV
df <- read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Northeastern - Analytics\\ALY 6150 Healthcare Data\\assignment2a.csv")

# Check the structure of the dataset
str(df)

# Calculate crude odds ratio
mod1 <- glm(cvd ~ obesity, data = df, family = binomial)
summary(mod1)
exp(coef(mod1))

# Calculate stratified odds ratio for young
mod1_young <- glm(cvd ~ obesity, data = df[df$age == 0, ], family = binomial)
summary(mod1_young)
exp(coef(mod1_young))

# Calculate stratified odds ratio for old
mod1_old <- glm(cvd ~ obesity, data = df[df$age == 1, ], family = binomial)
summary(mod1_old)
exp(coef(mod1_old))

# Check associations between age and CVD
mod_age_cvd <- glm(cvd ~ age, data = df, family = binomial)
summary(mod_age_cvd)
exp(coef(mod_age_cvd))

# Check associations between age and obesity
mod_age_obesity <- glm(obesity ~ age, data = df, family = binomial)
summary(mod_age_obesity)
exp(coef(mod_age_obesity))

# Adjusted for age
mod2 <- glm(cvd ~ obesity + age, data = df, family = binomial)
summary(mod2)
exp(coef(mod2))

#part 3 
# Load the epiR library
install.packages("epiR")
library(epiR)

# Create a 2x2 table with the counts
TD_table <- matrix(c(8, 500, 2, 9490), nrow = 2, byrow = TRUE)

# Convert the matrix to a table
TD_table <- as.table(TD_table)

# Calculate sensitivity and specificity
epi.tests(TD_table, conf.level = 0.95)


