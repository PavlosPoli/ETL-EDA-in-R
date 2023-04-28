# Load required packages
library(tidyverse)
library(janitor)

# Set the environment
set.seed(1234)
rm(list = ls()) # Clear environment
setwd("E:/backup17092018/Myappdir/Myprojects/Data Science/R Scripts/")


# -------------------------------------------------------- #
# ETL process
# -------------------------------------------------------- #


# Load dataset
bank <- read_csv2("bank-full.csv", col_names = TRUE)

dim(bank)
glimpse(bank)
View(bank)
head(bank)
tail(bank)

names(bank)
length(bank)
bank <- clean_names(bank)
bank <- remove_empty(bank, c("rows", "cols"), quiet = FALSE)
bank %>%
   mutate_if(is.character, trimws)

length(bank$y)
class(bank$y)
unique(bank$y)
# Repeat for every categorical field

# Shows the frequency of values for a field
bank %>%
  select(y) %>%
  count(y) %>%
  arrange(desc(n)) %>%
  View()
# Repeat for every categorical field

# Check for NULLS
sapply(bank, function(x) sum(is.na(x)))

# Show the records with NULL values
bank[is.na(bank$job), ]

# Calculate the proportion of NULLs
sum(is.na(bank$job))/nrow(bank)

# Load data with non-null values to a new dataset variable
bank_clean <- bank[complete.cases(bank), ]

# Group age function
group_age <- function(age) {
  if (age >= 0 & age <= 12) {return('0-12 Years')}
  else if(age > 12 & age <= 24) {return('12-24 Years')}
  else if (age > 24 & age <= 48) {return('24-48 Years')}
  else if (age > 48 & age <=60) {return('48-60 Years')}
  else if (age > 60) {return('> 60 Years')}
}

# Create a new field (age_group) 
bank_clean$age_group <- sapply(bank_clean$age, group_age)
bank_clean$age_group <- as.factor(bank_clean$age_group)
View(bank_clean)


# -------------------------------------------------------- #
# EDA process
# -------------------------------------------------------- #


# Descriptive statistics for numerical fields
class(bank_clean$age)
summary(bank_clean$age)
boxplot(bank_clean$age)
hist(bank_clean$age)
plot(density(bank_clean$age))
# Repeat for every numerical field

# Handle outliers

# Get Mean and Standard deviation
mean = mean(bank_clean$age)
std = sd(bank_clean$age)

# Get threshold values for outliers
Tmin = mean - (3 * std)
Tmax = mean + (3 * std)

# Find outlier
bank_clean$age[which(bank_clean$age < Tmin | bank_clean$age > Tmax)]

# Frequencies for categorical fields
table(bank_clean$y)
table(bank_clean$job)
table(bank_clean$marital)
# Repeat for every categorical field

