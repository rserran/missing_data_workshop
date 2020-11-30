# Impute missing values (Titanic dataset)
# Using missRanger

# load libraries
suppressMessages(library(tidyverse))
library(VIM)
library(naniar)
theme_set(theme_light())

# read dataset
df <- read_csv("train.csv")

# look at dataset
skimr::skim(df)

head(df)

# MCAR test
set.seed(42)
df_100 <- sample_n(df, 100)

# Little MCAR test
BaylorEdPsych::LittleMCAR(df_100)

# another MCAR test for `Age`
# select numeric columns
df_num <- df %>% 
        select_if(is.numeric)

# convert `Sex` to 0 - Female, 1 - male
df_num$Sex <- ifelse(df$Sex == "male", 1, 0)

df_num %>% 
        select(-PassengerId) %>% 
        MissMech::TestMCARNormality()

# p-value < 0.05 reject null hypothesis (data is MCAR) in favor of alternate
# data is not MCAR

# visualize missing values
visdat::vis_dat(df, palette = "qual")

visdat::vis_miss(df)

# visualize missing values
a <- aggr(df, plot = FALSE)
plot(a, numbers = TRUE, prop = FALSE)

# visualize `age` missing values by `Survived`
df %>% 
     ggplot(aes(factor(Survived), Age)) + 
     geom_miss_point()

# visualize `age` missing values by `Pclass`
df %>% 
     ggplot(aes(factor(Pclass), Age)) + 
     geom_miss_point()

# visualize `age` missing values by `Sex`
df %>% 
     ggplot(aes(factor(Sex), Age)) + 
     geom_miss_point()

## Impute missing values
## =====================

# impute `Embarked` with mode
# create mode function
# my_mode <- function(x) { 
#      unique_x <- unique(x)
#      mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
#      mode
# }

# copy df to df_imp
df_imp <- df
# mode <- my_mode(df_imp$Embarked)
# df_imp$Embarked <- df_imp$Embarked %>% 
#      replace_na(mode)

# extract title from name (passenger name)
df_imp$Title <- str_extract(df_imp$Name, "(?<=\\s)[[:alpha:]]+(?=\\.)" )

# show title counts by sex
table(df_imp$Sex, df_imp$Title)

# titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# reassign mlle, ms, and mme accordingly
df_imp$Title[df_imp$Title == 'Mlle']  <- 'Miss' 
df_imp$Title[df_imp$Title == 'Ms']    <- 'Miss'
df_imp$Title[df_imp$Title == 'Mme']   <- 'Mrs' 
df_imp$Title[df_imp$Title %in% rare_title]  <- 'Rare Title'

# drop Name
df_imp$Name <- NULL

# impute `Age` and `Cabin` with missRanger
# create `Cabin_letter` from first cabin letter
df_imp$Cabin_letter <- substr(df_imp$Cabin, 1, 1)
df_imp$Cabin <- NULL

# drop PassengerID
df_imp$PassengerId <- NULL

# impute with missRanger
df_imp <- missRanger::missRanger(df_imp)

# verify missing values
colMeans(is.na(df_imp))

# impute quality
plot(density(df$Age, na.rm = TRUE), ylim = c(0, 0.05))
lines(density(df_imp$Age))
