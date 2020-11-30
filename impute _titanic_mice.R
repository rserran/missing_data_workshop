# Impute missing values (Titanic dataset)
# Using mice

# load libraries
suppressMessages(library(tidyverse))
library(mice)
theme_set(theme_light())

# read dataset
df_full <- read_csv("~/Downloads/titanic.csv")

# look at dataset
skimr::skim(df_full)

# drop unnecessary features (boat, home.dest, ) with high percentage of missing values
df_full <- df_full %>% 
     select(-boat, -body, -home.dest)

# visualize missing values
visdat::vis_dat(df_full, palette = "qual")

visdat::vis_miss(df)

# extract title from name (passenger name)
df_full$title <- str_extract(df_full$name, "(?<=\\s)[[:alpha:]]+(?=\\.)" )

# show title counts by sex
table(df_full$sex, df_full$title)

# titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# reassign mlle, ms, and mme accordingly
df_full$title[df_full$title == 'Mlle']  <- 'Miss' 
df_full$title[df_full$title == 'Ms']    <- 'Miss'
df_full$title[df_full$title == 'Mme']   <- 'Mrs' 
df_full$title[df_full$title %in% rare_title]  <- 'Rare Title'

# extract cabin first letter
df_full$deck <- substr(df_full$cabin, 1, 1)

# drop name and cabin
df_full <- df_full %>% 
     select(-name, -cabin)

# convert deck and embarked to factor type
df_full$deck <- as.factor(df_full$deck)
df_full$embarked <- as.factor(df_full$embarked)

# boxplot age by title
df_full %>% 
     ggplot(aes(title, age, fill = factor(title))) + 
     geom_boxplot() + 
     scale_fill_brewer(palette = "Set2")

## mice imputation
mice_imp <- mice(df_full, m = 10, maxit = 10, seed = 42)

summary(mice_imp)

# build regression model
model_fit <- with(data = mice_imp, expr = lm(age ~ title + sex + pclass + sibsp + parch))

# combining results from all models
pool_out <- pool(model_fit)

summary(pool_out)

# use complete to get imputed values from the 10th imputed dataset
mice_out <- complete(mice_imp, 10)

# imputation quality (age)
par(mfrow=c(1, 2))
hist(df_full$age, freq=F, main='Age: Original Data', 
     col='lightblue', ylim=c(0,0.04))
hist(mice_out$age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
par(mfrow=c(1, 1))

# density plot
plot(density(df_full$age, na.rm = TRUE), ylim = c(0, 0.05))
lines(density(mice_out$age))

# correlation matrix using DataExplorer
DataExplorer::plot_correlation(mice_out)
