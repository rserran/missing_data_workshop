# Titanic dataset - missRanger/mice imputation
# Source: https://cran.r-project.org/web/packages/missRanger/vignettes/multiple_imputation.html

# load packages
suppressMessages(library(tidyverse))
library(missRanger)
library(mice)
theme_set(theme_bw())

# read df_imp_partial.csv
df <- read_csv('./df_imp_partial.csv') %>% 
     select(-1) %>% 
     mutate_if(is.character, as.factor) %>% 
     mutate(pclass = pclass %>% as.factor, 
            survived = survived %>% as.factor)

df

skimr::skim(df)

# Generate 20 complete data sets
set.seed(2020)
filled <- replicate(
     30, 
     missRanger(df, verbose = 0, num.trees = 100, pmm.k = 5), 
     simplify = FALSE
)

filled

# Run a linear model for each of the completed data sets                          
models <- lapply(filled, function(x) lm(age ~ ., x))

# Pool the results by mice
summary(pooled_fit <- pool(models))

# read df_encoded_imp.csv
df_encoded_imp <- read_csv('./df_encoded_imp.csv') %>% 
     select(-age, -fare, -deck, -deck_imp) %>% 
     mutate(
          pclass = pclass %>% as.factor, 
          survived = survived %>% as.factor, 
          deck_imp_value = deck_imp_value %>% as.factor
     )

df_encoded_imp

# compare with model on df_encoded+_imp (sklearn iterative imputer)
summary(lm(age_imp ~ ., data = df_encoded_imp))
