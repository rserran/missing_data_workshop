# Replicating Python missingno heatmap visualization

# load packages
suppressMessages(library(tidyverse))
library(naniar)
library(visdat)
library(heatmaply)
library(DataExplorer)
library(rlang)

# read Titanic dataset
df <- read_csv('./titanic.csv')

df |> 
     glimpse()

skimr::skim(df)

# plot_missing
df |> 
     plot_missing()

# verify if missing mechanism is MCAR
mcar_test(df)

# Since p value is < 0.05, then we reject the null hypothesis (data is MCAR) for 
# the alternate hypothesis (data is not MCAR)

# vis_miss()
df |> 
     vis_miss()

# heatmap
set.seed(2341)
df |> 
     slice_sample(prop = 0.3) |> 
     heatmaply_na(
          showticklabels = c(TRUE, FALSE),
          k_col = 3,
          k_row = 3
     )

# upset plot
df |> 
     gg_miss_upset()

# gg_miss_var
df |> 
     select(name, age, fare, embarked) |> 
     mutate(title = name %>% 
                 str_extract(pattern = '([A-Za-z]+)\\.') %>% 
                 str_replace('.$', '') |> 
                 str_trim()
            ) |> 
     select(-name) |> 
     gg_miss_var(
          facet = title, 
          show_pct = TRUE
          )

# gg_miss_fct
df_title <- df |> 
     mutate(title = name %>% 
                 str_extract(pattern = '([A-Za-z]+)\\.') %>% 
                 str_replace('.$', '') |> 
                 str_trim(), 
            title = title %>% as.factor
     ) |> 
     select(-name)

# gg_miss_fct with survived
df_title |> 
     select(-cabin, -body, -boat, -home.dest) |> 
     gg_miss_fct(
          fct = survived
     ) + 
     labs(title = 'Missing values in Titanic by Passenger Survival')

# gg_miss_fct with title
df_title |> 
     select(-cabin, -body, -boat, -home.dest) |> 
     gg_miss_fct(
          fct = title
     ) + 
     labs(title = 'Missing values in Titanic by Passenger Title')

# gg_miss_fct with sex
df_title |> 
     select(-cabin, -body, -boat, -home.dest) |> 
     gg_miss_fct(
          fct = sex
     ) + 
     labs(title = 'Missing values in Titanic by Passenger Gender')

# t-test missingness impact function
# Source: https://njtierney.github.io/talk-funfun-slc/#/so-eventually-i-wrote-a-function-for-my-missing-data-problem

missingness_impact <- function(when_missing, is_different){
     when_missing_index <- which(is.na(when_missing))
     when_complete_index <- which(!is.na(when_missing))
     
     is_different_miss <- is_different[when_missing_index]
     is_different_complete <- is_different[when_complete_index]
     
     result <- t.test(is_different_miss, y = is_different_complete)
     
     result
     
}

# when_missing = df_title$age, is_different = df_title$survived
missingness_impact(
     when_missing = df_title$age, 
     is_different = df_title$survived
)

# smae function with tidyverse syntax
missingness_impact <- function(data, when_missing, is_different) {
     when_missing <- enquo(when_missing)
     is_different <- enquo(is_different)
     
     data_miss <- data %>% filter(is.na(!!when_missing))
     data_complete <- data %>% filter(!is.na(!!when_missing))
     
     t.test(
          data_miss %>% pull(!!is_different),
          data_complete %>% pull(!!is_different)
     )
}

df_title |> 
     missingness_impact(
          when_missing = age, 
          is_different = survived
     )

df_title <- df_title |> 
     add_label_missings(
          age, 
          fare, 
          missing = 1, 
          complete = 0) |> 
     rename(age_missing = any_missing)

df_title_imp <- df_title |> 
     select(-survived, -cabin, -body, -boat, -home.dest, -age_missing) |> 
     missRanger::missRanger(seed = 4354)

summary(df_title$age)

summary(df_title_imp$age)

df_title_imp |> 
     glimpse()

# add survived and age_missing columns
df_title_imp$age_missing <- df_title$age_missing
df_title_imp$survived <- df_title$survived

df_title_imp |> 
     plot_correlation()

# correlation funnel
library(correlationfunnel)

binarized_tbl <- df_title_imp |> 
     binarize()

binarized_tbl |> 
     glimpse()

corr_tbl <- binarized_tbl |>
     correlate(survived__1)

corr_tbl |> 
     plot_correlation_funnel(interactive = TRUE)
