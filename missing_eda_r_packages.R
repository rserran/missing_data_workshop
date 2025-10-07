# Replicating Python missingno heatmap visualization

# load packages
suppressMessages(library(tidyverse))
library(naniar)
library(visdat)
library(heatmaply)
library(DataExplorer)

# read Titanic dataset
df <- read_csv('./titanic.csv')

df |> 
     glimpse()

skimr::skim(df)

# plot_missing
df |> 
     plot_missing()

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

df_title |> 
     select(-cabin, -body, -boat, -home.dest) |> 
     gg_miss_fct(
          fct = title
     ) + 
     labs(title = 'Missing values in Titanic by Passenger Title')
