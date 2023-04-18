# MCAR Test using naniar package
# missRanger
# PIMA dataset

# import packages
suppressMessages(library(tidyverse))
library(naniar)
library(missRanger)
theme_set(theme_minimal())

# read dataset
library(mlbench)
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes

# first look at dataset
skimr::skim(df)

# no missing values (apparently)
# replace 0 with NAs for selected columns
df_NA <- df %>% 
     mutate_at(vars(glucose:mass),  ~replace(., . == 0, NA))

colMeans(is.na(df_NA))

# visualize missing data pattern
visdat::vis_dat(df_NA, palette = 'qual')

# visualize and cluster all of the missingness in the dataset
vis_miss(df_NA, cluster = TRUE)

# upset plot
gg_miss_upset(df_NA)

# use DataExplorer plot_missing
DataExplorer::plot_missing(df_NA)

# MCAR test
mcar_test(df_NA)

# impute missing values with missRanger
df_imp <- df_NA %>% 
     select(-diabetes) %>% 
     missRanger()

colMeans(is.na(df_imp))

# glucose
plot(density(df_NA$glucose, na.rm = TRUE))
lines(density(df_imp$glucose), color = 'red')
legend('topright', c('original', 'imputed'), 
       col = c('black', 'red'), lty = 1)

# drop `body` and `home.dest`
df <- df %>% 
     select(-body, -home.dest)

# impute `embarked` with mode
Mode <- function(x) {
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
}

embarked_mode <- Mode(df$embarked)

df$embarked <- df$embarked %>% 
     replace_na(embarked_mode)

sum(is.na(df$embarked))

# create boat_na
df <- df %>% 
     mutate(boat_na = ifelse(is.na(boat), 0, 1)) %>% 
     select(-boat)

## Feature Engineering

# extract title from `name`
df$title <- df$name %>% 
     str_extract('([A-Za-z]+)\\.') %>% 
     str_remove('.$')

df %>% 
     count(title, sort = TRUE)

# combine titles into broader categories
rare_title <- c('Dona', 'Lady', 'Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 
                'Rev', 'Sir', 'Jonkheer')

df$title[df$title %in% rare_title] <- "rare_title"

df$title[df$title %in% c('Mlle', 'Ms')] <- "Miss"

df$title[df$title %in% 'Mme'] <- "Mrs"

# create `deck` from `cabin` first letter
df$deck <- df$cabin %>% 
     str_extract('^[A-Za-z]')

# remove unnecessary features
df <- df %>% 
     select(-name, -ticket, -cabin)

head(df)

# create df_imp
df_imp <- df %>% 
     select(-survived)

df_imp <- missRanger(df_imp, seed = 42)

# verify missing values
colMeans(is.na(df_imp))

# add `survived` feature
df_imp$survived <- df$survived

# compare original `age` and imputed `age`
df_age = data.frame(feature = 'age_orig', value = df$age)
df_age_imp = data.frame(feature = 'age_imp', value = df_imp$age)

df_age_comb <- rbind(df_age, df_age_imp)

df_age_comb %>% 
     ggplot(aes(x = value, fill = feature)) + 
     geom_density(alpha = 0.5)

# let's verify `pclass` and imputed `deck`
table(df_imp$deck, df_imp$pclass)

# output to csv
write_csv(df_imp, "df_imp_missRanger.csv")
