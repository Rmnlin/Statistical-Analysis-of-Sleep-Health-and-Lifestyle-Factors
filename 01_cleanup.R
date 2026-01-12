library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)

df <- read_csv("Sleep_health_and_lifestyle_dataset_part_2.csv", show_col_types = FALSE)

df_clean <- df %>% clean_names()
df <- df_clean