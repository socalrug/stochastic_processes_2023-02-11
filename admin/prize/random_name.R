library(tidyverse)
library(readr)
df <- readr::read_csv('attendee_list.csv', col_names = c('name', 'email'),
                      show_col_types = FALSE)
df$name[sample(nrow(df), 1)] |>
  print()
