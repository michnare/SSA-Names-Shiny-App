library(tidyverse)

babynamesUncleaned <- read.csv("babynames_pivot.csv")
names(babynamesUncleaned) <- sub("^X", "", names(babynamesUncleaned))

babynames <- babynamesUncleaned %>%
  rename(name = Name, gender = Gender) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "percent"
  ) %>%
  mutate(
    year = as.integer(year),
    name = factor(name),
    gender = factor(gender)
  )

# IMPORTANT: do NOT add gender="All" rows
# (We calculate those on the fly later.)

saveRDS(babynames, "babynames_small.rds", compress = "xz")



