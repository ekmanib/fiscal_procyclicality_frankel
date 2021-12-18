# In order to get the dataset to be used in modelling and analysis, it is necessary to transform the data and include more data from other sources
library(tidyverse)
library(readxl)
library(naniar)

IMFraw <- read_excel("_data/WEOOctober.xlsx", stringsAsFactors = FALSE)

IMFMeta <- IMFraw %>%
  select(c(`WEO Subject Code`, `Subject Descriptor`, 'Units', 'Scale')) %>%
  distinct()

IMF <- IMFraw %>%
  select(-c('ISO', "Subject Descriptor", 'Units', 'Scale', "Country/Series-specific Notes")) %>%
  replace_with_na_all(condition = ~.x %in% c("n/a", "--")) %>%
  mutate_at(.vars = c(3:43), .funs = as.double) %>%
  select(!("Estimates Start After")) %>%
  pivot_longer(cols = c(3:43), names_to = "Year") %>%
  pivot_wider(id_cols = c("Country", "Year"), names_from = "WEO Subject Code")

## the dataset with which the work is going to be performed
OCDELATAM <- c("Argentina", "Australia", "Austria", "Belgium", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Ecuador", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "South Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Turkey", "United Kingdom", "United Stated", "Uruguay")

dtaOCDELATAM <- IMF %>%
  inner_join(tibble("Country" = OCDELATAM))