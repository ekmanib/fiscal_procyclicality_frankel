# In order to get the dataset to be used in modelling and analysis, it is necessary to transform the data and include more data from other sources
library(tidyverse, warn.conflicts = FALSE)
library(readxl)
library(naniar)

IMFraw <- read_excel("_data/WEOOctober.xlsx")

IMFMeta <- IMFraw %>%
  select(c(`WEO Subject Code`, `Subject Descriptor`, 'Units', 'Scale')) %>%
  distinct()

IMF <- IMFraw %>%
  select(-c('ISO', "Subject Descriptor", 'Units', 'Scale', "Country/Series-specific Notes")) %>%
  replace_with_na_all(condition = ~.x %in% c("n/a", "--")) %>%
  mutate_at(.vars = c(3:43), .funs = as.double) %>%
  select(!("Estimates Start After")) %>%
  pivot_longer(cols = c(3:43), names_to = "Year") %>%
  pivot_wider(id_cols = c("Country", "Year"), names_from = "WEO Subject Code") %>%
  select(-c("NA"))
  

## the dataset with which the work is going to be performed
OCDELATAM <- c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Ecuador", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Peru", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Turkey", "United Kingdom", "United States", "Uruguay")

dtaOCDELATAM <- IMF %>%
  inner_join(tibble("Country" = OCDELATAM)) %>%
  mutate_at(.vars = 3:47,.funs = flatten_dbl) %>%
  rowwise() %>%
  mutate(GGX_R = (GGX/NGDP_D)*100)
  

# Series from official publications by the argentinan government on public spending, base == 2006
publicspend_arg <-  tibble(Year = 1980:2006, CGX = c(92.06, 99.52, 86.10, 113.82, 94.64, 99.86, 101.10, 110.91, 75.40, 59.83, 55.16, 63.26, 74.32, 89.72, 95.82, 92.63, 94.01, 97.26, 101.26, 109.24, 108.40, 107.26, 66.60, 72.58, 77.55, 96.08,  105.89))

## further transformations to the dataset
# include data for argentinan public spending before 1993
dtaOCDELATAM[1:13, "GGX_R"] <- publicspend_arg[1:13, "CGX"]

# include more data points for australian central government spending + payments to state/local governments - so far couldn't find data for general government. I take a rudimentaty approach in order to equate CGX and GGX: For the observations present in both the CGX data I found and the GGX data from the IMF, I calculate how many time GGX in bigger than CGX on average. Then I multiply the whole CGX series 
publicspend_aus <- tibble(Year = 1983:1996, CGX = c(49390, 57287, 64675, 70860, 76097, 79405, 82820, 87835, 96030, 102638, 109894, 115817, 118228, 122955))
publicspend_aus["CGX"] <- publicspend_aus["CGX"]*mean(unlist(dtaOCDELATAM[50:58, "GGX"]/(publicspend_aus[6:14, "CGX"] / 1000)))
dtaOCDELATAM[45:49, "GGX"] <- (publicspend_aus[1:5, "CGX"]/1000)
dtaOCDELATAM[45:49, "GGX_R"] <- (dtaOCDELATAM[45:49, "GGX"]/dtaOCDELATAM[45:49, "NGDP_D"])*100

# extended data for Chile's government expenditure. Source if the Economic History and Cliometrics lab from Universidad Católica de Chile. In Biliions of Pesos, current national currency units.
publicspend_chl <- tibble("Year" = 1980:1989, "GGX" = c(256.36, 340.21, 362.24, 449.09, 570.57, 779.95, 911.05, 1103.42, 1364.10, 1535.41))
dtaOCDELATAM[288:297, "GGX"] <- publicspend_chl[,"GGX"]
dtaOCDELATAM[288:297, "GGX_R"] <- (dtaOCDELATAM[288:297, "GGX"]/dtaOCDELATAM[288:297, "NGDP_D"])*100

#more data on government spending for Costa Rica from MIDEPLAN-BID: in millions of current national currency
publicspend_cri <- tibble("Year" = 1985:1989, "CGX" = c(35.27, 44.26, 48.93, 59.00, 77.85))
dtaOCDELATAM[375:379, "GGX"] <- publicspend_cri["CGX"]
dtaOCDELATAM[375:379, "GGX_R"] <- (dtaOCDELATAM[375:379, "GGX"]/dtaOCDELATAM[375:379, "NGDP_D"])*100

# data on Ecuador from the Central Bank Historical Statistics on the compendium on 90 years of data collected during the existance of the Bank
dtaOCDELATAM[493:507, "GGX"] <- c(2435.88235294118, 2929.03225806452, 2698.41642228739, 1337.24759615385, 1671.35922330097, 2545.77202072539, 2467.48784440843, 2346.61403508772, 1968.61120103593, 1871.64360819037, 2147.12185686654, 2170.18955111279, 2197.76967467817, 2704.3309540565, 3181.90814976969
)/1000
dtaOCDELATAM[493:507, "GGX_R"] <- (dtaOCDELATAM[493:507, "GGX"]/dtaOCDELATAM[493:507, "NGDP_D"])*100

# Data on Uruguay from iecon Instituto de Econom'ia
dtaOCDELATAM[1723:1741, "GGX"] <- c(0.019, 0.028, 0.036, 0.045, 0.067, 0.111, 0.208, 0.379, 0.650, 1.178, 2.421, 5.134, 9.211, 14.731, 22.432, 31.367, 41.109, 52.133, 60.161)
dtaOCDELATAM[1723:1741, "GGX_R"] <- (dtaOCDELATAM[1723:1741, "GGX"]/dtaOCDELATAM[1723:1741, "NGDP_D"])*100

## Work on the sample data from ICRG
ICRGraw <- read_excel("_data/Political Risk Services International Country Risk Guide 2021.xlsx", sheet = "long_year")
ICRGraw[, "var"] <- ICRGraw %>%
  pull(var) %>%
  recode("CC" = "Control_Corruption", "GE" = "Government_Effectiveness", "PV" = "Political_Viability", "RL" = "Rule_Law", "RQ" = "Regulatory_Quality","VA" = "Voice_Accountability")

ICRG <- ICRGraw %>% 
  pivot_longer(cols = c(3:142), names_to = "Country") %>%
  pivot_wider(id_cols = c("Year", "Country"), names_from = "var") %>%
  relocate(Country) %>%
  arrange(Country)

OCDELATAMcodes <- c("ARG", "AUS", "AUT", "BEL", "BOL", "BRA", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "ECU", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", 'LUX', "MEX", "NLD", "NZL", "NOR", "PER", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "TUR", "GBR", "USA", "URY")


panel <- ICRG %>%
  inner_join(tibble("Country" = OCDELATAMcodes))

panel[, "Country"] <- panel %>%
  pull(Country) %>%
  recode("ARG" = "Argentina", "AUS" = "Australia", "AUT" = "Austria", "BEL" = "Belgium", "BOL" = "Bolivia", "BRA" = "Brazil", "CAN" = "Canada", "CHL" = "Chile", "COL" = "Colombia", "CRI" = "Costa Rica", "CZE" = "Czech Republic", "DNK" = "Denmark", "ECU" = "Ecuador", "EST" = "Estonia", "FIN" = "Finland", "FRA" = "France", "DEU" = "Germany", "GRC" = "Greece", "HUN" = "Hungary", "ISL" = "Iceland", "IRL" = "Ireland", "ISR" = "Israel", "ITA" = "Italy", "JPN" = "Japan", "KOR" = "Korea", "LVA" = "Latvia", "LTU" = "Lithuania", 'LUX' = "Luxembourg", "MEX" = "Mexico", "NLD" = "Netherlands", "NZL" = "New Zealand", "NOR" = "Norway", "PER" = "Peru", "POL" = "Poland", "PRT" = "Portugal", "SVK" = "Slovak Republic", "SVN" = "Slovenia", "ESP" = "Spain", "SWE" = "Sweden", "TUR" = "Turkey", "GBR" = "United Kingdom", "USA" = "United States", "URY" = "Uruguay")


ICRG_OCDELATAM <- expand_grid(Country = unique(panel$Country), 
                     Year = 1996:2020) %>%
  left_join(panel) %>%
  mutate_at(.vars = c("Year"), .funs = as.character) %>%
  fill(c(2:8) , .direction = "down") %>%
  rowwise() %>%
  mutate(IQindex = mean(c(Control_Corruption, Government_Effectiveness, Rule_Law, Regulatory_Quality)))
  
  

