library(tidyverse)
library(broom)
library(magrittr)
library(mFilter)
  
df_corrs <- dtaOCDELATAM %>%
  group_by(Country) %>%
  filter(!is.na(GGX_R)) %>%
  mutate(lNGDP_R = log(NGDP_R), lGGX_R = log(GGX_R)) %>%
  mutate(cyclelNGDP_R = hpfilter(lNGDP_R, type = "lambda", freq = 6.25)$cycle,
         cyclelGGX_R = hpfilter(lGGX_R, type = "lambda", freq = 6.25)$cycle)
## Visualizing pro cyclicality across the countries available.
# All the years
df_corrs %>%
  group_by(Country) %>%
  summarise(Correlation = cor(cyclelNGDP_R, cyclelGGX_R)) %>%
  arrange(Correlation) %>%
  mutate(CountryName = factor(Country, levels=Country),
         Is_Procyc = if_else(condition = Correlation > 0,true = 1, false = 0)) %>%
  ggplot(mapping = aes(y = Correlation, x = CountryName, fill=factor(Is_Procyc))) +
  geom_bar(stat = "identity", alpha =.6, width = .4, show.legend = FALSE) +
  scale_fill_manual(values = c("#f68060", "#DD2C0C")) + 
  coord_flip() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45))
# Previous to the great global recession
df_corrs %>%
  group_by(Country) %>%
  filter(Year <= 2007) %>%
  summarise(Correlation = cor(cyclelNGDP_R, cyclelGGX_R)) %>%
  arrange(Correlation) %>%
  mutate(CountryName = factor(Country, levels=Country),
         Is_Procyc = if_else(condition = Correlation > 0,true = 1, false = 0)) %>%
  ggplot(mapping = aes(y = Correlation, x = CountryName, fill=factor(Is_Procyc))) +
  geom_bar(stat = "identity", alpha =.6, width = .4, show.legend = FALSE) +
  scale_fill_manual(values = c("#f68060", "#DD2C0C")) + 
  coord_flip() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45))
# After to the great global recession
df_corrs %>%
  group_by(Country) %>%
  filter(Year > 2007) %>%
  summarise(Correlation = cor(cyclelNGDP_R, cyclelGGX_R)) %>%
  arrange(Correlation) %>%
  mutate(CountryName = factor(Country, levels=Country),
         Is_Procyc = if_else(condition = Correlation > 0,true = 1, false = 0)) %>%
  ggplot(mapping = aes(y = Correlation, x = CountryName, fill=factor(Is_Procyc))) +
  geom_bar(stat = "identity", alpha =.6, width = .4, show.legend = FALSE) +
  scale_fill_manual(values = c("#f68060", "#DD2C0C")) + 
  coord_flip() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_bw()

# The observed relationship between Institutional Quality and Fiscal Policy Cyclicality.

IQaverage <- ICRG_OCDELATAM %>%
  group_by(Country) %>%
  summarise(Average_IQ = mean(IQindex))

df_corrs %>%
  group_by(Country) %>%
  summarise(Correlation = cor(cyclelNGDP_R, cyclelGGX_R)) %>%
  left_join(IQaverage) %>%
  ggplot(aes(x=Average_IQ, y=Correlation)) +
  geom_point(stat = "identity") +
  theme_bw()
  

