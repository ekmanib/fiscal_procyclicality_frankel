library(tidyverse)
library(broom)
library(lmtest)
library(gridExtra)
library(plm)

dtaFinal <- dtaFinal %>%
  mutate(Procyc_Deg = Procyc_Deg * 100)

# A pooled OLS model
mdl_pooled <- plm(
  Procyc_Deg ~
    IQindex +
    ChinnIto +
    DebtPCHGDP +
    output_var +
    llgdp + # liquid liabilities as percentage of gdp
    FER, # foreign exchange reserves
  model = 'pooling',
  data = dtaFinal
)

mdl_pooled_vcov <- tidy(coeftest(mdl_pooled,
                        vcov=vcovHC(mdl_pooled,
                                    type='HC0',
                                    cluster='group'))) %>%
  mutate(term = case_when(
    term == "IQindex" ~ "Institutional Quality",
    term == "ChinnIto" ~ "Chinn & Ito",
    term == "DebtPCHGDP" ~ "Debt to GDP",
    term == "output_var" ~ "Output Variability",
    term == "polity2" ~ "Democracy Index",
    term == "llgdp" ~ "Liquid Liabilities as GDP",
    term == "FER" ~ "International Reserves",
    TRUE ~ "Intercept"
  ))

# Fixed effects model
mdl_fe <- plm(
  Procyc_Deg ~
    IQindex +
    ChinnIto +
    DebtPCHGDP +
    output_var +
    llgdp + # liquid liabilities as percentage of gdp
    FER, # foreign exchange reserves
  model = 'within',
  data = dtaFinal
)

mdl_fe_results <- tidy(mdl_fe) %>%
  mutate(term = case_when(
    term == "IQindex" ~ "Institutional Quality",
    term == "ChinnIto" ~ "Chinn & Ito",
    term == "DebtPCHGDP" ~ "Debt to GDP",
    term == "output_var" ~ "Output Variability",
    term == "polity2" ~ "Democracy Index",
    term == "llgdp" ~ "Liquid Liabilities as GDP",
    term == "FER" ~ "International Reserves",
    TRUE ~ "Intercept"
  ))

# Random effects model
mdl_re <- plm(
  Procyc_Deg ~
    IQindex +
    ChinnIto +
    DebtPCHGDP +
    output_var +
    llgdp + # liquid liabilities as percentage of gdp
    FER, # foreign exchange reserves
  model = 'random',
  data = dtaFinal
)

mdl_re_results <- tidy(mdl_re) %>%
  mutate(term = case_when(
    term == "IQindex" ~ "Institutional Quality",
    term == "ChinnIto" ~ "Chinn & Ito",
    term == "DebtPCHGDP" ~ "Debt to GDP",
    term == "output_var" ~ "Output Variability",
    term == "polity2" ~ "Democracy Index",
    term == "llgdp" ~ "Liquid Liabilities as GDP",
    term == "FER" ~ "International Reserves",
    TRUE ~ "Intercept"
  ))

# Hausman test of significant difference between coefficients from random effects and fixed effects
htest_results <- tidy(phtest(mdl_fe, mdl_re))

# saving the table to png images
png('_img/pooling.png', height=250, width=500)
p <- tableGrob(mdl_pooled_vcov)
grid.arrange(p)
dev.off()

png('_img/fe.png', height=250, width=500)
p <- tableGrob(mdl_fe_results)
grid.arrange(p)
dev.off()

png('_img/re.png', height=250, width=500)
p <- tableGrob(mdl_re_results)
grid.arrange(p)
dev.off()

png('_img/hw_test.png', height=250, width=500)
p <- tableGrob(htest_results)
grid.arrange(p)
dev.off()
