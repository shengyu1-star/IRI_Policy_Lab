library(tidyverse)
library(readxl)
library(gridExtra)
library(glmnet)
library(stargazer)
library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)
library(viridis)
library(RColorBrewer)
library(plm)

setwd("~/Documents/School/Grad/Spring 2022/IRI Policy Lab/IRI_Policy_Lab")
merged_datasets3 <- read_csv("merged_datasets3.csv")
  
## Correlations between causes independent variables by category:
# Governance
# nelda3, nelda4, nelda5, POLITY5.parcomp, FH.fh_total, POLITY5.fragment, POLITY5.polity2, POLITY5.durable, 
gov_indc <- c("nelda3", "nelda4", "nelda5", "POLITY5.parcomp", "FH.fh_total", "POLITY5.fragment", 
                "POLITY5.polity2", "POLITY5.durable")
c_gov <- rcorr(as.matrix(select(merged_datasets3, unlist(gov_indc))))
corrplot(c_gov$r,  method = "circle", number.cex = 0.75, type = "upper", tl.cex = 0.75, 
         order = "hclust", p.mat = c_gov$P, sig.level = c(.01, .05, 0.1),  diag = FALSE,
         insig = "label_sig", pch.cex = 1)


# Socioeconomic
# nelda17, nelda18, WB.GDP_ppc, WB.FDI, WB.inflation, WB.gini, WB.urban_pop, literacy_imputed, VDEM.yrs_education
socioecon_indc <- c("nelda17", "nelda18", "WB.GDP_ppc", "WB.FDI", "VDEM.yrs_education",
                    "WB.inflation", "WB.gini", "WB.urban_pop", "literacy_imputed")
c_socio <- rcorr(as.matrix(select(merged_datasets3, unlist(socioecon_indc))))
corrplot(c_socio$r,  method = "circle", number.cex = 0.75, type = "upper", tl.cex = 0.75, 
         order = "hclust", p.mat = c_socio$P, sig.level = c(.01, .05, 0.1), diag= FALSE, 
         insig = "label_sig", pch.cex = 1, mar=c(0,0,2,0))


# Media
# press_freedom

# EMB
# PEI.EMB_cap,VDEM.EMB_cap, imputed, nelda11, nelda45, nelda46
emb_indc <- c("PEI.EMB_cap","VDEM.EMB_cap", "nelda11", "nelda45", "nelda46")
c_emb <- rcorr(as.matrix(select(merged_datasets3, unlist(emb_indc))))
corrplot(c_emb$r,  method = "circle", number.cex = 0.75, type = "upper", tl.cex = 0.75, 
         order = "hclust", p.mat = c_emb$P, sig.level = c(.01, .05, 0.1), diag= FALSE, 
         insig = "label_sig", pch.cex = 1)


# pre-election violence
# ,


# ethnic divisions
ethnic_indc <- c("VDEM.access_to_public_services_social_group", 
                 "VDEM.access_to_public_services_pol_group", 
                 "VDEM.regime_support_by_an_ethnic_group", 
                 "VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group",
                 "VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group")
c_ethnic <- rcorr(as.matrix(select(merged_datasets3, unlist(ethnic_indc))))
corrplot(c_ethnic$r,  method = "circle", number.cex = 0.75, type = "upper", tl.cex = 0.3, 
         order = "hclust", p.mat = c_ethnic$P, sig.level = c(.01, .05, 0.1), diag= FALSE, 
         insig = "label_sig", pch.cex = 1, tl.srt = 45)

# Consequences
conseq_indc <- c("PEI.protest", "nelda29", "VDEM.turnout", "VDEM.VAP_turnout")
c_conseq <- rcorr(as.matrix(select(merged_datasets3, unlist(conseq_indc))))
corrplot(c_conseq$r,  method = "circle", number.cex = 0.75, type = "upper", tl.cex = 0.75, 
         order = "hclust", p.mat = c_conseq$P, sig.level = c(.01, .05, 0.1), diag= FALSE, 
         insig = "label_sig", pch.cex = 1)


# correlation analysis for different integrity measures:
chart.Correlation(select(merged_datasets3, AF.el_rating, PEI.el_rating, VDEM.el_rating),
                  histogram = FALSE)

# individually regressing all variables on VDEM.el_rating
socioecon_indc <- c("nelda17", "nelda18", "WB.GDP_ppc", "WB.FDI", "VDEM.yrs_education",
                    "WB.inflation", "WB.gini", "WB.urban_pop", "literacy_imputed")
gov_indc <- c("nelda3", "nelda4", "nelda5", "POLITY5.parcomp", "FH.fh_total", "POLITY5.fragment", 
              "POLITY5.polity2", "POLITY5.durable")
emb_indc <- c("VDEM.EMB_cap", "nelda11", "nelda45", "nelda46")
ethnic_indc <- c("VDEM.access_to_public_services_social_group", 
                 "VDEM.access_to_public_services_pol_group", 
                 "VDEM.regime_support_by_an_ethnic_group", 
                 "VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group",
                 "VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group")

lm.nelda17 <- lm(VDEM.el_rating ~ nelda17, merged_datasets3)
lm.nelda18 <- lm(VDEM.el_rating ~ nelda18, merged_datasets3)
lm.WB.GDP_ppc <- lm(VDEM.el_rating ~ WB.GDP_ppc, merged_datasets3)
lm.WB.FDI <- lm(VDEM.el_rating ~ WB.FDI, merged_datasets3)
lm.VDEM.yrs_education <- lm(VDEM.el_rating ~ VDEM.yrs_education, merged_datasets3)
lm.WB.inflation <- lm(VDEM.el_rating ~ WB.inflation, merged_datasets3)
lm.WB.gini <- lm(VDEM.el_rating ~ WB.gini, merged_datasets3)
lm.WB.urban_pop <- lm(VDEM.el_rating ~ WB.urban_pop, merged_datasets3)
lm.literacy_imputed <- lm(VDEM.el_rating ~ literacy_imputed, merged_datasets3)

lm.nelda3 <- lm(VDEM.el_rating ~ nelda3, merged_datasets3)
lm.nelda4 <- lm(VDEM.el_rating ~ nelda4, merged_datasets3)
lm.nelda5 <- lm(VDEM.el_rating ~ nelda5, merged_datasets3)
lm.POLITY5.parcomp <- lm(VDEM.el_rating ~ POLITY5.parcomp, merged_datasets3)
lm.FH.fh_total <- lm(VDEM.el_rating ~ FH.fh_total, merged_datasets3)
lm.POLITY5.fragment <- lm(VDEM.el_rating ~ POLITY5.fragment, merged_datasets3)
lm.POLITY5.polity2 <- lm(VDEM.el_rating ~ POLITY5.polity2, merged_datasets3)
lm.POLITY5.durable <- lm(VDEM.el_rating ~ POLITY5.durable, merged_datasets3)

lm.VDEM.EMB_cap <- lm(VDEM.el_rating ~ VDEM.EMB_cap, merged_datasets3)
lm.nelda11 <- lm(VDEM.el_rating ~ nelda11, merged_datasets3)
lm.nelda45 <- lm(VDEM.el_rating ~ nelda45, merged_datasets3)
lm.nelda46 <- lm(VDEM.el_rating ~ nelda46, merged_datasets3)

lm.VDEM.access_to_public_services_social_group <- lm(VDEM.el_rating ~ VDEM.access_to_public_services_social_group, merged_datasets3)
lm.VDEM.access_to_public_services_pol_group <- lm(VDEM.el_rating ~ VDEM.access_to_public_services_pol_group, merged_datasets3)
lm.VDEM.regime_support_by_an_ethnic_group <- lm(VDEM.el_rating ~ VDEM.regime_support_by_an_ethnic_group, merged_datasets3)
lm.VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group <- lm(VDEM.el_rating ~ VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group, merged_datasets3)
lm.VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group <- lm(VDEM.el_rating ~ VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group, merged_datasets3)

lm.press_freedom <- lm(VDEM.el_rating ~ press_freedom, merged_datasets3)

lm.PEI.protest <- lm(VDEM.el_rating ~ PEI.protest, merged_datasets3)
lm.nelda29 <- lm(VDEM.el_rating ~ nelda29, merged_datasets3)
lm.VDEM.turnout <- lm(VDEM.el_rating ~ VDEM.turnout, merged_datasets3)
lm.VDEM.VAP_turnout <- lm(VDEM.el_rating ~ VDEM.VAP_turnout, merged_datasets3)

lm.seans <- lm(VDEM.el_rating ~ POLITY5.polity2 + VDEM.EMB_cap + WB.GDP_ppc + VDEM.EMB_cap:WB.GDP_ppc, merged_datasets3)


stargazer(title = "Socioeconomic indicators", lm.nelda17, lm.nelda18, lm.WB.GDP_ppc, lm.WB.FDI,
          lm.VDEM.yrs_education, lm.WB.inflation, lm.WB.gini,
          lm.WB.urban_pop, lm.literacy_imputed, font.size= "small", single.row = TRUE, column.sep.width = "1pt", no.space = TRUE) 

stargazer(title = "Governance indicators", lm.nelda3, lm.nelda4, lm.nelda5, lm.POLITY5.parcomp, 
          lm.FH.fh_total, lm.POLITY5.fragment, lm.POLITY5.polity2, 
          lm.POLITY5.durable, font.size= "small", single.row = TRUE, column.sep.width = "1pt", no.space = TRUE)

stargazer(title = "EMB indicators", lm.VDEM.EMB_cap, lm.nelda11, lm.nelda45, lm.nelda46, single.row = TRUE)

stargazer(title = "Ethnic indicators", lm.VDEM.access_to_public_services_social_group, lm.VDEM.access_to_public_services_pol_group, lm.VDEM.regime_support_by_an_ethnic_group, 
          lm.VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group, lm.VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group,
          single.row = TRUE)

stargazer(title = "Press indicators", lm.press_freedom)

stargazer(title = "Consequnces indicators", lm.PEI.protest, lm.VDEM.turnout, lm.VDEM.VAP_turnout)

# doing percent complete for merged3
perc_complete <- bind_rows(map(merged_datasets3, ~mean(is.na(.))))
perc_complete <- perc_complete %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(`%complete` = 1 - value)

perc_complete %>% write_csv("CleanedMergedData/perc_complete.csv")


# comparing ord vs osp
# `PEI.Electoral authorities index (0-100), imputed`, VDEM.EMB_capacity_osp, VDEM.EMB_capacity_ord
p_osp <- ggplot(merged_datasets3) + 
  geom_point(aes(`PEI.Electoral authorities index (0-100), imputed`, VDEM.EMB_capacity_osp))

p_ord <- ggplot(merged_datasets3) + 
  geom_point(aes(`PEI.Electoral authorities index (0-100), imputed`, VDEM.EMB_capacity_ord))

summary(lm(VDEM.EMB_capacity_osp ~ `PEI.Electoral authorities index (0-100), imputed`, merged_datasets3))
summary(lm(VDEM.EMB_capacity_ord ~ `PEI.Electoral authorities index (0-100), imputed`, merged_datasets3))

grid.arrange(p_osp, p_ord, ncol=2)


# lasso analysis
lm(VDEM.elction_free_fair ~ VDEM.EMB_capacity_osp, merged_datasets3)
lm(VDEM.elction_free_fair ~ `PEI.Electoral authorities index (0-100), imputed`, merged_datasets3)
lm(VDEM.elction_free_fair ~ VDEM.EMB_capacity_osp, merged_datasets3)

sapply(dt_split, function(x) sum(is.na(x)))

merged_lasso <- merged_datasets3 %>% 
  select(!c(country_year:`PEI.Rating of electoral integrity (1-10)`, 
            `PEI.Voting results/reactions index (protests/disputes) (0-100), imputed`,
            `PEI.Electoral authorities index (0-100), imputed`,
            `PEI.turnout`, `WB.Gini index`, `POLITY5.polity2`,
            nelda46, nelda30,POLITY5.fragment, POLITY5.durable, POLITY5.parcomp, `WB.Inflation, consumer prices (annual %)`,
            VDEM.yrs_education, `Press Freedom Index`)) %>%
  drop_na() 


dt_split <- merged_lasso %>%
  mutate(random = rnorm(nrow(merged_lasso))) %>%
  arrange(random) %>%
  mutate(set = case_when(
    row_number() <= nrow(merged_lasso)*0.75 ~ "training",
    row_number() > nrow(merged_lasso)*0.25 &  row_number() <= nrow(merged_lasso)*1 ~ "test")
  ) %>% 
  mutate(Y = VDEM.elction_free_fair) %>%
  dplyr::select(!VDEM.elction_free_fair) %>% 
  relocate(Y, .before = VDEM.EMB_capacity_osp)

train <- dt_split %>%
  filter(set == "training") %>%
  dplyr::select(-c(random, set))

test <- dt_split %>%
  filter(set == "test") %>%
  dplyr::select(-c(random, set))

x = as.matrix(train[,2:25]) 
y = as.matrix(train[,1])

lasso.cv = cv.glmnet(x, y)
coef.min = coef(lasso.cv, s=lasso.cv$lambda.min)

plot(lasso.cv)

plot(lasso.cv$glmnet.fit , xvar = "lambda")
abline(v = log(lasso.cv$lambda.min), lty = 2, col = "red")
abline(v = log(lasso.cv$lambda.1se), lty = 2, col = "green")
legend("topright", legend = c("min", "1se"), lty = 2, col = c("red", 
                                                              "green"))

coef(lasso.cv, s = c(lasso.cv$lambda.min))
as_tibble(coef(lasso.cv, s = c(lasso.cv$lambda.min)), validate = NULL, .name_repair = NULL)

# capture with Lasso only the ones that min the lamdba
coefList <- coef(lasso.cv, s='lambda.min')
coefList <- data.frame(coefList@Dimnames[[1]][coefList@i+1],coefList@x)
coefList <- coefList[2:19,]

# capture with Lasso only the ones that 1se the lamdba
coefList2 <- coef(lasso.cv, s='lambda.1se')
coefList2 <- data.frame(coefList2@Dimnames[[1]][coefList2@i+1],coefList2@x)
coefList2 <- coefList2[2:9,]

# lasso model that min lambda
lm.lasso.min <- lm(VDEM.el_rating ~ VDEM.EMB_cap + VDEM.turnout + nelda17 + nelda3 + nelda4 + nelda5 + nelda29 + 
                     FH.fh_total + WB.FDI + WB.urban_pop +  literacy_imputed + VDEM.elec_viol + VDEM.international_monitors_present + 
                     VDEM.access_to_public_services_pol_group +  VDEM.regime_support_by_an_ethnic_group + 
                     VDEM.is_an_ethnic_group_the_most_powerful_regime_duration_group + 
                     VDEM.is_an_ethnic_group_the_most_powerful_regime_support_group, data = merged_datasets3)


# lasso model that use 1se lambda
lm.lasso.1se <- lm(VDEM.el_rating ~ VDEM.EMB_cap + nelda3 + nelda29 + FH.fh_total + WB.urban_pop + 
                     `VDEM.elec_viol` + VDEM.international_monitors_present + VDEM.access_to_public_services_pol_group, data = merged_datasets3)

stargazer(lm.lasso.1se, lm.lasso.min, title = "lasso models", font.size= "small", column.labels =c("min 1se", "min lambda"),
          column.sep.width = "1pt", no.space = TRUE, omit.stat=c("LL","ser"), align = TRUE, single.row =TRUE)



# my model:
# control for gdp_per_cap, gini, education 
lm_sean <- lm(VDEM.el_rating ~ VDEM.EMB_cap + WB.GDP_ppc, merged_datasets3)
summary(lm_sean)
stargazer(lm_sean, type = "text")

stargazer(lm(VDEM.el_rating ~ WB.GDP_ppc + VDEM.EMB_cap, merged_datasets3), type = "text")
stargazer(lm(VDEM.el_rating ~ WB.GDP_ppc + VDEM.EMB_cap, filter(merged_datasets3, WB.GDP_ppc < 5000)), type = "text")
ggplot(filter(merged_datasets3, WB.GDP_ppc < 5000)) + 
  geom_point(aes(VDEM.EMB_cap + WB.GDP_ppc, VDEM.el_rating, color = country)) 

ggplot(merged_datasets3) + 
  geom_point(aes(VDEM.EMB_cap + WB.GDP_ppc, VDEM.el_rating, color = country)) 



stargazer(lm(VDEM.el_rating ~ VDEM.EMB_cap, merged_datasets3), type = "text")
stargazer(lm(VDEM.el_rating ~ VDEM.EMB_cap + WB.GDP_ppc, merged_datasets3), type = "text")
ggplot(merged_datasets3) + 
  geom_point(aes(VDEM.EMB_cap, VDEM.el_rating)) + 
  theme_minimal() + 
  labs(x = "Election Management Body capacity", 
       y = "Election integrity perception",
       title = "The relationship between free and fair elections, and EMB capacity") + 
  geom_abline(slope = 0.631-0.00003, intercept = 0.995, color = "red") + 
  geom_abline(slope = 0.600, intercept = 0.913, color = "blue")


stargazer(lm(VDEM.el_rating ~ WB.GDP_ppc, merged_datasets3), type = "text")
stargazer(lm(VDEM.el_rating ~ WB.GDP_ppc, filter(merged_datasets3, WB.GDP_ppc < 5000)), type = "text")
ggplot(filter(merged_datasets3, WB.GDP_ppc < 5000)) + 
  geom_point(aes(WB.GDP_ppc, VDEM.el_rating, color = country)) 


stargazer(lm(VDEM.el_rating ~ VDEM.access_to_public_services_pol_group, merged_datasets3), type = "text")
stargazer(lm(VDEM.el_rating ~ VDEM.access_to_public_services_pol_group + WB.GDP_ppc, merged_datasets3), type = "text")
ggplot(merged_datasets3) + 
  geom_point(aes(VDEM.access_to_public_services_pol_group, VDEM.el_rating)) 

ggplot(merged_datasets3) + 
  geom_point(aes(VDEM.access_to_public_services_pol_group + WB.GDP_ppc, VDEM.el_rating)) 
ggplot(filter(merged_datasets3, WB.GDP_ppc < 5000)) + 
  geom_point(aes(VDEM.access_to_public_services_pol_group + WB.GDP_ppc, VDEM.el_rating, color = country)) 

#fixed effects
#create panel dataset
merged.p <- pdata.frame(select(merged_datasets3, !country_year), index = c("country", "year"))
fixedeff <- plm(VDEM.el_rating ~ VDEM.EMB_cap, data = merged.p, model = "within")
stargazer(fixedeff, type = "text")

fixedeff.lag <- plm(VDEM.el_rating ~ lag(VDEM.el_rating), data = merged.p, model = "within")
stargazer(fixedeff.lag, type = "text")


stargazer(lm(VDEM.el_rating ~ VDEM.EMB_cap, data = merged_datasets3), type = "text")

# make lagged VDEM_el within each group
lagged.lm <- lm(VDEM.el_rating ~ VDEM.el_rating.lagged + country, merged_datasets3)
stargazer(lagged.lm, type = "text")

ggplot(merged_datasets3) + 
  geom_point(aes(VDEM.el_rating.lagged, VDEM.el_rating, color = country))

ggplot(filter(merged_datasets3, country == "Senegal")) + 
  geom_point(aes(VDEM.el_rating.lagged, VDEM.el_rating, color = country))
  
ggplot(filter(merged_datasets3, country == "Rwanda")) + 
  geom_point(aes(VDEM.el_rating.lagged, VDEM.el_rating))
  
