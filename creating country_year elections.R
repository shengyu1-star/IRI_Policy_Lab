library(tidyverse)
library(remotes)
#remotes::install_github("xmarquez/democracyData") #need this package to download FH data
library(democracyData)
library(WDI)
library(countrycode)

#fh <- download_fh(verbose = FALSE) #uncomment these for first time running program
#polity5 <- download_polity_annual(verbose = FALSE)

setwd("~/Documents/School/Grad/Spring 2022/IRI Policy Lab/IRI_Policy_Lab")

# converts QED into csv, then load back in
load("Raw data/31461-0002-Data.rda")
da31461.0002 %>% write_csv("CleanedMergedData/QED.csv")
QED.da <- read_csv("CleanedMergedData/QED.csv")

# Loading IAEP data
IAEP <- read_csv("Raw data/IAEPv2_0_2015labels.csv")

# loading PEI data
PEI_election <- read_delim("Raw data/PEI election-level data (PEI_7.0) v2 09-05-2019.tab", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

# IRI countries
iri_countries <- c("Kenya", "Chad", "Somalia", "Sudan", "Nigeria", 
                   "Sierra Leone", "Congo", "Zimbabwe")
iri_countries_ios2 <- countrycode(iri_countries, origin = 'country.name', destination = 'iso2c')

# other country groups
all_africa <- c()
subsarahan_africa <- c()
south_america <- c()
all_developing_countries <- c()

# select IRI countries and relevant indicators
QED.mod <- QED.da %>% 
  select(COUNTRY, YEAR, EXELEC, LEGELEC, SELTRANS, SELRUNOFF, SF1, SF2, SF3, SA1, SA2, 
         SR0STR, SR11CHEAT, SR13VIOL, SR12CAP, SR21CHEAT, SR23VIOL, SR22CAP) %>% 
  mutate(EXELEC = case_when(EXELEC == "(1) Executive Election" ~ 1,
                            EXELEC == "(0) Otherwise" ~ 0),
         LEGELEC = case_when(LEGELEC == "(1) Legislative Election" ~ 1,
                             LEGELEC == "(0) Otherwise" ~ 0),
         SELTRANS = case_when(SELTRANS == "(1) Transition or Transitional Election" ~ 1,
                              SELTRANS == "(0) Otherwise" ~ 0),
         SELRUNOFF = case_when(SELRUNOFF == "(1) Run-off Election" ~ 1,
                               SELRUNOFF == "(0) Otherwise" ~ 0)) %>% 
  rename("Transitional election" = SELTRANS,
         "Runoff election" = SELRUNOFF,
         "Freedom to change government" = SF1,
         "Freedom of association" = SF2, 
         "Freedom of speech" = SF3,
         "Overall election quality" = SA1,
         "Extent of election problems" = SA2, 
         "Pre-election legal structural enviornment" = SR0STR, 
         "Pre-election political conditions" = SR11CHEAT,
         "Pre-election violence/unrest" = SR13VIOL,
         "Pre-election administrative capacity" = SR12CAP,
         "Election day explicit cheating" = SR21CHEAT,
         "Election day violence/unrest" = SR23VIOL,
         "Election day administrative capacity" = SR22CAP) %>% 
  filter(COUNTRY %in% iri_countries) %>% 
  mutate(country_year = paste(COUNTRY, YEAR, sep = "-"),
         dup = duplicated(country_year, fromLast = TRUE),
         LEGELEC = ifelse(dup | LEGELEC == "1", 1, 0),
         EXELEC = ifelse(dup | EXELEC == "1", 1, 0)) %>% 
  filter(!duplicated(country_year)) %>% 
  select(!dup)

#####--------------------------------------#####
#IAEP

IAEP_mod <- IAEP %>% 
  select(cname, year, election, electexec, electleg, electboth, amyear, constage, govstruct, 
         regstruct, electtime, elecperiod, lpartynom, epartynom, lelecsystem, eelect, eelectrules, parties,
         stateparty, legcompet, execcompet, electboy, electprot, protestpart, legelig, execelig) %>% 
  #select(!c(cabr, ccode), ) %>% 
  rename("Age of constitution (years)" = constage,
         "Constitution amended this year?" = amyear,
         "Regional government reps selected" = regstruct,
         "Election scheduled" = electtime,
         "Election period" = elecperiod,
         "Do party nominations field leg candidates?" = lpartynom,
         "Do party nominations field executive candidates?" = epartynom,
         "Leg elec sys" = lelecsystem,
         "Exec elec sys" = eelect,
         "Exec elec rules" = eelectrules,
         "Num of parties that hold >= 5% of legislature" = parties,
         "Official state party?" = stateparty,
         "Leg elec competetive?" = legcompet,
         "Exec elec competetive?" = execcompet,
         "Boycotted by major party?" = electboy,
         "Did election cause protest/violence?" = electprot,
         "% of pop voting in leg elec" = legelig,
         "% of pop voting in exec elec" = execelig) %>% 
  filter(cname %in% iri_countries, 
         election == "Yes") %>% 
  mutate(country_year = paste(cname, year, sep = "-"))

## join together IAEP and QED
IAEP_QED <- IAEP_mod %>% 
  full_join(QED.mod, by = "country_year") %>% 
  mutate(cname = ifelse(is.na(cname), COUNTRY, cname),
         year = ifelse(is.na(year), YEAR, year),
         electexec = ifelse(is.na(electexec), EXELEC, electexec),
         electleg = ifelse(is.na(electleg), LEGELEC, electleg)) %>% 
  select(!c(COUNTRY, YEAR, election)) %>% 
  mutate(electexec = case_when(electexec == 0 ~ "No",
                               electexec == 1 ~ "Yes",
                               electexec == "Yes" ~ "Yes",
                               electexec == "No" ~ "No"),
         electleg = case_when(electleg == 0 ~ "No",
                              electleg == 1 ~ "Yes",
                              electleg == "Yes" ~ "Yes",
                              electleg == "No" ~ "No"),
         electboth = case_when(electleg == "Yes" & electexec == "Yes" ~ "Yes",
                              TRUE ~ "No"))



####### ----------------------- #######
# PEI 2019 (7.0) election data
PEI_election_mod <- PEI_election %>% 
  filter(country %in% iri_countries) %>% 
  select(election, year, country, office, PR, CL, gdp_pc_ppp, polity2, durable, development, 
         PEIIndexi, rating, lawsi, proceduresi, boundariesi, voteregi, partyregi, mediai, 
         financei, votingi, counti, resultsi, EMBsi, Turnout, DatePrevious,) %>% 
  rename("FreedomHouse political rights scale (1-7)" = PR,
         "FreedomHouse civil liberities scale (1-7)" = CL,
         "polity combined score (-10-10)" = polity2,
         "# years since last regime transition" = durable,
         "WB income level" = development,
         "PEI index of electoral integrity, imputed" = PEIIndexi,
         "Rating of electoral integrity (1-10)" = rating,
         "Electoral laws index (0-100), imputed" = lawsi,
         "Electoral prodcedures index (0-100), imputed" = proceduresi,
         "Voting district boundaries index (0-100), imputed" = boundariesi,
         "Voter regisration index (0-100), imputed" = voteregi,
         "Party/candidate registration index (0-100), imputed" = partyregi,
         "Media coverage index (0-100), imputed" = mediai,
         "Campaign finance index (0-100), imputed" = financei,
         "Voting process index (0-100), imputed" = votingi,
         "Vote count index (0-100), imputed" = counti,
         "Voting results/reactions index (protests/disputes) (0-100), imputed" = resultsi,
         "Electoral authorities index (0-100), imputed" = EMBsi) %>% 
  mutate(country_year = paste(country, year, sep = "-"))

###-------------------------------#####
# combine IAEP_QED with PEI:
IAEP_QED_PEI <- IAEP_QED %>% 
  full_join(PEI_election_mod, by = "country_year") %>% 
  mutate(cname = ifelse(is.na(cname), country, cname),
         year.x = ifelse(is.na(year.x), year.y, year.x),
         electexec = case_when(!is.na(electexec) ~ electexec,
                               is.na(electexec) & office == 1 ~ "Yes",
                               is.na(electexec) & office == 0 ~ "No"),
         electleg = case_when(!is.na(electleg) ~ electleg,
                              is.na(electleg) & office == 0 ~ "Yes",
                              is.na(electleg) & office == 1 ~ "No"),
         electboth = case_when(electleg == "Yes" & electexec == "Yes" ~ "Yes",
                               TRUE ~ "No")) %>% 
  select(!c(election, year.y, country, office, EXELEC, LEGELEC)) %>% 
  select(country_year, everything()) %>% 
  arrange(country_year)

#exporting IAEP_QED_PEI
#IAEP_QED_PEI %>% write_csv("CleanedMergedData/IAEP_QED_PEI_clean.csv")

###-----------------------------####
# Reconciling and consolidating variables in IAEP_QED_PEI
# variables reconciled so far: post_elec_violence, turnout, elec_integrity, pre_elec_legal_integrity, elec_explicit_cheating
# variables still to be reconciled: #_years_since_last_regime_change, FH_score, single-party

IAEP_QED_PEI_reconciled <- IAEP_QED_PEI %>% 
  mutate(post_elec_violence = case_when(`Voting results/reactions index (protests/disputes) (0-100), imputed` > 0 & `Voting results/reactions index (protests/disputes) (0-100), imputed` <= 25 ~ 3,
                                        `protestpart` == "Widespread participation" ~ 3,
                                        `Election day violence/unrest` == "(3) High - major problems" ~ 3,
                                        `Voting results/reactions index (protests/disputes) (0-100), imputed` > 25 & `Voting results/reactions index (protests/disputes) (0-100), imputed` <= 50 ~ 2,
                                        `protestpart` == "Moderate participation" ~ 2,
                                        `Election day violence/unrest` == "(2) Moderate - moderate problems" ~ 2,
                                        `Voting results/reactions index (protests/disputes) (0-100), imputed` > 50 & `Voting results/reactions index (protests/disputes) (0-100), imputed` <= 75 ~ 1,
                                        `protestpart` == "Low participation" ~ 1,
                                        `Election day violence/unrest` == "(1) Low - minor problems only" ~ 1,
                                        `Voting results/reactions index (protests/disputes) (0-100), imputed` > 75 & `Voting results/reactions index (protests/disputes) (0-100), imputed` <= 100 ~ 0,
                                        `Did election cause protest/violence?` == "No" ~ 0,
                                        `Election day violence/unrest` == "(0) Good - no problems" ~ 0,
                                        TRUE ~ NA_real_),
         `% of pop voting in leg elec` = as.numeric(ifelse(`% of pop voting in leg elec` == ".a", NA_real_, `% of pop voting in leg elec`)),
         `% of pop voting in exec elec` = as.numeric(ifelse(`% of pop voting in exec elec` == ".a", NA_real_, `% of pop voting in exec elec`))) %>% 
  rowwise() %>% 
  mutate(turnout = mean(c(Turnout, 
                          `% of pop voting in leg elec`, 
                          `% of pop voting in exec elec`), na.rm = TRUE)) %>% 
  mutate(elec_integrity = case_when(`Extent of election problems` == "(3) High - major problems" ~ 3,
                                    `Extent of election problems` == "(2) Moderate - moderate problems" ~ 2,
                                    `Extent of election problems` == "(1) Low - minor problems only" ~ 1,
                                    `Extent of election problems` == "(0) Good - no problems" ~ 0,
                                    `PEI index of electoral integrity, imputed` > 60 ~ 0,
                                    `PEI index of electoral integrity, imputed` > 50 & `PEI index of electoral integrity, imputed` <= 60 ~ 1,
                                    `PEI index of electoral integrity, imputed` > 40 & `PEI index of electoral integrity, imputed` <= 50 ~ 2,
                                    `PEI index of electoral integrity, imputed` <=40 ~ 3)) %>% 
  mutate(pre_elec_legal_integrity = case_when(`Pre-election legal structural enviornment` == "(3) High - major problems" ~ 3,
                                              `Pre-election legal structural enviornment` == "(2) Moderate - moderate problems" ~ 2,
                                              `Pre-election legal structural enviornment` == "(1) Low - minor problems only" ~ 1,
                                              `Pre-election legal structural enviornment` == "(0) Good - no problems" ~ 0,
                                              `Electoral laws index (0-100), imputed` > 60 ~ 0,
                                              `Electoral laws index (0-100), imputed` > 50 & `Electoral laws index (0-100), imputed` <= 60 ~ 1,
                                              `Electoral laws index (0-100), imputed` > 40 & `Electoral laws index (0-100), imputed` <= 50 ~ 2,
                                              `Electoral laws index (0-100), imputed` <=40 ~ 3)) %>% 
  mutate(elec_explicit_cheating = case_when(`Election day explicit cheating` == "(3) High - major problems" ~ 3,
                                              `Election day explicit cheating` == "(2) Moderate - moderate problems" ~ 2,
                                              `Election day explicit cheating` == "(1) Low - minor problems only" ~ 1,
                                              `Election day explicit cheating` == "(0) Good - no problems" ~ 0,
                                              `Vote count index (0-100), imputed` > 60 ~ 0,
                                              `Vote count index (0-100), imputed` > 50 & `Vote count index (0-100), imputed` <= 60 ~ 1,
                                              `Vote count index (0-100), imputed` > 40 & `Vote count index (0-100), imputed` <= 50 ~ 2,
                                              `Vote count index (0-100), imputed` <=40 ~ 3)) %>% 
  select(cname, year.x, country_year, electexec, electleg, electboth, elec_integrity, post_elec_violence, turnout, 
         pre_elec_legal_integrity, elec_explicit_cheating, govstruct, `Leg elec sys`)


## FH & Polity5 ##########

polity5_mod <- polity5 %>% 
  filter(polity_annual_country %in% iri_countries) %>% 
  mutate(country_year = paste(polity_annual_country, year, sep = "-")) %>%
  select(country_year, fragment, polity2, durable, parreg, parcomp, regtrans)

fh_mod <- fh %>% 
  filter(fh_country %in% iri_countries) %>% 
  mutate(country_year = paste(fh_country, year, sep = "-")) %>% 
  select(country_year, status, fh_total)

polity5_fh <- polity5_mod %>% 
  full_join(fh_mod, by = "country_year")

## IAEP_QED_PED_reconciled + fh_polity5
IAEP_QED_PEI_FH_POL5 <- IAEP_QED_PEI_reconciled %>% 
  left_join(polity5_fh, by = "country_year")

# World Bank Indicators (WDI)

iri_WDI <- WDI(country = iri_countries_ios2, indicator = c("NY.GDP.PCAP.KD", 
                                                           "BX.KLT.DINV.CD.WD",
                                                           "FP.CPI.TOTL.ZG",
                                                           "SI.POV.GINI"))
iri_WDI <- iri_WDI %>% 
  rename("GDP per capita (constant 2015 US$)" = "NY.GDP.PCAP.KD",
         "Foreign direct investment, net inflows (BoP, current US$" = "BX.KLT.DINV.CD.WD",
         "Inflation, consumer prices (annual %)" = "FP.CPI.TOTL.ZG",
         "Gini index" = "SI.POV.GINI") %>%
  mutate(country = ifelse(country == "Congo, Rep.", "Congo", country),
         country_year = paste(country, year, sep = "-")) %>% 
  select(!c(iso2c, country, year))

## IAEP_QED_PED_reconciled + fh_polity5
IAEP_QED_PEI_FH_POL5_WB <- IAEP_QED_PEI_FH_POL5 %>% 
  left_join(iri_WDI, by = "country_year")

IAEP_QED_PEI_FH_POL5_WB %>% write_csv("CleanedMergedData/IAEP_QED_PEI_FH_POL5_WB_clean.csv")

